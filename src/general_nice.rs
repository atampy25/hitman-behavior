use std::{convert::TryFrom, fmt::Debug, ops::Deref, sync::Arc};

use inflector::Inflector;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use tryvial::try_fn;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BehaviorTree {
	pub root: ConditionScope
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ConditionScope {
	#[serde(default, skip_serializing_if = "std::ops::Not::not")]
	pub is_end: bool,

	pub conditions: Vec<Condition>,
	pub behaviors: Vec<BehaviorNode>
}

impl Debug for ConditionScope {
	#[try_fn]
	fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		let conds = self
			.conditions
			.iter()
			.map(|c| {
				format!(
					" {}{}{}{}{}",
					if c.modifiers.weak { "WEAK " } else { "" },
					if c.modifiers.abort { "ABORT " } else { "" },
					if c.modifiers.not { "NOT " } else { "" },
					format!("{:?}", c.data)
						.split_once('(')
						.unwrap()
						.1
						.split_once('_')
						.unwrap()
						.1
						.chars()
						.rev()
						.skip(1)
						.collect::<Vec<_>>()
						.into_iter()
						.rev()
						.collect::<String>(),
					if c.assign_to == BehaviorTreeVariable::None {
						"".into()
					} else {
						format!(" -> {:?}", c.assign_to)
					}
				)
			})
			.collect::<Vec<_>>()
			.join(",");

		write!(f, "SCOPE{}{}", if self.is_end { " END" } else { "" }, conds);
		for behavior in self.behaviors.iter().map(|x| format!("\t{x:?}").replace("\n", "\n\t")) {
			write!(f, "\n{behavior}")?;
		}
	}
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum BehaviorNode {
	Scope(ConditionScope),
	Match(EcoString),
	Sequence(Vec<BehaviorNode>),
	Behavior(Behavior)
}

impl Debug for BehaviorNode {
	#[try_fn]
	fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			BehaviorNode::Scope(scope) => write!(f, "{scope:?}")?,

			BehaviorNode::Match(name) => write!(f, "MATCH {name}")?,

			BehaviorNode::Sequence(behaviors) => {
				write!(f, "SEQUENCE",)?;
				for behavior in behaviors.iter().map(|x| format!("\t{x:?}").replace("\n", "\n\t")) {
					write!(f, "\n{behavior}")?;
				}
			}

			BehaviorNode::Behavior(data) => write!(
				f,
				"BEHAVIOR {}",
				format!("{data:?}")
					.split_once('(')
					.unwrap()
					.1
					.split_once('_')
					.unwrap()
					.1
					.chars()
					.rev()
					.skip(1)
					.collect::<Vec<_>>()
					.into_iter()
					.rev()
					.collect::<String>()
			)?
		}
	}
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Condition {
	#[serde(flatten, default, skip_serializing_if = "is_default")]
	pub modifiers: ConditionModifiers,

	#[serde(default, skip_serializing_if = "is_default")]
	pub assign_to: BehaviorTreeVariable,

	#[serde(flatten)]
	pub data: ConditionData
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ConditionModifiers {
	#[serde(default, skip_serializing_if = "std::ops::Not::not")]
	pub not: bool,

	#[serde(default, skip_serializing_if = "std::ops::Not::not")]
	pub abort: bool,

	#[serde(default, skip_serializing_if = "std::ops::Not::not")]
	pub weak: bool
}

impl TryFrom<u32> for ConditionModifiers {
	type Error = BehaviorTreeError;

	#[try_fn]
	fn try_from(value: u32) -> Result<Self, Self::Error> {
		if value & !0b111 != 0 {
			return Err(BehaviorTreeError::UnrecognisedModifiers(value));
		}

		Self {
			not: value & 1 == 1,
			abort: value & 2 == 2,
			weak: value & 4 == 4
		}
	}
}

impl From<ConditionModifiers> for u32 {
	fn from(value: ConditionModifiers) -> Self {
		(if value.not { 1 } else { 0 }) | (if value.abort { 2 } else { 0 }) | (if value.weak { 4 } else { 0 })
	}
}

fn is_default<T: Default + PartialEq>(value: &T) -> bool {
	*value == T::default()
}

impl Condition {
	#[try_fn]
	pub fn from_raw(scene_reference_names: &[EcoString], value: raw::SCondition) -> Result<Self, BehaviorTreeError> {
		Self {
			modifiers: ConditionModifiers::try_from(value.condition_modifiers)?,
			assign_to: BehaviorTreeVariable::from_raw(scene_reference_names, value.assign_to)?,
			data: ConditionData::from_raw(scene_reference_names, value.data.to_owned())?
		}
	}

	pub fn into_raw(self, scene_reference_names: &mut Vec<EcoString>) -> raw::SCondition {
		raw::SCondition {
			condition_modifiers: self.modifiers.into(),
			assign_to: self.assign_to.into_raw(scene_reference_names),
			data: self.data.into_raw(scene_reference_names)
		}
	}
}

#[derive(Clone, PartialEq, Default, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum BehaviorTreeVariable {
	#[default]
	None,
	SceneReference(EcoString),
	Contextual(u32),
	Dynamic(DynamicVariableType)
}

impl Debug for BehaviorTreeVariable {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			BehaviorTreeVariable::None => write!(f, "null"),
			BehaviorTreeVariable::SceneReference(name) => write!(f, "ref {name}"),
			BehaviorTreeVariable::Contextual(index) => write!(f, "var {index}"),
			BehaviorTreeVariable::Dynamic(var_type) => write!(f, "dynamic {var_type:?}")
		}
	}
}

impl BehaviorTreeVariable {
	#[try_fn]
	pub fn from_raw(
		scene_reference_names: &[EcoString],
		value: raw::ZBehaviorTreeVariable
	) -> Result<Self, BehaviorTreeError> {
		match value.r#type {
			EBehaviorTreeVariableType::BTVT_SceneReference => BehaviorTreeVariable::SceneReference(
				scene_reference_names
					.get(value.index as usize)
					.ok_or(BehaviorTreeError::InvalidSceneReference(value.index))?
					.to_owned()
			),

			EBehaviorTreeVariableType::BTVT_Contextual => BehaviorTreeVariable::Contextual(value.index),

			EBehaviorTreeVariableType::BTVT_Dynamic => BehaviorTreeVariable::Dynamic(match value.index {
				x if x as i32 == i32::from(EDynamicVariableType::DV_Me) => DynamicVariableType::Me,
				x if x as i32 == i32::from(EDynamicVariableType::DV_Hitman) => DynamicVariableType::Hitman,
				x if x as i32 == i32::from(EDynamicVariableType::DV_InSight) => DynamicVariableType::InSight,
				x if x as i32 == i32::from(EDynamicVariableType::DV_RecentlyInSight) => {
					DynamicVariableType::RecentlyInSight
				}
				x if x as i32 == i32::from(EDynamicVariableType::DV_Sounds) => DynamicVariableType::Sounds,

				_ => {
					return Err(BehaviorTreeError::UnknownDynamicVariable(value.index));
				}
			}),

			EBehaviorTreeVariableType::BTVT_Invalid if value.index == 0 => BehaviorTreeVariable::None,

			_ => return Err(BehaviorTreeError::UnknownInvalidVariable)
		}
	}

	pub fn into_raw(self, scene_reference_names: &mut Vec<EcoString>) -> raw::ZBehaviorTreeVariable {
		match self {
			BehaviorTreeVariable::None => raw::ZBehaviorTreeVariable {
				r#type: EBehaviorTreeVariableType::BTVT_Invalid,
				index: 0
			},

			BehaviorTreeVariable::SceneReference(name) => {
				let index = scene_reference_names.iter().position(|x| *x == name);

				let index = if let Some(index) = index {
					index as u32
				} else {
					let index = scene_reference_names.len() as u32;
					scene_reference_names.push(name);
					index
				};

				raw::ZBehaviorTreeVariable {
					r#type: EBehaviorTreeVariableType::BTVT_SceneReference,
					index
				}
			}

			BehaviorTreeVariable::Contextual(index) => raw::ZBehaviorTreeVariable {
				r#type: EBehaviorTreeVariableType::BTVT_Contextual,
				index
			},

			BehaviorTreeVariable::Dynamic(var_type) => {
				let index = match var_type {
					DynamicVariableType::Me => i32::from(EDynamicVariableType::DV_Me) as u32,
					DynamicVariableType::Hitman => i32::from(EDynamicVariableType::DV_Hitman) as u32,
					DynamicVariableType::InSight => i32::from(EDynamicVariableType::DV_InSight) as u32,
					DynamicVariableType::RecentlyInSight => i32::from(EDynamicVariableType::DV_RecentlyInSight) as u32,
					DynamicVariableType::Sounds => i32::from(EDynamicVariableType::DV_Sounds) as u32
				};

				raw::ZBehaviorTreeVariable {
					r#type: EBehaviorTreeVariableType::BTVT_Dynamic,
					index
				}
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DynamicVariableType {
	Me,
	Hitman,
	InSight,
	RecentlyInSight,
	Sounds
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum BehaviorTreeError {
	#[error("unknown dynamic variable index {0}")]
	UnknownDynamicVariable(u32),

	#[error("invalid scene reference index {0}")]
	InvalidSceneReference(u32),

	#[error("unknown invalid variable index")]
	UnknownInvalidVariable,

	#[error("invalid type {0:?} for simple behavior")]
	InvalidBehaviorType(ECompiledBehaviorType),

	#[error("unrecognised condition modifiers {0}")]
	UnrecognisedModifiers(u32),

	#[error("no end behavior set")]
	NoEndBehavior,

	#[error("multiple end behaviors set")]
	MultipleEndBehaviors,

	#[error("behaviors and sequences can only be the last child of a scope")]
	InvalidNesting
}

#[derive(Debug, thiserror::Error)]
pub enum BehaviorTreeParseError {
	#[error("unrecognised keyword: {0}")]
	UnrecognisedKeyword(String),

	#[error("failed to deserialize behavior '{name}': {source}")]
	BehaviorDeserializationError {
		name: String,
		#[source]
		source: serde_json::Error
	},

	#[error("failed to deserialize condition '{name}': {source}")]
	ConditionDeserializationError {
		name: String,
		#[source]
		source: serde_json::Error
	},

	#[error("invalid variable string: {variable}")]
	InvalidVariable { variable: String },

	#[error("invalid var index: {index}")]
	InvalidVarIndex {
		index: String,
		#[source]
		source: std::num::ParseIntError
	},

	#[error("missing colon in argument: {argument}")]
	MissingColon { argument: String },

	#[error("unexpected end of input")]
	UnexpectedEndOfInput,

	#[error("indentation must be tabs, not spaces")]
	InvalidIndentation
}

impl BehaviorTree {
	#[try_fn]
	pub fn from_raw(raw: raw::ZCompiledBehaviorTree) -> Result<Self, BehaviorTreeError> {
		#[try_fn]
		fn process_behavior(
			end: &Option<Arc<raw::SBehavior>>,
			scene_reference_names: &[EcoString],
			behavior: &Arc<raw::SBehavior>
		) -> Result<BehaviorNode, BehaviorTreeError> {
			match behavior.as_ref() {
				raw::SBehavior::Match(m) => BehaviorNode::Match(m.name.to_owned()),

				raw::SBehavior::ConditionScope(scope) => BehaviorNode::Scope(ConditionScope {
					is_end: end.as_ref().map(|end| Arc::ptr_eq(behavior, end)).unwrap_or(false),
					behaviors: collect_children(end, scene_reference_names, &scope.child, &scope.sibling)?,
					conditions: scope
						.conditions
						.iter()
						.map(|x| Condition::from_raw(scene_reference_names, x.deref().to_owned()))
						.collect::<Result<_, _>>()?
				}),

				raw::SBehavior::Sequence(seq) => BehaviorNode::Sequence(
					seq.children
						.iter()
						.map(|x| process_behavior(end, scene_reference_names, x))
						.collect::<Result<_, _>>()?
				),

				_ => BehaviorNode::Behavior(Behavior::from_raw(scene_reference_names, behavior.deref().to_owned())?)
			}
		}

		#[try_fn]
		fn collect_children(
			tree_end: &Option<Arc<raw::SBehavior>>,
			scene_reference_names: &[EcoString],
			start: &Option<Arc<raw::SBehavior>>,
			end: &Option<Arc<raw::SBehavior>>
		) -> Result<Vec<BehaviorNode>, BehaviorTreeError> {
			let mut children = vec![];
			let mut current = start;

			while current.as_ref().map(Arc::as_ptr) != end.as_ref().map(Arc::as_ptr)
				&& let Some(behavior) = current
			{
				children.push(process_behavior(tree_end, scene_reference_names, behavior)?);

				current = match behavior.as_ref() {
					raw::SBehavior::Match(m) => &m.sibling,

					raw::SBehavior::ConditionScope(scope) => &scope.sibling,

					_ => &None
				};
			}

			children
		}

		Self {
			root: ConditionScope {
				is_end: raw.end.is_none(),
				behaviors: collect_children(&raw.end, &raw.scene_reference_names, &raw.root.child, &raw.root.sibling)?,
				conditions: raw
					.root
					.conditions
					.iter()
					.map(|x| Condition::from_raw(&raw.scene_reference_names, x.deref().to_owned()))
					.collect::<Result<_, _>>()?
			}
		}
	}

	#[try_fn]
	pub fn into_raw(self) -> Result<raw::ZCompiledBehaviorTree, BehaviorTreeError> {
		let mut scene_reference_names = vec![];
		let mut end = None;

		let root_end = self.root.is_end;

		#[try_fn]
		fn process_behavior(
			end: &mut Option<Arc<raw::SBehavior>>,
			is_root: bool,
			root_end: bool,
			behavior: BehaviorNode,
			sibling: Option<&Arc<raw::SBehavior>>,
			scene_reference_names: &mut Vec<EcoString>
		) -> Result<Arc<raw::SBehavior>, BehaviorTreeError> {
			match behavior {
				BehaviorNode::Match(name) => Arc::new(raw::SBehavior::Match(raw::SBehaviorMatch {
					name,
					sibling: sibling.cloned()
				})),

				BehaviorNode::Scope(scope) => {
					let is_end = scope.is_end;
					let child = link_children(end, root_end, scope.behaviors, sibling, scene_reference_names)?;

					let scope = Arc::new(raw::SBehavior::ConditionScope(raw::SConditionScope {
						conditions: scope
							.conditions
							.into_iter()
							.map(|c| Arc::new(c.into_raw(scene_reference_names)))
							.collect(),
						child,
						sibling: sibling.cloned()
					}));

					if !is_root && is_end {
						if end.is_some() || root_end {
							return Err(BehaviorTreeError::MultipleEndBehaviors);
						} else {
							*end = Some(scope.clone());
						}
					}

					scope
				}

				BehaviorNode::Sequence(behaviors) => Arc::new(raw::SBehavior::Sequence(raw::SBehaviorSequence {
					children: behaviors
						.into_iter()
						.map(|b| process_behavior(end, false, root_end, b, None, scene_reference_names))
						.collect::<Result<_, _>>()?
				})),

				BehaviorNode::Behavior(data) => Arc::new(data.into_raw(scene_reference_names))
			}
		}

		#[try_fn]
		fn link_children(
			end: &mut Option<Arc<raw::SBehavior>>,
			root_end: bool,
			children: Vec<BehaviorNode>,
			sibling: Option<&Arc<raw::SBehavior>>,
			scene_reference_names: &mut Vec<EcoString>
		) -> Result<Option<Arc<raw::SBehavior>>, BehaviorTreeError> {
			let mut processed: Vec<Arc<raw::SBehavior>> = vec![];

			for child in children.into_iter().rev() {
				if !processed.is_empty()
					&& let BehaviorNode::Behavior(_) | BehaviorNode::Sequence(_) = child
				{
					return Err(BehaviorTreeError::InvalidNesting);
				}

				processed.insert(
					0,
					process_behavior(
						end,
						false,
						root_end,
						child,
						processed.first().or(sibling),
						scene_reference_names
					)?
				);
			}

			processed.into_iter().next()
		}

		let root = Arc::into_inner(process_behavior(
			&mut end,
			true,
			root_end,
			BehaviorNode::Scope(self.root),
			None,
			&mut scene_reference_names
		)?)
		.unwrap();

		let root_scope = match root {
			raw::SBehavior::ConditionScope(s) => s,
			_ => unreachable!()
		};

		raw::ZCompiledBehaviorTree {
			scene_reference_names,
			end: if root_end {
				None
			} else {
				Some(end.ok_or(BehaviorTreeError::NoEndBehavior)?)
			},
			root: root_scope
		}
	}

	pub fn as_pseudocode(&self) -> String {
		format!("{:?}", self.root)
	}

	#[try_fn]
	pub fn from_pseudocode(input: &str) -> Result<Self, BehaviorTreeParseError> {
		struct Line {
			indent: usize,
			content: String
		}

		let lines: Vec<Line> = input
			.lines()
			.filter(|line| !line.trim().is_empty())
			.filter(|line| !line.trim().starts_with("//"))
			.map(|line| {
				let indent = line.chars().take_while(|c| *c == '\t').count();
				if line.starts_with(' ') {
					Err(BehaviorTreeParseError::InvalidIndentation)
				} else {
					Ok(Line {
						indent,
						content: line.trim().to_string()
					})
				}
			})
			.collect::<Result<_, _>>()?;

		#[try_fn]
		fn parse_variable(s: &str) -> Result<BehaviorTreeVariable, BehaviorTreeParseError> {
			if s == "null" {
				BehaviorTreeVariable::None
			} else if let Some(rest) = s.strip_prefix("dynamic ") {
				let var_type = match rest {
					"Me" => DynamicVariableType::Me,
					"Hitman" => DynamicVariableType::Hitman,
					"InSight" => DynamicVariableType::InSight,
					"RecentlyInSight" => DynamicVariableType::RecentlyInSight,
					"Sounds" => DynamicVariableType::Sounds,

					_ => {
						return Err(BehaviorTreeParseError::InvalidVariable { variable: s.into() });
					}
				};
				BehaviorTreeVariable::Dynamic(var_type)
			} else if let Some(rest) = s.strip_prefix("var ") {
				BehaviorTreeVariable::Contextual(rest.parse().map_err(|e| BehaviorTreeParseError::InvalidVarIndex {
					index: rest.to_string(),
					source: e
				})?)
			} else if let Some(rest) = s.strip_prefix("ref ") {
				BehaviorTreeVariable::SceneReference(rest.into())
			} else {
				return Err(BehaviorTreeParseError::InvalidVariable { variable: s.into() });
			}
		}

		#[try_fn]
		fn parse_condition_line(line: &str) -> Result<Condition, BehaviorTreeParseError> {
			let mut weak = false;
			let mut abort = false;
			let mut not = false;
			let mut rest = line;

			if let Some(r) = rest.strip_prefix("WEAK ") {
				weak = true;
				rest = r;
			}

			if let Some(r) = rest.strip_prefix("ABORT ") {
				abort = true;
				rest = r;
			}

			if let Some(r) = rest.strip_prefix("NOT ") {
				not = true;
				rest = r;
			}

			let (condition, assign_to) = if let Some((condition, var)) = rest.split_once(" -> ") {
				(condition, parse_variable(var)?)
			} else {
				(rest.trim(), BehaviorTreeVariable::None)
			};

			let (name, args) = if let Some((name, args)) = condition.split_once(" {") {
				(name, Some(&args[0..args.len() - 1]))
			} else {
				(condition, None)
			};

			let data: ConditionData = serde_json::from_value(json!({
				"type": name,
				"data": if let Some(args) = args { parse_args_to_json(args)? } else { Value::Null }
			}))
			.map_err(|e| BehaviorTreeParseError::ConditionDeserializationError {
				name: name.into(),
				source: e
			})?;

			Condition {
				modifiers: ConditionModifiers { weak, abort, not },
				assign_to,
				data
			}
		}

		#[try_fn]
		fn parse_args_to_json(args: &str) -> Result<Value, BehaviorTreeParseError> {
			let mut obj = serde_json::Map::new();

			let pairs = split_by_comma(args);

			for pair in pairs {
				if pair.trim().is_empty() {
					continue;
				}

				let (key, value) = pair
					.split_once(": ")
					.ok_or_else(|| BehaviorTreeParseError::MissingColon {
						argument: pair.to_string()
					})?;

				obj.insert(key.to_camel_case(), parse_value_to_json(value)?);
			}

			obj.into()
		}

		fn parse_value_to_json(value: &str) -> Result<serde_json::Value, BehaviorTreeParseError> {
			if value == "null"
				|| value.starts_with("dynamic ")
				|| value.starts_with("var ")
				|| value.starts_with("ref ")
			{
				let var = parse_variable(value)?;
				Ok(serde_json::to_value(var).unwrap())
			} else if value == "true" {
				Ok(serde_json::Value::Bool(true))
			} else if value == "false" {
				Ok(serde_json::Value::Bool(false))
			} else if let Ok(i) = value.parse::<i64>() {
				Ok(serde_json::Value::Number(i.into()))
			} else if let Ok(f) = value.parse::<f64>() {
				Ok(serde_json::Value::Number(serde_json::Number::from_f64(f).unwrap()))
			} else if value.starts_with('"') && value.ends_with('"') {
				Ok(serde_json::Value::String(value[1..value.len() - 1].to_string()))
			} else {
				// Assume it's an enum variant
				Ok(serde_json::Value::String(value.to_string()))
			}
		}

		fn split_by_comma(s: &str) -> Vec<String> {
			let mut result = Vec::new();
			let mut current = String::new();
			let mut brace_depth = 0;
			let mut in_quotes = false;

			for ch in s.chars() {
				match ch {
					'"' => {
						in_quotes = !in_quotes;
						current.push(ch);
					}
					'{' if !in_quotes => {
						brace_depth += 1;
						current.push(ch);
					}
					'}' if !in_quotes => {
						brace_depth -= 1;
						current.push(ch);
					}
					',' if !in_quotes && brace_depth == 0 => {
						result.push(current.trim().to_string());
						current.clear();
					}
					_ => {
						current.push(ch);
					}
				}
			}

			if !current.trim().is_empty() {
				result.push(current.trim().to_string());
			}

			result
		}

		#[try_fn]
		fn parse_scope(lines: &[Line], idx: &mut usize) -> Result<ConditionScope, BehaviorTreeParseError> {
			if *idx >= lines.len() {
				return Err(BehaviorTreeParseError::UnexpectedEndOfInput);
			}

			let line = &lines[*idx];

			let (conditions, is_end) = if let Some(rest) = line.content.strip_prefix("SCOPE END") {
				(rest.trim(), true)
			} else if let Some(rest) = line.content.strip_prefix("SCOPE") {
				(rest.trim(), false)
			} else {
				unreachable!()
			};

			let conditions = split_by_comma(conditions)
				.iter()
				.map(|s| parse_condition_line(s.trim()))
				.collect::<Result<Vec<_>, _>>()?;

			*idx += 1;

			let mut behaviors = vec![];
			while *idx < lines.len() && lines[*idx].indent > line.indent {
				behaviors.push(parse_behavior_node(lines, idx)?);
			}

			ConditionScope {
				is_end,
				conditions,
				behaviors
			}
		}

		#[try_fn]
		fn parse_behavior_node(lines: &[Line], idx: &mut usize) -> Result<BehaviorNode, BehaviorTreeParseError> {
			if *idx >= lines.len() {
				return Err(BehaviorTreeParseError::UnexpectedEndOfInput);
			}

			let line = &lines[*idx];

			if line.content.starts_with("SCOPE") {
				BehaviorNode::Scope(parse_scope(lines, idx)?)
			} else if line.content.starts_with("MATCH ") {
				*idx += 1;
				BehaviorNode::Match(line.content.strip_prefix("MATCH ").unwrap().into())
			} else if line.content.starts_with("SEQUENCE") {
				let current_indent = line.indent;
				*idx += 1;

				let mut children = vec![];
				while *idx < lines.len() && lines[*idx].indent > current_indent {
					children.push(parse_behavior_node(lines, idx)?);
				}

				BehaviorNode::Sequence(children)
			} else if line.content.starts_with("BEHAVIOR ") {
				let behavior = line.content.strip_prefix("BEHAVIOR ").unwrap().trim();

				let (name, args) = if let Some((name, args)) = behavior.split_once(" {") {
					(name, Some(&args[0..args.len() - 1]))
				} else {
					(behavior, None)
				};

				let behavior: Behavior = serde_json::from_value(json!({
					"type": name,
					"data": if let Some(args) = args { parse_args_to_json(args)? } else { Value::Null }
				}))
				.map_err(|e| BehaviorTreeParseError::BehaviorDeserializationError {
					name: name.into(),
					source: e
				})?;

				*idx += 1;
				BehaviorNode::Behavior(behavior)
			} else {
				return Err(BehaviorTreeParseError::UnrecognisedKeyword(line.content.to_owned()));
			}
		}

		let mut idx = 0;
		let root = parse_scope(&lines, &mut idx)?;

		BehaviorTree { root }
	}
}
