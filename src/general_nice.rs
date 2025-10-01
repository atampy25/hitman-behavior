use std::{fmt::Debug, ops::Deref, sync::Arc};

use serde::{Deserialize, Serialize};
use tryvial::try_fn;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BehaviorTree {
	pub root: ConditionScope
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
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
					if c.weak { "WEAK " } else { "" },
					if c.abort { "ABORT " } else { "" },
					if c.not { "NOT " } else { "" },
					format!("{:?}", c.data)
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
	#[serde(default, skip_serializing_if = "std::ops::Not::not")]
	pub not: bool,

	#[serde(default, skip_serializing_if = "std::ops::Not::not")]
	pub abort: bool,

	#[serde(default, skip_serializing_if = "std::ops::Not::not")]
	pub weak: bool,

	#[serde(default, skip_serializing_if = "is_default")]
	pub assign_to: BehaviorTreeVariable,

	#[serde(flatten)]
	pub data: ConditionData
}

fn is_default<T: Default + PartialEq>(value: &T) -> bool {
	*value == T::default()
}

impl Condition {
	#[try_fn]
	pub fn from_raw(scene_reference_names: &[EcoString], value: raw::SCondition) -> Result<Self, BehaviorTreeError> {
		Self {
			not: value.condition_modifiers & 1 == 1,
			abort: value.condition_modifiers & 2 == 2,
			weak: value.condition_modifiers & 4 == 4,
			assign_to: BehaviorTreeVariable::from_raw(scene_reference_names, value.assign_to)?,
			data: ConditionData::from_raw(scene_reference_names, value.data.to_owned())?
		}
	}

	pub fn into_raw(self, scene_reference_names: &mut Vec<EcoString>) -> raw::SCondition {
		raw::SCondition {
			condition_modifiers: (if self.not { 1 } else { 0 })
				| (if self.abort { 2 } else { 0 })
				| (if self.weak { 4 } else { 0 }),
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

	#[error("no end behavior set")]
	NoEndBehavior,

	#[error("multiple end behaviors set")]
	MultipleEndBehaviors
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
}
