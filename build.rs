#![feature(trim_prefix_suffix)]

use std::{collections::VecDeque, env, fs, path::PathBuf};

use anyhow::Result;
use codegen::{Block, Scope};
use inflector::Inflector;
use lazy_regex::{regex_captures, regex_captures_iter, regex_is_match, regex_replace};

pub enum Member {
	Padding(usize),
	Field(String, String, String)
}

fn parse_enums(enums: &str) -> Vec<(String, Vec<(String, i64)>)> {
	let enums = enums.replace('\r', "");
	regex_captures_iter!(
		r#"\(\*g_Enums\)\["(.*?)"] = \{\n((?:\s+\{ -?\d+, ".*?" \},\n)*)\s+\};"#,
		&enums
	)
	.map(|entry| {
		let members = entry[2]
			.lines()
			.map(|x| regex_captures!(r#"\{\s*(-?\d+),\s*"(.+?)"\s*\}"#, x))
			.filter_map(|x| x.map(|x| (x.2.to_owned(), x.1.parse::<i64>().unwrap())))
			.collect::<Vec<_>>();

		(entry[1].to_owned(), members)
	})
	.collect()
}

fn parse_classes(classes: &str) -> Vec<(String, Vec<Member>)> {
	let classes = classes.replace('\r', "");
	classes
		.split("};\n\n")
		.filter_map(|section| {
			let section = section
				.split('\n')
				.filter(|x| !x.is_empty() && !x.trim_start().starts_with("//"))
				.collect::<Vec<_>>()
				.join("\n");

			if section.starts_with("class") {
				let (name, members) = section.split_once("\n{\npublic:").unwrap();

				let name = name.trim_start_matches("class ");
				let name = regex_replace!(r" */\*.*?\*/ *", name, "");
				let name = regex_replace!(r"(?s) :.*", &name, "").into_owned();

				let members = members
					.lines()
					.map(|x| x.trim())
					.filter(|x| !x.is_empty() && !x.starts_with("static") && !x.starts_with("bool operator"))
					.map(|member| {
						if let Some((_, amount)) = regex_captures!(r"uint8_t _pad\w+\[(\d+)\] \{\};", member) {
							Member::Padding(amount.parse().unwrap())
						} else {
							let (_, type_name, field_name) = regex_captures!(r"^(.+) (.+);.*$", member).unwrap();

							let original_field_name = field_name;

							let field_name = if field_name.len() != 2 && !regex_is_match!(r"m\d+", field_name) {
								field_name.to_snake_case()
							} else {
								field_name.into()
							};

							let field_name = if let Some((start, rest)) = field_name.split_once('_')
								&& start.len() == 1 && !["x", "y", "z"].contains(&start)
								&& !rest.is_empty()
							{
								rest.into()
							} else {
								field_name
							};

							let field_name = if let Some((start, rest)) = field_name.split_once('_')
								&& start.len() == 1 && !["x", "y", "z"].contains(&start)
								&& !rest.is_empty()
							{
								rest.into()
							} else {
								field_name
							};

							let field_name = match field_name.as_str() {
								"type" => "r#type",
								"ref" => "reference",
								"move" => "r#move",
								x => x
							};

							fn process_type_name(type_name: &str) -> String {
								match type_name {
									"int8_t" => "i8".into(),
									"int16" => "i16".into(),
									"int32" => "i32".into(),
									"int64" => "i64".into(),

									"char" => "u8".into(),
									"uint8" => "u8".into(),
									"uint8_t" => "u8".into(),
									"uint16" => "u16".into(),
									"uint32" => "u32".into(),
									"uint64" => "u64".into(),

									"float32" => "f32".into(),
									"float64" => "f64".into(),

									"bool" => "bool".into(),

									"ZString" => "EcoString".into(),

									x if x.starts_with("TArray<") => format!(
										"Vec<{}>",
										process_type_name(
											&x.trim_prefix("TArray<")
												.chars()
												.rev()
												.skip(1)
												.collect::<Vec<_>>()
												.into_iter()
												.rev()
												.collect::<String>()
										)
									),

									x if x.starts_with("TPair<") => format!(
										"({}, {})",
										process_type_name(regex_captures!(r"TPair<(.*), *(.*)>", x).unwrap().1),
										process_type_name(regex_captures!(r"TPair<(.*), *(.*)>", x).unwrap().2)
									),

									x => x.into()
								}
							}

							let type_name = process_type_name(type_name);

							Member::Field(
								original_field_name.to_owned(),
								field_name.to_owned(),
								type_name.to_owned()
							)
						}
					})
					.collect::<Vec<_>>();

				Some((name.to_owned(), members))
			} else {
				None
			}
		})
		.collect()
}

fn generate(scope: &mut Scope, scope_nice: &mut Scope, game: &str, classes_code: &str, enums_code: &str) {
	scope.import("hitman_bin1::ser", "Aligned");
	scope.import("hitman_bin1::ser", "Bin1Serialize");
	scope.import("hitman_bin1::ser", "Bin1Serializer");
	scope.import("hitman_bin1::ser", "SerializeError");
	scope.import("hitman_bin1::de", "Bin1Deserialize");
	scope.import("hitman_bin1::de", "Bin1Deserializer");
	scope.import("hitman_bin1::de", "DeserializeError");

	let mut classes = parse_classes(classes_code);
	let enums = parse_enums(enums_code);

	// Special cased
	classes.remove(classes.iter().position(|x| x.0 == "ZBehaviorTreeVariable").unwrap());

	let mut class_queue = VecDeque::new();

	for ty in classes
		.iter()
		.filter(|(ty, members)| {
			(ty.starts_with("SCondition_") || ty.starts_with("SBehavior_"))
				&& !members.iter().any(|m| {
					if let Member::Field(orig_name, _, ty) = m {
						ty.starts_with("TEntityRef") || orig_name.ends_with(']')
					} else {
						false
					}
				})
		})
		.map(|(ty, _)| ty)
		.cloned()
		.collect::<Vec<_>>()
	{
		if let Some(pos) = classes.iter().position(|x| x.0 == *ty) {
			class_queue.push_back(classes.remove(pos));
		}
	}

	let mut conditions = vec![];
	let mut behaviors = vec![];

	while let Some((name, members)) = class_queue.pop_front() {
		for member in &members {
			if let Member::Field(_, _, ty) = member {
				if ty == "EcoString" {
					scope.import("ecow", "EcoString");
					scope_nice.import("ecow", "EcoString");
				} else {
					let mut tys = vec![ty.trim_start_matches("Vec<").trim_end_matches(">")];
					for ty in tys.clone() {
						if ty.starts_with('(') {
							let (first, second) = ty
								.trim_start_matches('(')
								.trim_end_matches(')')
								.split_once(',')
								.unwrap();

							tys.push(first.trim());
							tys.push(second.trim());
						}
					}

					for ty in tys {
						if let Some(pos) = classes.iter().position(|x| x.0 == *ty) {
							class_queue.push_back(classes.remove(pos));
						} else if ty.starts_with('E') {
							scope.import(&format!("hitman_bin1::game::{game}"), ty);
							scope_nice.import(&format!("hitman_bin1::game::{game}"), ty);
						}
					}
				}
			}
		}

		if name.starts_with("SCondition_") {
			conditions.push(name.clone());
		} else if name.starts_with("SBehavior_") {
			behaviors.push(name.clone());
		}

		let cls = scope
			.new_struct(&name)
			.derive("Debug")
			.derive("Clone")
			.derive("PartialEq")
			.derive("Bin1Serialize")
			.derive("Bin1Deserialize")
			.derive("serde::Serialize")
			.derive("serde::Deserialize")
			.vis("pub");

		let cls_nice = scope_nice
			.new_struct(&regex_replace!(r"^S", &name, ""))
			.derive("Debug")
			.derive("Clone")
			.derive("PartialEq")
			.derive("serde::Serialize")
			.derive("serde::Deserialize")
			.r#macro(r#"#[serde(rename_all = "camelCase")]"#)
			.vis("pub");

		let mut padding = 0;

		let mut last_field = None;

		for member in &members {
			match member {
				Member::Padding(amount) => {
					padding = *amount;
				}

				Member::Field(orig_name, field_name, type_name) => {
					last_field = Some({
						let field = cls
							.new_field(field_name, type_name)
							.vis("pub")
							.annotation(format!("#[serde(rename = \"{}\")]", orig_name));

						if padding != 0 {
							field.annotation(format!("#[bin1(pad = {padding})]"));
						}

						cls_nice
							.new_field(
								field_name,
								match type_name.as_ref() {
									"ZBehaviorTreeVariable" => "BehaviorTreeVariable",
									x => x
								}
							)
							.vis("pub");

						field
					});

					padding = 0;
				}
			}
		}

		if padding != 0 {
			last_field.unwrap().annotation(format!("#[bin1(pad_end = {padding})]"));
		}

		scope_nice
			.new_impl(&regex_replace!(r"^S", &name, ""))
			.new_fn("from_raw")
			.vis("pub")
			.arg("scene_reference_names", "&[EcoString]")
			.arg("value", format!("raw::{name}"))
			.ret("Result<Self, BehaviorTreeError>")
			.push_block({
				let mut block = Block::new("Ok(Self");
				for member in &members {
					if let Member::Field(_, field, ty) = member {
						let line = if ty == "ZBehaviorTreeVariable" {
							format!("{field}: BehaviorTreeVariable::from_raw(scene_reference_names, value.{field})?,")
						} else if ty.starts_with("Vec<") {
							let inner = ty.trim_start_matches("Vec<").trim_end_matches('>');
							if inner == "ZBehaviorTreeVariable" {
								format!(
									"{field}: value.{field}.into_iter().map(|x| \
									 {inner}::from_raw(scene_reference_names, x)).collect(),"
								)
							} else {
								format!("{field}: value.{field},")
							}
						} else {
							format!("{field}: value.{field},")
						};
						block.line(line);
					}
				}
				block.after(")");
				block
			});

		scope_nice
			.new_impl(&regex_replace!(r"^S", &name, ""))
			.new_fn("into_raw")
			.vis("pub")
			.arg_self()
			.arg("scene_reference_names", "&mut Vec<EcoString>")
			.ret(format!("raw::{name}"))
			.push_block({
				let mut block = Block::new(&format!("raw::{name}"));
				for member in &members {
					if let Member::Field(_, field, ty) = member {
						let line = if ty == "ZBehaviorTreeVariable" {
							format!("{field}: self.{field}.into_raw(scene_reference_names),")
						} else if ty.starts_with("Vec<") {
							let inner = ty.trim_start_matches("Vec<").trim_end_matches('>');
							if inner == "ZBehaviorTreeVariable" {
								format!(
									"{field}: self.{field}.into_iter().map(|x| \
									 x.into_raw(scene_reference_names)).collect(),"
								)
							} else {
								format!("{field}: self.{field},")
							}
						} else {
							format!("{field}: self.{field},")
						};
						block.line(line);
					}
				}
				block
			});
	}

	behaviors.sort_by_cached_key(|ty| {
		enums
			.iter()
			.find(|(name, _)| name == "ECompiledBehaviorType")
			.unwrap()
			.1
			.iter()
			.position(|(name, _)| *name == format!("BT_{}", ty.split_once('_').unwrap().1))
			.unwrap()
	});

	conditions.sort_by_cached_key(|ty| {
		enums
			.iter()
			.find(|(name, _)| name == "ECompiledConditionType")
			.unwrap()
			.1
			.iter()
			.position(|(name, _)| *name == format!("CT_{}", ty.split_once('_').unwrap().1))
			.unwrap()
	});

	let behavior = scope
		.new_enum("SBehavior")
		.derive("Debug")
		.derive("Clone")
		.derive("PartialEq")
		.derive("serde::Serialize")
		.derive("serde::Deserialize")
		.vis("pub")
		.r#macro(r#"#[serde(tag = "eBehaviorType")]"#);

	for ty in ["SConditionScope", "SBehaviorMatch", "SBehaviorSequence"] {
		let variant = ty.trim_start_matches('S').trim_start_matches("Behavior");
		behavior
			.new_variant(variant)
			.tuple(ty)
			.annotation(format!(r#"#[serde(rename = "BT_{variant}")]"#));
	}

	for ty in &behaviors {
		behavior
			.new_variant(ty.split_once('_').unwrap().1)
			.tuple(ty)
			.annotation(format!(r#"#[serde(rename = "BT_{}")]"#, ty.split_once('_').unwrap().1));
	}

	scope
		.new_impl("SBehavior")
		.new_fn("as_type")
		.vis("pub")
		.arg_ref_self()
		.ret("ECompiledBehaviorType")
		.push_block({
			let mut block = Block::new("match self");
			for ty in ["ConditionScope", "Match", "Sequence"] {
				block.line(format!("Self::{ty}(_) => ECompiledBehaviorType::BT_{ty},"));
			}
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(_) => ECompiledBehaviorType::BT_{ty},"));
			}
			block
		});

	scope
		.new_impl("SBehavior")
		.impl_trait("Aligned")
		.associate_const("ALIGNMENT", "usize", "4", "");

	let ser_impl = scope.new_impl("SBehavior").impl_trait("Bin1Serialize");
	ser_impl
		.new_fn("alignment")
		.arg_ref_self()
		.ret("usize")
		.line("Self::ALIGNMENT");
	ser_impl
		.new_fn("write")
		.arg_ref_self()
		.arg("ser", "&mut Bin1Serializer")
		.ret("Result<(), SerializeError>")
		.line("self.as_type().write(ser)?;")
		.push_block({
			let mut block = Block::new("match self");
			for ty in ["ConditionScope", "Match", "Sequence"] {
				block.line(format!("Self::{ty}(data) => data.write(ser),"));
			}
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(data) => data.write(ser),"));
			}
			block
		});
	ser_impl
		.new_fn("resolve")
		.arg_ref_self()
		.arg("ser", "&mut Bin1Serializer")
		.ret("Result<(), SerializeError>")
		.push_block({
			let mut block = Block::new("match self");
			for ty in ["ConditionScope", "Match", "Sequence"] {
				block.line(format!("Self::{ty}(data) => data.resolve(ser),"));
			}
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(data) => data.resolve(ser),"));
			}
			block
		});

	let de_impl = scope.new_impl("SBehavior").impl_trait("Bin1Deserialize");
	de_impl.associate_const("SIZE", "usize", "0", ""); // cannot use with bare Vec<T>
	de_impl
		.new_fn("read")
		.arg("de", "&mut Bin1Deserializer")
		.ret("Result<Self, DeserializeError>")
		.line("let ty = ECompiledBehaviorType::read(de)?;")
		.push_block({
			let mut block = Block::new("match ty");
			for ty in ["SConditionScope", "SBehaviorMatch", "SBehaviorSequence"] {
				let variant = ty.trim_start_matches('S').trim_start_matches("Behavior");
				block.line(format!(
					"ECompiledBehaviorType::BT_{variant} => Ok(Self::{variant}({ty}::read(de)?)),"
				));
			}
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!(
					"ECompiledBehaviorType::BT_{ty} => Ok(Self::{ty}(SBehavior_{ty}::read(de)?)),"
				));
			}
			block.line("_ => Err(DeserializeError::InvalidEnumValue(ty as i64))");
			block
		});

	scope_nice.import(&format!("hitman_bin1::game::{game}"), "ECompiledBehaviorType");

	let behavior_nice = scope_nice
		.new_enum("Behavior")
		.derive("Debug")
		.derive("Clone")
		.derive("PartialEq")
		.derive("serde::Serialize")
		.derive("serde::Deserialize")
		.vis("pub")
		.r#macro(r#"#[serde(tag = "type", content = "data")]"#);

	for ty in &behaviors {
		behavior_nice
			.new_variant(ty.split_once('_').unwrap().1)
			.tuple(&regex_replace!(r"^S", ty, ""));
	}

	scope_nice
		.new_impl("Behavior")
		.new_fn("as_type")
		.vis("pub")
		.arg_ref_self()
		.ret("ECompiledBehaviorType")
		.push_block({
			let mut block = Block::new("match self");
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(_) => ECompiledBehaviorType::BT_{ty},"));
			}
			block
		});

	scope_nice
		.new_impl("Behavior")
		.new_fn("from_raw")
		.vis("pub")
		.arg("scene_reference_names", "&[EcoString]")
		.arg("value", "raw::SBehavior")
		.ret("Result<Self, BehaviorTreeError>")
		.push_block({
			let mut block = Block::new("Ok(match value");
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!(
					"raw::SBehavior::{ty}(value) => Self::{ty}(Behavior_{ty}::from_raw(scene_reference_names, \
					 value)?),"
				));
			}
			block.line("_ => return Err(BehaviorTreeError::InvalidBehaviorType(value.as_type()))");
			block.after(")");
			block
		});

	scope_nice
		.new_impl("Behavior")
		.new_fn("into_raw")
		.vis("pub")
		.arg_self()
		.arg("scene_reference_names", "&mut Vec<EcoString>")
		.ret("raw::SBehavior")
		.push_block({
			let mut block = Block::new("match self");
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!(
					"Self::{ty}(data) => raw::SBehavior::{ty}(data.into_raw(scene_reference_names)),"
				));
			}
			block
		});

	let condition_data = scope
		.new_enum("SConditionData")
		.derive("Debug")
		.derive("Clone")
		.derive("PartialEq")
		.derive("serde::Serialize")
		.derive("serde::Deserialize")
		.vis("pub")
		.r#macro(r#"#[serde(tag = "eConditionType")]"#);

	for ty in &conditions {
		condition_data
			.new_variant(ty.split_once('_').unwrap().1)
			.tuple(ty)
			.annotation(format!(r#"#[serde(rename = "CT_{}")]"#, ty.split_once('_').unwrap().1));
	}

	scope
		.new_impl("SConditionData")
		.new_fn("as_type")
		.vis("pub")
		.arg_ref_self()
		.ret("ECompiledConditionType")
		.push_block({
			let mut block = Block::new("match self");
			for ty in &conditions {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(_) => ECompiledConditionType::CT_{ty},"));
			}
			block
		});

	scope
		.new_impl("SConditionData")
		.new_fn("write_data")
		.arg_ref_self()
		.arg("ser", "&mut Bin1Serializer")
		.ret("Result<(), SerializeError>")
		.push_block({
			let mut block = Block::new("match self");
			for ty in &conditions {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(data) => data.write(ser),"));
			}
			block
		});

	scope
		.new_impl("SConditionData")
		.new_fn("resolve_data")
		.arg_ref_self()
		.arg("ser", "&mut Bin1Serializer")
		.ret("Result<(), SerializeError>")
		.push_block({
			let mut block = Block::new("match self");
			for ty in &conditions {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(data) => data.resolve(ser),"));
			}
			block
		});

	scope
		.new_impl("SConditionData")
		.new_fn("read_data")
		.arg("de", "&mut Bin1Deserializer")
		.arg("ty", "ECompiledConditionType")
		.ret("Result<Self, DeserializeError>")
		.push_block({
			let mut block = Block::new("match ty");
			for ty in &conditions {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!(
					"ECompiledConditionType::CT_{ty} => Ok(Self::{ty}(SCondition_{ty}::read(de)?)),"
				));
			}
			block.line("_ => Err(DeserializeError::InvalidEnumValue(ty as i64))");
			block
		});

	scope_nice.import(&format!("hitman_bin1::game::{game}"), "ECompiledConditionType");

	let condition_data_nice = scope_nice
		.new_enum("ConditionData")
		.derive("Debug")
		.derive("Clone")
		.derive("PartialEq")
		.derive("serde::Serialize")
		.derive("serde::Deserialize")
		.vis("pub")
		.r#macro(r#"#[serde(tag = "type", content = "data")]"#);

	for ty in &conditions {
		condition_data_nice
			.new_variant(ty.split_once('_').unwrap().1)
			.tuple(&regex_replace!(r"^S", ty, ""));
	}

	scope_nice
		.new_impl("ConditionData")
		.new_fn("as_type")
		.vis("pub")
		.arg_ref_self()
		.ret("ECompiledConditionType")
		.push_block({
			let mut block = Block::new("match self");
			for ty in &conditions {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(_) => ECompiledConditionType::CT_{ty},"));
			}
			block
		});

	scope_nice
		.new_impl("ConditionData")
		.new_fn("from_raw")
		.vis("pub")
		.arg("scene_reference_names", "&[EcoString]")
		.arg("value", "raw::SConditionData")
		.ret("Result<Self, BehaviorTreeError>")
		.push_block({
			let mut block = Block::new("Ok(match value");
			for ty in &conditions {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!(
					"raw::SConditionData::{ty}(value) => Self::{ty}(Condition_{ty}::from_raw(scene_reference_names, \
					 value)?),"
				));
			}
			block.after(")");
			block
		});

	scope_nice
		.new_impl("ConditionData")
		.new_fn("into_raw")
		.vis("pub")
		.arg_self()
		.arg("scene_reference_names", "&mut Vec<EcoString>")
		.ret("raw::SConditionData")
		.push_block({
			let mut block = Block::new("match self");
			for ty in &conditions {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!(
					"Self::{ty}(data) => raw::SConditionData::{ty}(data.into_raw(scene_reference_names)),"
				));
			}
			block
		});
}

pub fn main() -> Result<()> {
	let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());

	// let mut h1 = Scope::new();

	// generate(&mut h1, &fs::read_to_string("h1.txt")?);

	// fs::write(out_dir.join("h1.rs"), h1.to_string())?;

	// let mut h2 = Scope::new();

	// generate(&mut h2, &fs::read_to_string("h2.txt")?);

	// fs::write(out_dir.join("h2.rs"), h2.to_string())?;

	let mut h3 = Scope::new();
	let mut h3_nice = Scope::new();

	generate(
		&mut h3,
		&mut h3_nice,
		"h3",
		&fs::read_to_string("h3.txt")?,
		&fs::read_to_string("h3-enums.txt")?
	);

	fs::write(out_dir.join("h3.rs"), h3.to_string())?;
	fs::write(out_dir.join("h3_nice.rs"), h3_nice.to_string())?;

	println!("cargo::rerun-if-changed=build.rs");
	// println!("cargo::rerun-if-changed=h1.txt");
	// println!("cargo::rerun-if-changed=h1-enums.txt");
	// println!("cargo::rerun-if-changed=h2.txt");
	// println!("cargo::rerun-if-changed=h2-enums.txt");
	println!("cargo::rerun-if-changed=h3.txt");
	println!("cargo::rerun-if-changed=h3-enums.txt");

	Ok(())
}
