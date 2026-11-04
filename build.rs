use std::{collections::VecDeque, env, fs, path::PathBuf};

use anyhow::Result;
use codegen::{Block, Scope};
use inflector::Inflector;
use lazy_regex::{regex_captures, regex_is_match, regex_replace};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
struct Enum {
	name: String,
	size: usize,
	values: Vec<Variant>
}

#[derive(Serialize, Deserialize, Clone)]
struct Variant {
	name: String,
	value: i64
}

#[derive(Serialize, Deserialize, Clone)]
struct Struct {
	name: String,
	alignment: Option<usize>,
	fields: Vec<Field>
}

#[derive(Serialize, Deserialize, Clone)]
struct Field {
	name: String,

	#[serde(rename = "type")]
	ty: Option<String>,

	#[serde(rename = "padEnd")]
	pad_end: Option<usize>
}

#[derive(Serialize, Deserialize, Clone)]
struct Types {
	structs: Vec<Struct>,
	enums: Vec<Enum>
}

#[derive(Serialize, Deserialize, Clone)]
struct RustEnum {
	type_id: String,
	rust_name: String,
	size: usize,
	values: Vec<RustVariant>
}

#[derive(Serialize, Deserialize, Clone)]
struct RustVariant {
	variant_name: String,
	rust_name: String,
	value: i64
}

#[derive(Serialize, Deserialize, Clone)]
struct RustStruct {
	type_id: String,
	rust_name: String,
	alignment: Option<usize>,
	fields: Vec<RustField>
}

#[derive(Serialize, Deserialize, Clone)]
struct RustField {
	field_name: String,
	rust_name: String,

	#[serde(rename = "type")]
	ty: String,

	pad_end: Option<usize>
}

#[derive(Serialize, Deserialize, Clone)]
struct RustTypes {
	structs: Vec<RustStruct>,
	enums: Vec<RustEnum>
}

fn process_types(types: Types) -> RustTypes {
	RustTypes {
		structs: types
			.structs
			.into_par_iter()
			.filter_map(|s| {
				Some(RustStruct {
					rust_name: s.name.replace('.', "_"),
					alignment: if s.name == "ZGuid" { Some(4) } else { s.alignment },
					fields: s
						.fields
						.into_iter()
						.map(|f| {
							Some(RustField {
								rust_name: if f.name == "m_nNeighbourBits" {
									"n_neighbour_bits".into()
								} else {
									let field_name = &f.name;

									let field_name = if (field_name.len() != 2 && !regex_is_match!(r"m\d+", field_name))
										|| field_name.chars().next().unwrap().is_uppercase()
									{
										field_name.to_snake_case()
									} else {
										field_name.into()
									};

									let field_name = if let Some((start, rest)) = field_name.split_once('_')
										&& start.len() == 1
										&& !["x", "y", "z"].contains(&start)
										&& !rest.is_empty()
									{
										rest.into()
									} else {
										field_name
									};

									let field_name = if let Some((start, rest)) = field_name.split_once('_')
										&& start.len() == 1
										&& !["x", "y", "z"].contains(&start)
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

									field_name.replace("_i_d", "_id")
								},
								ty: if s.name == "SVoxelSpaceDataHeader" && f.name == "aReserved" {
									"[u8; 8]".into()
								} else {
									fn process_type_name(
										struct_name: &str,
										field_name: &str,
										type_name: &str
									) -> String {
										match type_name {
											"int8" => "i8".into(),
											"int16" => "i16".into(),
											"int32" => "i32".into(),
											"int64" => "i64".into(),

											"char" | "uint8" => "u8".into(),
											"uint16" => "u16".into(),
											"uint32" => "u32".into(),
											"uint64" => "u64".into(),

											"float32" => "f32".into(),
											"float64" => "f64".into(),

											"bool" => "bool".into(),

											"ZString" => "EcoString".into(),

											"ZResourcePtr" => "TResourcePtr".into(),
											x if x.starts_with("TResourcePtr<") => "TResourcePtr".into(),
											"TResourcePtr" if struct_name == "ZEditorEnumValue" => {
												"ZRuntimeResourceID".into()
											}

											x if x.starts_with("TArray<") => format!(
												"Vec<{}>",
												process_type_name(
													struct_name,
													field_name,
													&x["TArray<".len()..]
														.chars()
														.rev()
														.skip(1)
														.collect::<Vec<_>>()
														.into_iter()
														.rev()
														.collect::<String>()
												)
											),

											x if x.starts_with("TFixedArray<") => format!(
												"[{}; {}]",
												process_type_name(
													struct_name,
													field_name,
													regex_captures!(r"TFixedArray<(.*), *(.*)>", x).unwrap().1
												),
												regex_captures!(r"TFixedArray<(.*), *(.*)>", x)
													.unwrap()
													.2
													.parse::<usize>()
													.unwrap()
											),

											x if x.starts_with("TPair<") => format!(
												"({}, {})",
												process_type_name(
													struct_name,
													field_name,
													regex_captures!(r"TPair<(.*), *(.*)>", x).unwrap().1
												),
												process_type_name(
													struct_name,
													field_name,
													regex_captures!(r"TPair<(.*), *(.*)>", x).unwrap().2
												)
											),

											x if x.starts_with("ZHMPtrLen<") => format!(
												"ZHMPtrLen<{}>",
												process_type_name(
													struct_name,
													field_name,
													&x["ZHMPtrLen<".len()..]
														.chars()
														.rev()
														.skip(1)
														.collect::<Vec<_>>()
														.into_iter()
														.rev()
														.collect::<String>()
												)
											),

											"ZRuntimeResourceID"
												if field_name == "externalSceneRuntimeResourceIDs"
													|| [
														"SBlobsConfigResourceEntry",
														"SContractConfigResourceEntry",
														"SVideoDatabaseEntry"
													]
													.iter()
													.any(|&x| x == struct_name) =>
											{
												"ZRuntimeResourceIDNoFixup".into()
											}

											x => x.replace('.', "_")
										}
									}

									process_type_name(&s.name, &f.name, &f.ty?)
								},
								field_name: f.name,
								pad_end: f.pad_end
							})
						})
						.collect::<Option<_>>()?,
					type_id: s.name
				})
			})
			.collect(),
		enums: types
			.enums
			.into_par_iter()
			.map(|e| {
				let common_prefix = e
					.values
					.iter()
					.map(|v| v.name.split_once("_").map(|x| x.0))
					.collect::<Option<Vec<_>>>()
					.and_then(|prefixes| {
						let first = prefixes[0];
						if prefixes.iter().all(|x| *x == first) {
							Some(first.to_owned())
						} else {
							None
						}
					});

				RustEnum {
					rust_name: e.name.replace('.', "_"),
					type_id: e.name,
					size: e.size,
					values: e
						.values
						.into_iter()
						.map(|v| RustVariant {
							rust_name: {
								let mut name = v.name.as_str();
								if let Some(common_prefix) = &common_prefix {
									name = v.name.trim_start_matches(common_prefix).trim_start_matches('_');
								}

								if name.starts_with(|c: char| c.is_ascii_digit()) || name.to_lowercase() == "self" {
									format!(
										"_{}",
										if name.chars().all(|c| c.is_uppercase() || !c.is_ascii_alphabetic()) {
											name.to_pascal_case()
										} else {
											name.into()
										}
									)
								} else {
									if name.chars().all(|c| c.is_uppercase() || !c.is_ascii_alphabetic()) {
										name.to_pascal_case()
									} else {
										name.into()
									}
								}
							},
							variant_name: v.name,
							value: v.value
						})
						.collect()
				}
			})
			.collect()
	}
}

fn generate(scope: &mut Scope, scope_nice: &mut Scope, game: &str, types_json: &str) {
	scope.import("glacier_bin1::ser", "Aligned");
	scope.import("glacier_bin1::ser", "Bin1Serialize");
	scope.import("glacier_bin1::ser", "Bin1Serializer");
	scope.import("glacier_bin1::ser", "SerializeError");
	scope.import("glacier_bin1::de", "Bin1Sized");
	scope.import("glacier_bin1::de", "Bin1Deserialize");
	scope.import("glacier_bin1::de", "Bin1Deserializer");
	scope.import("glacier_bin1::de", "DeserializeError");

	let RustTypes { mut structs, enums } = process_types(serde_json::from_str(types_json).unwrap());

	let mut struct_queue = VecDeque::new();

	for ty in structs
		.iter()
		.filter(|ty| {
			(ty.type_id.starts_with("SCondition_") || ty.type_id.starts_with("SBehavior_"))
				&& !ty.fields.iter().any(|f| f.ty.starts_with("TEntityRef"))
		})
		.map(|ty| ty.type_id.to_owned())
		.collect::<Vec<_>>()
	{
		if let Some(pos) = structs.iter().position(|x| x.type_id == ty) {
			struct_queue.push_back(structs.remove(pos));
		}
	}

	let mut conditions = vec![];
	let mut behaviors = vec![];

	while let Some(RustStruct {
		type_id,
		rust_name,
		alignment,
		fields
	}) = struct_queue.pop_front()
	{
		for field in &fields {
			if field.ty == "EcoString" {
				scope.import("ecow", "EcoString");
				scope_nice.import("ecow", "EcoString");
			} else {
				let mut tys = vec![field.ty.trim_start_matches("Vec<").trim_end_matches(">")];
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
					if let Some(pos) = structs.iter().position(|x| x.type_id == *ty) {
						struct_queue.push_back(structs.remove(pos));
					} else if ty.split('_').any(|x| x.starts_with('E')) {
						scope.import(format!("glacier_bin1::game::{game}"), ty);
						scope_nice.import(format!("glacier_bin1::game::{game}"), ty);
					}
				}
			}
		}

		if type_id.starts_with("SCondition_") {
			conditions.push(type_id.clone());
		} else if type_id.starts_with("SBehavior_") {
			behaviors.push(type_id.clone());
		}

		let cls = scope
			.new_struct(&rust_name)
			.derive("Debug")
			.derive("Clone")
			.derive("PartialEq")
			.derive("Bin1Serialize")
			.derive("Bin1Deserialize")
			.derive("serde::Serialize")
			.derive("serde::Deserialize")
			.r#macro("#[bin1(crate = glacier_bin1)]")
			.vis("pub");

		if let Some(alignment) = alignment {
			cls.r#macro(format!("#[bin1(alignment = {alignment})]"));
		}

		let nice_name = regex_replace!(r"^S", &rust_name, "");

		let cls_nice = scope_nice
			.new_struct(&nice_name)
			.derive("Debug")
			.derive("Clone")
			.derive("PartialEq")
			.derive("serde::Serialize")
			.derive("serde::Deserialize")
			.r#macro(r#"#[serde(rename_all = "camelCase")]"#)
			.vis("pub");

		for RustField {
			rust_name,
			field_name,
			ty,
			pad_end
		} in fields.iter()
		{
			let field = cls
				.new_field(rust_name, ty)
				.vis("pub")
				.annotation(format!(r#"#[serde(rename = "{field_name}")]"#));

			if let Some(pad_end) = pad_end {
				field.annotation(format!(r#"#[bin1(pad_end = {pad_end})]"#));
			}

			cls_nice
				.new_field(
					rust_name,
					match ty.as_ref() {
						"ZBehaviorTreeVariable" => "BehaviorTreeVariable",
						x => x
					}
				)
				.vis("pub");
		}

		scope_nice
			.new_impl(&nice_name)
			.new_fn("from_raw")
			.vis("pub")
			.arg("scene_reference_names", "&[EcoString]")
			.arg("value", format!("raw::{rust_name}"))
			.ret("Result<Self, BehaviorTreeError>")
			.push_block({
				let mut block = Block::new("Ok(Self");
				for RustField {
					rust_name: field, ty, ..
				} in &fields
				{
					let line = if ty == "ZBehaviorTreeVariable" {
						format!("{field}: BehaviorTreeVariable::from_raw(scene_reference_names, value.{field})?,")
					} else if ty.starts_with("Vec<") {
						let inner = ty.trim_start_matches("Vec<").trim_end_matches('>');
						if inner == "ZBehaviorTreeVariable" {
							format!(
								"{field}: value.{field}.into_iter().map(|x| {inner}::from_raw(scene_reference_names, \
								 x)).collect(),"
							)
						} else {
							format!("{field}: value.{field},")
						}
					} else {
						format!("{field}: value.{field},")
					};
					block.line(line);
				}
				block.after(")");
				block
			});

		scope_nice
			.new_impl(&nice_name)
			.new_fn("into_raw")
			.vis("pub")
			.arg_self()
			.arg("scene_reference_names", "&mut Vec<EcoString>")
			.ret(format!("raw::{rust_name}"))
			.push_block({
				let mut block = Block::new(format!("raw::{rust_name}"));
				for RustField {
					rust_name: field, ty, ..
				} in &fields
				{
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

				block
			});
	}

	behaviors.sort_by_cached_key(|ty| {
		enums
			.iter()
			.find(|e| e.type_id == "ECompiledBehaviorType")
			.unwrap()
			.values
			.iter()
			.position(|e| e.variant_name == format!("BT_{}", ty.split_once('_').unwrap().1))
			.unwrap()
	});

	conditions.sort_by_cached_key(|ty| {
		enums
			.iter()
			.find(|e| e.type_id == "ECompiledConditionType")
			.unwrap()
			.values
			.iter()
			.position(|e| e.variant_name == format!("CT_{}", ty.split_once('_').unwrap().1))
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
				block.line(format!("Self::{ty}(_) => ECompiledBehaviorType::{ty},"));
			}
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!("Self::{ty}(_) => ECompiledBehaviorType::{ty},"));
			}
			block
		});

	scope
		.new_impl("SBehavior")
		.impl_trait("Aligned")
		.associate_const("ALIGNMENT", "usize", "4", "");

	scope
		.new_impl("SBehavior")
		.impl_trait("Bin1Sized")
		.associate_const("SIZE", "usize", "0", "");

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
					"ECompiledBehaviorType::{variant} => Ok(Self::{variant}({ty}::read(de)?)),"
				));
			}
			for ty in &behaviors {
				let ty = ty.split_once('_').unwrap().1;
				block.line(format!(
					"ECompiledBehaviorType::{ty} => Ok(Self::{ty}(SBehavior_{ty}::read(de)?)),"
				));
			}
			block.line("_ => Err(DeserializeError::InvalidEnumValue(ty.0 as i64))");
			block
		});

	scope_nice.import(format!("glacier_bin1::game::{game}"), "ECompiledBehaviorType");

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
				block.line(format!("Self::{ty}(_) => ECompiledBehaviorType::{ty},"));
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
				block.line(format!("Self::{ty}(_) => ECompiledConditionType::{ty},"));
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
					"ECompiledConditionType::{ty} => Ok(Self::{ty}(SCondition_{ty}::read(de)?)),"
				));
			}
			block.line("_ => Err(DeserializeError::InvalidEnumValue(ty.0 as i64))");
			block
		});

	scope_nice.import(format!("glacier_bin1::game::{game}"), "ECompiledConditionType");

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
				block.line(format!("Self::{ty}(_) => ECompiledConditionType::{ty},"));
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

	let mut h1 = Scope::new();
	let mut h1_nice = Scope::new();

	generate(&mut h1, &mut h1_nice, "h1", &fs::read_to_string("h1-ZHMTypes.json")?);

	fs::write(out_dir.join("h1.rs"), h1.to_string())?;
	fs::write(out_dir.join("h1_nice.rs"), h1_nice.to_string())?;

	let mut h2 = Scope::new();
	let mut h2_nice = Scope::new();

	generate(&mut h2, &mut h2_nice, "h2", &fs::read_to_string("h2-ZHMTypes.json")?);

	fs::write(out_dir.join("h2.rs"), h2.to_string())?;
	fs::write(out_dir.join("h2_nice.rs"), h2_nice.to_string())?;

	let mut h3 = Scope::new();
	let mut h3_nice = Scope::new();

	generate(&mut h3, &mut h3_nice, "h3", &fs::read_to_string("h3-ZHMTypes.json")?);

	fs::write(out_dir.join("h3.rs"), h3.to_string())?;
	fs::write(out_dir.join("h3_nice.rs"), h3_nice.to_string())?;

	println!("cargo::rerun-if-changed=build.rs");
	println!("cargo::rerun-if-changed=h1-ZHMTypes.json");
	println!("cargo::rerun-if-changed=h2-ZHMTypes.json");
	println!("cargo::rerun-if-changed=h3-ZHMTypes.json");

	Ok(())
}
