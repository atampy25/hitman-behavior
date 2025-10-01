use std::sync::Arc;

use hitman_bin1::types::{array::TArrayRef, pointers::WithZeroNull};
use serde::{Deserialize, Serialize};
use tryvial::try_fn;

#[derive(Debug, Clone, PartialEq, Bin1Serialize, Bin1Deserialize, Serialize, Deserialize)]
pub struct ZBehaviorTreeVariable {
	#[serde(rename = "m_eType")]
	pub r#type: EBehaviorTreeVariableType,

	#[serde(rename = "m_nIndex")]
	pub index: u32
}

#[derive(Debug, Clone, PartialEq, Bin1Serialize, Bin1Deserialize, Serialize, Deserialize)]
pub struct SBehaviorBase {
	#[serde(rename = "eBehaviorType")]
	pub behavior_type: ECompiledBehaviorType
}

#[derive(Debug, Clone, PartialEq, Bin1Serialize, Bin1Deserialize, Serialize, Deserialize)]
pub struct SConditionBase {
	#[serde(rename = "eConditionType")]
	pub condition_type: ECompiledConditionType,

	#[serde(rename = "nConditionModifiers")]
	pub condition_modifiers: u32,

	#[serde(rename = "assignTo")]
	pub assign_to: ZBehaviorTreeVariable
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SCondition {
	#[serde(rename = "nConditionModifiers")]
	pub condition_modifiers: u32,

	#[serde(rename = "assignTo")]
	pub assign_to: ZBehaviorTreeVariable,

	#[serde(flatten)]
	pub data: SConditionData
}

impl Aligned for SCondition {
	const ALIGNMENT: usize = 4;
}

impl Bin1Serialize for SCondition {
	fn alignment(&self) -> usize {
		Self::ALIGNMENT
	}

	#[try_fn]
	fn write(&self, ser: &mut Bin1Serializer) -> Result<(), SerializeError> {
		let ty = self.data.as_type();
		ty.write(ser)?;
		self.condition_modifiers.write(ser)?;
		self.assign_to.write(ser)?;
		self.data.write_data(ser)?;
	}

	fn resolve(&self, ser: &mut Bin1Serializer) -> Result<(), SerializeError> {
		self.data.resolve_data(ser)
	}
}

impl Bin1Deserialize for SCondition {
	const SIZE: usize = 0; // cannot use with bare Vec<T>

	#[try_fn]
	fn read(de: &mut Bin1Deserializer) -> Result<Self, DeserializeError> {
		let base = SConditionBase::read(de)?;
		let data = SConditionData::read_data(de, base.condition_type)?;

		Self {
			condition_modifiers: base.condition_modifiers,
			assign_to: base.assign_to,
			data
		}
	}
}

fn serialize_arc_behavior<S: serde::Serializer>(value: &Arc<SBehavior>, serializer: S) -> Result<S::Ok, S::Error> {
	value.as_ref().serialize(serializer)
}

fn serialize_option_arc_behavior<S: serde::Serializer>(
	value: &Option<Arc<SBehavior>>,
	serializer: S
) -> Result<S::Ok, S::Error> {
	value.as_deref().serialize(serializer)
}

fn serialize_vec_arc_behavior<S: serde::Serializer>(
	value: &[Arc<SBehavior>],
	serializer: S
) -> Result<S::Ok, S::Error> {
	serializer.collect_seq(value.iter().map(|x| x.as_ref()))
}

fn serialize_vec_arc_condition<S: serde::Serializer>(
	value: &[Arc<SCondition>],
	serializer: S
) -> Result<S::Ok, S::Error> {
	serializer.collect_seq(value.iter().map(|x| x.as_ref()))
}

#[try_fn]
fn deserialize_arc_behavior<'de, D: serde::Deserializer<'de>>(deserializer: D) -> Result<Arc<SBehavior>, D::Error> {
	SBehavior::deserialize(deserializer)?.into()
}

#[try_fn]
fn deserialize_option_arc_behavior<'de, D: serde::Deserializer<'de>>(
	deserializer: D
) -> Result<Option<Arc<SBehavior>>, D::Error> {
	Option::<SBehavior>::deserialize(deserializer)?.map(Into::into)
}

#[try_fn]
fn deserialize_vec_arc_behavior<'de, D: serde::Deserializer<'de>>(
	deserializer: D
) -> Result<Vec<Arc<SBehavior>>, D::Error> {
	Vec::<SBehavior>::deserialize(deserializer)?
		.into_iter()
		.map(Into::into)
		.collect()
}

#[try_fn]
fn deserialize_vec_arc_condition<'de, D: serde::Deserializer<'de>>(
	deserializer: D
) -> Result<Vec<Arc<SCondition>>, D::Error> {
	Vec::<SCondition>::deserialize(deserializer)?
		.into_iter()
		.map(Into::into)
		.collect()
}

#[derive(Debug, Clone, PartialEq, Bin1Serialize, Bin1Deserialize, Serialize, Deserialize)]
pub struct SConditionScope {
	#[serde(
		serialize_with = "serialize_option_arc_behavior",
		deserialize_with = "deserialize_option_arc_behavior"
	)]
	#[serde(rename = "pChild")]
	pub child: Option<Arc<SBehavior>>,

	#[serde(
		serialize_with = "serialize_option_arc_behavior",
		deserialize_with = "deserialize_option_arc_behavior"
	)]
	#[serde(rename = "pSibling")]
	pub sibling: Option<Arc<SBehavior>>,

	#[bin1(as = "TArrayRef::<Arc<SCondition>>")]
	#[serde(
		serialize_with = "serialize_vec_arc_condition",
		deserialize_with = "deserialize_vec_arc_condition"
	)]
	pub conditions: Vec<Arc<SCondition>>
}

#[derive(Debug, Clone, PartialEq, Bin1Serialize, Bin1Deserialize, Serialize, Deserialize)]
pub struct SBehaviorMatch {
	#[serde(
		serialize_with = "serialize_option_arc_behavior",
		deserialize_with = "deserialize_option_arc_behavior"
	)]
	#[serde(rename = "pSibling")]
	pub sibling: Option<Arc<SBehavior>>,

	#[serde(rename = "sName")]
	pub name: EcoString
}

#[derive(Debug, Clone, PartialEq, Bin1Serialize, Bin1Deserialize, Serialize, Deserialize)]
pub struct SBehaviorSequence {
	#[bin1(as = "TArrayRef::<Arc<SBehavior>>")]
	#[serde(
		serialize_with = "serialize_vec_arc_behavior",
		deserialize_with = "deserialize_vec_arc_behavior"
	)]
	pub children: Vec<Arc<SBehavior>>
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ZCompiledBehaviorTree {
	#[serde(rename = "m_root")]
	pub root: SConditionScope,

	#[serde(
		serialize_with = "serialize_option_arc_behavior",
		deserialize_with = "deserialize_option_arc_behavior"
	)]
	#[serde(rename = "m_pEnd")]
	pub end: Option<Arc<SBehavior>>,

	#[serde(rename = "m_sceneReferenceNames")]
	pub scene_reference_names: Vec<EcoString>
}

impl Aligned for ZCompiledBehaviorTree {
	const ALIGNMENT: usize = 8;
}

impl Bin1Serialize for ZCompiledBehaviorTree {
	fn alignment(&self) -> usize {
		Self::ALIGNMENT
	}

	fn write(&self, ser: &mut Bin1Serializer) -> Result<(), SerializeError> {
		ECompiledBehaviorType::BT_ConditionScope.write(ser)?;
		ser.align_to(SConditionScope::ALIGNMENT);
		self.root.write(ser)?;
		WithZeroNull::Ser(&self.end).write(ser)?;
		TArrayRef::Ser(&self.scene_reference_names).write(ser)
	}

	fn resolve(&self, ser: &mut Bin1Serializer) -> Result<(), SerializeError> {
		self.root.resolve(ser)?;
		WithZeroNull::Ser(&self.end).resolve(ser)?;
		TArrayRef::Ser(&self.scene_reference_names).resolve(ser)
	}
}

impl Bin1Deserialize for ZCompiledBehaviorTree {
	const SIZE: usize =
		u32::SIZE + SConditionScope::SIZE + WithZeroNull::De::<SBehavior>::SIZE + TArrayRef::De::<EcoString>::SIZE;

	#[try_fn]
	fn read(de: &mut Bin1Deserializer) -> Result<Self, DeserializeError> {
		let _ = de.read_u32()?; // behavior type, always ConditionScope
		de.align_to(SConditionScope::ALIGNMENT)?;

		Self {
			root: SConditionScope::read(de)?,
			end: WithZeroNull::De::<SBehavior>::read(de)?.into(),
			scene_reference_names: TArrayRef::De::<EcoString>::read(de)?.into()
		}
	}
}
