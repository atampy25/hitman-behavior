#![allow(non_camel_case_types, unused, clippy::ptr_arg)]

pub mod raw {
	use hitman_bin1::game::h2::{EBehaviorTreeVariableType, ECompiledBehaviorType, ECompiledConditionType};

	include!("general.rs");

	include!(concat!(env!("OUT_DIR"), "/h2.rs"));
}

use hitman_bin1::game::h2::{EBehaviorTreeVariableType, EDynamicVariableType};

include!("general_nice.rs");

include!(concat!(env!("OUT_DIR"), "/h2_nice.rs"));
