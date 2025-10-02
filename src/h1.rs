#![allow(non_camel_case_types, unused, clippy::ptr_arg)]

pub mod raw {
	use hitman_bin1::game::h1::{EBehaviorTreeVariableType, ECompiledBehaviorType, ECompiledConditionType};

	include!("general.rs");

	include!(concat!(env!("OUT_DIR"), "/h1.rs"));
}

use hitman_bin1::game::h1::{EBehaviorTreeVariableType, EDynamicVariableType};

include!("general_nice.rs");

include!(concat!(env!("OUT_DIR"), "/h1_nice.rs"));
