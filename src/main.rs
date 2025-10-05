use std::fs;

use hitman_behavior::{h1, h3};
use hitman_bin1::{de::deserialize, ser::serialize};

fn main() {
	match std::env::args().nth(1).as_deref() {
		Some("h1") => match std::env::args().nth(2).as_deref() {
			Some("convert-json") => {
				let input_path = std::env::args().nth(3).expect("3rd argument must be input path");
				let output_path = std::env::args().nth(4).expect("4th argument must be output path");

				let raw = std::thread::Builder::new()
					.stack_size(64 * 1024 * 1024)
					.spawn(|| {
						deserialize(&fs::read(input_path).unwrap())
							.map_err(|e| format!("{e}"))
							.unwrap()
					})
					.unwrap()
					.join()
					.unwrap();

				let tree = h1::BehaviorTree::from_raw(raw).map_err(|e| format!("{e}")).unwrap();

				fs::write(output_path, serde_json::to_vec_pretty(&tree).unwrap()).unwrap();
			}

			Some("convert-txt") => {
				let input_path = std::env::args().nth(3).expect("3rd argument must be input path");
				let output_path = std::env::args().nth(4).expect("4th argument must be output path");

				let raw = std::thread::Builder::new()
					.stack_size(64 * 1024 * 1024)
					.spawn(|| {
						deserialize(&fs::read(input_path).unwrap())
							.map_err(|e| format!("{e}"))
							.unwrap()
					})
					.unwrap()
					.join()
					.unwrap();

				let tree = h1::BehaviorTree::from_raw(raw).map_err(|e| format!("{e}")).unwrap();

				fs::write(output_path, format!("{:?}", tree.root)).unwrap();
			}

			Some("generate-json") => {
				let input_path = std::env::args().nth(3).expect("3rd argument must be input path");
				let output_path = std::env::args().nth(4).expect("4th argument must be output path");

				let tree: h1::BehaviorTree = serde_json::from_slice(&fs::read(input_path).unwrap()).unwrap();

				let raw = tree.into_raw().map_err(|e| format!("{e}")).unwrap();

				fs::write(
					output_path,
					std::thread::Builder::new()
						.stack_size(64 * 1024 * 1024)
						.spawn(move || serialize(&raw).map_err(|e| format!("{e}")).unwrap())
						.unwrap()
						.join()
						.unwrap()
				)
				.unwrap();
			}

			Some("generate-txt") => {
				let input_path = std::env::args().nth(3).expect("3rd argument must be input path");
				let output_path = std::env::args().nth(4).expect("4th argument must be output path");

				let tree = h1::BehaviorTree::from_pseudocode(&fs::read_to_string(input_path).unwrap()).unwrap();

				let raw = tree.into_raw().map_err(|e| format!("{e}")).unwrap();

				fs::write(
					output_path,
					std::thread::Builder::new()
						.stack_size(64 * 1024 * 1024)
						.spawn(move || serialize(&raw).map_err(|e| format!("{e}")).unwrap())
						.unwrap()
						.join()
						.unwrap()
				)
				.unwrap();
			}

			_ => panic!("2nd argument must be convert-json, convert-txt, generate-json or generate-txt")
		},

		Some("h3") => match std::env::args().nth(2).as_deref() {
			Some("convert-json") => {
				let input_path = std::env::args().nth(3).expect("3rd argument must be input path");
				let output_path = std::env::args().nth(4).expect("4th argument must be output path");

				let raw = std::thread::Builder::new()
					.stack_size(64 * 1024 * 1024)
					.spawn(|| {
						deserialize(&fs::read(input_path).unwrap())
							.map_err(|e| format!("{e}"))
							.unwrap()
					})
					.unwrap()
					.join()
					.unwrap();

				let tree = h3::BehaviorTree::from_raw(raw).map_err(|e| format!("{e}")).unwrap();

				fs::write(output_path, serde_json::to_vec_pretty(&tree).unwrap()).unwrap();
			}

			Some("convert-txt") => {
				let input_path = std::env::args().nth(3).expect("3rd argument must be input path");
				let output_path = std::env::args().nth(4).expect("4th argument must be output path");

				let raw = std::thread::Builder::new()
					.stack_size(64 * 1024 * 1024)
					.spawn(|| {
						deserialize(&fs::read(input_path).unwrap())
							.map_err(|e| format!("{e}"))
							.unwrap()
					})
					.unwrap()
					.join()
					.unwrap();

				let tree = h3::BehaviorTree::from_raw(raw).map_err(|e| format!("{e}")).unwrap();

				fs::write(output_path, format!("{:?}", tree.root)).unwrap();
			}

			Some("generate-json") => {
				let input_path = std::env::args().nth(3).expect("3rd argument must be input path");
				let output_path = std::env::args().nth(4).expect("4th argument must be output path");

				let tree: h3::BehaviorTree = serde_json::from_slice(&fs::read(input_path).unwrap()).unwrap();

				let raw = tree.into_raw().map_err(|e| format!("{e}")).unwrap();

				fs::write(
					output_path,
					std::thread::Builder::new()
						.stack_size(64 * 1024 * 1024)
						.spawn(move || serialize(&raw).map_err(|e| format!("{e}")).unwrap())
						.unwrap()
						.join()
						.unwrap()
				)
				.unwrap();
			}

			Some("generate-txt") => {
				let input_path = std::env::args().nth(3).expect("3rd argument must be input path");
				let output_path = std::env::args().nth(4).expect("4th argument must be output path");

				let tree = h3::BehaviorTree::from_pseudocode(&fs::read_to_string(input_path).unwrap()).unwrap();

				let raw = tree.into_raw().map_err(|e| format!("{e}")).unwrap();

				fs::write(
					output_path,
					std::thread::Builder::new()
						.stack_size(64 * 1024 * 1024)
						.spawn(move || serialize(&raw).map_err(|e| format!("{e}")).unwrap())
						.unwrap()
						.join()
						.unwrap()
				)
				.unwrap();
			}

			_ => panic!("2nd argument must be convert-json, convert-txt, generate-json or generate-txt")
		},

		_ => panic!("1st argument must be h1 or h3")
	}
}
