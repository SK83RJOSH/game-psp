use aligned_vec::{AVec, ConstAlign};
use alloc::vec::Vec;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct File {
    pub meshes: Vec<Mesh>,
}

#[derive(Serialize, Deserialize)]
pub struct Mesh {
    pub primitive_type: u32,
    pub vertex_type: u32,
    pub vertex_count: u32,
    pub index_buffer: AVec<u8, ConstAlign<16>>,
    pub vertex_buffer: AVec<u8, ConstAlign<16>>,
}
