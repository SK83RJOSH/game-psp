use alloc::vec::Vec;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub struct File {
    pub meshes: Vec<Mesh>,
}

#[derive(Serialize, Deserialize)]
pub struct Mesh {
    pub primitive_type: u32,
    pub vertex_type: u32,
    pub vertex_count: u32,
    pub index_buffer: Vec<u8>,
    pub vertex_buffer: Vec<u8>,
}
