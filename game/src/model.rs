use aligned_vec::{AVec, ConstAlign};
use alloc::vec::Vec;
use psp::sys::*;

pub struct Model {
    meshes: Vec<Mesh>,
    pub position: [f32; 3],
    pub rotation: [f32; 3],
    pub scale: [f32; 3],
}

impl Model {
    pub fn draw(&self) {
        unsafe {
            sceGumLoadIdentity();
            sceGumTranslate(&ScePspFVector3 {
                x: self.position[0],
                y: self.position[1],
                z: self.position[2],
            });
            sceGumRotateXYZ(&ScePspFVector3 {
                x: self.rotation[0],
                y: self.rotation[1],
                z: self.rotation[2],
            });
            sceGumScale(&ScePspFVector3 {
                x: self.scale[0],
                y: self.scale[1],
                z: self.scale[2],
            });
        }

        for mesh in &self.meshes {
            mesh.draw();
        }
    }
}

impl From<psp_file_formats::model::File> for Model {
    fn from(value: psp_file_formats::model::File) -> Self {
        Self {
            meshes: value.meshes.into_iter().map(Mesh::from).collect(),
            position: [0.0, 0.0, 0.0],
            rotation: [0.0, 0.0, 0.0],
            scale: [1.0, 1.0, 1.0],
        }
    }
}

struct Mesh {
    primitive_type: GuPrimitive,
    vertex_type: VertexType,
    vertex_count: i32,
    index_buffer: AVec<u8, ConstAlign<16>>,
    vertex_buffer: AVec<u8, ConstAlign<16>>,
}

impl Mesh {
    pub fn draw(&self) {
        let index_buffer = if self.index_buffer.is_empty() {
            core::ptr::null()
        } else {
            self.index_buffer.as_ptr() as _
        };

        unsafe {
            sceGuMaterial(LightComponent::AMBIENT, 0xffffffff);
            sceGumDrawArray(
                self.primitive_type,
                self.vertex_type,
                self.vertex_count,
                index_buffer,
                self.vertex_buffer.as_ptr() as _,
            );
        }
    }
}

impl From<psp_file_formats::model::Mesh> for Mesh {
    fn from(value: psp_file_formats::model::Mesh) -> Self {
        let primitive_type = match value.primitive_type {
            0 => GuPrimitive::Points,
            1 => GuPrimitive::Lines,
            2 => GuPrimitive::LineStrip,
            3 => GuPrimitive::Triangles,
            4 => GuPrimitive::TriangleStrip,
            5 => GuPrimitive::TriangleFan,
            _ => panic!(),
        };
        let vertex_type = VertexType::from_bits_truncate(value.vertex_type as _);
        Self {
            primitive_type,
            vertex_type,
            vertex_count: value.vertex_count as _,
            index_buffer: value.index_buffer,
            vertex_buffer: value.vertex_buffer,
        }
    }
}
