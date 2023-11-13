#[cfg(feature = "psp")]
use crate::psp::*;

pub use crate::psp::GuPrimitive;
pub use crate::psp::GuStateFlags;
pub use crate::psp::GuTexWrapMode;
pub use crate::psp::TextureFilter;
pub use crate::psp::TexturePixelFormat;
pub use crate::psp::VertexType;

use aligned_vec::{AVec, ConstAlign};
use alloc::vec::Vec;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Model {
    pub textures: Vec<Texture>,
    pub samplers: Vec<Sampler>,
    pub materials: Vec<Material>,
    pub meshes: Vec<Mesh>,
}

#[derive(Serialize, Deserialize)]
pub struct Texture {
    #[cfg_attr(feature = "psp", serde(with = "TexturePixelFormatDef"))]
    pub format: TexturePixelFormat,
    pub swizzle: i32,
    pub width: i32,
    pub height: i32,
    pub buffer_width: i32,
    pub data: Vec<AVec<u8, ConstAlign<16>>>,
}

#[derive(Serialize, Deserialize)]
pub struct Sampler {
    #[cfg_attr(feature = "psp", serde(with = "TextureFilterDef"))]
    pub min_filter: TextureFilter,
    #[cfg_attr(feature = "psp", serde(with = "TextureFilterDef"))]
    pub mag_filter: TextureFilter,
    #[cfg_attr(feature = "psp", serde(with = "GuTexWrapModeDef"))]
    pub u_wrap_mode: GuTexWrapMode,
    #[cfg_attr(feature = "psp", serde(with = "GuTexWrapModeDef"))]
    pub v_wrap_mode: GuTexWrapMode,
}

impl Sampler {
    pub const DEFAULT: Sampler = Sampler {
        min_filter: TextureFilter::LinearMipmapLinear,
        mag_filter: TextureFilter::LinearMipmapLinear,
        u_wrap_mode: GuTexWrapMode::Repeat,
        v_wrap_mode: GuTexWrapMode::Repeat,
    };
}

#[derive(Serialize, Deserialize)]
pub enum TextureBind {
    None,
    Texture(usize),
    TextureAndSampler(usize, usize),
}

#[derive(Serialize, Deserialize)]
pub struct Material {
    pub state_flags: GuStateFlags,
    pub alpha_cutoff: u8,
    pub diffuse_color: u32,
    pub texture_bind: TextureBind,
    pub emission_color: u32,
}

impl Material {
    pub const STATE_FLAGS_MASK: GuStateFlags = GuStateFlags::AlphaTest
        .union(GuStateFlags::Blend)
        .union(GuStateFlags::CullFace)
        .union(GuStateFlags::Texture2D)
        .union(GuStateFlags::Lighting);

    pub const DEFAULT: Material = Material {
        state_flags: GuStateFlags::CullFace.union(GuStateFlags::Lighting),
        alpha_cutoff: 0x0,
        diffuse_color: 0xffffffff,
        texture_bind: TextureBind::None,
        emission_color: 0x00000000,
    };
}

#[derive(Serialize, Deserialize)]
pub struct Mesh {
    pub material: Option<usize>,
    #[cfg_attr(feature = "psp", serde(with = "GuPrimitiveDef"))]
    pub primitive_type: GuPrimitive,
    #[cfg_attr(
        feature = "psp",
        serde(
            serialize_with = "serialize_vertex_type",
            deserialize_with = "deserialize_vertex_type"
        )
    )]
    pub vertex_type: VertexType,
    pub vertex_count: i32,
    pub index_buffer: AVec<u8, ConstAlign<16>>,
    pub vertex_buffer: AVec<u8, ConstAlign<16>>,
}
