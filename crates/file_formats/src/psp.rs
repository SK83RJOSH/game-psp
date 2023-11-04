use serde::{Deserialize, Serialize};

#[cfg(not(feature = "psp"))]
pub type GuPrimitive = GuPrimitiveDef;
#[cfg(feature = "psp")]
pub type GuPrimitive = psp::sys::GuPrimitive;

#[repr(u32)]
#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "psp", serde(remote = "GuPrimitive"))]
pub enum GuPrimitiveDef {
    Points = 0,
    Lines = 1,
    LineStrip = 2,
    Triangles = 3,
    TriangleStrip = 4,
    TriangleFan = 5,
    Sprites = 6,
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
    pub struct GuStateFlags: i32 {
        const AlphaTest = 1 << 0;
        const DepthTest = 1 << 1;
        const ScissorTest = 1 << 2;
        const StencilTest = 1 << 3;
        const Blend = 1 << 4;
        const CullFace = 1 << 5;
        const Dither = 1 << 6;
        const Fog = 1 << 7;
        const ClipPlanes = 1 << 8;
        const Texture2D = 1 << 9;
        const Lighting = 1 << 10;
        const Light0 = 1 << 11;
        const Light1 = 1 << 12;
        const Light2 = 1 << 13;
        const Light3 = 1 << 14;
        const LineSmooth = 1 << 15;
        const PatchCullFace = 1 << 16;
        const ColorTest = 1 << 17;
        const ColorLogicOp = 1 << 18;
        const FaceNormalReverse = 1 << 19;
        const PatchFace = 1 << 20;
        const Fragment2X = 1 << 21;
    }
}

#[cfg(not(feature = "psp"))]
pub type GuTexWrapMode = GuTexWrapModeDef;
#[cfg(feature = "psp")]
pub type GuTexWrapMode = psp::sys::GuTexWrapMode;

#[repr(u32)]
#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "psp", serde(remote = "GuTexWrapMode"))]
pub enum GuTexWrapModeDef {
    Repeat = 0,
    Clamp = 1,
}

#[cfg(not(feature = "psp"))]
pub type TextureFilter = TextureFilterDef;
#[cfg(feature = "psp")]
pub type TextureFilter = psp::sys::TextureFilter;

#[repr(u32)]
#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "psp", serde(remote = "TextureFilter"))]
pub enum TextureFilterDef {
    Nearest = 0,
    Linear = 1,
    NearestMipmapNearest = 4,
    LinearMipmapNearest = 5,
    NearestMipmapLinear = 6,
    LinearMipmapLinear = 7,
}

#[cfg(not(feature = "psp"))]
pub type TexturePixelFormat = TexturePixelFormatDef;
#[cfg(feature = "psp")]
pub type TexturePixelFormat = psp::sys::TexturePixelFormat;

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "psp", serde(remote = "TexturePixelFormat"))]
#[repr(u32)]
pub enum TexturePixelFormatDef {
    Psm5650 = 0,
    Psm5551 = 1,
    Psm4444 = 2,
    Psm8888 = 3,
    PsmT4 = 4,
    PsmT8 = 5,
    PsmT16 = 6,
    PsmT32 = 7,
    PsmDxt1 = 8,
    PsmDxt3 = 9,
    PsmDxt5 = 10,
}

#[cfg(not(feature = "psp"))]
pub type VertexType = VertexTypeDef;
#[cfg(feature = "psp")]
pub type VertexType = psp::sys::VertexType;

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Serialize, Deserialize)]
    pub struct VertexTypeDef: i32 {
        const TEXTURE_8BIT = 1;
        const TEXTURE_16BIT = 2;
        const TEXTURE_32BITF = 3;
        const COLOR_5650 = 4 << 2;
        const COLOR_5551 = 5 << 2;
        const COLOR_4444 = 6 << 2;
        const COLOR_8888 = 7 << 2;
        const NORMAL_8BIT = 1 << 5;
        const NORMAL_16BIT = 2 << 5;
        const NORMAL_32BITF = 3 << 5;
        const VERTEX_8BIT = 1 << 7;
        const VERTEX_16BIT = 2 << 7;
        const VERTEX_32BITF = 3 << 7;
        const WEIGHT_8BIT = 1 << 9;
        const WEIGHT_16BIT = 2 << 9;
        const WEIGHT_32BITF = 3 << 9;
        const INDEX_8BIT = 1 << 11;
        const INDEX_16BIT = 2 << 11;
        const WEIGHTS1 = 0 << 14;
        const WEIGHTS2 = 1 << 14;
        const WEIGHTS3 = 2 << 14;
        const WEIGHTS4 = 3 << 14;
        const WEIGHTS5 = 4 << 14;
        const WEIGHTS6 = 5 << 14;
        const WEIGHTS7 = 6 << 14;
        const WEIGHTS8 = 7 << 14;
        const VERTICES1 = 0 << 18;
        const VERTICES2 = 1 << 18;
        const VERTICES3 = 2 << 18;
        const VERTICES4 = 3 << 18;
        const VERTICES5 = 4 << 18;
        const VERTICES6 = 5 << 18;
        const VERTICES7 = 6 << 18;
        const VERTICES8 = 7 << 18;
        const TRANSFORM_2D = 1 << 23;
        const TRANSFORM_3D = 0;
    }
}

#[cfg(feature = "psp")]
pub fn serialize_vertex_type<S: serde::Serializer>(
    value: &VertexType,
    s: S,
) -> Result<S::Ok, S::Error> {
    s.serialize_i32(value.bits())
}

#[cfg(feature = "psp")]
pub fn deserialize_vertex_type<'de, D: serde::Deserializer<'de>>(
    deserializer: D,
) -> Result<VertexType, D::Error> {
    let value: i32 = serde::Deserialize::deserialize(deserializer)?;
    Ok(VertexType::from_bits_truncate(value))
}
