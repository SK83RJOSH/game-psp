use thiserror::Error;

type PspTexture = psp_file_formats::model::Texture;
type PspSampler = psp_file_formats::model::Sampler;
type PspMaterial = psp_file_formats::model::Material;
type PspMaterialFlags = psp_file_formats::model::GuStateFlags;
type PspTextureBind = psp_file_formats::model::TextureBind;

type GltfMaterials<'a> = gltf::iter::Materials<'a>;
type GltfAlphaMode = gltf::material::AlphaMode;
type GltfMaterial<'a> = gltf::Material<'a>;

#[derive(Debug)]
pub enum IndexSemantic {
    Sampler,
    Texture,
}

#[derive(Error, Debug)]
pub enum IndexError {
    #[error("missing: {index:?}")]
    Missing { index: usize },
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid alpha cutoff: {alpha_cutoff:?}")]
    InvalidAlphaCutoff { alpha_cutoff: f32 },
    #[error("invalid emissive factor: {base_color_factor:?}")]
    InvalidBaseColorFactor { base_color_factor: [f32; 4] },
    #[error("invalid emissive factor: {emissive_factor:?}")]
    InvalidEmissiveFactor { emissive_factor: [f32; 3] },
    #[error("invalid index ({semantic:?}): {error:?}")]
    InvalidIndex {
        semantic: IndexSemantic,
        error: IndexError,
    },
}

type Result<T, E = Error> = core::result::Result<T, E>;

pub fn read_materials(
    textures: &Vec<PspTexture>,
    samplers: &Vec<PspSampler>,
    materials: GltfMaterials,
) -> Result<Vec<PspMaterial>> {
    let mut result = vec![];
    for material in materials {
        result.push(PspMaterial {
            state_flags: material_state_flags(&material),
            alpha_cutoff: material_alpha_cutoff(&material)?,
            diffuse_color: material_diffuse_color(&material)?,
            texture_bind: material_texture_bind(&textures, &samplers, &material)?,
            emission_color: material_emission_color(&material)?,
        });
    }
    Ok(result)
}

fn material_state_flags(material: &gltf::Material) -> PspMaterialFlags {
    let mut flags = PspMaterialFlags::default();
    if material.alpha_mode() == GltfAlphaMode::Mask {
        flags.set(PspMaterialFlags::AlphaTest, true);
    } else if material.alpha_mode() == GltfAlphaMode::Blend {
        flags.set(PspMaterialFlags::Blend, true);
    }
    flags.set(PspMaterialFlags::CullFace, !material.double_sided());
    let material_textured = material
        .pbr_metallic_roughness()
        .base_color_texture()
        .is_some();
    flags.set(PspMaterialFlags::Texture2D, material_textured);
    flags.set(PspMaterialFlags::Lighting, !material.unlit());
    flags
}

fn material_alpha_cutoff(material: &gltf::Material) -> Result<u8> {
    if let Some(alpha_cutoff) = material.alpha_cutoff() {
        if alpha_cutoff < 0.0 || 1.0 < alpha_cutoff {
            Err(Error::InvalidAlphaCutoff { alpha_cutoff })
        } else {
            Ok((alpha_cutoff * 255.0) as u8)
        }
    } else {
        Ok(128)
    }
}

fn material_diffuse_color(material: &gltf::Material) -> Result<u32> {
    let base_color_factor = material.pbr_metallic_roughness().base_color_factor();
    for factor in base_color_factor {
        if factor < 0.0 || 1.0 < factor {
            return Err(Error::InvalidBaseColorFactor { base_color_factor });
        }
    }
    let [r, g, b, a] = base_color_factor.map(|f| (f * 255.0) as u8);
    Ok(u32::from_ne_bytes([a, b, g, r]))
}

fn material_texture_bind(
    textures: &Vec<PspTexture>,
    samplers: &Vec<PspSampler>,
    material: &GltfMaterial,
) -> Result<PspTextureBind> {
    if let Some(base_color) = material.pbr_metallic_roughness().base_color_texture() {
        let texture = base_color.texture().source().index();
        if texture >= textures.len() {
            Err(Error::InvalidIndex {
                semantic: IndexSemantic::Texture,
                error: IndexError::Missing { index: texture },
            })
        } else if let Some(sampler) = base_color.texture().sampler().index() {
            if sampler >= samplers.len() {
                Err(Error::InvalidIndex {
                    semantic: IndexSemantic::Sampler,
                    error: IndexError::Missing { index: sampler },
                })
            } else {
                Ok(PspTextureBind::TextureAndSampler(texture, sampler))
            }
        } else {
            Ok(PspTextureBind::Texture(texture))
        }
    } else {
        Ok(PspTextureBind::None)
    }
}

fn material_emission_color(material: &GltfMaterial) -> Result<u32> {
    let emissive_factor = material.emissive_factor();
    for factor in emissive_factor {
        if factor < 0.0 || 1.0 < factor {
            return Err(Error::InvalidEmissiveFactor { emissive_factor });
        }
    }
    let [r, g, b] = emissive_factor.map(|f| (f * 255.0) as u8);
    Ok(u32::from_ne_bytes([255, b, g, r]))
}
