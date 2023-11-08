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
pub enum Error {
    #[error("invalid alpha cutoff: {alpha_cutoff:?}")]
    InvalidAlphaCutoff { alpha_cutoff: f32 },
    #[error("invalid emissive factor: {base_color_factor:?}")]
    InvalidBaseColorFactor { base_color_factor: [f32; 4] },
    #[error("invalid emissive factor: {emissive_factor:?}")]
    InvalidEmissiveFactor { emissive_factor: [f32; 3] },
    #[error("missing index ({semantic:?}): {index:?}")]
    MissingIndex {
        semantic: IndexSemantic,
        index: usize,
    },
}

type Result<T, E = Error> = core::result::Result<T, E>;

pub fn read_materials(
    textures: &[PspTexture],
    samplers: &[PspSampler],
    materials: GltfMaterials,
) -> Result<Vec<PspMaterial>> {
    materials
        .map(|material| {
            Ok(PspMaterial {
                state_flags: material_state_flags(&material),
                alpha_cutoff: material_alpha_cutoff(&material)?,
                diffuse_color: material_diffuse_color(&material)?,
                texture_bind: material_texture_bind(textures, samplers, &material)?,
                emission_color: material_emission_color(&material)?,
            })
        })
        .collect()
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
    let alpha_cutoff = material.alpha_cutoff().unwrap_or(0.5);
    if (0.0..=1.0).contains(&alpha_cutoff) {
        Ok((alpha_cutoff * 255.0) as u8)
    } else {
        Err(Error::InvalidAlphaCutoff { alpha_cutoff })
    }
}

fn material_diffuse_color(material: &gltf::Material) -> Result<u32> {
    let base_color_factor = material.pbr_metallic_roughness().base_color_factor();
    if base_color_factor.iter().all(|f| (0.0..=1.0).contains(f)) {
        let [r, g, b, a] = base_color_factor.map(|f| (f * 255.0) as u8);
        Ok(u32::from_ne_bytes([a, b, g, r]))
    } else {
        Err(Error::InvalidBaseColorFactor { base_color_factor })
    }
}

fn material_texture_bind(
    textures: &[PspTexture],
    samplers: &[PspSampler],
    material: &GltfMaterial,
) -> Result<PspTextureBind> {
    let Some(base_color) = material.pbr_metallic_roughness().base_color_texture() else {
        return Ok(PspTextureBind::None);
    };
    let texture = base_color.texture().source().index();
    if texture >= textures.len() {
        return Err(Error::MissingIndex {
            semantic: IndexSemantic::Texture,
            index: texture,
        });
    }
    let Some(sampler) = base_color.texture().sampler().index() else {
        return Ok(PspTextureBind::Texture(texture));
    };
    if sampler >= samplers.len() {
        return Err(Error::MissingIndex {
            semantic: IndexSemantic::Sampler,
            index: sampler,
        });
    }
    Ok(PspTextureBind::TextureAndSampler(texture, sampler))
}

fn material_emission_color(material: &GltfMaterial) -> Result<u32> {
    let emissive_factor = material.emissive_factor();
    if emissive_factor.iter().all(|f| (0.0..=1.0).contains(f)) {
        let [r, g, b] = emissive_factor.map(|f| (f * 255.0) as u8);
        Ok(u32::from_ne_bytes([255, b, g, r]))
    } else {
        Err(Error::InvalidEmissiveFactor { emissive_factor })
    }
}
