use std::collections::HashSet;

type PspTexture = psp_file_formats::model::Texture;
type PspSampler = psp_file_formats::model::Sampler;
type PspMaterial = psp_file_formats::model::Material;
type PspMesh = psp_file_formats::model::Mesh;
type PspTextureBind = psp_file_formats::model::TextureBind;

pub fn strip_unused(
    textures: &mut Vec<PspTexture>,
    samplers: &mut Vec<PspSampler>,
    materials: &mut Vec<PspMaterial>,
    meshes: &mut [PspMesh],
) {
    let used_materials = meshes.iter().filter_map(|m| m.material).collect();
    let material_map = remove_and_remap_values(&used_materials, materials);
    for mesh in meshes.iter_mut() {
        if let Some(material) = mesh.material {
            mesh.material = Some(material_map[material]);
        }
    }

    let mut used_textures = HashSet::default();
    let mut used_samplers = HashSet::default();
    for material in materials.iter() {
        if let PspTextureBind::Texture(texture) = material.texture_bind {
            used_textures.insert(texture);
        } else if let PspTextureBind::TextureAndSampler(texture, sampler) = material.texture_bind {
            used_textures.insert(texture);
            used_samplers.insert(sampler);
        }
    }

    let texture_map = remove_and_remap_values(&used_textures, textures);
    let sampler_map = remove_and_remap_values(&used_samplers, samplers);
    for material in materials.iter_mut() {
        material.texture_bind = match material.texture_bind {
            PspTextureBind::Texture(texture) => PspTextureBind::Texture(texture_map[texture]),
            PspTextureBind::TextureAndSampler(texture, sampler) => {
                PspTextureBind::TextureAndSampler(texture_map[texture], sampler_map[sampler])
            }
            PspTextureBind::None => PspTextureBind::None,
        };
    }
}

fn remove_and_remap_values<T>(used: &HashSet<usize>, values: &mut Vec<T>) -> Vec<usize> {
    if used.len() == values.len() {
        return (0..values.len()).collect();
    };

    let mut new_index = 0;
    let mut removed_indices = 0;
    let mut map = vec![0; values.len()];
    while new_index < values.len() {
        let old_idx = new_index + removed_indices;
        if used.contains(&old_idx) {
            map[old_idx] = new_index;
            new_index += 1;
        } else {
            values.remove(new_index);
            removed_indices += 1;
        }
    }
    map
}
