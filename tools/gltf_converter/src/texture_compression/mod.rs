use std::collections::HashSet;

type PspMaterial = psp_file_formats::model::Material;
type PspMaterialFlags = psp_file_formats::model::GuStateFlags;
type PspTextureBind = psp_file_formats::model::TextureBind;

pub mod dxt;
pub mod high_color;

fn texture_usage(materials: &[PspMaterial]) -> (HashSet<usize>, HashSet<usize>) {
    let mut masked = HashSet::<usize>::default();
    let mut blended = HashSet::<usize>::default();
    for material in materials {
        if let PspTextureBind::Texture(texture) | PspTextureBind::TextureAndSampler(texture, _) =
            material.texture_bind
        {
            if material.state_flags.contains(PspMaterialFlags::AlphaTest) {
                masked.insert(texture);
            } else if material.state_flags.contains(PspMaterialFlags::Blend) {
                blended.insert(texture);
            }
        }
    }
    (masked, blended)
}
