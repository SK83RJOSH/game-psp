use std::collections::HashSet;

use aligned_vec::AVec;
use image::{imageops, ImageBuffer, Rgba};

type PspMaterial = psp_file_formats::model::Material;
type PspSampler = psp_file_formats::model::Sampler;
type PspTexture = psp_file_formats::model::Texture;
type PspTextureBind = psp_file_formats::model::TextureBind;
type PspTextureFilter = psp_file_formats::model::TextureFilter;
type PspTextureFormat = psp_file_formats::model::TexturePixelFormat;

pub fn generate(materials: &[PspMaterial], samplers: &[PspSampler], textures: &mut [PspTexture]) {
    let linear = sampler_usage(materials, samplers);
    for (index, texture) in textures.iter_mut().enumerate() {
        if texture.data.is_empty() || !matches!(texture.format, PspTextureFormat::Psm8888) {
            continue;
        }

        let (width, height) = (texture.width as u32, texture.height as u32);
        let Some(image) =
            ImageBuffer::<Rgba<u8>, _>::from_raw(width, height, texture.data[0].clone())
        else {
            continue;
        };

        let filter = if linear.contains(&index) {
            imageops::Triangle
        } else {
            imageops::Nearest
        };

        for level in 1..max_level(texture) {
            let (width, height) = (width >> level, height >> level);
            let image = imageops::resize(&image, width, height, filter);
            texture.data.push(AVec::from_slice(16, image.as_raw()));
        }
    }
}

fn sampler_usage(materials: &[PspMaterial], samplers: &[PspSampler]) -> HashSet<usize> {
    let mut linear = HashSet::<usize>::default();
    fn is_linear(texture_filter: &PspTextureFilter) -> bool {
        matches!(
            texture_filter,
            PspTextureFilter::Linear
                | PspTextureFilter::LinearMipmapLinear
                | PspTextureFilter::LinearMipmapNearest
        )
    }
    for material in materials {
        if let PspTextureBind::TextureAndSampler(texture, sampler) = material.texture_bind {
            let Some(sampler) = samplers.get(sampler) else {
                continue;
            };
            if is_linear(&sampler.min_filter) || is_linear(&sampler.mag_filter) {
                linear.insert(texture);
            }
        } else if let PspTextureBind::Texture(texture) = material.texture_bind {
            linear.insert(texture);
        }
    }
    linear
}

pub fn max_level(texture: &PspTexture) -> usize {
    let (w, h) = (texture.width, texture.height);
    (0..8)
        .position(|l| (w >> l).min(h >> l) <= 4)
        .map_or(1, |l| l + 1)
}

pub fn strip(textures: &mut [PspTexture]) {
    for texture in textures {
        let (width, height) = (texture.width as usize, texture.height as usize);
        texture.data = texture
            .data
            .iter()
            .enumerate()
            .take_while(|(level, data)| {
                let width = width >> level;
                let height = height >> level;
                let bits_per_pixel = (8 * data.len()) / (width * height);
                (bits_per_pixel * width) % 128 == 0
            })
            .map(|(_, data)| data.clone())
            .collect();
    }
}
