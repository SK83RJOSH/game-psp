use std::collections::HashSet;

use aligned_vec::AVec;
use itertools::Itertools;

type PspMaterial = psp_file_formats::model::Material;
type PspTexture = psp_file_formats::model::Texture;
type PspTextureFormat = psp_file_formats::model::TexturePixelFormat;

pub fn compress(materials: &[PspMaterial], textures: &mut [PspTexture]) {
    let (blended, masked) = super::texture_usage(materials);
    for (index, texture) in textures.iter_mut().enumerate() {
        let format = texture_format(&index, &blended, &masked);
        texture.format = format;

        let (width, height) = (texture.width as usize, texture.height as usize);
        let mut output = vec![0u8; format.compressed_size(width, height)];

        let input = texture.data.as_slice();
        format.compress(input, output.as_mut_slice());
        texture.data = AVec::from_slice(16, &output);
    }
}

trait Compressor {
    fn compressed_size(&self, width: usize, height: usize) -> usize;
    fn compress(&self, rgba: &[u8], output: &mut [u8]);
}

impl Compressor for PspTextureFormat {
    fn compressed_size(&self, width: usize, height: usize) -> usize {
        match self {
            PspTextureFormat::Psm4444 | PspTextureFormat::Psm5551 | PspTextureFormat::Psm5650 => {
                width * height * 2
            }
            _ => panic!("unsupported high-color compressor"),
        }
    }

    fn compress(&self, rgba: &[u8], output: &mut [u8]) {
        let pixels = rgba
            .iter()
            .map(|&v| v as u16)
            .tuples()
            .map(match self {
                PspTextureFormat::Psm5650 => compress_5650,
                PspTextureFormat::Psm5551 => compress_5551,
                PspTextureFormat::Psm4444 => compress_4444,
                _ => panic!("unsupported high-color compressor"),
            })
            .flat_map(|v| v.to_ne_bytes());
        for (output, input) in output.iter_mut().zip(pixels) {
            *output = input;
        }
    }
}

pub fn compress_5650((r, g, b, _): (u16, u16, u16, u16)) -> u16 {
    (r / 8) << 11 | (g / 4) << 5 | (b / 8)
}

pub fn compress_5551((r, g, b, a): (u16, u16, u16, u16)) -> u16 {
    (r / 8) << 11 | (g / 8) << 6 | (b / 8) << 1 | (a / 128)
}

pub fn compress_4444((r, g, b, a): (u16, u16, u16, u16)) -> u16 {
    (r / 16) << 12 | (g / 16) << 8 | (b / 16) << 4 | (a / 16)
}

fn texture_format(
    index: &usize,
    blended: &HashSet<usize>,
    masked: &HashSet<usize>,
) -> PspTextureFormat {
    let (blended, masked) = (blended.contains(index), masked.contains(index));
    if blended {
        PspTextureFormat::Psm4444
    } else if masked {
        PspTextureFormat::Psm5551
    } else {
        PspTextureFormat::Psm5650
    }
}
