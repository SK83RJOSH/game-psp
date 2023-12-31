use std::collections::HashSet;

use aligned_vec::AVec;
use itertools::Itertools;

type PspMaterial = psp_file_formats::model::Material;
type PspTexture = psp_file_formats::model::Texture;
type PspTextureFormat = psp_file_formats::model::TexturePixelFormat;

type TexpressoParams = texpresso::Params;
type TexpressoAlgo = texpresso::Algorithm;
type TexpressoCompressor = texpresso::Format;

pub fn compress(materials: &[PspMaterial], textures: &mut [PspTexture]) {
    let (blended, masked) = super::texture_usage(materials);
    for (index, texture) in textures.iter_mut().enumerate() {
        if !matches!(texture.format, PspTextureFormat::Psm8888) {
            continue;
        }

        let (params, compressor, format) = texture_format(&index, &blended, &masked);
        texture.format = format;

        let (width, height) = (texture.width as usize, texture.height as usize);
        for (level, data) in texture.data.iter_mut().enumerate() {
            let input = data.as_slice();
            let (width, height) = (width >> level, height >> level);
            let mut output = vec![0u8; compressor.compressed_size(width, height)];
            compressor.compress(input, width, height, params, output.as_mut_slice());
            *data = AVec::from_slice(16, &texture_shuffle_data(&output, compressor));
        }
    }
}

fn texture_format(
    index: &usize,
    blended: &HashSet<usize>,
    masked: &HashSet<usize>,
) -> (TexpressoParams, TexpressoCompressor, PspTextureFormat) {
    let mut params = TexpressoParams {
        algorithm: TexpressoAlgo::IterativeClusterFit,
        weigh_colour_by_alpha: true,
        ..TexpressoParams::default()
    };
    let (blended, masked) = (blended.contains(index), masked.contains(index));
    if blended && masked {
        (params, TexpressoCompressor::Bc2, PspTextureFormat::PsmDxt3)
    } else if blended {
        (params, TexpressoCompressor::Bc3, PspTextureFormat::PsmDxt5)
    } else {
        params.weigh_colour_by_alpha = false;
        (params, TexpressoCompressor::Bc1, PspTextureFormat::PsmDxt1)
    }
}

fn texture_shuffle_data(data: &[u8], compressor: TexpressoCompressor) -> Vec<u8> {
    let data = data
        .iter()
        .tuples()
        .map(|(a, b)| u16::from_ne_bytes([*a, *b]));
    match compressor {
        TexpressoCompressor::Bc1 => data
            .tuples()
            .map(|(a, b, c, d)| -> u64 { bytemuck::must_cast([c, d, a, b]) })
            .flat_map(|v| v.to_ne_bytes())
            .collect_vec(),
        TexpressoCompressor::Bc2 => data
            .tuples()
            .map(|(a, b, c, d, e, f, g, h)| -> u128 {
                bytemuck::must_cast([g, h, e, f, a, b, c, d])
            })
            .flat_map(|v| v.to_ne_bytes())
            .collect_vec(),
        TexpressoCompressor::Bc3 => data
            .tuples()
            .map(|(a, b, c, d, e, f, g, h)| -> u128 {
                bytemuck::must_cast([g, h, e, f, b, c, d, a])
            })
            .flat_map(|v| v.to_ne_bytes())
            .collect_vec(),
        _ => panic!("unsupported texpressor compressor"),
    }
}
