use aligned_vec::AVec;
use itertools::Itertools;

type PspTexture = psp_file_formats::model::Texture;
type PspTextureFormat = psp_file_formats::model::TexturePixelFormat;

pub fn swizzle(textures: &mut [PspTexture]) {
    for texture in textures {
        let texture_info = PspTextureInfo::new(texture);
        if texture_info.encoded {
            continue;
        }

        let pitch = texture_info.get_pitch();
        let height = texture.data.len() / pitch;
        let row_blocks = pitch / 16;
        let row_pitch = (row_blocks - 1) * 8;

        let src = &texture
            .data
            .iter()
            .tuples()
            .map(|(a, b)| -> u16 { bytemuck::must_cast([*a, *b]) })
            .tuples()
            .map(|(a, b, c, d, e, f, g, h)| -> u128 {
                bytemuck::must_cast([a, b, c, d, e, f, g, h])
            })
            .collect_vec();
        let mut dst = src.clone();

        let mut src_pos = 0;
        let mut dst_pos = 0;
        for block_y in 0..height {
            for dst_off in 0..row_blocks {
                dst[dst_pos + dst_off + (dst_off * 7)] = src[src_pos];
                src_pos += 1;
            }
            if (block_y & 7) == 7 {
                dst_pos += row_pitch;
            }
            dst_pos += 1;
        }
        texture.swizzle = 1;
        texture.data = AVec::from_slice(16, bytemuck::must_cast_slice(&dst));
    }
}

struct PspTextureInfo<'a> {
    texture: &'a PspTexture,
    encoded: bool,
    bits_per_pixel: usize,
    pitch_align: usize,
}

impl<'a> PspTextureInfo<'a> {
    fn new(texture: &'a PspTexture) -> Self {
        let encoded = matches!(
            texture.format,
            PspTextureFormat::PsmDxt1 | PspTextureFormat::PsmDxt3 | PspTextureFormat::PsmDxt5
        );
        let (bits_per_pixel, pitch_align) = match texture.format {
            PspTextureFormat::Psm5650 => (16, 16),
            PspTextureFormat::Psm5551 => (16, 16),
            PspTextureFormat::Psm4444 => (16, 16),
            PspTextureFormat::Psm8888 => (32, 16),
            PspTextureFormat::PsmT4 => (4, 16),
            PspTextureFormat::PsmT8 => (8, 16),
            PspTextureFormat::PsmT16 => (16, 16),
            PspTextureFormat::PsmT32 => (32, 16),
            PspTextureFormat::PsmDxt1 => (4, 4),
            PspTextureFormat::PsmDxt3 => (8, 4),
            PspTextureFormat::PsmDxt5 => (8, 4),
        };
        Self {
            texture,
            encoded,
            bits_per_pixel,
            pitch_align,
        }
    }

    fn get_pitch(&self) -> usize {
        fn align(value: usize, alignment: usize) -> usize {
            value + (alignment - (value % alignment)) % alignment
        }

        align(
            (self.bits_per_pixel * self.texture.width as usize + 7) / 8,
            self.pitch_align,
        )
    }
}
