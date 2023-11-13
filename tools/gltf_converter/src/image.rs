use aligned_vec::AVec;
use itertools::Itertools;
use thiserror::Error;

type PspTexture = psp_file_formats::model::Texture;
type PspTextureFormat = psp_file_formats::model::TexturePixelFormat;

type GltfImageData = gltf::image::Data;
type GltfImageFormat = gltf::image::Format;

#[derive(Error, Debug)]
pub enum Error {
    #[error("unsupported image format: {format:?}")]
    UnsupportedImageFormat { format: GltfImageFormat },
    #[error("unsupported image dimensions: {width:?}, {height:?}")]
    UnsupportedImageDimensions { width: u32, height: u32 },
}

type Result<T, E = Error> = core::result::Result<T, E>;

pub fn read_textures(images: &[GltfImageData]) -> Result<Vec<PspTexture>> {
    images
        .iter()
        .map(|image| {
            let (width, height) = image_dimensions(image)?;
            Ok(PspTexture {
                format: image_format(image)?,
                swizzle: 0,
                width,
                height,
                buffer_width: width,
                data: vec![AVec::from_slice(16, &image_data(image)?)],
            })
        })
        .collect()
}

fn image_dimensions(image: &GltfImageData) -> Result<(i32, i32)> {
    let (width, height) = (image.width, image.height);
    if !width.is_power_of_two() || !height.is_power_of_two() || width > 512 || height > 512 {
        Err(Error::UnsupportedImageDimensions { width, height })
    } else {
        Ok((width as i32, height as i32))
    }
}

fn image_format(image: &GltfImageData) -> Result<PspTextureFormat> {
    match image.format {
        GltfImageFormat::R8G8B8
        | GltfImageFormat::R8G8B8A8
        | GltfImageFormat::R16G16B16
        | GltfImageFormat::R16G16B16A16 => Ok(PspTextureFormat::Psm8888),
        format => Err(Error::UnsupportedImageFormat { format }),
    }
}

fn image_data(image: &GltfImageData) -> Result<Vec<u8>> {
    let pixels = image.pixels.iter();
    match image.format {
        GltfImageFormat::R8G8B8 => Ok(pixels
            .tuples()
            .flat_map(|(r, g, b)| [*r, *g, *b, 255])
            .collect_vec()),
        GltfImageFormat::R8G8B8A8 => Ok(pixels
            .tuples()
            .flat_map(|(r, g, b, a)| [*r, *g, *b, *a])
            .collect_vec()),
        GltfImageFormat::R16G16B16 => Ok(pixels
            .tuples()
            .map(|(a, b)| (u16::from_le_bytes([*a, *b]) / 257) as u8)
            .tuples()
            .flat_map(|(r, g, b)| [r, g, b, 255])
            .collect_vec()),
        GltfImageFormat::R16G16B16A16 => Ok(pixels
            .tuples()
            .map(|(a, b)| (u16::from_le_bytes([*a, *b]) / 257) as u8)
            .tuples()
            .flat_map(|(r, g, b, a)| [r, g, b, a])
            .collect_vec()),
        format => Err(Error::UnsupportedImageFormat { format }),
    }
}
