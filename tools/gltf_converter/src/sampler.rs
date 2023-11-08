use thiserror::Error;

type PspSampler = psp_file_formats::model::Sampler;
type PspTextureFilter = psp_file_formats::model::TextureFilter;
type PspWrapMode = psp_file_formats::model::GuTexWrapMode;

type GltfSamplers<'a> = gltf::iter::Samplers<'a>;
type GltfMinFilter = gltf::texture::MinFilter;
type GltfMagFilter = gltf::texture::MagFilter;
type GltfWrapMode = gltf::texture::WrappingMode;

#[derive(Error, Debug)]
pub enum Error {
    #[error("unsupported sampler wrap mode: {mode:?}")]
    UnsupportedSamplerWrapMode { mode: GltfWrapMode },
}

type Result<T, E = Error> = core::result::Result<T, E>;

pub fn read_samplers(samplers: GltfSamplers) -> Result<Vec<PspSampler>> {
    let mut result = vec![];
    for sampler in samplers {
        result.push(PspSampler {
            min_filter: sampler_get_min_filter(sampler.min_filter()),
            mag_filter: sampler_get_mag_filter(sampler.mag_filter()),
            u_wrap_mode: sampler_get_wrap_mode(sampler.wrap_s())?,
            v_wrap_mode: sampler_get_wrap_mode(sampler.wrap_t())?,
        });
    }
    Ok(result)
}

fn sampler_get_min_filter(filter: Option<GltfMinFilter>) -> PspTextureFilter {
    match filter {
        Some(GltfMinFilter::Linear) => PspTextureFilter::Linear,
        Some(GltfMinFilter::LinearMipmapLinear) => PspTextureFilter::LinearMipmapLinear,
        Some(GltfMinFilter::LinearMipmapNearest) => PspTextureFilter::LinearMipmapNearest,
        Some(GltfMinFilter::Nearest) => PspTextureFilter::Nearest,
        Some(GltfMinFilter::NearestMipmapLinear) => PspTextureFilter::NearestMipmapLinear,
        Some(GltfMinFilter::NearestMipmapNearest) => PspTextureFilter::NearestMipmapNearest,
        None => PspTextureFilter::LinearMipmapNearest,
    }
}

fn sampler_get_mag_filter(filter: Option<GltfMagFilter>) -> PspTextureFilter {
    match filter {
        Some(GltfMagFilter::Linear) => PspTextureFilter::Linear,
        Some(GltfMagFilter::Nearest) => PspTextureFilter::Nearest,
        None => PspTextureFilter::LinearMipmapNearest,
    }
}

fn sampler_get_wrap_mode(mode: GltfWrapMode) -> Result<PspWrapMode> {
    match mode {
        GltfWrapMode::ClampToEdge => Ok(PspWrapMode::Clamp),
        GltfWrapMode::Repeat => Ok(PspWrapMode::Repeat),
        GltfWrapMode::MirroredRepeat => Err(Error::UnsupportedSamplerWrapMode { mode }),
    }
}
