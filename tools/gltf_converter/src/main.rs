use std::{
    collections::{HashMap, HashSet},
    io::Write,
    path::PathBuf,
};

use aligned_vec::AVec;
use clap::Parser;
use gltf::{accessor, Semantic};
use itertools::Itertools;
use thiserror::Error;

#[derive(Debug)]
pub enum IndexSemantic {
    Sampler,
    Texture,
    Material,
}

#[derive(Error, Debug)]
pub enum IndexError {
    #[error("missing: {index:?}")]
    Missing { index: usize },
}

#[derive(Debug)]
pub enum AccessorSemantic {
    Position,
    Normal,
    Tangent,
    Color,
    TexCoord,
    Joint,
    Weight,
    Index,
}

#[derive(Error, Debug)]
pub enum AccessorError {
    #[error("missing: {index:?}")]
    Missing { index: usize },
    #[error("unsupported count: {count:?}")]
    UnsupportedCount { count: usize },
    #[error("unsupported data type: {data_type:?}")]
    UnsupportedDataType { data_type: gltf::accessor::DataType },
    #[error("mismatched data types")]
    MismatchedDataTypes,
    #[error("unsupported dimensions: {dimensions:?}")]
    UnsupportedDimensions {
        dimensions: gltf::accessor::Dimensions,
    },
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("gltf error: {0}")]
    Gltf(#[from] gltf::Error),
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
    #[error("mesh writer error: {0:?}")]
    MeshWriter(psp_mesh_writer::Error),
    #[error("postcard error: {0}")]
    Postcard(#[from] postcard::Error),
    #[error("unsupported image format: {format:?}")]
    UnsupportedImageFormat { format: gltf::image::Format },
    #[error("unsupported image dimensions: {width:?}, {height:?}")]
    UnsupportedImageDimensions { width: u32, height: u32 },
    #[error("unsupported sampler wrap mode: {mode:?}")]
    UnsupportedSamplerWrapMode { mode: gltf::texture::WrappingMode },
    #[error("unsupported material alpha cutoff: {alpha_cutoff:?}")]
    UnsupportedMaterialAlphaCutoff { alpha_cutoff: f32 },
    #[error("unsupported material emissive factor: {base_color_factor:?}")]
    UnsupportedMaterialBaseColorFactor { base_color_factor: [f32; 4] },
    #[error("unsupported material emissive factor: {emissive_factor:?}")]
    UnsupportedMaterialEmissiveFactor { emissive_factor: [f32; 3] },
    #[error("unsupported primitive mode: {mode:?}")]
    UnsupportedPrimitive { mode: gltf::mesh::Mode },
    #[error("unsupported weight count: {count:?}")]
    UnsupportedWeightCount { count: usize },
    #[error("unsupported morph count: {count:?}")]
    UnsupportedMorphCount { count: usize },
    #[error("index error ({semantic:?}): {error:?}")]
    InvalidIndex {
        semantic: IndexSemantic,
        error: IndexError,
    },
    #[error("invalid accessor ({semantic:?}): {error:?}")]
    InvalidAccessor {
        semantic: AccessorSemantic,
        error: AccessorError,
    },
}

impl From<psp_mesh_writer::Error> for Error {
    fn from(value: psp_mesh_writer::Error) -> Self {
        Error::MeshWriter(value)
    }
}

type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(Parser)]
struct Args {
    #[arg(default_value = "C:/Users/Josh/Desktop/game-psp/target/debug/BoxVertexColors.glb")]
    file: PathBuf,
    #[arg(short, long)]
    compress: Option<bool>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let (gltf, buffers, images) = gltf::import(args.file.clone())?;

    let mut samplers = vec![];
    for sampler in gltf.samplers() {
        samplers.push(psp_file_formats::model::Sampler {
            min_filter: sampler_get_min_filter(sampler.min_filter()),
            mag_filter: sampler_get_mag_filter(sampler.mag_filter()),
            u_wrap_mode: sampler_get_wrap_mode(sampler.wrap_s())?,
            v_wrap_mode: sampler_get_wrap_mode(sampler.wrap_t())?,
        });
    }

    let mut textures = vec![];
    for image in &images {
        let (width, height) = image_dimensions(image)?;
        textures.push(psp_file_formats::model::Texture {
            format: image_format(image)?,
            mip_levels: 0,
            width,
            height,
            buffer_width: width,
            data: AVec::from_slice(16, &image_data(image)?),
        });
    }

    let mut materials = vec![];
    for material in gltf.materials() {
        materials.push(psp_file_formats::model::Material {
            state_flags: material_state_flags(&material),
            alpha_cutoff: material_alpha_cutoff(&material)?,
            diffuse_color: material_diffuse_color(&material)?,
            sampler_index: material_sampler_index(&samplers, &material)?,
            texture_index: material_texture_index(&textures, &material)?,
            emission_color: material_emission_color(&material)?,
        });
    }

    let mut meshes = vec![];
    let mut skinned_meshes = HashSet::<usize>::default();
    let mut non_skinned_meshes = HashSet::<usize>::default();
    let mut used_materials = HashSet::<usize>::default();
    for gltf_node in gltf.nodes() {
        if let Some(gltf_mesh) = gltf_node.mesh() {
            if gltf_node.skin().is_some() {
                skinned_meshes.insert(gltf_mesh.index());
            } else {
                non_skinned_meshes.insert(gltf_mesh.index());
            }
        }
    }
    for mesh in gltf.meshes() {
        for primitive in mesh.primitives() {
            let material_index = primitive_material_index(&materials, &primitive.material())?;
            if let Some(used_material) = material_index {
                used_materials.insert(used_material);
            }
            let primitive_type = primitive_type(primitive.mode())?;
            let primitive_attributes = GltfPrimitiveAttributes::new(&primitive);
            let mesh_description = mesh_description(&primitive_attributes)?;

            let reader = primitive.reader(|b| Some(&buffers[b.index()]));
            let vertex_count = primitive_attributes
                .position_accessor
                .map(|a| a.count())
                .unwrap_or(0);
            let index_count = primitive_attributes
                .index_accessor
                .map(|a| a.count())
                .unwrap_or(0);
            let mut vertex_buffer = vec![0_u8; mesh_description.vertex_buffer_size(vertex_count)];
            let mut index_buffer = vec![0_u8; mesh_description.index_buffer_size(index_count)];
            let mut writer = psp_mesh_writer::MeshWriter::new_indexed(
                mesh_description,
                vertex_buffer.as_mut_slice(),
                index_buffer.as_mut_slice(),
            );

            if let Some(positions) = reader.read_positions() {
                writer.positions(&positions.collect::<Vec<_>>())?;
            };

            if let Some(normals) = reader.read_normals() {
                writer.normals(&normals.collect::<Vec<_>>())?;
            }

            if let Some(colors) = reader.read_colors(0) {
                let colors: Vec<u32> = colors.into_rgba_u8().map(u32::from_ne_bytes).collect();
                writer.colors(&colors)?;
            }

            if let Some(texcoords) = reader.read_tex_coords(0) {
                match texcoords {
                    gltf::mesh::util::ReadTexCoords::U8(texcoords) => {
                        writer.texcoords(&texcoords.collect::<Vec<_>>())?;
                    }
                    gltf::mesh::util::ReadTexCoords::U16(texcoords) => {
                        writer.texcoords(&texcoords.collect::<Vec<_>>())?;
                    }
                    gltf::mesh::util::ReadTexCoords::F32(texcoords) => {
                        writer.texcoords(&texcoords.collect::<Vec<_>>())?;
                    }
                }
            }

            writer.advance_vertex(vertex_count)?;

            if let Some(indices) = reader.read_indices() {
                match indices {
                    gltf::mesh::util::ReadIndices::U8(indices) => {
                        writer.indices(&indices.collect::<Vec<_>>())?;
                    }
                    gltf::mesh::util::ReadIndices::U16(indices) => {
                        writer.indices(&indices.collect::<Vec<_>>())?;
                    }
                    gltf::mesh::util::ReadIndices::U32(indices) => {
                        writer.indices(&indices.map(|i| i as u16).collect::<Vec<_>>())?;
                    }
                }
            }
            meshes.push(psp_file_formats::model::Mesh {
                material_index,
                primitive_type,
                vertex_type: psp_file_formats::model::VertexType::from_bits_truncate(
                    mesh_description.flags() as _,
                ),
                vertex_count: if index_count > 0 {
                    index_count as i32
                } else {
                    vertex_count as i32
                },
                index_buffer: AVec::from_slice(16, &index_buffer),
                vertex_buffer: AVec::from_slice(16, &vertex_buffer),
            });
        }
    }

    let material_map = remove_and_remap_values(&used_materials, &mut materials);
    for mesh in &mut meshes {
        if let Some(material_index) = mesh.material_index {
            mesh.material_index = Some(material_map[material_index]);
        }
    }
    
    let mut used_samplers = HashSet::<usize>::default();
    let mut used_textures = HashSet::<usize>::default();
    for material in &mut materials {
        if let Some(sampler_index) = material.sampler_index {
            used_samplers.insert(sampler_index);
        }
        if let Some(texture_index) = material.texture_index {
            used_textures.insert(texture_index);
        }
    }

    let sampler_map = remove_and_remap_values(&used_samplers, &mut samplers);
    let texture_map = remove_and_remap_values(&used_textures, &mut textures);
    for material in &mut materials {
        if let Some(sampler_index) = material.sampler_index {
            material.sampler_index = Some(sampler_map[sampler_index]);
        }
        if let Some(texture_index) = material.texture_index {
            material.texture_index = Some(texture_map[texture_index]);
        }
    }

    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open(args.file.clone().with_extension("psp"))?;

    let model_file = psp_file_formats::model::Model {
        textures,
        samplers,
        materials,
        meshes,
    };

    file.write_all(&postcard::to_allocvec(&model_file)?)?;

    Ok(())
}

fn image_dimensions(image: &gltf::image::Data) -> Result<(i32, i32)> {
    let (width, height) = (image.width, image.height);
    if !width.is_power_of_two() || !height.is_power_of_two() || width > 512 || height > 512 {
        Err(Error::UnsupportedImageDimensions { width, height })
    } else {
        Ok((width as i32, height as i32))
    }
}

fn image_format(image: &gltf::image::Data) -> Result<psp_file_formats::model::TexturePixelFormat> {
    match image.format {
        gltf::image::Format::R8G8B8
        | gltf::image::Format::R8G8B8A8
        | gltf::image::Format::R16G16B16
        | gltf::image::Format::R16G16B16A16 => {
            Ok(psp_file_formats::model::TexturePixelFormat::Psm8888)
        }
        format => Err(Error::UnsupportedImageFormat { format }),
    }
}

fn image_data(image: &gltf::image::Data) -> Result<Vec<u8>> {
    let mut result = Vec::<u8>::with_capacity((image.width * image.height) as usize);
    match image.format {
        gltf::image::Format::R8G8B8 => {
            image.pixels.iter().tuples().for_each(|(r, g, b)| {
                result.push(*r);
                result.push(*g);
                result.push(*b);
                result.push(255);
            });
            Ok(result)
        }
        gltf::image::Format::R8G8B8A8 => {
            image.pixels.iter().tuples().for_each(|(r, g, b, a)| {
                result.push(*r);
                result.push(*g);
                result.push(*b);
                result.push(*a);
            });
            Ok(result)
        }
        gltf::image::Format::R16G16B16 => {
            image
                .pixels
                .iter()
                .tuples()
                .map(|(a, b)| (u16::from_le_bytes([*a, *b]) / 257) as u8)
                .tuple_windows()
                .for_each(|(r, g, b)| {
                    result.push(255);
                    result.push(r);
                    result.push(g);
                    result.push(b);
                });
            Ok(result)
        }
        gltf::image::Format::R16G16B16A16 => {
            image
                .pixels
                .iter()
                .tuples()
                .map(|(a, b)| (u16::from_le_bytes([*a, *b]) / 257) as u8)
                .tuple_windows()
                .for_each(|(r, g, b, a)| {
                    result.push(a);
                    result.push(r);
                    result.push(g);
                    result.push(b);
                });
            Ok(result)
        }
        format => Err(Error::UnsupportedImageFormat { format }),
    }
}

fn sampler_get_min_filter(
    filter: Option<gltf::texture::MinFilter>,
) -> psp_file_formats::model::TextureFilter {
    use gltf::texture::MinFilter;
    use psp_file_formats::model::TextureFilter;
    match filter {
        Some(MinFilter::Linear) => TextureFilter::Linear,
        Some(MinFilter::LinearMipmapLinear) => TextureFilter::LinearMipmapLinear,
        Some(MinFilter::LinearMipmapNearest) => TextureFilter::LinearMipmapNearest,
        Some(MinFilter::Nearest) => TextureFilter::Nearest,
        Some(MinFilter::NearestMipmapLinear) => TextureFilter::NearestMipmapLinear,
        Some(MinFilter::NearestMipmapNearest) => TextureFilter::NearestMipmapNearest,
        None => TextureFilter::Linear,
    }
}

fn sampler_get_mag_filter(
    filter: Option<gltf::texture::MagFilter>,
) -> psp_file_formats::model::TextureFilter {
    match filter {
        Some(gltf::texture::MagFilter::Linear) => psp_file_formats::model::TextureFilter::Linear,
        Some(gltf::texture::MagFilter::Nearest) => psp_file_formats::model::TextureFilter::Nearest,
        None => psp_file_formats::model::TextureFilter::Linear,
    }
}

fn sampler_get_wrap_mode(
    mode: gltf::texture::WrappingMode,
) -> Result<psp_file_formats::model::GuTexWrapMode> {
    use gltf::texture::WrappingMode;
    use psp_file_formats::model::GuTexWrapMode;
    match mode {
        WrappingMode::ClampToEdge => Ok(GuTexWrapMode::Clamp),
        WrappingMode::Repeat => Ok(GuTexWrapMode::Repeat),
        WrappingMode::MirroredRepeat => Err(Error::UnsupportedSamplerWrapMode { mode }),
    }
}

fn material_state_flags(material: &gltf::Material) -> psp_file_formats::model::GuStateFlags {
    use gltf::material::AlphaMode;
    use psp_file_formats::model::GuStateFlags;
    let mut state_flags = GuStateFlags::default();
    if material.alpha_mode() == AlphaMode::Mask {
        state_flags.set(GuStateFlags::AlphaTest, true);
    } else if material.alpha_mode() == AlphaMode::Blend {
        state_flags.set(GuStateFlags::Blend, true);
    }
    state_flags.set(GuStateFlags::CullFace, !material.double_sided());
    let has_texture = material
        .pbr_metallic_roughness()
        .base_color_texture()
        .is_some();
    state_flags.set(GuStateFlags::Texture2D, has_texture);
    state_flags.set(GuStateFlags::Lighting, !material.unlit());
    state_flags
}

fn material_alpha_cutoff(material: &gltf::Material) -> Result<u8> {
    if let Some(alpha_cutoff) = material.alpha_cutoff() {
        if alpha_cutoff < 0.0 || 1.0 < alpha_cutoff {
            Err(Error::UnsupportedMaterialAlphaCutoff { alpha_cutoff })
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
            return Err(Error::UnsupportedMaterialBaseColorFactor { base_color_factor });
        }
    }
    let [r, g, b, a] = base_color_factor.map(|f| (f * 255.0) as u8);
    Ok(u32::from_ne_bytes([a, b, g, r]))
}

fn material_sampler_index(
    samplers: &Vec<psp_file_formats::model::Sampler>,
    material: &gltf::Material,
) -> Result<Option<usize>> {
    if let Some(texture) = material.pbr_metallic_roughness().base_color_texture() {
        if let Some(index) = texture.texture().sampler().index() {
            if index < samplers.len() {
                Ok(Some(index))
            } else {
                Err(Error::InvalidIndex {
                    semantic: IndexSemantic::Sampler,
                    error: IndexError::Missing { index },
                })
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

fn material_texture_index(
    textures: &Vec<psp_file_formats::model::Texture>,
    material: &gltf::Material,
) -> Result<Option<usize>> {
    if let Some(texture) = material.pbr_metallic_roughness().base_color_texture() {
        let index = texture.texture().source().index();
        if index < textures.len() {
            Ok(Some(index))
        } else {
            Err(Error::InvalidIndex {
                semantic: IndexSemantic::Texture,
                error: IndexError::Missing { index },
            })
        }
    } else {
        Ok(None)
    }
}

fn material_emission_color(material: &gltf::Material) -> Result<u32> {
    let emissive_factor = material.emissive_factor();
    for factor in emissive_factor {
        if factor < 0.0 || 1.0 < factor {
            return Err(Error::UnsupportedMaterialEmissiveFactor { emissive_factor });
        }
    }
    let [r, g, b] = emissive_factor.map(|f| (f * 255.0) as u8);
    Ok(u32::from_ne_bytes([255, b, g, r]))
}

fn primitive_material_index(
    materials: &Vec<psp_file_formats::model::Material>,
    material: &gltf::Material,
) -> Result<Option<usize>> {
    if let Some(index) = material.index() {
        if index < materials.len() {
            Ok(Some(index))
        } else {
            Err(Error::InvalidIndex {
                semantic: IndexSemantic::Material,
                error: IndexError::Missing { index },
            })
        }
    } else {
        Ok(None)
    }
}

fn primitive_type(mode: gltf::mesh::Mode) -> Result<psp_file_formats::model::GuPrimitive> {
    match mode {
        gltf::mesh::Mode::Points => Ok(psp_file_formats::model::GuPrimitive::Points),
        gltf::mesh::Mode::Lines => Ok(psp_file_formats::model::GuPrimitive::Lines),
        gltf::mesh::Mode::LineStrip => Ok(psp_file_formats::model::GuPrimitive::LineStrip),
        gltf::mesh::Mode::Triangles => Ok(psp_file_formats::model::GuPrimitive::Triangles),
        gltf::mesh::Mode::TriangleStrip => Ok(psp_file_formats::model::GuPrimitive::TriangleStrip),
        gltf::mesh::Mode::TriangleFan => Ok(psp_file_formats::model::GuPrimitive::TriangleFan),
        _ => Err(Error::UnsupportedPrimitive { mode }),
    }
}

#[derive(Default)]
struct GltfPrimitiveAttributes<'a> {
    index_accessor: Option<gltf::Accessor<'a>>,
    position_accessor: Option<gltf::Accessor<'a>>,
    normal_accessor: Option<gltf::Accessor<'a>>,
    tangent_accessor: Option<gltf::Accessor<'a>>,
    color_accessors: HashMap<u32, gltf::Accessor<'a>>,
    texcoord_accessors: HashMap<u32, gltf::Accessor<'a>>,
    joint_accessors: HashMap<u32, gltf::Accessor<'a>>,
    weight_accessors: HashMap<u32, gltf::Accessor<'a>>,
    morph_targets: Option<gltf::mesh::iter::MorphTargets<'a>>,
}

impl<'a> GltfPrimitiveAttributes<'a> {
    fn new(primitive: &gltf::Primitive<'a>) -> Self {
        let mut result = Self {
            index_accessor: primitive.indices(),
            morph_targets: Some(primitive.morph_targets()),
            ..Self::default()
        };
        for (sematic, accessor) in primitive.attributes() {
            match sematic {
                Semantic::Positions => {
                    result.position_accessor = Some(accessor);
                }
                Semantic::Normals => {
                    result.normal_accessor = Some(accessor);
                }
                Semantic::Tangents => {
                    result.tangent_accessor = Some(accessor);
                }
                Semantic::Colors(i) => {
                    result.color_accessors.insert(i, accessor);
                }
                Semantic::TexCoords(i) => {
                    result.texcoord_accessors.insert(i, accessor);
                }
                Semantic::Joints(i) => {
                    result.joint_accessors.insert(i, accessor);
                }
                Semantic::Weights(i) => {
                    result.weight_accessors.insert(i, accessor);
                }
            }
        }
        result
    }

    fn index_format(&self) -> Result<psp_mesh_writer::IndexFormat> {
        match &self.index_accessor {
            Some(accessor) => match accessor.dimensions() {
                accessor::Dimensions::Scalar => match accessor.data_type() {
                    gltf::accessor::DataType::U8 => Ok(psp_mesh_writer::IndexFormat::U8),
                    gltf::accessor::DataType::U16 => Ok(psp_mesh_writer::IndexFormat::U16),
                    data_type => Err(Error::InvalidAccessor {
                        semantic: AccessorSemantic::Index,
                        error: AccessorError::UnsupportedDataType { data_type },
                    }),
                },
                dimensions => Err(Error::InvalidAccessor {
                    semantic: AccessorSemantic::Index,
                    error: AccessorError::UnsupportedDimensions { dimensions },
                }),
            },
            None => Ok(psp_mesh_writer::IndexFormat::None),
        }
    }

    fn position_format(&self) -> Result<psp_mesh_writer::PositionFormat> {
        match &self.position_accessor {
            Some(accessor) => match accessor.dimensions() {
                accessor::Dimensions::Vec3 => match accessor.data_type() {
                    gltf::accessor::DataType::I8 => Ok(psp_mesh_writer::PositionFormat::I8),
                    gltf::accessor::DataType::I16 => Ok(psp_mesh_writer::PositionFormat::I16),
                    gltf::accessor::DataType::F32 => Ok(psp_mesh_writer::PositionFormat::F32),
                    data_type => Err(Error::InvalidAccessor {
                        semantic: AccessorSemantic::Position,
                        error: AccessorError::UnsupportedDataType { data_type },
                    }),
                },
                dimensions => Err(Error::InvalidAccessor {
                    semantic: AccessorSemantic::Position,
                    error: AccessorError::UnsupportedDimensions { dimensions },
                }),
            },
            None => Err(Error::InvalidAccessor {
                semantic: AccessorSemantic::Position,
                error: AccessorError::Missing { index: 0 },
            }),
        }
    }

    fn normal_format(&self) -> Result<psp_mesh_writer::NormalFormat> {
        match &self.normal_accessor {
            Some(accessor) => match accessor.dimensions() {
                accessor::Dimensions::Vec3 => match accessor.data_type() {
                    gltf::accessor::DataType::I8 => Ok(psp_mesh_writer::NormalFormat::I8),
                    gltf::accessor::DataType::I16 => Ok(psp_mesh_writer::NormalFormat::I16),
                    gltf::accessor::DataType::F32 => Ok(psp_mesh_writer::NormalFormat::F32),
                    data_type => Err(Error::InvalidAccessor {
                        semantic: AccessorSemantic::Normal,
                        error: AccessorError::UnsupportedDataType { data_type },
                    }),
                },
                dimensions => Err(Error::InvalidAccessor {
                    semantic: AccessorSemantic::Normal,
                    error: AccessorError::UnsupportedDimensions { dimensions },
                }),
            },
            None => Ok(psp_mesh_writer::NormalFormat::None),
        }
    }

    fn color_format(&self) -> Result<psp_mesh_writer::ColorFormat> {
        match self.color_accessors.len() {
            0 => Ok(psp_mesh_writer::ColorFormat::None),
            1 => match self.color_accessors.get(&0) {
                Some(accessor) => match accessor.dimensions() {
                    accessor::Dimensions::Vec3 | accessor::Dimensions::Vec4 => {
                        Ok(psp_mesh_writer::ColorFormat::A8B8G8R8)
                    }
                    dimensions => Err(Error::InvalidAccessor {
                        semantic: AccessorSemantic::Color,
                        error: AccessorError::UnsupportedDimensions { dimensions },
                    }),
                },
                None => Err(Error::InvalidAccessor {
                    semantic: AccessorSemantic::Color,
                    error: AccessorError::Missing { index: 0 },
                }),
            },
            count => Err(Error::InvalidAccessor {
                semantic: AccessorSemantic::Color,
                error: AccessorError::UnsupportedCount { count },
            }),
        }
    }

    fn texcoord_format(&self) -> Result<psp_mesh_writer::TexcoordFormat> {
        match self.texcoord_accessors.len() {
            0 => Ok(psp_mesh_writer::TexcoordFormat::None),
            1 => match self.texcoord_accessors.get(&0) {
                Some(accessor) => match accessor.dimensions() {
                    accessor::Dimensions::Vec2 => match accessor.data_type() {
                        gltf::accessor::DataType::U8 => Ok(psp_mesh_writer::TexcoordFormat::U8),
                        gltf::accessor::DataType::U16 => Ok(psp_mesh_writer::TexcoordFormat::U16),
                        gltf::accessor::DataType::F32 => Ok(psp_mesh_writer::TexcoordFormat::F32),
                        data_type => Err(Error::InvalidAccessor {
                            semantic: AccessorSemantic::TexCoord,
                            error: AccessorError::UnsupportedDataType { data_type },
                        }),
                    },
                    dimensions => Err(Error::InvalidAccessor {
                        semantic: AccessorSemantic::TexCoord,
                        error: AccessorError::UnsupportedDimensions { dimensions },
                    }),
                },
                None => Err(Error::InvalidAccessor {
                    semantic: AccessorSemantic::TexCoord,
                    error: AccessorError::Missing { index: 0 },
                }),
            },
            count => Err(Error::InvalidAccessor {
                semantic: AccessorSemantic::TexCoord,
                error: AccessorError::UnsupportedCount { count },
            }),
        }
    }

    fn weight_format(&self) -> Result<psp_mesh_writer::WeightFormat> {
        match self.weight_accessors.is_empty() {
            true => Ok(psp_mesh_writer::WeightFormat::None),
            false => {
                let data_types: Vec<_> = self
                    .weight_accessors
                    .values()
                    .map(|x| x.data_type())
                    .collect();
                let data_type = data_types[0];
                match data_types.iter().all(|&x| x == data_type) {
                    true => match self.weight_accessors.get(&0) {
                        Some(_) => match data_type {
                            gltf::accessor::DataType::U8 => Ok(psp_mesh_writer::WeightFormat::U8),
                            gltf::accessor::DataType::U16 => Ok(psp_mesh_writer::WeightFormat::U16),
                            gltf::accessor::DataType::F32 => Ok(psp_mesh_writer::WeightFormat::F32),
                            data_type => Err(Error::InvalidAccessor {
                                semantic: AccessorSemantic::TexCoord,
                                error: AccessorError::UnsupportedDataType { data_type },
                            }),
                        },
                        None => Err(Error::InvalidAccessor {
                            semantic: AccessorSemantic::TexCoord,
                            error: AccessorError::Missing { index: 0 },
                        }),
                    },
                    false => Err(Error::InvalidAccessor {
                        semantic: AccessorSemantic::TexCoord,
                        error: AccessorError::MismatchedDataTypes,
                    }),
                }
            }
        }
    }

    fn weight_count(&self) -> Result<psp_mesh_writer::Count> {
        let mut dimensions = 0;
        for accessor in self.weight_accessors.values() {
            dimensions += accessor.dimensions().multiplicity();
        }
        match dimensions {
            0 => Ok(psp_mesh_writer::COUNT_1),
            1 => Ok(psp_mesh_writer::COUNT_2),
            2 => Ok(psp_mesh_writer::COUNT_3),
            3 => Ok(psp_mesh_writer::COUNT_4),
            4 => Ok(psp_mesh_writer::COUNT_5),
            5 => Ok(psp_mesh_writer::COUNT_6),
            6 => Ok(psp_mesh_writer::COUNT_7),
            7 => Ok(psp_mesh_writer::COUNT_8),
            count => Err(Error::UnsupportedWeightCount { count }),
        }
    }

    fn morph_count(&self) -> Result<psp_mesh_writer::Count> {
        match self.morph_targets.as_ref().map_or(0, |x| x.len()) {
            0 => Ok(psp_mesh_writer::COUNT_1),
            1 => Ok(psp_mesh_writer::COUNT_2),
            2 => Ok(psp_mesh_writer::COUNT_3),
            3 => Ok(psp_mesh_writer::COUNT_4),
            4 => Ok(psp_mesh_writer::COUNT_5),
            5 => Ok(psp_mesh_writer::COUNT_6),
            6 => Ok(psp_mesh_writer::COUNT_7),
            7 => Ok(psp_mesh_writer::COUNT_8),
            count => Err(Error::UnsupportedMorphCount { count }),
        }
    }
}

fn mesh_description(
    attributes: &GltfPrimitiveAttributes,
) -> Result<psp_mesh_writer::MeshDescription> {
    let mesh_description =
        psp_mesh_writer::MeshDescriptionBuilder::new(attributes.position_format()?)
            .index_format(attributes.index_format()?)
            .weight_format_with_count(attributes.weight_format()?, attributes.weight_count()?)
            .normal_format(attributes.normal_format()?)
            .color_format(attributes.color_format()?)
            .texcoord_format(attributes.texcoord_format()?)
            .morph_count(attributes.morph_count()?)
            .build();
    Ok(mesh_description)
}

fn remove_and_remap_values<T>(used: &HashSet<usize>, values: &mut Vec<T>) -> Vec<usize> {
    let mut map = (0..values.len()).collect::<Vec<_>>();
    if used.len() != values.len() {
        let mut new_idx = 0;
        let mut removed_idx = 0;
        while new_idx < values.len() {
            let old_idx = new_idx + removed_idx;
            if !used.contains(&old_idx) {
                values.remove(new_idx);
                removed_idx += 1;
            } else {
                map[old_idx] = new_idx;
                new_idx += 1;
            }
        }
    }
    map
}
