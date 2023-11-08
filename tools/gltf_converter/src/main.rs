use std::{collections::HashMap, io::Write, path::PathBuf};

use aligned_vec::AVec;
use clap::Parser;
use gltf::{accessor, Semantic};
use thiserror::Error;

mod image;
mod material;
mod sampler;

mod compression;
mod stripping;

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
    #[error("image error: {0}")]
    Image(#[from] image::Error),
    #[error("sampler error: {0}")]
    Sampler(#[from] sampler::Error),
    #[error("material error: {0}")]
    Material(#[from] material::Error),
    #[error("unsupported primitive mode: {mode:?}")]
    UnsupportedPrimitive { mode: gltf::mesh::Mode },
    #[error("unsupported weight count: {count:?}")]
    UnsupportedWeightCount { count: usize },
    #[error("unsupported morph count: {count:?}")]
    UnsupportedMorphCount { count: usize },
    #[error("missing material: {index:?}")]
    MissingMaterial { index: usize },
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
    #[arg()]
    file: PathBuf,
    #[arg(short, long, default_value_t = true)]
    strip: bool,
    #[arg(short, long, default_value_t = true)]
    compress: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let (gltf, buffers, images) = gltf::import(args.file.clone())?;

    let mut textures = image::read_textures(&images)?;
    let mut samplers = sampler::read_samplers(gltf.samplers())?;
    let mut materials = material::read_materials(&textures, &samplers, gltf.materials())?;

    let mut meshes = vec![];
    for mesh in gltf.meshes() {
        for primitive in mesh.primitives() {
            let material = primitive_material(&materials, &primitive.material())?;
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
                material,
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

    if args.strip {
        stripping::strip_unused(&mut textures, &mut samplers, &mut materials, &mut meshes);
    }

    if args.compress {
        compression::compress_textures_dxt(&materials, &mut textures);
    }

    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
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

fn primitive_material(
    materials: &[psp_file_formats::model::Material],
    material: &gltf::Material,
) -> Result<Option<usize>> {
    if let Some(index) = material.index() {
        if index < materials.len() {
            Ok(Some(index))
        } else {
            Err(Error::MissingMaterial { index })
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
