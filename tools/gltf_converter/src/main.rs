use std::{
    collections::{HashMap, HashSet},
    io::Write,
    path::PathBuf,
};

use clap::Parser;
use gltf::{accessor, Semantic};
use thiserror::Error;

#[derive(Debug)]
pub enum GltfAccessorSemantic {
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
pub enum GltfAccessorError {
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
pub enum GltfConversionError {
    #[error("gltf error: {0}")]
    Gltf(#[from] gltf::Error),
    #[error("buffer missing blob: {index:?}")]
    BufferMissingBlob { index: usize },
    #[error("buffer missing blob: {index:?}")]
    BufferFormatUnsupported { index: usize },
    #[error("unsupported primitive mode: {mode:?}")]
    UnsupportedPrimitive { mode: gltf::mesh::Mode },
    #[error("unsupported weight count: {count:?}")]
    UnsupportedWeightCount { count: usize },
    #[error("unsupported morph count: {count:?}")]
    UnsupportedMorphCount { count: usize },
    #[error("invalid accessor ({semantic:?}): {error:?}")]
    InvalidAccessor {
        semantic: GltfAccessorSemantic,
        error: GltfAccessorError,
    },
    #[error("mesh writer error: {0:?}")]
    MeshWriterError(psp_mesh_writer::MeshWriterError),
}

impl From<psp_mesh_writer::MeshWriterError> for GltfConversionError {
    fn from(value: psp_mesh_writer::MeshWriterError) -> Self {
        GltfConversionError::MeshWriterError(value)
    }
}

type GltfConversionResult<T, E = GltfConversionError> = Result<T, E>;

#[derive(Parser)]
struct Args {
    #[arg(default_value = "C:/Users/Josh/Desktop/game-psp/target/debug/BoxVertexColors.glb")]
    file: PathBuf,
    #[arg(short, long)]
    compress: Option<bool>,
}

fn main() -> GltfConversionResult<()> {
    let args = Args::parse();
    let (gltf, buffers, _images) = gltf::import(args.file.clone())?;

    let mut meshes = vec![];
    let mut skinned_meshes = HashSet::<usize>::default();
    let mut non_skinned_meshes = HashSet::<usize>::default();
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
            let _label = primitive_label(&mesh, &primitive);
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
                let colors: Vec<u32> = colors
                    .into_rgba_u8()
                    .map(|c| {
                        (c[3] as u32) << 24 | (c[2] as u32) << 16 | (c[1] as u32) << 8 | c[0] as u32
                    })
                    .collect();
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
                primitive_type: primitive_type as u32,
                vertex_type: mesh_description.flags(),
                vertex_count: if index_count > 0 {
                    index_count as u32
                } else {
                    vertex_count as u32
                },
                index_buffer,
                vertex_buffer,
            });
        }
    }

    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open(args.file.clone().with_extension("psp"))
        .unwrap();

    let model_file = psp_file_formats::model::File { meshes };

    file.write_all(&postcard::to_allocvec(&model_file).unwrap())
        .unwrap();

    Ok(())
}

fn primitive_label(mesh: &gltf::Mesh, primitive: &gltf::Primitive) -> String {
    format!("Mesh{}/Primitive{}", mesh.index(), primitive.index())
}

fn primitive_type(mode: gltf::mesh::Mode) -> GltfConversionResult<u8> {
    match mode {
        gltf::mesh::Mode::Points => Ok(0),
        gltf::mesh::Mode::Lines => Ok(1),
        gltf::mesh::Mode::LineStrip => Ok(2),
        gltf::mesh::Mode::Triangles => Ok(3),
        gltf::mesh::Mode::TriangleStrip => Ok(4),
        gltf::mesh::Mode::TriangleFan => Ok(5),
        _ => Err(GltfConversionError::UnsupportedPrimitive { mode }),
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

    fn index_format(&self) -> GltfConversionResult<psp_mesh_writer::IndexFormat> {
        match &self.index_accessor {
            Some(accessor) => match accessor.dimensions() {
                accessor::Dimensions::Scalar => match accessor.data_type() {
                    gltf::accessor::DataType::U8 => Ok(psp_mesh_writer::IndexFormat::U8),
                    gltf::accessor::DataType::U16 => Ok(psp_mesh_writer::IndexFormat::U16),
                    data_type => Err(GltfConversionError::InvalidAccessor {
                        semantic: GltfAccessorSemantic::Index,
                        error: GltfAccessorError::UnsupportedDataType { data_type },
                    }),
                },
                dimensions => Err(GltfConversionError::InvalidAccessor {
                    semantic: GltfAccessorSemantic::Index,
                    error: GltfAccessorError::UnsupportedDimensions { dimensions },
                }),
            },
            None => Ok(psp_mesh_writer::IndexFormat::None),
        }
    }

    fn position_format(&self) -> GltfConversionResult<psp_mesh_writer::PositionFormat> {
        match &self.position_accessor {
            Some(accessor) => match accessor.dimensions() {
                accessor::Dimensions::Vec3 => match accessor.data_type() {
                    gltf::accessor::DataType::I8 => Ok(psp_mesh_writer::PositionFormat::I8),
                    gltf::accessor::DataType::I16 => Ok(psp_mesh_writer::PositionFormat::I16),
                    gltf::accessor::DataType::F32 => Ok(psp_mesh_writer::PositionFormat::F32),
                    data_type => Err(GltfConversionError::InvalidAccessor {
                        semantic: GltfAccessorSemantic::Position,
                        error: GltfAccessorError::UnsupportedDataType { data_type },
                    }),
                },
                dimensions => Err(GltfConversionError::InvalidAccessor {
                    semantic: GltfAccessorSemantic::Position,
                    error: GltfAccessorError::UnsupportedDimensions { dimensions },
                }),
            },
            None => Err(GltfConversionError::InvalidAccessor {
                semantic: GltfAccessorSemantic::Position,
                error: GltfAccessorError::Missing { index: 0 },
            }),
        }
    }

    fn normal_format(&self) -> GltfConversionResult<psp_mesh_writer::NormalFormat> {
        match &self.normal_accessor {
            Some(accessor) => match accessor.dimensions() {
                accessor::Dimensions::Vec3 => match accessor.data_type() {
                    gltf::accessor::DataType::I8 => Ok(psp_mesh_writer::NormalFormat::I8),
                    gltf::accessor::DataType::I16 => Ok(psp_mesh_writer::NormalFormat::I16),
                    gltf::accessor::DataType::F32 => Ok(psp_mesh_writer::NormalFormat::F32),
                    data_type => Err(GltfConversionError::InvalidAccessor {
                        semantic: GltfAccessorSemantic::Normal,
                        error: GltfAccessorError::UnsupportedDataType { data_type },
                    }),
                },
                dimensions => Err(GltfConversionError::InvalidAccessor {
                    semantic: GltfAccessorSemantic::Normal,
                    error: GltfAccessorError::UnsupportedDimensions { dimensions },
                }),
            },
            None => Ok(psp_mesh_writer::NormalFormat::None),
        }
    }

    fn color_format(&self) -> GltfConversionResult<psp_mesh_writer::ColorFormat> {
        match self.color_accessors.len() {
            0 => Ok(psp_mesh_writer::ColorFormat::None),
            1 => match self.color_accessors.get(&0) {
                Some(accessor) => match accessor.dimensions() {
                    accessor::Dimensions::Vec3 | accessor::Dimensions::Vec4 => {
                        Ok(psp_mesh_writer::ColorFormat::R8G8B8A8)
                    }
                    dimensions => Err(GltfConversionError::InvalidAccessor {
                        semantic: GltfAccessorSemantic::Color,
                        error: GltfAccessorError::UnsupportedDimensions { dimensions },
                    }),
                },
                None => Err(GltfConversionError::InvalidAccessor {
                    semantic: GltfAccessorSemantic::Color,
                    error: GltfAccessorError::Missing { index: 0 },
                }),
            },
            count => Err(GltfConversionError::InvalidAccessor {
                semantic: GltfAccessorSemantic::Color,
                error: GltfAccessorError::UnsupportedCount { count },
            }),
        }
    }

    fn texcoord_format(&self) -> GltfConversionResult<psp_mesh_writer::TexcoordFormat> {
        match self.texcoord_accessors.len() {
            0 => Ok(psp_mesh_writer::TexcoordFormat::None),
            1 => match self.texcoord_accessors.get(&0) {
                Some(accessor) => match accessor.dimensions() {
                    accessor::Dimensions::Vec2 => match accessor.data_type() {
                        gltf::accessor::DataType::U8 => Ok(psp_mesh_writer::TexcoordFormat::U8),
                        gltf::accessor::DataType::U16 => Ok(psp_mesh_writer::TexcoordFormat::U16),
                        gltf::accessor::DataType::F32 => Ok(psp_mesh_writer::TexcoordFormat::F32),
                        data_type => Err(GltfConversionError::InvalidAccessor {
                            semantic: GltfAccessorSemantic::TexCoord,
                            error: GltfAccessorError::UnsupportedDataType { data_type },
                        }),
                    },
                    dimensions => Err(GltfConversionError::InvalidAccessor {
                        semantic: GltfAccessorSemantic::TexCoord,
                        error: GltfAccessorError::UnsupportedDimensions { dimensions },
                    }),
                },
                None => Err(GltfConversionError::InvalidAccessor {
                    semantic: GltfAccessorSemantic::TexCoord,
                    error: GltfAccessorError::Missing { index: 0 },
                }),
            },
            count => Err(GltfConversionError::InvalidAccessor {
                semantic: GltfAccessorSemantic::TexCoord,
                error: GltfAccessorError::UnsupportedCount { count },
            }),
        }
    }

    fn weight_format(&self) -> GltfConversionResult<psp_mesh_writer::WeightFormat> {
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
                            data_type => Err(GltfConversionError::InvalidAccessor {
                                semantic: GltfAccessorSemantic::TexCoord,
                                error: GltfAccessorError::UnsupportedDataType { data_type },
                            }),
                        },
                        None => Err(GltfConversionError::InvalidAccessor {
                            semantic: GltfAccessorSemantic::TexCoord,
                            error: GltfAccessorError::Missing { index: 0 },
                        }),
                    },
                    false => Err(GltfConversionError::InvalidAccessor {
                        semantic: GltfAccessorSemantic::TexCoord,
                        error: GltfAccessorError::MismatchedDataTypes,
                    }),
                }
            }
        }
    }

    fn weight_count(&self) -> GltfConversionResult<psp_mesh_writer::Count> {
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
            count => Err(GltfConversionError::UnsupportedWeightCount { count }),
        }
    }

    fn morph_count(&self) -> GltfConversionResult<psp_mesh_writer::Count> {
        match self.morph_targets.as_ref().map_or(0, |x| x.len()) {
            0 => Ok(psp_mesh_writer::COUNT_1),
            1 => Ok(psp_mesh_writer::COUNT_2),
            2 => Ok(psp_mesh_writer::COUNT_3),
            3 => Ok(psp_mesh_writer::COUNT_4),
            4 => Ok(psp_mesh_writer::COUNT_5),
            5 => Ok(psp_mesh_writer::COUNT_6),
            6 => Ok(psp_mesh_writer::COUNT_7),
            7 => Ok(psp_mesh_writer::COUNT_8),
            count => Err(GltfConversionError::UnsupportedMorphCount { count }),
        }
    }
}

fn mesh_description(
    attributes: &GltfPrimitiveAttributes,
) -> GltfConversionResult<psp_mesh_writer::MeshDescription> {
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
