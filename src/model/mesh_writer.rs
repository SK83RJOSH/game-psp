#![allow(dead_code)]

use core::mem::size_of;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TexcoordFormat {
    None,
    U8,
    U16,
    F32,
}

impl TexcoordFormat {
    pub fn stride(&self) -> usize {
        match *self {
            TexcoordFormat::None => 0,
            TexcoordFormat::U8 => size_of::<[u8; 2]>(),
            TexcoordFormat::U16 => size_of::<[u16; 2]>(),
            TexcoordFormat::F32 => size_of::<[f32; 2]>(),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ColorFormat {
    None,
    R5G6B5A0,
    R5G5B5A1,
    R4G4B4A4,
    R8G8B8A8,
}

impl ColorFormat {
    pub fn stride(&self) -> usize {
        match *self {
            ColorFormat::None => 0,
            ColorFormat::R5G6B5A0 | ColorFormat::R5G5B5A1 | ColorFormat::R4G4B4A4 => {
                size_of::<u16>()
            }
            ColorFormat::R8G8B8A8 => size_of::<u32>(),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NormalFormat {
    None,
    I8,
    I16,
    F32,
}

impl NormalFormat {
    pub fn stride(&self) -> usize {
        match *self {
            NormalFormat::None => 0,
            NormalFormat::I8 => size_of::<[i8; 3]>(),
            NormalFormat::I16 => size_of::<[i16; 3]>(),
            NormalFormat::F32 => size_of::<[f32; 3]>(),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PositionFormat {
    I8,
    I16,
    F32,
}

impl PositionFormat {
    pub fn stride(&self) -> usize {
        match *self {
            PositionFormat::I8 => size_of::<[i8; 3]>(),
            PositionFormat::I16 => size_of::<[i16; 3]>(),
            PositionFormat::F32 => size_of::<[f32; 3]>(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct VertexDescription {
    texcoord_format: TexcoordFormat,
    color_format: ColorFormat,
    normal_format: NormalFormat,
    position_format: PositionFormat,
    color_offset: usize,
    normal_offset: usize,
    position_offset: usize,
    stride: usize,
}

impl VertexDescription {
    pub fn vertex_type(&self) -> psp::sys::VertexType {
        let (tex, nor, pos) = (
            self.texcoord_format as i32,
            (self.normal_format as i32) << 2,
            (self.position_format as i32 + 1) << 7,
        );
        let col = match self.color_format {
            ColorFormat::None => 0,
            _ => (self.color_format as i32 + 3) << 2,
        };
        psp::sys::VertexType::from_bits_truncate(tex | col | nor | pos)
    }

    pub fn texcoord_format(&self) -> TexcoordFormat {
        self.texcoord_format
    }

    pub fn color_format(&self) -> ColorFormat {
        self.color_format
    }

    pub fn normal_format(&self) -> NormalFormat {
        self.normal_format
    }

    pub fn position_format(&self) -> PositionFormat {
        self.position_format
    }

    pub fn texcoord_offset(&self) -> Option<usize> {
        match self.texcoord_format {
            TexcoordFormat::None => None,
            _ => Some(0),
        }
    }

    pub fn color_offset(&self) -> Option<usize> {
        match self.color_format {
            ColorFormat::None => None,
            _ => Some(self.color_offset),
        }
    }

    pub fn normal_offset(&self) -> Option<usize> {
        match self.normal_format {
            NormalFormat::None => None,
            _ => Some(self.normal_offset),
        }
    }

    pub fn position_offset(&self) -> usize {
        self.position_offset
    }

    pub fn stride(&self) -> usize {
        self.stride
    }
}

pub struct VertexDescriptionBuilder {
    texcoord_format: TexcoordFormat,
    color_format: ColorFormat,
    normal_format: NormalFormat,
    position_format: PositionFormat,
}

impl VertexDescriptionBuilder {
    pub fn new(position_format: PositionFormat) -> Self {
        Self {
            texcoord_format: TexcoordFormat::None,
            color_format: ColorFormat::None,
            normal_format: NormalFormat::None,
            position_format,
        }
    }

    pub fn texcoord_format(&mut self, texcoord_format: TexcoordFormat) -> &mut Self {
        self.texcoord_format = texcoord_format;
        self
    }

    pub fn color_format(&mut self, color_format: ColorFormat) -> &mut Self {
        self.color_format = color_format;
        self
    }

    pub fn normal_format(&mut self, normal_format: NormalFormat) -> &mut Self {
        self.normal_format = normal_format;
        self
    }

    pub fn position_format(&mut self, position_format: PositionFormat) -> &mut Self {
        self.position_format = position_format;
        self
    }

    pub fn build(&self) -> VertexDescription {
        let color_offset = self.texcoord_format.stride();
        let normal_offset = color_offset + self.color_format.stride();
        let position_offset = normal_offset + self.normal_format.stride();
        let stride = position_offset + self.position_format.stride();
        VertexDescription {
            texcoord_format: self.texcoord_format,
            color_format: self.color_format,
            normal_format: self.normal_format,
            position_format: self.position_format,
            color_offset,
            normal_offset,
            position_offset,
            stride,
        }
    }
}

#[derive(Debug)]
pub enum MeshWriterError {
    AtMaxVertexCount,
    OutOfMemory { capacity: usize, requested: usize },
    TexcoordFormat { expected: TexcoordFormat },
    ColorFormat { expected: ColorFormat },
    NormalFormat { expected: NormalFormat },
    PositionFormat { expected: PositionFormat },
}

pub struct MeshWriter<'a> {
    vertex_description: VertexDescription,
    vertex_buffer: &'a mut [u8],
    vertex_count: u16,
}

type MeshBuilderResult<T, E = MeshWriterError> = Result<T, E>;

impl<'a> MeshWriter<'a> {
    pub fn new(vertex_description: VertexDescription, vertex_buffer: &'a mut [u8]) -> Self {
        Self {
            vertex_description,
            vertex_buffer,
            vertex_count: 0,
        }
    }

    fn check_capacity(&mut self) -> MeshBuilderResult<&mut Self> {
        if self.vertex_count < u16::MAX {
            let capacity = self.vertex_buffer.len();
            let requested = self.vertex_description.stride() * (self.vertex_count + 1) as usize;
            if requested > capacity {
                Err(MeshWriterError::OutOfMemory {
                    capacity,
                    requested,
                })
            } else {
                Ok(self)
            }
        } else {
            Err(MeshWriterError::AtMaxVertexCount)
        }
    }

    pub fn insert(&mut self) -> MeshBuilderResult<&mut Self> {
        self.check_capacity()?;
        self.vertex_count += 1;
        Ok(self)
    }

    unsafe fn write<T>(
        &mut self,
        src: *const T,
        offset: usize,
        stride: usize,
    ) -> MeshBuilderResult<&mut Self> {
        self.check_capacity()?;
        let offset = (self.vertex_count as usize * self.vertex_description.stride) + offset;
        core::ptr::copy_nonoverlapping(src as _, &mut self.vertex_buffer[offset], stride);
        Ok(self)
    }

    unsafe fn texcoord<T>(
        &mut self,
        format: TexcoordFormat,
        texcoord: &[T; 2],
    ) -> MeshBuilderResult<&mut Self> {
        if self.vertex_description.texcoord_format == format {
            self.write(texcoord.as_ptr(), 0, format.stride())
        } else {
            Err(MeshWriterError::TexcoordFormat {
                expected: self.vertex_description.texcoord_format,
            })
        }
    }

    pub fn texcoord_i8(&mut self, texcoord: &[u8; 2]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.texcoord(TexcoordFormat::U8, texcoord) }
    }

    pub fn texcoord_i16(&mut self, texcoord: &[u16; 2]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.texcoord(TexcoordFormat::U16, texcoord) }
    }

    pub fn texcoord_f32(&mut self, texcoord: &[f32; 2]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.texcoord(TexcoordFormat::F32, texcoord) }
    }

    unsafe fn color<T>(&mut self, color: T) -> MeshBuilderResult<&mut Self> {
        if self.vertex_description.color_format.stride() == size_of::<T>() {
            self.write(
                core::slice::from_raw_parts(&color, 1).as_ptr(),
                self.vertex_description.color_offset,
                size_of::<T>(),
            )
        } else {
            Err(MeshWriterError::ColorFormat {
                expected: self.vertex_description.color_format,
            })
        }
    }

    pub fn color_u16(&mut self, color: u16) -> MeshBuilderResult<&mut Self> {
        unsafe { self.color(color) }
    }

    pub fn color_u32(&mut self, color: u32) -> MeshBuilderResult<&mut Self> {
        unsafe { self.color(color) }
    }

    unsafe fn normal<T>(
        &mut self,
        format: NormalFormat,
        normal: &[T; 3],
    ) -> MeshBuilderResult<&mut Self> {
        if self.vertex_description.normal_format == format {
            self.write(
                normal.as_ptr(),
                self.vertex_description.normal_offset,
                format.stride(),
            )
        } else {
            Err(MeshWriterError::NormalFormat {
                expected: self.vertex_description.normal_format,
            })
        }
    }

    pub fn normal_i8(&mut self, normal: &[i8; 3]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.normal(NormalFormat::I8, normal) }
    }

    pub fn normal_i16(&mut self, normal: &[i16; 3]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.normal(NormalFormat::I16, normal) }
    }

    pub fn normal_f32(&mut self, normal: &[f32; 3]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.normal(NormalFormat::F32, normal) }
    }

    unsafe fn position<T>(
        &mut self,
        format: PositionFormat,
        position: &[T; 3],
    ) -> MeshBuilderResult<&mut Self> {
        if self.vertex_description.position_format == format {
            self.write(
                position.as_ptr(),
                self.vertex_description.position_offset,
                format.stride(),
            )
        } else {
            Err(MeshWriterError::PositionFormat {
                expected: self.vertex_description.position_format,
            })
        }
    }

    pub fn position_i8(&mut self, position: &[i8; 3]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.position(PositionFormat::I8, position) }
    }

    pub fn position_i16(&mut self, position: &[i16; 3]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.position(PositionFormat::I16, position) }
    }

    pub fn position_f32(&mut self, position: &[f32; 3]) -> MeshBuilderResult<&mut Self> {
        unsafe { self.position(PositionFormat::F32, position) }
    }

    pub fn vertex_count(&self) -> u16 {
        self.vertex_count
    }
}
