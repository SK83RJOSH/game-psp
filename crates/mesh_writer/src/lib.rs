#![no_std]

use core::{mem::size_of, slice};

pub mod prelude;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IndexFormat {
    None,
    U8,
    U16,
}

impl IndexFormat {
    pub fn stride(&self) -> usize {
        match *self {
            IndexFormat::None => 0,
            IndexFormat::U8 => size_of::<u8>(),
            IndexFormat::U16 => size_of::<u16>(),
        }
    }
}

pub trait Index {
    const FORMAT: IndexFormat;
}

impl Index for u8 {
    const FORMAT: IndexFormat = IndexFormat::U8;
}

impl Index for u16 {
    const FORMAT: IndexFormat = IndexFormat::U16;
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum WeightFormat {
    None,
    U8,
    U16,
    F32,
}

impl WeightFormat {
    pub fn stride(&self) -> usize {
        match *self {
            WeightFormat::None => 0,
            WeightFormat::U8 => size_of::<u8>(),
            WeightFormat::U16 => size_of::<u16>(),
            WeightFormat::F32 => size_of::<f32>(),
        }
    }
}

pub trait Weight {
    const FORMAT: WeightFormat;
}

impl Weight for u8 {
    const FORMAT: WeightFormat = WeightFormat::U8;
}

impl Weight for u16 {
    const FORMAT: WeightFormat = WeightFormat::U16;
}

impl Weight for f32 {
    const FORMAT: WeightFormat = WeightFormat::F32;
}

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

pub trait Texcoord {
    const FORMAT: TexcoordFormat;
}

impl Texcoord for [u8; 2] {
    const FORMAT: TexcoordFormat = TexcoordFormat::U8;
}

impl Texcoord for [u16; 2] {
    const FORMAT: TexcoordFormat = TexcoordFormat::U16;
}

impl Texcoord for [f32; 2] {
    const FORMAT: TexcoordFormat = TexcoordFormat::F32;
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

pub trait Color {}

impl Color for u16 {}
impl Color for u32 {}

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

pub trait Normal {
    const FORMAT: NormalFormat;
}

impl Normal for [i8; 3] {
    const FORMAT: NormalFormat = NormalFormat::I8;
}

impl Normal for [i16; 3] {
    const FORMAT: NormalFormat = NormalFormat::I16;
}

impl Normal for [f32; 3] {
    const FORMAT: NormalFormat = NormalFormat::F32;
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

pub trait Position {
    const FORMAT: PositionFormat;
}

impl Position for [i8; 3] {
    const FORMAT: PositionFormat = PositionFormat::I8;
}

impl Position for [i16; 3] {
    const FORMAT: PositionFormat = PositionFormat::I16;
}

impl Position for [f32; 3] {
    const FORMAT: PositionFormat = PositionFormat::F32;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Count(usize);

pub const COUNT_1: Count = Count(1);
pub const COUNT_2: Count = Count(2);
pub const COUNT_3: Count = Count(3);
pub const COUNT_4: Count = Count(4);
pub const COUNT_5: Count = Count(5);
pub const COUNT_6: Count = Count(6);
pub const COUNT_7: Count = Count(7);
pub const COUNT_8: Count = Count(8);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MeshDescription {
    index_format: IndexFormat,
    weight_format: WeightFormat,
    texcoord_format: TexcoordFormat,
    color_format: ColorFormat,
    normal_format: NormalFormat,
    position_format: PositionFormat,
    weight_count: Count,
    morph_count: Count,
    texcoord_offset: usize,
    color_offset: usize,
    normal_offset: usize,
    position_offset: usize,
    stride: usize,
}

impl MeshDescription {
    pub fn flags(&self) -> u32 {
        let (tf, nf, pf, wf, idf, wc, mc) = (
            self.texcoord_format as u32,
            (self.normal_format as u32) << 5,
            (self.position_format as u32 + 1) << 7,
            (self.weight_format as u32) << 9,
            (self.index_format as u32) << 11,
            (self.weight_count.0 as u32 - 1) << 14,
            (self.morph_count.0 as u32 - 1) << 18,
        );
        let cf = match self.color_format {
            ColorFormat::None => 0,
            _ => (self.color_format as u32 + 3) << 2,
        };
        tf | cf | nf | pf | wf | idf | wc | mc
    }

    pub fn index_format(&self) -> IndexFormat {
        self.index_format
    }

    pub fn index_stride(&self) -> usize {
        self.index_format.stride()
    }

    pub fn index_buffer_size(&self, count: usize) -> usize {
        self.index_format.stride() * count
    }

    pub fn weight_format(&self) -> WeightFormat {
        self.weight_format
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

    pub fn weight_count(&self) -> Option<Count> {
        match self.weight_format {
            WeightFormat::None => None,
            _ => Some(self.weight_count),
        }
    }

    pub fn morph_count(&self) -> usize {
        self.morph_count.0
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

    pub fn vertex_stride(&self) -> usize {
        self.stride
    }

    pub fn vertex_buffer_size(&self, count: usize) -> usize {
        self.stride * self.morph_count.0 * count
    }
}

pub struct MeshDescriptionBuilder {
    index_format: IndexFormat,
    weight_format: WeightFormat,
    weight_count: Count,
    texcoord_format: TexcoordFormat,
    color_format: ColorFormat,
    normal_format: NormalFormat,
    position_format: PositionFormat,
    morph_count: Count,
}

impl MeshDescriptionBuilder {
    pub fn new(position_format: PositionFormat) -> Self {
        Self {
            index_format: IndexFormat::None,
            weight_format: WeightFormat::None,
            weight_count: COUNT_1,
            texcoord_format: TexcoordFormat::None,
            color_format: ColorFormat::None,
            normal_format: NormalFormat::None,
            position_format,
            morph_count: COUNT_1,
        }
    }

    pub fn index_format(&mut self, index_format: IndexFormat) -> &mut Self {
        self.index_format = index_format;
        self
    }

    pub fn weight_format(&mut self, weight_format: WeightFormat) -> &mut Self {
        self.weight_format = weight_format;
        self.weight_count = COUNT_1;
        self
    }

    pub fn weight_format_with_count(
        &mut self,
        weight_format: WeightFormat,
        weight_count: Count,
    ) -> &mut Self {
        self.weight_format = weight_format;
        self.weight_count = weight_count;
        self
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

    pub fn morph_count(&mut self, morph_count: Count) -> &mut Self {
        self.morph_count = morph_count;
        self
    }

    pub fn build(&self) -> MeshDescription {
        let weight_count = self.weight_count.0;
        let texcoord_offset = self.weight_format.stride() * weight_count;
        let color_offset = texcoord_offset + self.texcoord_format.stride();
        let normal_offset = color_offset + self.color_format.stride();
        let position_offset = normal_offset + self.normal_format.stride();
        let stride = position_offset + self.position_format.stride();
        MeshDescription {
            index_format: self.index_format,
            weight_format: self.weight_format,
            texcoord_format: self.texcoord_format,
            color_format: self.color_format,
            normal_format: self.normal_format,
            position_format: self.position_format,
            weight_count: self.weight_count,
            morph_count: self.morph_count,
            texcoord_offset,
            color_offset,
            normal_offset,
            position_offset,
            stride,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    AtMaxVertexCount,
    AtMaxIndexCount,
    OutOfVertexMemory { capacity: usize, requested: usize },
    OutOfIndexMemory { capacity: usize, requested: usize },
    IndexOutOfRange { vertex_count: u16, index_value: u16 },
    IndexFormat { expected: IndexFormat },
    WeightFormat { expected: WeightFormat },
    TexcoordFormat { expected: TexcoordFormat },
    ColorFormat { expected: ColorFormat },
    NormalFormat { expected: NormalFormat },
    PositionFormat { expected: PositionFormat },
    WeightCount { expected: usize },
    MorphCount { expected: usize },
}

type Result<T, E = Error> = core::result::Result<T, E>;

pub struct MeshWriter<'a> {
    mesh_description: MeshDescription,
    vertex_buffer: &'a mut [u8],
    vertex_count: u16,
    index_buffer: Option<&'a mut [u8]>,
    index_count: u16,
}

impl<'a> MeshWriter<'a> {
    pub fn new(mesh_description: MeshDescription, vertex_buffer: &'a mut [u8]) -> Self {
        Self {
            mesh_description,
            vertex_buffer,
            vertex_count: 0,
            index_buffer: None,
            index_count: 0,
        }
    }

    pub fn new_indexed(
        mesh_description: MeshDescription,
        vertex_buffer: &'a mut [u8],
        index_buffer: &'a mut [u8],
    ) -> Self {
        Self {
            mesh_description,
            vertex_buffer,
            vertex_count: 0,
            index_buffer: Some(index_buffer),
            index_count: 0,
        }
    }

    pub fn check_index_capacity(&mut self, count: usize) -> Result<&mut Self> {
        if count > (u16::MAX - self.index_count) as usize {
            return Err(Error::AtMaxIndexCount);
        }
        let requested =
            self.mesh_description.index_format.stride() * (self.index_count as usize + count);
        let capacity = self.index_buffer.as_ref().map_or(0, |b| b.len());
        if requested > capacity {
            return Err(Error::OutOfIndexMemory {
                capacity,
                requested,
            });
        }
        Ok(self)
    }

    pub fn tell_index(&self) -> u16 {
        self.index_count
    }

    pub fn indices<T: Index + bytemuck::Pod + Into<u16>>(
        &mut self,
        indices: &[T],
    ) -> Result<&mut Self> {
        if T::FORMAT != self.mesh_description.index_format {
            return Err(Error::IndexFormat {
                expected: self.mesh_description.index_format,
            });
        }
        let count = indices.len();
        self.check_index_capacity(count)?;
        if let Some(&i) = indices.iter().find(|&&i| i.into() >= self.vertex_count) {
            return Err(Error::IndexOutOfRange {
                vertex_count: self.vertex_count,
                index_value: i.into(),
            });
        }
        let source_buffer: &[u8] = bytemuck::must_cast_slice(indices);
        let index_buffer = self.index_buffer.as_mut().unwrap();
        for i in 0..count {
            let stride = self.mesh_description.index_format.stride();
            let offset = ((self.index_count as usize) + i) * stride;
            unsafe {
                core::ptr::copy_nonoverlapping(
                    source_buffer.as_ptr().add(i * stride),
                    index_buffer.as_mut_ptr().add(offset),
                    stride,
                );
            }
        }
        self.index_count += count as u16;
        Ok(self)
    }

    pub fn index<T: Index + bytemuck::Pod + Into<u16>>(
        &mut self,
        index: T,
    ) -> Result<&mut Self> {
        self.indices(&[index])
    }

    pub fn check_vertex_capacity(&mut self, count: usize) -> Result<&mut Self> {
        if count > (u16::MAX - self.vertex_count) as usize {
            return Err(Error::AtMaxVertexCount);
        }
        let capacity = self.vertex_buffer.len();
        let requested = self.mesh_description.vertex_stride()
            * self.mesh_description.morph_count.0
            * (self.vertex_count as usize + count);
        if requested > capacity {
            return Err(Error::OutOfVertexMemory {
                capacity,
                requested,
            });
        }
        Ok(self)
    }

    pub fn advance_vertex(&mut self, count: usize) -> Result<&mut Self> {
        self.check_vertex_capacity(count)?;
        self.vertex_count += count as u16;
        Ok(self)
    }

    pub fn tell_vertex(&self) -> u16 {
        self.vertex_count
    }

    unsafe fn write_vertex<T, const MC: usize>(
        &mut self,
        src: *const T,
        offset: usize,
        stride: usize,
        count: usize,
    ) -> Result<&mut Self> {
        if MC != self.mesh_description.morph_count.0 || (count % MC) != 0 {
            return Err(Error::MorphCount {
                expected: self.mesh_description.morph_count.0,
            });
        }
        self.check_vertex_capacity(count / MC)?;
        for i in 0..count {
            let desc = &self.mesh_description;
            let offset =
                (((self.vertex_count as usize * desc.morph_count.0) + i) * desc.stride) + offset;
            core::ptr::copy_nonoverlapping(
                (src as *const u8).add(i * stride) as _,
                &mut self.vertex_buffer[offset],
                stride,
            );
        }
        Ok(self)
    }

    fn write_weights<T: Weight, const WC: usize, const MC: usize>(
        &mut self,
        weights: &[T],
    ) -> Result<&mut Self> {
        if self.mesh_description.weight_format != T::FORMAT {
            return Err(Error::WeightFormat {
                expected: self.mesh_description.weight_format,
            });
        }
        if WC != self.mesh_description.weight_count.0 {
            return Err(Error::WeightCount {
                expected: self.mesh_description.weight_count.0,
            });
        }
        unsafe {
            self.write_vertex::<T, MC>(
                weights.as_ptr(),
                0,
                T::FORMAT.stride() * WC,
                weights.len() / WC,
            )
        }
    }

    pub fn weights<T: Weight>(&mut self, weights: &[T]) -> Result<&mut Self> {
        self.write_weights::<T, 1, 1>(weights)
    }

    pub fn weight<T: Weight>(&mut self, weight: T) -> Result<&mut Self> {
        self.weights(&[weight])
    }

    pub fn weights_morph<T: Weight + bytemuck::Pod, const MC: usize>(
        &mut self,
        weights: &[[T; MC]],
    ) -> Result<&mut Self>
    where
        [T; MC]: bytemuck::Pod,
    {
        self.write_weights::<T, 1, MC>(bytemuck::must_cast_slice(weights))
    }

    pub fn weight_morph<T: Weight + bytemuck::Pod, const MC: usize>(
        &mut self,
        weight: &[T; MC],
    ) -> Result<&mut Self>
    where
        [T; MC]: bytemuck::Pod,
    {
        self.weights_morph(slice::from_ref(weight))
    }

    pub fn weights_multi<T: Weight + bytemuck::Pod, const WC: usize>(
        &mut self,
        weights: &[[T; WC]],
    ) -> Result<&mut Self>
    where
        [T; WC]: bytemuck::Pod,
    {
        self.write_weights::<T, WC, 1>(bytemuck::must_cast_slice(weights))
    }

    pub fn weight_multi<T: Weight + bytemuck::Pod, const WC: usize>(
        &mut self,
        weight: &[T; WC],
    ) -> Result<&mut Self>
    where
        [T; WC]: bytemuck::Pod,
    {
        self.weights_multi(slice::from_ref(weight))
    }

    pub fn weights_multi_morph<T: Weight + bytemuck::Pod, const WC: usize, const MC: usize>(
        &mut self,
        weights: &[[[T; WC]; MC]],
    ) -> Result<&mut Self>
    where
        [[T; WC]; MC]: bytemuck::Pod,
    {
        self.write_weights::<T, WC, MC>(bytemuck::must_cast_slice(weights))
    }

    pub fn weight_multi_morph<T: Weight + bytemuck::Pod, const WC: usize, const MC: usize>(
        &mut self,
        weight: &[[T; WC]; MC],
    ) -> Result<&mut Self>
    where
        [[T; WC]; MC]: bytemuck::Pod,
    {
        self.weights_multi_morph(slice::from_ref(weight))
    }

    fn write_texcoords<T: Texcoord, const MC: usize>(
        &mut self,
        texcoords: &[T],
    ) -> Result<&mut Self> {
        if self.mesh_description.texcoord_format != T::FORMAT {
            return Err(Error::TexcoordFormat {
                expected: self.mesh_description.texcoord_format,
            });
        }
        unsafe {
            self.write_vertex::<T, MC>(
                texcoords.as_ptr(),
                self.mesh_description.texcoord_offset,
                T::FORMAT.stride(),
                texcoords.len(),
            )
        }
    }

    pub fn texcoords<T: Texcoord>(&mut self, texcoords: &[T]) -> Result<&mut Self> {
        self.write_texcoords::<T, 1>(texcoords)
    }

    pub fn texcoord<T: Texcoord>(&mut self, texcoord: &T) -> Result<&mut Self> {
        self.texcoords(slice::from_ref(texcoord))
    }

    pub fn texcoords_morph<T: Texcoord + bytemuck::Pod, const MC: usize>(
        &mut self,
        texcoords: &[[T; MC]],
    ) -> Result<&mut Self>
    where
        [T; MC]: bytemuck::Pod,
    {
        self.write_texcoords::<T, MC>(bytemuck::must_cast_slice(texcoords))
    }

    pub fn texcoord_morph<T: Texcoord + bytemuck::Pod, const MC: usize>(
        &mut self,
        texcoord: &[T; MC],
    ) -> Result<&mut Self>
    where
        [T; MC]: bytemuck::Pod,
    {
        self.texcoords_morph(slice::from_ref(texcoord))
    }

    fn write_colors<T: Color, const MC: usize>(
        &mut self,
        colors: &[T],
    ) -> Result<&mut Self> {
        if self.mesh_description.color_format.stride() != size_of::<T>() {
            return Err(Error::ColorFormat {
                expected: self.mesh_description.color_format,
            });
        }
        unsafe {
            self.write_vertex::<T, MC>(
                colors.as_ptr(),
                self.mesh_description.color_offset,
                size_of::<T>(),
                colors.len(),
            )
        }
    }

    pub fn colors<T: Color>(&mut self, colors: &[T]) -> Result<&mut Self> {
        self.write_colors::<T, 1>(colors)
    }

    pub fn color<T: Color>(&mut self, color: T) -> Result<&mut Self> {
        self.colors(&[color])
    }

    pub fn colors_morph<T: Color + bytemuck::Pod, const MC: usize>(
        &mut self,
        colors: &[[T; MC]],
    ) -> Result<&mut Self>
    where
        [T; MC]: bytemuck::Pod,
    {
        self.write_colors::<T, MC>(bytemuck::must_cast_slice(colors))
    }

    pub fn color_morph<T: Color + bytemuck::Pod, const N: usize>(
        &mut self,
        color: &[T; N],
    ) -> Result<&mut Self>
    where
        [T; N]: bytemuck::Pod,
    {
        self.colors_morph(slice::from_ref(color))
    }

    fn write_normals<T: Normal, const MC: usize>(
        &mut self,
        normals: &[T],
    ) -> Result<&mut Self> {
        if self.mesh_description.normal_format != T::FORMAT {
            return Err(Error::NormalFormat {
                expected: self.mesh_description.normal_format,
            });
        }
        unsafe {
            self.write_vertex::<T, MC>(
                normals.as_ptr(),
                self.mesh_description.normal_offset,
                T::FORMAT.stride(),
                normals.len(),
            )
        }
    }

    pub fn normals<T: Normal>(&mut self, normals: &[T]) -> Result<&mut Self> {
        self.write_normals::<T, 1>(normals)
    }

    pub fn normal<T: Normal>(&mut self, normal: &T) -> Result<&mut Self> {
        self.normals(slice::from_ref(normal))
    }

    pub fn normals_morph<T: Normal + bytemuck::Pod, const MC: usize>(
        &mut self,
        normals: &[[T; MC]],
    ) -> Result<&mut Self>
    where
        [T; MC]: bytemuck::Pod,
    {
        self.write_normals::<T, MC>(bytemuck::must_cast_slice(normals))
    }

    pub fn normal_morph<T: Normal + bytemuck::Pod, const N: usize>(
        &mut self,
        normal: &[T; N],
    ) -> Result<&mut Self>
    where
        [T; N]: bytemuck::Pod,
    {
        self.normals_morph(slice::from_ref(normal))
    }

    fn write_positions<T: Position, const MC: usize>(
        &mut self,
        positions: &[T],
    ) -> Result<&mut Self> {
        if self.mesh_description.position_format != T::FORMAT {
            return Err(Error::PositionFormat {
                expected: self.mesh_description.position_format,
            });
        }
        unsafe {
            self.write_vertex::<T, MC>(
                positions.as_ptr(),
                self.mesh_description.position_offset,
                T::FORMAT.stride(),
                positions.len(),
            )
        }
    }

    pub fn positions<T: Position>(&mut self, positions: &[T]) -> Result<&mut Self> {
        self.write_positions::<T, 1>(positions)
    }

    pub fn position<T: Position>(&mut self, position: &T) -> Result<&mut Self> {
        self.positions(slice::from_ref(position))
    }

    pub fn positions_morph<T: Position + bytemuck::Pod, const MC: usize>(
        &mut self,
        positions: &[[T; MC]],
    ) -> Result<&mut Self>
    where
        [T; MC]: bytemuck::Pod,
    {
        self.write_positions::<T, MC>(bytemuck::must_cast_slice(positions))
    }

    pub fn position_morph<T: Position + bytemuck::Pod, const N: usize>(
        &mut self,
        position: &[T; N],
    ) -> Result<&mut Self>
    where
        [T; N]: bytemuck::Pod,
    {
        self.positions_morph(slice::from_ref(position))
    }
}
