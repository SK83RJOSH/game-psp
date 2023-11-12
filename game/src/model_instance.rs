use psp::sys::*;
use psp_file_formats::model::{GuStateFlags, Material, Mesh, Sampler, Texture, TextureBind};

pub trait Drawable {
    fn draw(&self);
}

pub trait Appliable {
    fn apply(&self);
}

pub struct ModelInstance {
    model: psp_file_formats::model::Model,
    pub position: [f32; 3],
    pub rotation: [f32; 3],
    pub scale: [f32; 3],
}

impl From<psp_file_formats::model::Model> for ModelInstance {
    fn from(model: psp_file_formats::model::Model) -> Self {
        Self {
            model,
            position: [0.0; 3],
            rotation: [0.0; 3],
            scale: [1.0; 3],
        }
    }
}

impl Drawable for ModelInstance {
    fn draw(&self) {
        unsafe {
            sceGumLoadIdentity();
            sceGumTranslate(&ScePspFVector3 {
                x: self.position[0],
                y: self.position[1],
                z: self.position[2],
            });
            sceGumRotateXYZ(&ScePspFVector3 {
                x: self.rotation[0],
                y: self.rotation[1],
                z: self.rotation[2],
            });
            sceGumScale(&ScePspFVector3 {
                x: self.scale[0],
                y: self.scale[1],
                z: self.scale[2],
            });
        }

        for mesh in &self.model.meshes {
            if let Some(material) = mesh.material.and_then(|i| self.model.materials.get(i)) {
                let (texture, sampler) = match material.texture_bind {
                    TextureBind::None => (None, None),
                    TextureBind::Texture(texture) => (self.model.textures.get(texture), None),
                    TextureBind::TextureAndSampler(texture, sampler) => (
                        self.model.textures.get(texture),
                        self.model.samplers.get(sampler),
                    ),
                };
                if let Some(texture) = texture {
                    texture.apply();
                    sampler.unwrap_or(&Sampler::DEFAULT).apply();
                }
                material.apply();
            } else {
                Material::DEFAULT.apply();
            }
            mesh.draw();
        }
    }
}

impl Appliable for Texture {
    fn apply(&self) {
        unsafe {
            sceGuTexMode(self.format, self.mip_levels, 0, self.swizzle);
            sceGuTexImage(
                MipmapLevel::None,
                self.width,
                self.height,
                self.buffer_width,
                self.data.as_ptr() as _,
            );
        }
    }
}

impl Appliable for Sampler {
    fn apply(&self) {
        fn filter(mode: &TextureFilter) -> TextureFilter {
            match mode {
                TextureFilter::Nearest => TextureFilter::Nearest,
                TextureFilter::Linear => TextureFilter::Linear,
                TextureFilter::NearestMipmapNearest => TextureFilter::NearestMipmapNearest,
                TextureFilter::LinearMipmapNearest => TextureFilter::LinearMipmapNearest,
                TextureFilter::NearestMipmapLinear => TextureFilter::NearestMipmapLinear,
                TextureFilter::LinearMipmapLinear => TextureFilter::LinearMipmapLinear,
            }
        }

        fn wrap(mode: &GuTexWrapMode) -> GuTexWrapMode {
            match mode {
                GuTexWrapMode::Repeat => GuTexWrapMode::Repeat,
                GuTexWrapMode::Clamp => GuTexWrapMode::Clamp,
            }
        }

        unsafe {
            sceGuTexFilter(filter(&self.min_filter), filter(&self.mag_filter));
            sceGuTexWrap(wrap(&self.u_wrap_mode), wrap(&self.v_wrap_mode));
        }
    }
}

impl Appliable for Material {
    fn apply(&self) {
        unsafe {
            sceGuAlphaFunc(AlphaFunc::Greater, self.alpha_cutoff as i32, 0xff);
            sceGuModelColor(
                self.emission_color,
                0xffffffff,
                self.diffuse_color,
                0xffffffff,
            );
            let state_flags = {
                GuStateFlags::from_bits_retain(sceGuGetAllStatus()) & !Material::STATE_FLAGS_MASK
            };
            for flag in state_flags.difference(self.state_flags).iter() {
                unsafe fn apply(flags: GuStateFlags, state: GuState, flag: GuStateFlags) {
                    sceGuSetStatus(state, !flags.intersection(flag).is_empty() as i32);
                }
                match flag {
                    GuStateFlags::AlphaTest => apply(self.state_flags, GuState::AlphaTest, flag),
                    GuStateFlags::Blend => apply(self.state_flags, GuState::Blend, flag),
                    GuStateFlags::CullFace => apply(self.state_flags, GuState::CullFace, flag),
                    GuStateFlags::Texture2D => apply(self.state_flags, GuState::Texture2D, flag),
                    GuStateFlags::Lighting => apply(self.state_flags, GuState::Lighting, flag),
                    _ => {}
                }
            }
            sceGuSetAllStatus((state_flags | self.state_flags).bits());
        }
    }
}

impl Drawable for Mesh {
    fn draw(&self) {
        let index_buffer = if self.index_buffer.is_empty() {
            core::ptr::null()
        } else {
            self.index_buffer.as_ptr() as _
        };

        unsafe {
            sceGumDrawArray(
                self.primitive_type,
                self.vertex_type,
                self.vertex_count,
                index_buffer,
                self.vertex_buffer.as_ptr() as _,
            );
        }
    }
}
