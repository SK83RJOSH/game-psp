#![allow(dead_code)]

use core::ffi::c_void;

pub const SCR_WIDTH: i32 = 480;
pub const SCR_HEIGHT: i32 = 272;
pub const SCR_ASPECT: f32 = SCR_WIDTH as f32 / SCR_HEIGHT as f32;
pub const SCR_OFFSETX: u32 = (4096 - SCR_WIDTH as u32) / 2;
pub const SCR_OFFSETY: u32 = (4096 - SCR_HEIGHT as u32) / 2;

pub const VRAM_TOP: usize = 0x00000000 as _;
pub const VRAM_WIDTH: i32 = 512;

pub const VRAM_BUFSIZE: usize = (VRAM_WIDTH * SCR_HEIGHT * 2) as usize;
pub const VRAM_BP_0: *mut c_void = (VRAM_TOP) as _;
pub const VRAM_BP_1: *mut c_void = (VRAM_TOP + VRAM_BUFSIZE) as _;
pub const VRAM_BP_2: *mut c_void = (VRAM_TOP + VRAM_BUFSIZE * 2) as _;

pub const VRAM_BUFSIZE32: usize = (VRAM_WIDTH * SCR_HEIGHT * 4) as usize;
pub const VRAM_BP32_0: *mut c_void = (VRAM_TOP) as _;
pub const VRAM_BP32_1: *mut c_void = (VRAM_TOP + VRAM_BUFSIZE32) as _;
pub const VRAM_BP32_2: *mut c_void = (VRAM_TOP + VRAM_BUFSIZE32 * 2) as _;