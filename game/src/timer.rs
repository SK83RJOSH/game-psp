use psp::sys::*;

pub struct Timer {
    last_system_time: u64,
    next_system_time: u64,
}

impl Timer {
    pub fn new() -> Self {
        let system_time = unsafe { sceKernelGetSystemTimeWide() as u64 };
        Self {
            last_system_time: system_time,
            next_system_time: system_time,
        }
    }

    pub fn step(&mut self) {
        self.last_system_time = self.next_system_time;
        self.next_system_time = unsafe { sceKernelGetSystemTimeWide() as u64 };
        if self.next_system_time < self.last_system_time {
            self.next_system_time += u64::MAX - self.last_system_time;
            self.last_system_time = 0;
        }
    }

    pub fn delta_f32(&self) -> f32 {
        (self.next_system_time - self.last_system_time) as f32 / 1000000.0
    }
}
