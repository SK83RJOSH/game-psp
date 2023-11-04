#[repr(align(2))]
#[derive(Copy, Clone)]
pub struct Align2<T>(pub T);

#[repr(align(4))]
#[derive(Copy, Clone)]
pub struct Align4<T>(pub T);

#[repr(align(8))]
#[derive(Copy, Clone)]
pub struct Align8<T>(pub T);

#[repr(align(16))]
#[derive(Copy, Clone)]
pub struct Align16<T>(pub T);

#[repr(align(32))]
#[derive(Copy, Clone)]
pub struct Align32<T>(pub T);

#[repr(align(64))]
#[derive(Copy, Clone)]
pub struct Align64<T>(pub T);
