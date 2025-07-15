use crate::{mod2::mod2_add, mod3::mod3_add};

pub fn mod1_add(left: u64, right: u64) -> u64 {
    mod2_add(left, right) + mod3_add(left, right)
}
