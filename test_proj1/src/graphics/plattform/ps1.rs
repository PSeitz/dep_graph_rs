use crate::mod3::mod3_add;

use super::PS1;

#[allow(dead_code)]
pub fn render(left: u64, right: u64) -> u64 {
    mod3_add(left, right) + PS1.len() as u64
}
