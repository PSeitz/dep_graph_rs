pub fn mod3_add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod test {
    use crate::mod2::mod2_add;

    #[test]
    fn test_other_mod() {
        let result = mod2_add(2, 2);
        assert_eq!(result, 4);
    }
}
