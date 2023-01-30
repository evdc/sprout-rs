use std::fmt::Display;

pub fn format_vec<T: Display>(v: &Vec<T>) -> String {
    let mut s = "[".to_string();
    for (i, it) in v.iter().enumerate() {
        if i == 0 {
            s = format!("{}{}", s, it);
        } else {
            s = format!("{}, {}", s, it);
        }
        
    }
    format!("{}]", s)
}

pub fn is_subsequence<T: PartialEq>(haystack: &[T], needle: &[T]) -> bool {
    // O(n^2). Knuth-Morris-Pratt is faster, but this will do for now
    haystack.windows(needle.len()).any(|c| c == needle)
}
