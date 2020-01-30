// Defines value types in the Sprout language, and their representation in memory.

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    True,
    False,
    Num(f64)
}