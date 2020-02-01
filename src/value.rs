// Defines value types in the Sprout language, and their representation in memory.

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64)
}