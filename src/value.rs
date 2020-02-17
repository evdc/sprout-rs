// Defines value types in the Sprout language, and their representation in memory.

// Defining strings this way (in a Box) is in line with the Crafting Interpreters tutorial, and others,
// but counter to the everything-in-a-database design hypothesis.

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String)     // Already a heap type: we don't need to box it
}