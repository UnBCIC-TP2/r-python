pub mod io;

use std::collections::HashMap;
use crate::ir::ast::Function;

pub fn builtins() -> HashMap<String, Function> {
    let mut map = HashMap::new();
    io::register_builtins(&mut map);
    map
}
