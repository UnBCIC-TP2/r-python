/* use std::collections::HashMap;
use crate::ir::ast::{Statement, Trait, TraitImplementation};

pub struct TraitEnv {
    pub traits: HashMap<String, Trait>,
    pub impls: Vec<TraitImplementation>,
}

impl TraitEnv {
    pub fn new() -> Self { TraitEnv { traits: HashMap::new(), impls: Vec::new() } }
    pub fn add_trait(&mut self, tr: Trait) { self.traits.insert(tr.name.clone(), tr); }
    pub fn add_impl(&mut self, imp: TraitImplementation) { self.impls.push(imp); }
}

pub fn build_trait_env(stmts: &[Statement]) -> TraitEnv {
    let mut env = TraitEnv::new();
    for stmt in stmts {
        match stmt {
            Statement::Trait(tr)               => env.add_trait(tr.clone()),
            Statement::TraitImplementation(i)  => env.add_impl(i.clone()),
            _ => {}
        }
    }
    env
}

pub fn check_trait_impl(env: &TraitEnv) -> Result<(), String> {
    for imp in &env.impls {
        let tr = env.traits.get(&imp.trait_name)
            .ok_or_else(|| format!("Trait '{}' não declarada", imp.trait_name))?;
        for req in &tr.methods {
            if !imp.methods.iter().any(|m| m.name == req.name) {
                return Err(format!(
                    "Tipo '{}' não implementa '{}' requerido por trait '{}'",
                    imp.type_name, req.name, imp.trait_name
                ));
            }
        }
    }
    Ok(())
}
 */