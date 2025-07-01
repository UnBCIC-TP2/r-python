use crate::ir::ast::Function;
use crate::ir::ast::Name;
use crate::ir::ast::ValueConstructor;
use std::collections::HashMap;
use std::collections::LinkedList;

#[derive(Clone)]
pub struct Scope<A> {
    pub variables: HashMap<Name, (bool, A)>,
    pub functions: HashMap<Name, Function>,
    pub adts: HashMap<Name, Vec<ValueConstructor>>,
}

impl<A: Clone> Scope<A> {
    fn new() -> Scope<A> {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            adts: HashMap::new(),
        }
    }

    fn map_variable(&mut self, var: Name, mutable: bool, value: A) -> () {
        self.variables.insert(var, (mutable, value));
        return ();
    }

    fn map_function(&mut self, function: Function) -> () {
        self.functions.insert(function.name.clone(), function);
        return ();
    }

    fn map_adt(&mut self, name: Name, adt: Vec<ValueConstructor>) -> () {
        self.adts.insert(name.clone(), adt);
        return ();
    }

    fn lookup_var(&self, var: &Name) -> Option<(bool, A)> {
        self.variables
            .get(var)
            .map(|(mutable, value)| (*mutable, value.clone()))
    }

    fn lookup_function(&self, name: &Name) -> Option<&Function> {
        self.functions.get(name)
    }

    fn lookup_adt(&self, name: &Name) -> Option<&Vec<ValueConstructor>> {
        self.adts.get(name)
    }
}

#[derive(Clone)]
pub struct Environment<A> {
    pub globals: Scope<A>,
    pub stack: LinkedList<Scope<A>>,
}

impl<A: Clone> Environment<A> {
    pub fn new() -> Environment<A> {
        let mut env = Environment {
            globals: Scope::new(),
            stack: LinkedList::new(),
        };
        for (_name, func) in crate::stdlib::builtins() {
            env.map_function(func);
        }
        env
    }

    pub fn map_variable(&mut self, var: Name, mutable: bool, value: A) -> () {
        match self.stack.front_mut() {
            None => self.globals.map_variable(var, mutable, value),
            Some(top) => top.map_variable(var, mutable, value),
        }
    }

    pub fn map_function(&mut self, function: Function) -> () {
        match self.stack.front_mut() {
            None => self.globals.map_function(function),
            Some(top) => top.map_function(function),
        }
    }

    pub fn map_adt(&mut self, name: Name, cons: Vec<ValueConstructor>) -> () {
        match self.stack.front_mut() {
            None => self.globals.map_adt(name, cons),
            Some(top) => top.map_adt(name, cons),
        }
    }

    pub fn lookup(&self, var: &Name) -> Option<(bool, A)> {
        for scope in self.stack.iter() {
            if let Some(value) = scope.lookup_var(var) {
                return Some(value);
            }
        }
        self.globals.lookup_var(var)
    }

    pub fn lookup_function(&self, name: &Name) -> Option<&Function> {
        for scope in self.stack.iter() {
            if let Some(func) = scope.lookup_function(name) {
                return Some(func);
            }
        }
        self.globals.lookup_function(name)
    }

    pub fn lookup_adt(&self, name: &Name) -> Option<&Vec<ValueConstructor>> {
        for scope in self.stack.iter() {
            if let Some(cons) = scope.lookup_adt(name) {
                return Some(cons);
            }
        }
        self.globals.lookup_adt(name)
    }

    pub fn scoped_function(&self) -> bool {
        !self.stack.is_empty()
    }

    pub fn push(&mut self) -> () {
        self.stack.push_front(Scope::new());
    }

    pub fn pop(&mut self) -> () {
        self.stack.pop_front();
    }

    pub fn get_all_variables(&self) -> Vec<(Name, (bool, A))> {
        let mut vars = Vec::new();

        // First get variables from local scopes (in reverse order to respect shadowing)
        for scope in self.stack.iter() {
            for (name, value) in &scope.variables {
                if !vars.iter().any(|(n, _)| n == name) {
                    vars.push((name.clone(), value.clone()));
                }
            }
        }

        // Then get variables from global scope (if not already found)
        for (name, value) in &self.globals.variables {
            if !vars.iter().any(|(n, _)| n == name) {
                vars.push((name.clone(), value.clone()));
            }
        }

        vars
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::Type;

    #[test]
    fn test_variable_scoping() {
        let mut env: Environment<i32> = Environment::new();

        // Test global scope
        env.map_variable("x".to_string(), true, 32);
        assert_eq!(Some((true, 32)), env.lookup(&"x".to_string()));

        // Test nested scopes
        env.push(); // scope 1
        env.map_variable("y".to_string(), true, 23);
        env.map_variable("x".to_string(), true, 55); // shadows global x

        env.push(); // scope 2
        env.map_variable("z".to_string(), true, 44);

        // Variables from all scopes should be accessible
        assert_eq!(Some((true, 55)), env.lookup(&"x".to_string())); // from scope 1
        assert_eq!(Some((true, 23)), env.lookup(&"y".to_string())); // from scope 1
        assert_eq!(Some((true, 44)), env.lookup(&"z".to_string())); // from scope 2

        // Pop scope 2
        env.pop();
        assert_eq!(Some((true, 55)), env.lookup(&"x".to_string())); // still in scope 1
        assert_eq!(Some((true, 23)), env.lookup(&"y".to_string())); // still in scope 1
        assert_eq!(None, env.lookup(&"z".to_string())); // z is gone

        // Pop scope 1
        env.pop();
        assert_eq!(Some((true, 32)), env.lookup(&"x".to_string())); // back to global x
        assert_eq!(None, env.lookup(&"y".to_string())); // y is gone
    }

    #[test]
    fn test_function_scoping() {
        let mut env: Environment<i32> = Environment::new();

        let global_func = Function {
            name: "global".to_string(),
            kind: Type::TVoid,
            params: Vec::new(),
            body: None,
            builtin: None,
        };

        let local_func = Function {
            name: "local".to_string(),
            kind: Type::TVoid,
            params: Vec::new(),
            body: None,
            builtin: None,
        };

        // Test function scoping
        env.map_function(global_func.clone());
        assert!(env.lookup_function(&"global".to_string()).is_some());

        env.push();
        env.map_function(local_func.clone());

        assert!(env.lookup_function(&"global".to_string()).is_some()); // can see global
        assert!(env.lookup_function(&"local".to_string()).is_some()); // can see local

        env.pop();
        assert!(env.lookup_function(&"global".to_string()).is_some()); // global still visible
        assert!(env.lookup_function(&"local".to_string()).is_none()); // local gone
    }

    #[test]
    fn test_input_builtin_registration() {
        let mut env: Environment<i32> = Environment::new();
        for (_name, func) in crate::stdlib::builtins() {
            env.map_function(func);
        }
        assert!(env.lookup_function(&"input".to_string()).is_some());
    }
}
