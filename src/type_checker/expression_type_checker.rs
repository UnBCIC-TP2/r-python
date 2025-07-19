use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Type};
use crate::ir::typed_ast::TypedExpression;  

type ErrorMessage = String;

pub fn check_expr(exp: Expression, env: &Environment<Type>) -> Result<TypedExpression, ErrorMessage> {  
    match exp {
        Expression::CTrue => Ok(TypedExpression::CTrue {
            type_info: Type::TBool,
        }),
        Expression::CFalse => Ok(TypedExpression::CFalse {
            type_info: Type::TBool,
        }),
        Expression::CVoid => Ok(TypedExpression::CVoid {
            type_info: Type::TVoid,
        }),
        Expression::CInt(v) => Ok(TypedExpression::CInt {
            value: v,
            type_info: Type::TInteger,
        }),
        Expression::CReal(v) => Ok(TypedExpression::CReal {
            value: v,
            type_info: Type::TReal,
        }),
        Expression::CString(v) => Ok(TypedExpression::CString {
            value: v,
            type_info: Type::TString,
        }),
        Expression::Add(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            let result_type = match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) => Type::TInteger,
                (Type::TReal, Type::TReal) | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => Type::TReal,
                (lt, rt) => return Err(format!("[Type Error] Cannot add non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };
            
            Ok(TypedExpression::Add {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: result_type,
            })
        },

        Expression::Sub(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            let result_type = match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) => Type::TInteger,
                (Type::TReal, Type::TReal) | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => Type::TReal,
                (lt, rt) => return Err(format!("[Type Error] Cannot subtract non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };
            
            Ok(TypedExpression::Sub {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: result_type,
            })
        },

        Expression::Mul(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            let result_type = match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) => Type::TInteger,
                (Type::TReal, Type::TReal) | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => Type::TReal,
                (lt, rt) => return Err(format!("[Type Error] Cannot multiply non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };
            
            Ok(TypedExpression::Mul {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: result_type,
            })
        },

        Expression::Div(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            let result_type = match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) => Type::TInteger,
                (Type::TReal, Type::TReal) | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => Type::TReal,
                (lt, rt) => return Err(format!("[Type Error] Cannot divide non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };
            
            Ok(TypedExpression::Div {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: result_type,
            })
        },

       // Operações Booleanas
        Expression::And(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TBool, Type::TBool) => (), 
                (lt, rt) => return Err(format!("[Type Error] 'And' operator expects boolean values, but got '{:?}' and '{:?}'.", lt, rt)),
            };

            Ok(TypedExpression::And {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: Type::TBool,
            })
        },

        Expression::Or(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TBool, Type::TBool) => (), 
                (lt, rt) => return Err(format!("[Type Error] 'Or' operator expects boolean values, but got '{:?}' and '{:?}'.", lt, rt)),
            };

            Ok(TypedExpression::Or {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: Type::TBool,
            })
        },
        Expression::Not(e) => {
            let typed_e = check_expr(*e, env)?;
            if typed_e.type_info() != &Type::TBool {
                return Err(format!("[Type Error] 'Not' operator expects a boolean type, but got '{:?}'.", typed_e.type_info()));
            }
            Ok(TypedExpression::Not {
                expr: Box::new(typed_e),
                type_info: Type::TBool,
            })
        }

        // Operações Relacionais
        Expression::EQ(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) | (Type::TReal, Type::TReal)
                | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => (),
                (lt, rt) => return Err(format!("[Type Error] Cannot compare non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };

            Ok(TypedExpression::EQ {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: Type::TBool,
            })
        },

        Expression::NEQ(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) | (Type::TReal, Type::TReal)
                | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => (),
                (lt, rt) => return Err(format!("[Type Error] Cannot compare non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };

            Ok(TypedExpression::NEQ {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: Type::TBool,
            })
        },

        Expression::GT(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) | (Type::TReal, Type::TReal)
                | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => (),
                (lt, rt) => return Err(format!("[Type Error] Cannot compare non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };

            Ok(TypedExpression::GT {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: Type::TBool,
            })
        },

        Expression::LT(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) | (Type::TReal, Type::TReal)
                | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => (),
                (lt, rt) => return Err(format!("[Type Error] Cannot compare non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };

            Ok(TypedExpression::LT {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: Type::TBool,
            })
        },

        Expression::GTE(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) | (Type::TReal, Type::TReal)
                | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => (),
                (lt, rt) => return Err(format!("[Type Error] Cannot compare non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };

            Ok(TypedExpression::GTE {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: Type::TBool,
            })
        },

        Expression::LTE(l, r) => {
            let typed_left = check_expr(*l, env)?;
            let typed_right = check_expr(*r, env)?;

            match (typed_left.type_info(), typed_right.type_info()) {
                (Type::TInteger, Type::TInteger) | (Type::TReal, Type::TReal)
                | (Type::TInteger, Type::TReal) | (Type::TReal, Type::TInteger) => (),
                (lt, rt) => return Err(format!("[Type Error] Cannot compare non-numeric types '{:?}' and '{:?}'.", lt, rt)),
            };

            Ok(TypedExpression::LTE {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                type_info: Type::TBool,
            })
        },

        // Variável
        Expression::Var(name) => match env.lookup(&name) {
            Some((_, t)) => Ok(TypedExpression::Var {
                name,
                type_info: t.clone(),
            }),
            None => Err(format!("[Name Error] '{}' is not defined.", name)),
        },

        // Tipos Maybe/Result
        Expression::COk(e) => {
            let typed_e = check_expr(*e, env)?;
            let ok_type = typed_e.type_info().clone();
            Ok(TypedExpression::COk {
                expr: Box::new(typed_e),
                type_info: Type::TResult(Box::new(ok_type), Box::new(Type::TAny)),
            })
        }
        Expression::CErr(e) => {
            let typed_e = check_expr(*e, env)?;
            let err_type = typed_e.type_info().clone();
            Ok(TypedExpression::CErr {
                expr: Box::new(typed_e),
                type_info: Type::TResult(Box::new(Type::TAny), Box::new(err_type)),
            })
        }
        Expression::CJust(e) => {
            let typed_e = check_expr(*e, env)?;
            let just_type = typed_e.type_info().clone();
            Ok(TypedExpression::CJust {
                expr: Box::new(typed_e),
                type_info: Type::TMaybe(Box::new(just_type)),
            })
        }
        Expression::CNothing => Ok(TypedExpression::CNothing {
            type_info: Type::TMaybe(Box::new(Type::TAny)),
        }),

        Expression::IsError(e) => {
            let typed_e = check_expr(*e, env)?;
            if let Type::TResult(_,_) = typed_e.type_info() {
                 Ok(TypedExpression::IsError {
                    expr: Box::new(typed_e),
                    type_info: Type::TBool,
                })
            } else {
                 Err(String::from("[Type Error] expecting a result type value."))
            }
        }
        Expression::IsNothing(e) => {
            let typed_e = check_expr(*e, env)?;
            if let Type::TMaybe(_) = typed_e.type_info() {
                Ok(TypedExpression::IsNothing {
                    expr: Box::new(typed_e),
                    type_info: Type::TBool,
                })
            } else {
                Err(String::from("[Type Error] expecting a maybe type value."))
            }
        }

        Expression::Unwrap(e) => {
            let typed_e = check_expr(*e, env)?;
            let unwrapped_type = match typed_e.type_info() {
                Type::TMaybe(t) => *t.clone(),
                Type::TResult(t, _) => *t.clone(),
                t => return Err(format!("[Type Error] 'Unwrap' expects a Maybe or Result type, but got '{:?}'.", t)),
            };

            Ok(TypedExpression::Unwrap {
                expr: Box::new(typed_e),
                type_info: unwrapped_type,
            })
        },

        Expression::Propagate(e) => {
            let typed_e = check_expr(*e, env)?;
            let unwrapped_type = match typed_e.type_info() {
                Type::TMaybe(t) => *t.clone(),
                Type::TResult(t, _) => *t.clone(),
                t => return Err(format!("[Type Error] 'Propagate' expects a Maybe or Result type, but got '{:?}'.", t)),
            };

            Ok(TypedExpression::Propagate {
                expr: Box::new(typed_e),
                type_info: unwrapped_type,
            })
        },

        // Estruturas de Dados
        Expression::ListValue(elements) => {
            let mut typed_elements = Vec::new();
            for el in elements {
                typed_elements.push(check_expr(el, env)?);
            }

            let list_type = if typed_elements.is_empty() {
                Type::TList(Box::new(Type::TAny))
            } else {
                let first_type = typed_elements[0].type_info();
                for element in typed_elements.iter().skip(1) {
                    if element.type_info() != first_type {
                        return Err(format!(
                            "[Type Error] List elements must have the same type. Expected '{:?}', found '{:?}'.",
                            first_type, element.type_info()
                        ));
                    }
                }
                Type::TList(Box::new(first_type.clone()))
            };

            Ok(TypedExpression::ListValue {
                elements: typed_elements,
                type_info: list_type,
            })
        },
        
    

        Expression::Constructor(name, args) => {
            let mut adt_def = None;
            for scope in env.stack.iter().rev() {
                for (adt_name, constructors) in scope.adts.iter() {
                    if let Some(constructor) = constructors.iter().find(|c| c.name == name) {
                        adt_def = Some((adt_name.clone(), constructor.clone(), constructors.clone()));
                        break;
                    }
                }
                if adt_def.is_some() { break; }
            }
            if adt_def.is_none() {
                for (adt_name, constructors) in env.globals.adts.iter() {
                    if let Some(constructor) = constructors.iter().find(|c| c.name == name) {
                        adt_def = Some((adt_name.clone(), constructor.clone(), constructors.clone()));
                        break;
                    }
                }
            }

            match adt_def {
                Some((adt_type_name, constructor_def, all_constructors)) => {
                    if args.len() != constructor_def.types.len() {
                        return Err(format!(
                            "[Type Error] Constructor '{}' expects {} arguments, but got {}.",
                            name, constructor_def.types.len(), args.len()
                        ));
                    }

                    let mut typed_args = Vec::new();
                    for (arg_expr, expected_type) in args.into_iter().zip(constructor_def.types.iter()) {
                        let typed_arg = check_expr(*arg_expr, env)?;
                        if typed_arg.type_info() != expected_type {
                            return Err(format!(
                                "[Type Error] Argument type mismatch in constructor '{}'. Expected '{:?}', found '{:?}'.",
                                name, expected_type, typed_arg.type_info()
                            ));
                        }
                        typed_args.push(Box::new(typed_arg));
                    }

                    Ok(TypedExpression::Constructor {
                        name,
                        args: typed_args,
                        type_info: Type::TAlgebraicData(adt_type_name, all_constructors),
                    })
                }
                None => Err(format!(
                    "[Type Error] Constructor '{}' is not defined in any ADT.",
                    name
                )),
            }
        },

        //FuncCall
        Expression::FuncCall(name, args) => {
            match env.lookup_function(&name) {
                Some(function_def) => {
                    if args.len() != function_def.params.len() {
                        return Err(format!(
                            "[Type Error] Function '{}' expects {} arguments, but got {}.",
                            name,
                            function_def.params.len(),
                            args.len()
                        ));
                    }

                    let mut typed_args = Vec::new();
                    for (arg_expr, param_def) in args.into_iter().zip(function_def.params.iter()) {
                        let typed_arg = check_expr(arg_expr, env)?;

                        if typed_arg.type_info() != &param_def.argument_type {
                            return Err(format!(
                                "[Type Error] Argument type mismatch for parameter '{}' in function '{}'. Expected '{:?}', but found '{:?}'.",
                                param_def.argument_name, name, param_def.argument_type, typed_arg.type_info()
                            ));
                        }
                        typed_args.push(Box::new(typed_arg));
                    }

                    Ok(TypedExpression::FuncCall {
                        name,
                        args: typed_args,
                        type_info: function_def.kind.clone(),
                    })
                }
                None => Err(format!("[Name Error] Function '{}' is not defined.", name)),
            }
        },


       // _ => Err("not implemented yet.".to_string()),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::environment::Environment;
    use crate::ir::ast::Expression::*;
    use crate::ir::ast::ValueConstructor;
    use crate::ir::typed_ast::TypedExpression;

    #[test]
    fn test_add_integers_produces_typed_node() {
        let env = Environment::new();
        let add_expr = Expression::Add(Box::new(CInt(10)), Box::new(CInt(20)));

        let result = check_expr(add_expr, &env);

        assert!(result.is_ok()); 

        let typed_expr = result.unwrap();

        assert_eq!(typed_expr.type_info(), &Type::TInteger);

        if let TypedExpression::Add { left, right, .. } = typed_expr {
            assert_eq!(left.type_info(), &Type::TInteger);
            assert_eq!(right.type_info(), &Type::TInteger);
        } else {
            panic!("Expected a TypedExpression::Add node, but got another variant.");
        }
    }

    #[test]
    fn check_constant() {
        let env = Environment::new();
        let c10 = CInt(10);

      
        let result = check_expr(c10, &env);

        
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(typed_expr.type_info(), &Type::TInteger);
        if let TypedExpression::CInt { value, .. } = typed_expr {
            assert_eq!(value, 10); 
        } else {
            panic!("Expected a CInt variant");
        }
    }

    #[test]
    fn check_add_integers() {
      
        let env = Environment::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        
        let result = check_expr(add, &env);

        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.type_info(), &Type::TInteger);
        assert!(matches!(typed_expr, TypedExpression::Add { .. }));
    }

    #[test]
    fn check_add_reals() {
        
        let env = Environment::new();
        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

      
        let result = check_expr(add, &env);

       
        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.type_info(), &Type::TReal);
        assert!(matches!(typed_expr, TypedExpression::Add { .. }));
    }

    #[test]
    fn check_add_real_and_integer() {
      
        let env = Environment::new();
        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

       
        let result = check_expr(add, &env);

        
        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.type_info(), &Type::TReal);
        assert!(matches!(typed_expr, TypedExpression::Add { .. }));
    }

    #[test]
    fn check_add_integer_and_real() {
       
        let env = Environment::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        let result = check_expr(add, &env);

        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.type_info(), &Type::TReal);
        assert!(matches!(typed_expr, TypedExpression::Add { .. }));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = Environment::new();

        let e1 = CInt(10);
        let e2 = CFalse;
        let e3 = Add(Box::new(e1), Box::new(e2));

        assert!(
            matches!(check_expr(e3, &env), Err(_)),
            "Expecting a type error."
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = Environment::new();

        let e1 = CInt(10);
        let e2 = Not(Box::new(e1));

        assert!(
            matches!(check_expr(e2, &env), Err(_)),
            "Expecting a type error."
        );
    }

    #[test]
    fn check_type_error_and_expression() {
        let env = Environment::new();

        let e1 = CInt(10);
        let e2 = CTrue;
        let e3 = And(Box::new(e1), Box::new(e2));

        assert!(
            matches!(check_expr(e3, &env), Err(_)),
            "Expecting a type error."
        );
    }

    #[test]
    fn check_type_error_or_expression() {
        let env = Environment::new();

        let e1 = CInt(10);
        let e2 = CTrue;
        let e3 = Or(Box::new(e1), Box::new(e2));

        assert!(
            matches!(check_expr(e3, &env), Err(_)),
            "Expecting a type error."
        );
    }

    #[test]
    fn check_ok_result() {
      
        let env = Environment::new();
        let e1 = CReal(10.0);
        let e2 = COk(Box::new(e1));

        
        let result = check_expr(e2, &env);

       
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(
            typed_expr.type_info(),
            &Type::TResult(Box::new(Type::TReal), Box::new(Type::TAny))
        );
        assert!(matches!(typed_expr, TypedExpression::COk { .. }));
    }

    #[test]
    fn check_err_result() {
        
        let env = Environment::new();
        let e1 = CInt(1);
        let e2 = CErr(Box::new(e1));

       
        let result = check_expr(e2, &env);

      
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(
            typed_expr.type_info(),
            &Type::TResult(Box::new(Type::TAny), Box::new(Type::TInteger))
        );
        assert!(matches!(typed_expr, TypedExpression::CErr { .. }));
    }

    #[test]
    fn check_just_integer() {
       
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = CJust(Box::new(e1));

       
        let result = check_expr(e2, &env);

    
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(
            typed_expr.type_info(),
            &Type::TMaybe(Box::new(Type::TInteger))
        );
        assert!(matches!(typed_expr, TypedExpression::CJust { .. }));
    }

    #[test]
    fn check_is_error_result_positive() {
      
        let env = Environment::new();
        let e1 = CTrue;
        let e2 = COk(Box::new(e1));
        let e3 = IsError(Box::new(e2));

       
        let result = check_expr(e3, &env);

        
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(typed_expr.type_info(), &Type::TBool);
        assert!(matches!(typed_expr, TypedExpression::IsError { .. }));
    }

    #[test]
    fn check_is_error_result_error() {
        let env = Environment::new();
        let e1 = CTrue;
        let e2 = IsError(Box::new(e1));

        assert!(
            matches!(check_expr(e2, &env), Err(_)),
            "Expecting a result type value."
        );
    }

    #[test]
    fn check_nothing() {
        let env = Environment::new();

        let result = check_expr(CNothing, &env);

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(
            typed_expr.type_info(),
            &Type::TMaybe(Box::new(Type::TAny))
        );
        
        assert!(matches!(typed_expr, TypedExpression::CNothing { .. }));
    }

    #[test]
    fn check_is_nothing_on_maybe() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = CJust(Box::new(e1));
        let e3 = IsNothing(Box::new(e2));

        let result = check_expr(e3, &env);

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(typed_expr.type_info(), &Type::TBool);
        
        assert!(matches!(typed_expr, TypedExpression::IsNothing { .. }));
    }

    #[test]
    fn check_is_nothing_type_error() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = IsNothing(Box::new(e1));

        assert!(
            matches!(check_expr(e2, &env), Err(_)),
            "expecting a maybe type value."
        );
    }

    #[test]
    fn check_unwrap_maybe() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = CJust(Box::new(e1));
        let e3 = Unwrap(Box::new(e2));

        let result = check_expr(e3, &env);

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(typed_expr.type_info(), &Type::TInteger);

        assert!(matches!(typed_expr, TypedExpression::Unwrap { .. }));
    }

    #[test]
    fn check_unwrap_maybe_type_error() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = Unwrap(Box::new(e1));

        assert!(
            matches!(check_expr(e2, &env), Err(_)),
            "expecting a maybe or result type value."
        );
    }

    #[test]
    fn check_unwrap_result() {
        let env: Environment<Type> = Environment::new();
        let e1: Expression = CTrue;
        let e2: Expression = COk(Box::new(e1)); 
        let e3: Expression = Unwrap(Box::new(e2)); 

       
        let result = check_expr(e3, &env);

        
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(typed_expr.type_info(), &Type::TBool);
        
        assert!(matches!(typed_expr, TypedExpression::Unwrap { .. }));
    }

    #[test]
    fn check_propagate_maybe() {
       
        let env: Environment<Type> = Environment::new();
        let e1: Expression = CInt(5);
        let e2: Expression = CJust(Box::new(e1)); 
        let e3: Expression = Propagate(Box::new(e2)); 

        let result = check_expr(e3, &env);

        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert_eq!(typed_expr.type_info(), &Type::TInteger);

        assert!(matches!(typed_expr, TypedExpression::Propagate { .. }));
    }

    #[test]
    fn check_propagate_maybe_type_error() {
        let env = Environment::new();
        let e1 = CInt(5);
        let e2 = Propagate(Box::new(e1));

        assert!(
            matches!(check_expr(e2, &env), Err(_)),
            "expecting a maybe or result type value."
        );
    }

    #[test]
    fn check_propagate_result() {
        let env: Environment<Type> = Environment::new();
        let e1: Expression = CTrue;
        let e2: Expression = COk(Box::new(e1));
        let e3: Expression = Propagate(Box::new(e2));

        let result = check_expr(e3, &env);

        assert!(result.is_ok());

        let typed_expr = result.unwrap();

        assert_eq!(typed_expr.type_info(), &Type::TBool);

        assert!(matches!(typed_expr, TypedExpression::Propagate { .. }));
    }

    #[test]
    fn test_undefined_variable() {
        let env = Environment::new();
        let exp = Expression::Var("x".to_string());

        assert!(check_expr(exp, &env).is_err());
    }

    #[test]
    fn test_defined_variable_typed() { 
        let mut env: Environment<Type> = Environment::new();
        env.map_variable("x".to_string(), true, Type::TInteger);
        let exp = Expression::Var("x".to_string());

        let result = check_expr(exp, &env);
        assert!(result.is_ok());

        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.type_info(), &Type::TInteger);

        assert!(matches!(typed_expr, TypedExpression::Var { .. }));
    }

    #[test]
    fn test_adt_constructor_wrong_args() {
        let mut env = Environment::new();
        let figure_type = vec![
            ValueConstructor::new("Circle".to_string(), vec![Type::TInteger]),
            ValueConstructor::new(
                "Rectangle".to_string(),
                vec![Type::TInteger, Type::TInteger],
            ),
        ];
        env.map_adt("Figure".to_string(), figure_type);

        let circle = Constructor(
            "Circle".to_string(),
            vec![Box::new(CString("invalid".to_string()))],
        );
        let result = check_expr(circle, &env);
        assert!(result.is_err());
    }

    #[test]
    fn test_adt_constructor_wrong_count() {
        let mut env = Environment::new();
        let figure_type = vec![
            ValueConstructor::new("Circle".to_string(), vec![Type::TInteger]),
            ValueConstructor::new(
                "Rectangle".to_string(),
                vec![Type::TInteger, Type::TInteger],
            ),
        ];
        env.map_adt("Figure".to_string(), figure_type);

        let rectangle = Constructor("Rectangle".to_string(), vec![Box::new(CInt(5))]); 
        let result = check_expr(rectangle, &env);
        assert!(result.is_err());
    }

    #[test]
    fn test_adt_constructor_valid() {
        
        let mut env = Environment::new();
        let figure_constructors = vec![
            ValueConstructor::new("Circle".to_string(), vec![Type::TInteger]),
            ValueConstructor::new(
                "Rectangle".to_string(),
                vec![Type::TInteger, Type::TInteger],
            ),
        ];
        env.map_adt("Figure".to_string(), figure_constructors.clone());

        let circle = Constructor("Circle".to_string(), vec![Box::new(CInt(5))]);
        
       
        let result = check_expr(circle, &env);

     
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

       
        assert_eq!(
            typed_expr.type_info(),
            &Type::TAlgebraicData("Figure".to_string(), figure_constructors)
        );

       
        assert!(matches!(typed_expr, TypedExpression::Constructor { .. }));
    }

    #[test]
    fn test_adt_constructor_with_mutable_vars() {
        let mut env = Environment::new();
        let figure_constructors = vec![
            ValueConstructor::new("Circle".to_string(), vec![Type::TInteger]),
            ValueConstructor::new(
                "Rectangle".to_string(),
                vec![Type::TInteger, Type::TInteger],
            ),
        ];
        env.map_adt("Figure".to_string(), figure_constructors.clone());
        env.map_variable("radius".to_string(), true, Type::TInteger);

        let circle = Constructor(
            "Circle".to_string(),
            vec![Box::new(Var("radius".to_string()))],
        );
        
        let result = check_expr(circle, &env);
        
        assert!(result.is_ok()); 
        let typed_expr = result.unwrap();

        assert_eq!(
            typed_expr.type_info(),
            &Type::TAlgebraicData("Figure".to_string(), figure_constructors)
        );

        assert!(matches!(typed_expr, TypedExpression::Constructor { .. }));
    }
}
