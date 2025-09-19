// src/pretty_print/pretty_statements.rs

use super::pretty_print::{concat, group, hardline, line, nest, nil, text, Doc, ToDoc};
use crate::ir::ast::{FormalArgument, Function, Statement};
use std::rc::Rc;

/// Função auxiliar para juntar uma lista de documentos (`Vec<Rc<Doc>>`)
/// com um separador, resultando em um único documento.
fn join(sep: Rc<Doc>, docs: Vec<Rc<Doc>>) -> Rc<Doc> {
    docs.into_iter()
        .reduce(|acc, doc| concat(acc, concat(sep.clone(), doc)))
        .unwrap_or_else(nil)
}

/// Implementa a conversão de nós de `Statement` da AST para a representação `Doc`.
/// Cada variante do `enum Statement` é mapeada para um layout de formatação específico.
impl ToDoc for Statement {
    fn to_doc(&self) -> Rc<Doc> {
        match self {
            // Formata declarações como "var <nome> = <expr>;"
            Statement::VarDeclaration(name, expr) => concat(
                text("var "),
                concat(
                    text(name.clone()),
                    concat(text(" = "), concat(expr.to_doc(), text(";"))),
                ),
            ),
            // Formata declarações de constantes como "val <nome> = <expr>;"
            Statement::ValDeclaration(name, expr) => concat(
                text("val "),
                concat(
                    text(name.clone()),
                    concat(text(" = "), concat(expr.to_doc(), text(";"))),
                ),
            ),
            // Formata atribuições como "<nome> = <expr>;"
            Statement::Assignment(name, expr) => concat(
                text(name.clone()),
                concat(text(" = "), concat(expr.to_doc(), text(";"))),
            ),
            // Formata retornos como "return <expr>;"
            Statement::Return(expr) => concat(text("return "), concat(expr.to_doc(), text(";"))),
            // Formata asserções como "assert(<check>, <msg>);"
            Statement::Assert(check, msg) => concat(
                text("assert("),
                concat(
                    check.to_doc(),
                    concat(text(", "), concat(msg.to_doc(), text(");"))),
                ),
            ),

            // Formata um bloco de código. Usa `hardline` para garantir quebras de linha
            // e `nest` para indentar o conteúdo do bloco.
            Statement::Block(stmts) => {
                let stmts_doc = stmts.iter().map(|s| s.to_doc()).collect();
                concat(
                    text(":"),
                    concat(
                        // Indenta o conteúdo do bloco em 4 espaços.
                        nest(4, concat(hardline(), join(hardline(), stmts_doc))),
                        concat(hardline(), text("end")),
                    ),
                )
            }
            // Formata estruturas `if-then-else`.
            Statement::IfThenElse(cond, then_branch, else_branch) => {
                let mut doc = concat(
                    text("if "),
                    concat(cond.to_doc(), concat(text(" "), then_branch.to_doc())),
                );
                // Adiciona a cláusula `else` apenas se ela existir.
                if let Some(else_b) = else_branch {
                    doc = concat(doc, concat(text(" else "), else_b.to_doc()));
                }
                doc
            }
            // Formata laços `while`.
            Statement::While(cond, body) => concat(
                text("while "),
                concat(cond.to_doc(), concat(text(" "), body.to_doc())),
            ),
            // Formata laços `for`.
            Statement::For(var, iterable, body) => concat(
                text("for "),
                concat(
                    text(var.clone()),
                    concat(
                        text(" in "),
                        concat(iterable.to_doc(), concat(text(" "), body.to_doc())),
                    ),
                ),
            ),
            // Delega a formatação da definição de função para a implementação `ToDoc` de `Function`.
            Statement::FuncDef(func) => func.to_doc(),

            // Sequência explícita; imprime first\nsecond mantendo layout de bloco/indentação natural.
            Statement::Sequence(s1, s2) => concat(s1.to_doc(), concat(hardline(), s2.to_doc())),

            // Asserts especializados.
            Statement::AssertTrue(expr, msg) => concat(
                text("assert_true("),
                concat(
                    expr.to_doc(),
                    concat(text(", \""), concat(text(msg.clone()), text("\");"))),
                ),
            ),
            Statement::AssertFalse(expr, msg) => concat(
                text("assert_false("),
                concat(
                    expr.to_doc(),
                    concat(text(", \""), concat(text(msg.clone()), text("\");"))),
                ),
            ),
            Statement::AssertEQ(a, b, msg) => concat(
                text("assert_eq("),
                concat(
                    a.to_doc(),
                    concat(
                        text(", "),
                        concat(
                            b.to_doc(),
                            concat(text(", \""), concat(text(msg.clone()), text("\");"))),
                        ),
                    ),
                ),
            ),
            Statement::AssertNEQ(a, b, msg) => concat(
                text("assert_neq("),
                concat(
                    a.to_doc(),
                    concat(
                        text(", "),
                        concat(
                            b.to_doc(),
                            concat(text(", \""), concat(text(msg.clone()), text("\");"))),
                        ),
                    ),
                ),
            ),

            Statement::AssertFails(msg) => concat(
                text("assert_fails(\""),
                concat(text(msg.clone()), text("\");")),
            ),

            // Declaração de tipo (ADT) como bloco.
            Statement::TypeDeclaration(name, ctors) => {
                let ctor_docs: Vec<Rc<Doc>> = ctors
                    .iter()
                    .map(|c| {
                        // "| Nome t1 t2"
                        let types_docs: Vec<Rc<Doc>> = c.types.iter().map(|t| t.to_doc()).collect();
                        let tail = if types_docs.is_empty() {
                            nil()
                        } else {
                            concat(text(" "), join(text(" "), types_docs))
                        };
                        concat(text("| "), concat(text(c.name.clone()), tail))
                    })
                    .collect();
                concat(
                    text("type "),
                    concat(
                        text(name.clone()),
                        concat(
                            text(":"),
                            concat(
                                nest(4, concat(hardline(), join(hardline(), ctor_docs))),
                                concat(hardline(), text("end")),
                            ),
                        ),
                    ),
                )
            }

            // Test definitions (similar a function, mas preservando palavra-chave test/modtest)
            Statement::TestDef(func) => {
                // Reusa ToDoc de Function mas prefixa com 'test '.
                concat(text("test "), func.to_doc())
            }
            Statement::ModTestDef(module, stmt) => concat(
                text("modtest "),
                concat(text(module.clone()), concat(text(" "), stmt.to_doc())),
            ),
        }
    }
}

/// Implementa a conversão de uma `Function` da AST para `Doc`.
impl ToDoc for Function {
    fn to_doc(&self) -> Rc<Doc> {
        // Mapeia cada argumento formal para sua representação em `Doc`.
        let params_docs: Vec<Rc<Doc>> = self.params.iter().map(|p| p.to_doc()).collect();
        // Agrupa a lista de parâmetros. Se não couber em uma linha, quebra e indenta.
        let params_doc = group(concat(
            text("("),
            concat(
                nest(
                    4,
                    concat(line(), join(concat(text(","), line()), params_docs)),
                ),
                concat(line(), text(")")),
            ),
        ));

        // Formata o corpo da função. Se não houver corpo, imprime um bloco vazio.
        let body_doc = self
            .body
            .as_ref()
            .map_or_else(|| concat(text(":"), text(" end")), |b| b.to_doc());

        // Concatena todas as partes para formar a definição completa da função.
        concat(
            text("def "),
            concat(
                text(self.name.clone()),
                concat(
                    params_doc,
                    concat(
                        text(" -> "),
                        concat(
                            self.kind.to_doc(), // O tipo de retorno.
                            concat(text(" "), body_doc),
                        ),
                    ),
                ),
            ),
        )
    }
}

/// Implementa a conversão de um `FormalArgument` (parâmetro de função) para `Doc`.
impl ToDoc for FormalArgument {
    fn to_doc(&self) -> Rc<Doc> {
        // Formata um argumento como "<nome>: <tipo>".
        concat(
            text(self.argument_name.clone()),
            concat(text(": "), self.argument_type.to_doc()),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{Expression, Statement};
    use crate::pretty_print::pretty;

    #[test]
    fn test_block_formatting() {
        let block = Statement::Block(vec![
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(1))),
            Statement::Assignment("y".to_string(), Box::new(Expression::CInt(2))),
        ]);
        let expected = ":\n    var x = 1;\n    y = 2;\nend";
        assert_eq!(pretty(80, &block.to_doc()), expected);
    }

    #[test]
    fn test_nested_structure_formatting() {
        let while_stmt = Statement::While(
            Box::new(Expression::Var("cond".to_string())),
            Box::new(Statement::Block(vec![Statement::IfThenElse(
                Box::new(Expression::Var("a".to_string())),
                Box::new(Statement::Block(vec![Statement::Assignment(
                    "x".to_string(),
                    Box::new(Expression::CInt(1)),
                )])),
                None,
            )])),
        );
        let expected_while = "while cond :\n    if a :\n        x = 1;\n    end\nend";
        assert_eq!(pretty(80, &while_stmt.to_doc()), expected_while);
    }

    #[test]
    fn test_assert_variants_and_type_declaration() {
        use crate::ir::ast::{Type, ValueConstructor};
        let stmt = Statement::Sequence(
            Box::new(Statement::AssertTrue(
                Box::new(Expression::Var("flag".into())),
                "ok".into(),
            )),
            Box::new(Statement::AssertFalse(
                Box::new(Expression::Var("flag".into())),
                "fail".into(),
            )),
        );
        let printed = pretty(120, &stmt.to_doc());
        assert!(printed.contains("assert_true(flag, \"ok\");"));
        assert!(printed.contains("assert_false(flag, \"fail\");"));

        let td = Statement::TypeDeclaration(
            "Opt".into(),
            vec![
                ValueConstructor {
                    name: "Some".into(),
                    types: vec![Type::TInteger],
                },
                ValueConstructor {
                    name: "None".into(),
                    types: vec![],
                },
            ],
        );
        let td_printed = pretty(120, &td.to_doc());
        assert!(td_printed.starts_with("type Opt:"));
        assert!(td_printed.contains("| Some Int"));
        assert!(td_printed.contains("| None"));
    }

    #[test]
    fn test_testdef_and_modtestdef() {
        use crate::ir::ast::{Function, Type};
        let func = Function {
            name: "f".into(),
            kind: Type::TInteger,
            params: vec![],
            body: None,
        };
        let test_def = Statement::TestDef(func.clone());
        let mod_test = Statement::ModTestDef(
            "m".into(),
            Box::new(Statement::Return(Box::new(Expression::CInt(1)))),
        );
        let t1 = pretty(80, &test_def.to_doc());
        assert!(t1.starts_with("test def f"));
        let t2 = pretty(80, &mod_test.to_doc());
        assert!(t2.starts_with("modtest m "));
    }
}
