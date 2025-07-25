// src/pretty_print/pretty_expressions.rs

use std::rc::Rc;
use crate::ir::ast::Expression;
use super::pretty_print::{
    group, nest, nil, text, ToDoc, Doc, concat, line
};

// --- Níveis de Precedência dos Operadores ---
// Define a ordem de operações para evitar o uso desnecessário de parênteses.
// Um operador com maior precedência é avaliado antes de um com menor precedência.
// Ex: `*` (PREC_MUL_DIV) tem maior precedência que `+` (PREC_ADD_SUB).
const PREC_NONE: usize = 0;       // Contexto sem operador, como o topo de uma expressão.
const PREC_OR: usize = 1;         // Precedência para o operador `or`.
const PREC_AND: usize = 2;        // Precedência para o operador `and`.
const PREC_RELATIONAL: usize = 3; // Precedência para operadores como `==`, `>`, `<`.
const PREC_ADD_SUB: usize = 4;    // Precedência para `+`, `-`.
const PREC_MUL_DIV: usize = 5;    // Precedência para `*`, `/`.
const PREC_UNARY: usize = 6;      // Precedência para operadores unários como `not`.
const PREC_CALL: usize = 7;       // Precedência para chamadas de função e construtores.

// --- Implementação do Trait ---

/// Ponto de entrada para a conversão de uma `Expression` em `Doc`.
impl ToDoc for Expression {
    fn to_doc(&self) -> Rc<Doc> {
        // Inicia a conversão recursiva com a menor precedência possível.
        self.to_doc_inner(PREC_NONE)
    }
}

impl Expression {
    /// Função recursiva auxiliar que formata uma `Expression` em `Doc`,
    /// levando em conta a precedência do operador pai para decidir se
    /// parênteses são necessários.
    ///
    /// # Argumentos
    /// * `parent_precedence` - O nível de precedência da expressão externa (pai).
    fn to_doc_inner(&self, parent_precedence: usize) -> Rc<Doc> {
        /// Closure que adiciona parênteses a um `Doc` se a precedência atual
        /// for menor que a do operador pai. Ex: em `(a + b) * c`, `+` tem
        /// menor precedência que `*`, então `a + b` precisa de parênteses.
        let maybe_paren = |current_precedence, doc| {
            if current_precedence < parent_precedence {
                concat(text("("), concat(doc, text(")")))
            } else {
                doc
            }
        };
        
        /// Closure auxiliar para formatar operadores binários de forma consistente,
        /// aplicando a lógica de parênteses.
        let binary_op = |op, current_prec, lhs: &Expression, rhs: &Expression| {
            let doc = concat(
                // Formata o lado esquerdo, passando a precedência atual.
                lhs.to_doc_inner(current_prec),
                // Adiciona o operador.
                concat(text(op), rhs.to_doc_inner(current_prec)),
            );
            // Adiciona parênteses se necessário.
            maybe_paren(current_prec, doc)
        };

        match self {
            // Literais e variáveis são simples: apenas converte para texto.
            Expression::CTrue => text("True"),
            Expression::CFalse => text("False"),
            Expression::CInt(i) => text(i.to_string()),
            Expression::CReal(f) => text(f.to_string()),
            Expression::CString(s) => text(format!("\"{}\"", s)),
            Expression::CVoid => text("()"),
            Expression::Var(name) => text(name.clone()),

            // Formatação para todos os operadores binários usando a closure `binary_op`.
            Expression::Or(l, r) => binary_op(" or ", PREC_OR, l, r),
            Expression::And(l, r) => binary_op(" and ", PREC_AND, l, r),
            Expression::EQ(l, r) => binary_op(" == ", PREC_RELATIONAL, l, r),
            Expression::NEQ(l, r) => binary_op(" != ", PREC_RELATIONAL, l, r),
            Expression::GT(l, r) => binary_op(" > ", PREC_RELATIONAL, l, r),
            Expression::LT(l, r) => binary_op(" < ", PREC_RELATIONAL, l, r),
            Expression::GTE(l, r) => binary_op(" >= ", PREC_RELATIONAL, l, r),
            Expression::LTE(l, r) => binary_op(" <= ", PREC_RELATIONAL, l, r),
            Expression::Add(l, r) => binary_op(" + ", PREC_ADD_SUB, l, r),
            Expression::Sub(l, r) => binary_op(" - ", PREC_ADD_SUB, l, r),
            Expression::Mul(l, r) => binary_op(" * ", PREC_MUL_DIV, l, r),
            Expression::Div(l, r) => binary_op(" / ", PREC_MUL_DIV, l, r),

            // Formatação para operadores unários.
            Expression::Not(expr) => {
                let doc = concat(text("not "), expr.to_doc_inner(PREC_UNARY));
                maybe_paren(PREC_UNARY, doc)
            }
            Expression::Unwrap(expr) => {
                 concat(expr.to_doc_inner(PREC_CALL), text("!"))
            }

            // Estruturas complexas que podem ter quebra de linha.
            Expression::FuncCall(name, args) => {
                let separator = concat(text(","), line()); // Separador: "," e uma possível quebra de linha.
                let args_docs = args.iter().map(|arg| arg.to_doc_inner(PREC_NONE)).collect();
                
                concat(
                    text(name.clone()),
                    // `group` permite que a lista de argumentos fique em uma linha ou
                    // seja quebrada e indentada se não couber.
                    group(concat(
                        text("("),
                        concat(nest(4, join(separator, args_docs)), text(")")),
                    )),
                )
            }

            Expression::ListValue(elements) => {
                let separator = concat(text(","), line());
                let elems_docs = elements.iter().map(|el| el.to_doc_inner(PREC_NONE)).collect();
                
                group(concat(
                    text("["),
                    concat(nest(4, concat(line(), join(separator, elems_docs))), concat(line(), text("]")))
                ))
            }

            Expression::Constructor(name, args) => {
                let args_docs = args.iter().map(|arg| arg.to_doc_inner(PREC_NONE)).collect();
                let doc = concat(text(name.clone()), concat(text(" "), join(text(" "), args_docs)));
                maybe_paren(PREC_CALL, doc)
            }

            // Tipos de tratamento de erro (Maybe/Result).
            Expression::COk(expr) => concat(text("Ok("), concat(expr.to_doc(), text(")"))),
            Expression::CErr(expr) => concat(text("Err("), concat(expr.to_doc(), text(")"))),
            Expression::CJust(expr) => concat(text("Just("), concat(expr.to_doc(), text(")"))),
            Expression::CNothing => text("Nothing"),
            
            // Fallback para expressões que ainda não têm uma regra de formatação.
            _ => text("/* expr not implemented */"),
        }
    }
}

/// Função auxiliar para juntar uma lista de documentos com um separador.
fn join(sep: Rc<Doc>, docs: Vec<Rc<Doc>>) -> Rc<Doc> {
    docs.into_iter().reduce(|acc, doc| concat(acc, concat(sep.clone(), doc))).unwrap_or_else(nil)
}

#[cfg(test)]
mod tests {
    use crate::ir::ast::Expression;
    use super::*;
    use crate::pretty_print::pretty;

    #[test]
    fn test_precedence_add_mul() {
        let expr = Expression::Mul(
            Box::new(Expression::Add(
                Box::new(Expression::Var("a".to_string())),
                Box::new(Expression::Var("b".to_string())),
            )),
            Box::new(Expression::Var("c".to_string())),
        );
        assert_eq!(pretty(80, &expr.to_doc()), "(a + b) * c");
    }
    
    #[test]
    fn test_precedence_relational_and() {
        let expr = Expression::And(
            Box::new(Expression::GT(
                Box::new(Expression::Var("a".to_string())),
                Box::new(Expression::Var("b".to_string())),
            )),
            Box::new(Expression::LT(
                Box::new(Expression::Var("c".to_string())),
                Box::new(Expression::Var("d".to_string())),
            )),
        );
        assert_eq!(pretty(80, &expr.to_doc()), "a > b and c < d");
    }

    #[test]
    fn test_list_layout() {
        let list = Expression::ListValue(vec![
            Expression::CString("item longo".to_string()),
            Expression::CString("outro item longo".to_string()),
        ]);
        
        let doc = list.to_doc();

        // Com espaço suficiente, a lista é formatada em uma única linha.
        assert_eq!(pretty(100, &doc), "[ \"item longo\", \"outro item longo\" ]");

        // Com espaço limitado, a lista quebra a linha e indenta os elementos.
        let narrow_expected = "[\n    \"item longo\",\n    \"outro item longo\"\n]";
        assert_eq!(pretty(20, &doc), narrow_expected);
    }
}