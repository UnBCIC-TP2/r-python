// src/pretty_print/pretty_print.rs

use std::rc::Rc;

/// Representa a estrutura abstrata de um documento formatável.
/// Baseado na abordagem algébrica de Philip Wadler, cada variante é uma instrução
/// de layout que será interpretada pelo motor de renderização.
/// O uso de `Rc<Doc>` permite o compartilhamento eficiente da estrutura de dados
/// sem a necessidade de múltiplas cópias.
#[derive(Clone)]
pub enum Doc {
    /// Um documento vazio. Atua como o elemento neutro para a concatenação.
    Nil,
    /// Um bloco de texto atômico que nunca deve ser quebrado em múltiplas linhas.
    Text(String),
    /// Representa uma possível quebra de linha. Dentro de um `Group`, pode ser
    /// renderizado como um espaço (se o grupo couber em uma linha) ou como uma
    /// quebra de linha real.
    Line,
    /// Uma quebra de linha obrigatória. Sempre será renderizada como uma nova linha,
    /// independentemente do contexto.
    HardLine,
    /// Adiciona um nível de indentação ao documento aninhado. O `usize` representa
    /// o número de espaços a serem adicionados.
    Nest(usize, Rc<Doc>),
    /// Concatena dois documentos sequencialmente.
    Concat(Rc<Doc>, Rc<Doc>),
    /// O operador mais importante para layouts flexíveis. Oferece ao motor
    /// uma escolha: renderizar o documento contido em uma única linha (achatado)
    /// ou com quebras de linha (se não couber).
    Group(Rc<Doc>),
}

/// Um trait que define o contrato para converter uma estrutura de dados (como um nó da AST)
/// em sua representação de `Doc`. Isso desacopla o motor de renderização das
/// estruturas de dados específicas da linguagem.
pub trait ToDoc {
    fn to_doc(&self) -> Rc<Doc>;
}

// --- Funções Construtoras ---
// Estas funções servem como uma API mais limpa e ergonômica para criar `Doc`s.

/// Cria um documento vazio (`Doc::Nil`).
pub fn nil() -> Rc<Doc> { Rc::new(Doc::Nil) }
/// Cria um documento de texto (`Doc::Text`).
pub fn text<S: Into<String>>(s: S) -> Rc<Doc> { Rc::new(Doc::Text(s.into())) }
/// Cria uma quebra de linha suave (`Doc::Line`).
pub fn line() -> Rc<Doc> { Rc::new(Doc::Line) }
/// Cria uma quebra de linha dura (`Doc::HardLine`).
pub fn hardline() -> Rc<Doc> { Rc::new(Doc::HardLine) }
/// Aninha um documento com um nível de indentação (`Doc::Nest`).
pub fn nest(i: usize, doc: Rc<Doc>) -> Rc<Doc> { Rc::new(Doc::Nest(i, doc)) }
/// Agrupa um documento, permitindo layouts flexíveis (`Doc::Group`).
pub fn group(doc: Rc<Doc>) -> Rc<Doc> { Rc::new(Doc::Group(doc)) }
/// Concatena dois documentos (`Doc::Concat`).
pub fn concat(d1: Rc<Doc>, d2: Rc<Doc>) -> Rc<Doc> {
    Rc::new(Doc::Concat(d1, d2))
}

// --- O Motor de Renderização ---

/// A função principal que renderiza um `Doc` em uma `String`, dada uma largura máxima.
///
/// # Argumentos
/// * `width` - A largura máxima da linha desejada.
/// * `doc` - O documento a ser formatado.
pub fn pretty(width: usize, doc: &Rc<Doc>) -> String {
    let mut out = String::new();
    // Inicia o processo de renderização com a largura máxima, posição inicial 0,
    // e indentação inicial 0.
    best(width as isize, 0, &[(0, doc.clone())], &mut out);
    out
}

/// Transforma um `Doc` em sua versão "achatada", onde todos os `Line`s
/// são convertidos em espaços. Usado para verificar se um `Group` cabe em uma única linha.
fn flatten(doc: &Rc<Doc>) -> Rc<Doc> {
    match &**doc {
        Doc::Nil => nil(),
        Doc::Text(_) => doc.clone(),
        Doc::Line => text(" "), // Quebras de linha suaves viram espaços.
        Doc::HardLine => nil(),   // Quebras de linha duras são removidas.
        Doc::Nest(_, d) => flatten(d), // Indentação é ignorada no modo achatado.
        Doc::Concat(d1, d2) => concat(flatten(d1), flatten(d2)),
        Doc::Group(d) => flatten(d), // Grupos aninhados são recursivamente achatados.
    }
}

/// O coração do algoritmo de renderização. Ele percorre a estrutura do `Doc` e
/// decide qual o melhor layout, escrevendo o resultado na string de saída.
///
/// # Argumentos
/// * `w` - A largura máxima da linha (constante durante a recursão).
/// * `k` - A posição atual do cursor na linha.
/// * `docs` - Uma lista de `(indentação, documento)` a serem processados.
/// * `out` - A `String` de saída que está sendo construída.
///
/// # Retorno
/// A nova posição do cursor `k` após processar os documentos.
fn best(w: isize, mut k: isize, docs: &[(usize, Rc<Doc>)], out: &mut String) -> isize {
    for (indent, doc) in docs {
        match &**doc {
            Doc::Nil => {} // Não faz nada para um documento vazio.
            Doc::Text(s) => {
                // Adiciona o texto à saída e avança o cursor.
                k += s.len() as isize;
                out.push_str(s);
            }
            Doc::Line | Doc::HardLine => {
                // Insere uma quebra de linha e a indentação correta.
                out.push('\n');
                let new_indent_str = " ".repeat(*indent);
                // Reseta a posição do cursor para a nova indentação.
                k = *indent as isize;
                out.push_str(&new_indent_str);
            }
            Doc::Nest(i, d) => {
                // Processa o documento aninhado com um nível de indentação aumentado.
                k = best(w, k, &[(indent + i, d.clone())], out);
            }
            Doc::Concat(d1, d2) => {
                // Processa o primeiro documento, depois o segundo.
                k = best(w, k, &[(*indent, d1.clone())], out);
                k = best(w, k, &[(*indent, d2.clone())], out);
            }
            Doc::Group(d) => {
                // A lógica central: decide entre o layout achatado e o quebrado.
                // `w - k` é o espaço restante na linha.
                if fits(w - k, &flatten(d)) {
                    // Se a versão achatada cabe, use-a.
                    k = best(w, k, &[(*indent, flatten(d))], out);
                } else {
                    // Senão, use a versão original com quebras de linha.
                    k = best(w, k, &[(*indent, d.clone())], out);
                }
            }
        }
    }
    k
}

/// Verifica se um documento (tipicamente achatado) cabe em `w` colunas restantes.
/// Esta função implementa o "bounded lookahead": ela para assim que o limite é excedido.
///
/// # Argumentos
/// * `w` - O número de colunas restantes na linha.
/// * `doc` - O documento a ser verificado.
fn fits(mut w: isize, doc: &Rc<Doc>) -> bool {
    // Se o espaço restante já é negativo, é impossível caber.
    if w < 0 { return false; }
    match &**doc {
        Doc::Nil => true, // O vazio sempre cabe.
        Doc::Text(s) => {
            // Subtrai o comprimento do texto do espaço restante.
            w -= s.len() as isize;
            w >= 0 // Retorna se ainda há espaço.
        }
        Doc::Line => true, // Uma linha achatada (espaço) não causa quebra, então sempre "cabe".
        Doc::HardLine => false, // Uma quebra de linha forçada nunca cabe em uma linha.
        Doc::Nest(_, d) => fits(w, d), // A indentação não afeta o cálculo em uma única linha.
        Doc::Concat(d1, d2) => {
            // Verifica a primeira parte; se ela couber, calcula o espaço que ela
            // ocupou e verifica a segunda parte com o espaço restante.
            if !fits(w, d1) { return false; }
            let mut temp_s = String::new();
            // Renderiza d1 temporariamente para saber seu comprimento real.
            let k = best(w, 0, &[(0, d1.clone())], &mut temp_s);
            fits(w - k, d2)
        }
        Doc::Group(d) => {
            // Um grupo cabe se sua versão achatada couber.
            fits(w, &flatten(d))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Função auxiliar para juntar múltiplos documentos com um separador.
    fn join(sep: Rc<Doc>, docs: Vec<Rc<Doc>>) -> Rc<Doc> {
        docs.into_iter().reduce(|acc, doc| concat(acc, concat(sep.clone(), doc))).unwrap_or_else(nil)
    }

    #[test]
    fn test_nesting() {
        let doc = concat(text("inicio"), nest(4, concat(hardline(), text("meio"))));
        let expected = "inicio\n    meio";
        assert_eq!(pretty(80, &doc), expected);
    }

    #[test]
    fn test_group_fits_on_one_line() {
        let list_doc = group(concat(text("["), concat(nest(4, concat(line(), join(concat(text(","), line()), vec![text("1"), text("2"), text("3")]))), concat(line(), text("]")))));
        assert_eq!(pretty(80, &list_doc), "[ 1, 2, 3 ]");
    }

    #[test]
    fn test_group_breaks_into_multiple_lines() {
        let list_doc = group(concat(
            text("["),
            concat(
                nest(4, concat(
                    line(),
                    join(concat(text(","), line()), vec![text("\"item1\""), text("\"item2\"")])
                )),
                concat(line(), text("]"))
            )
        ));
        let expected = "[\n    \"item1\",\n    \"item2\"\n]";
        assert_eq!(pretty(10, &list_doc), expected);
    }
}