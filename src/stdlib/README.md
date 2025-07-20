# Documentação da Biblioteca Padrão

A biblioteca padrão fornece um conjunto de funções integradas (metabuiltins) para a linguagem R-Python. Essas funções permitem interação com o sistema, entrada/saída de dados e manipulação de arquivos.

## Visão Geral

O módulo implementa um sistema de metabuiltins que são funções especiais executadas em tempo de interpretação. Estas funções têm acesso direto ao ambiente de execução e podem retornar statements para o interpretador.

A biblioteca padrão atual inclui três funcionalidades principais:
- Entrada de dados do usuário (`input`)
- Saída de dados para console (`print`)
- Manipulação de arquivos (`open`)

## Estruturas

### MetaBuiltinStmt

Tipo de função que define a assinatura das funções integradas.

```rust
pub type MetaBuiltinStmt = fn(&mut Environment<Expression>) -> Statement;
```

Cada metabuiltin:
- Recebe uma referência mutável para o ambiente de execução
- Acessa argumentos através de lookups no ambiente
- Retorna um `Statement` (geralmente `Statement::Return`)

### Tabela de Metabuiltins

Uma tabela estática global que mapeia nomes de funções para suas implementações.

```rust
static METABUILTINS_TABLE: OnceLock<HashMap<String, MetaBuiltinStmt>>
```

## Funções Disponíveis

### input(prompt?)

Lê uma linha de entrada do usuário.

**Parâmetros:**
- `prompt` (opcional): String que será exibida antes da entrada

**Retorno:**
- String contendo a linha lida (sem quebra de linha)

**Comportamento:**
1. Exibe o prompt se fornecido
2. Lê uma linha da entrada padrão
3. Remove caracteres de quebra de linha
4. Retorna a string resultante

**Exemplo de uso:**
```
nome = input("Digite seu nome: ")
```

### print(value?)

Imprime um valor na saída padrão.

**Parâmetros:**
- `value` (opcional): Valor a ser impresso (padrão: string vazia)

**Retorno:**
- `CVoid` (nenhum valor)

**Tipos suportados:**
- `CString`: Imprime a string diretamente
- `CInt`: Imprime o número inteiro
- `CReal`: Imprime o número real
- Outros tipos: Imprime representação de debug

**Exemplo de uso:**
```
print("Olá, mundo!")
print(42)
print(3.14)
```

### open(path, mode?, content?)

Manipula operações de arquivo (leitura, escrita, append).

**Parâmetros:**
- `path`: String com o caminho do arquivo (obrigatório)
- `mode`: String com o modo de operação (opcional, padrão: "r")
- `content`: String com conteúdo para escrita/append (obrigatório para "w" e "a")

**Modos disponíveis:**
- `"r"`: Leitura - retorna o conteúdo do arquivo como string
- `"w"`: Escrita - escreve conteúdo no arquivo (sobrescreve)
- `"a"`: Append - adiciona conteúdo ao final do arquivo

**Retorno:**
- Modo "r": String com o conteúdo do arquivo
- Modos "w" e "a": `CVoid` se bem-sucedido
- Em caso de erro: String com mensagem de erro

**Tratamento de erros:**
- Arquivo não encontrado (modo "r")
- Permissões insuficientes
- Argumentos inválidos ou ausentes
- Modo não suportado

**Exemplos de uso:**
```
# Leitura
conteudo = open("arquivo.txt", "r")

# Escrita
open("novo_arquivo.txt", "w", "Conteúdo do arquivo")

# Append
open("log.txt", "a", "Nova linha de log")
```

## Gerenciamento da Biblioteca

### get_metabuiltins_table()

Retorna a tabela estática de metabuiltins disponíveis.

**Retorno:**
- Referência para `HashMap<String, MetaBuiltinStmt>`

**Características:**
- Inicialização única (lazy initialization)
- Thread-safe através de `OnceLock`
- Contém mapeamentos nome → função

## Arquitetura de Integração

### Acesso aos Argumentos

As funções integradas acessam seus argumentos através do ambiente de execução:

```rust
let valor = match env.lookup(&"nome_argumento".to_string()) {
    Some((_, Expression::TipoEsperado(v))) => v.clone(),
    _ => valor_padrao,
};
```

### Tratamento de Erros

Erros são retornados como `Statement::Return(Expression::CString)` contendo mensagens descritivas em português.

### Integração com o Interpretador

1. O interpretador consulta a tabela de metabuiltins
2. Localiza a função pelo nome
3. Configura o ambiente com os argumentos
4. Executa a função
5. Processa o statement retornado

## Notas de Implementação

1. **Thread Safety**: A tabela de metabuiltins usa `OnceLock` para inicialização thread-safe
2. **Gestão de Memória**: Argumentos são clonados do ambiente quando necessário
3. **Validação de Tipos**: Cada função valida seus argumentos e retorna erros descritivos
4. **Compatibilidade**: Argumentos opcionais permitem diferentes formas de chamada
5. **Internacionalização**: Mensagens de erro são fornecidas em português brasileiro

## Extensibilidade

Para adicionar novas funções integradas:

1. Implemente uma função com assinatura `MetaBuiltinStmt`
2. Adicione-a à tabela em `get_metabuiltins_table()`
3. Documente os parâmetros e comportamento esperados
4. Inclua testes para validar a funcionalidade

**Exemplo de nova função:**
```rust
pub fn nova_funcao_builtin(env: &mut Environment<Expression>) -> Statement {
    // Acessar argumentos do ambiente
    // Executar lógica da função
    // Retornar Statement::Return com resultado
}
``` 