# Exemplos de Operações de Arquivo - RPython

Este arquivo contém exemplos práticos de como usar as operações de arquivo implementadas na STDLIB do RPython.

## Exemplo 1: Sistema de Configuração Simples

```python
# config_manager.rpy
# Sistema simples de gerenciamento de configuração

# Define arquivo de configuração
config_file = "app_config.txt"

# Verifica se existe configuração
config_exists = file_exists(config_file)

if config_exists:
    # Carrega configuração existente
    config_content = read_file(config_file)
    x = 1  # Configuração carregada
else:
    # Cria configuração padrão
    default_config = "debug=false\nport=8080\nmax_users=100"
    write_success = write_file(config_file, default_config)
    
    if write_success:
        x = 2  # Configuração criada
    else:
        x = 0  # Erro ao criar configuração
```

## Exemplo 2: Sistema de Log

```python
# logger.rpy
# Sistema simples de logging

log_file = "application.log"
new_message = "Aplicacao iniciada em modo debug"

# Verifica se arquivo de log existe
log_exists = file_exists(log_file)

if log_exists:
    # Lê log atual e adiciona nova mensagem
    current_log = read_file(log_file)
    updated_log = current_log + "\n[INFO] " + new_message
    write_success = write_file(log_file, updated_log)
else:
    # Cria novo arquivo de log
    initial_log = "[INFO] " + new_message
    write_success = write_file(log_file, initial_log)

if write_success:
    status = 1  # Log atualizado com sucesso
else:
    status = 0  # Erro ao atualizar log
```

## Exemplo 3: Backup de Dados

```python
# backup_system.rpy
# Sistema simples de backup

source_file = "important_data.txt"
backup_file = "important_data_backup.txt"

# Verifica se arquivo original existe
source_exists = file_exists(source_file)

if source_exists:
    # Lê dados originais
    original_data = read_file(source_file)
    
    # Cria backup
    backup_success = write_file(backup_file, original_data)
    
    if backup_success:
        # Verifica se backup foi criado corretamente
        backup_exists = file_exists(backup_file)
        
        if backup_exists:
            # Verifica integridade do backup
            backup_data = read_file(backup_file)
            
            # Compara conteúdos (simulação simples)
            if backup_data:
                result = 1  # Backup criado e verificado
            else:
                result = 2  # Backup criado mas vazio
        else:
            result = 3  # Falha na criação do backup
    else:
        result = 4  # Erro ao escrever backup
else:
    result = 0  # Arquivo original não existe
```

## Exemplo 4: Sistema de Templates

```python
# template_system.rpy
# Sistema simples de templates

template_file = "email_template.txt"
output_file = "generated_email.txt"

# Carrega template
template_exists = file_exists(template_file)

if template_exists:
    template_content = read_file(template_file)
    
    # Simula processamento de template (substituição simples)
    # Em um caso real, faria substituições de variáveis
    processed_content = "Processado: " + template_content
    
    # Gera arquivo final
    generation_success = write_file(output_file, processed_content)
    
    if generation_success:
        # Verifica se arquivo foi gerado
        output_exists = file_exists(output_file)
        
        if output_exists:
            success = 1  # Template processado com sucesso
        else:
            success = 2  # Erro na verificação do arquivo gerado
    else:
        success = 0  # Erro ao gerar arquivo
else:
    # Cria template padrão
    default_template = "Olá {nome},\n\nEsta é uma mensagem padrão.\n\nAtenciosamente,\nSistema"
    create_success = write_file(template_file, default_template)
    
    if create_success:
        success = 3  # Template padrão criado
    else:
        success = 0  # Erro ao criar template padrão
```

## Exemplo 5: Validação de Arquivos

```python
# file_validator.rpy
# Sistema de validação de arquivos

files_to_check = "file1.txt"  # Em um caso real, seria uma lista
validation_report = "validation_report.txt"

# Inicia relatório
report_content = "=== RELATÓRIO DE VALIDAÇÃO ===\n"

# Verifica arquivo 1
file1_exists = file_exists("file1.txt")

if file1_exists:
    file1_content = read_file("file1.txt")
    
    if file1_content:
        report_content = report_content + "file1.txt: OK - Arquivo existe e não está vazio\n"
        file1_status = 1
    else:
        report_content = report_content + "file1.txt: AVISO - Arquivo existe mas está vazio\n"
        file1_status = 2
else:
    report_content = report_content + "file1.txt: ERRO - Arquivo não encontrado\n"
    file1_status = 0

# Verifica arquivo 2
file2_exists = file_exists("file2.txt")

if file2_exists:
    file2_content = read_file("file2.txt")
    
    if file2_content:
        report_content = report_content + "file2.txt: OK - Arquivo existe e não está vazio\n"
        file2_status = 1
    else:
        report_content = report_content + "file2.txt: AVISO - Arquivo existe mas está vazio\n"
        file2_status = 2
else:
    report_content = report_content + "file2.txt: ERRO - Arquivo não encontrado\n"
    file2_status = 0

# Finaliza relatório
report_content = report_content + "=== FIM DO RELATÓRIO ===\n"

# Salva relatório
report_success = write_file(validation_report, report_content)

if report_success:
    final_status = 1  # Validação concluída
else:
    final_status = 0  # Erro ao salvar relatório
```

## Exemplo 6: Sistema de Cache Simples

```python
# cache_system.rpy
# Sistema simples de cache em arquivo

cache_file = "data_cache.txt"
data_key = "user_preferences"

# Verifica se cache existe
cache_exists = file_exists(cache_file)

if cache_exists:
    # Tenta carregar do cache
    cached_data = read_file(cache_file)
    
    if cached_data:
        # Cache hit - dados encontrados
        loaded_data = cached_data
        cache_status = 1  # Dados carregados do cache
    else:
        # Cache existe mas está vazio
        cache_status = 2  # Cache vazio
        loaded_data = ""
else:
    # Cache miss - arquivo não existe
    cache_status = 0  # Cache não existe
    loaded_data = ""

# Se não há dados em cache, cria dados padrão
if cache_status == 0:
    # Simula busca de dados (em caso real, viria de database/API)
    fresh_data = "theme=dark\nlanguage=pt\nnotifications=true"
    
    # Salva no cache
    cache_write_success = write_file(cache_file, fresh_data)
    
    if cache_write_success:
        loaded_data = fresh_data
        final_status = 3  # Dados criados e salvos no cache
    else:
        final_status = 4  # Erro ao salvar no cache
else:
    final_status = cache_status  # Mantém status do cache
```

## Como Executar os Exemplos

1. **Compile o RPython:**
   ```bash
   cargo build
   ```

2. **Execute um exemplo:**
   ```bash
   cargo run
   ```

3. **Para testar as operações de arquivo:**
   - Crie arquivos de teste quando necessário
   - Verifique os arquivos gerados após a execução
   - Observe as mensagens de erro/sucesso

## Considerações Importantes

### Tratamento de Erros
- Sempre verifique o retorno das operações
- Use `file_exists` antes de tentar ler arquivos
- `write_file` retorna `true`/`false` para sucesso/erro

### Limitações
- Apenas texto (UTF-8)
- Não há operações de append
- Não há controle de permissões
- Caminhos devem ser válidos no sistema operacional

### Boas Práticas
- Use caminhos relativos quando possível
- Faça backup antes de sobrescrever arquivos importantes
- Valide entrada do usuário antes de usar como caminho
- Implemente verificação de integridade quando necessário
