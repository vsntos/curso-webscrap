# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 11: Organização de Projetos
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - estruturar projetos de scraping de forma profissional;
# - usar naming conventions para arquivos e pastas;
# - separar dados raw de dados processados;
# - gerenciar credenciais com .Renviron;
# - versionar com Git e documentar com README;
# - garantir reprodutibilidade com renv;
# - criar funções utilitárias reutilizáveis.
#
# =========================================================
# 1. CRIAR ESTRUTURA DE PASTAS
# =========================================================

# fs facilita criação de diretórios
# install.packages("fs")
library(fs)
library(tidyverse)

# Criar estrutura completa do projeto
criar_estrutura_projeto <- function(raiz = ".") {

  pastas <- file.path(raiz, c(
    "data/raw",
    "data/processed",
    "scripts/utils",
    "outputs/figures",
    "outputs/tables",
    "docs",
    "logs",
    "screenshots"
  ))

  dir_create(pastas, recurse = TRUE)

  cat("Estrutura criada:\n")
  cat(dir_tree(raiz, recurse = FALSE))
}

# Criar no diretório atual do projeto
# criar_estrutura_projeto(".")

# Verificar que as pastas existem
dir.exists("data/raw")
dir.exists("data/processed")
dir.exists("scripts")
dir.exists("logs")

# =========================================================
# 2. NAMING CONVENTIONS
# =========================================================
#
# REGRAS:
#
# - snake_case: palavras_separadas_por_underscore
# - sem espaços, acentos ou caracteres especiais
# - datas em ISO 8601: YYYYMMDD
# - scripts numerados: 01_, 02_, 03_
# - arquivos de dados: fonte_conteudo_data.csv
#
# =========================================================

# Gerar nome de arquivo padronizado
nomear_arquivo <- function(
    fonte,
    conteudo,
    extensao = "csv",
    data     = Sys.Date()
) {

  data_str <- format(data, "%Y%m%d")

  # Normalizar: minúsculo, sem espaços, sem acentos
  normalizar <- function(x) {
    x |>
      str_to_lower() |>
      iconv(to = "ASCII//TRANSLIT") |>
      str_replace_all("[^a-z0-9]", "_") |>
      str_replace_all("_+", "_") |>
      str_remove("^_|_$")
  }

  sprintf(
    "%s_%s_%s.%s",
    normalizar(fonte),
    normalizar(conteudo),
    data_str,
    extensao
  )
}

# Exemplos
nomear_arquivo("Agência Brasil", "Notícias Internacionais")
nomear_arquivo("World Bank", "PIB BRICS 2000-2024")
nomear_arquivo("Câmara dos Deputados", "Votações RI 2025")

# =========================================================
# 3. SEPARAÇÃO RAW / PROCESSED
# =========================================================
#
# PRINCÍPIO:
#
# raw/       → dados exatamente como coletados
#              nunca sobrescrever, nunca modificar
#
# processed/ → resultado de scripts reproduzíveis
#              pode sempre ser regerado de raw/
#
# =========================================================

# Salvar arquivo raw com timestamp
salvar_raw <- function(df, fonte, conteudo) {

  nome <- nomear_arquivo(fonte, conteudo)
  caminho <- file.path("data/raw", nome)

  # Nunca sobrescrever raw
  if (file.exists(caminho)) {
    warning(sprintf(
      "Arquivo raw já existe: %s\nCriando versão com timestamp.", nome
    ))
    ts <- format(Sys.time(), "%H%M%S")
    nome <- str_replace(nome, "\\.csv$", paste0("_", ts, ".csv"))
    caminho <- file.path("data/raw", nome)
  }

  write_csv(df, caminho)
  cat(sprintf("Raw salvo: %s (%d obs.)\n", caminho, nrow(df)))

  caminho
}

# Salvar arquivo processado
salvar_processed <- function(df, nome_base, versao = NULL) {

  if (!is.null(versao)) {
    nome <- paste0(nome_base, "_v", versao, ".csv")
  } else {
    nome <- paste0(nome_base, ".csv")
  }

  caminho <- file.path("data/processed", nome)

  write_csv(df, caminho)
  cat(sprintf("Processed salvo: %s (%d obs.)\n", caminho, nrow(df)))

  caminho
}

# =========================================================
# 4. VARIÁVEIS DE AMBIENTE (.Renviron)
# =========================================================
#
# NUNCA coloque credenciais diretamente no código.
# Use variáveis de ambiente.
#
# Para configurar, abra ~/.Renviron e adicione:
#
# NEWS_API_KEY=sua_chave_aqui
# WTO_KEY=sua_chave_aqui
# PORTAL_USER=seu_usuario
# PORTAL_SENHA=sua_senha
#
# =========================================================

# Verificar se variável está configurada
verificar_env <- function(vars) {

  for (v in vars) {
    valor <- Sys.getenv(v)
    if (nchar(valor) == 0) {
      cat(sprintf("⚠ %s não configurada em ~/.Renviron\n", v))
    } else {
      cat(sprintf("✓ %s configurada (%d chars)\n", v, nchar(valor)))
    }
  }
}

verificar_env(c("NEWS_API_KEY", "WTO_KEY", "PORTAL_USER"))

# Acessar variável com fallback
obter_api_key <- function(var_nome, obrigatorio = FALSE) {

  valor <- Sys.getenv(var_nome)

  if (nchar(valor) == 0) {
    if (obrigatorio) {
      stop(sprintf(
        "Variável '%s' não encontrada.\nAdicione em ~/.Renviron e reinicie o R.",
        var_nome
      ))
    }
    warning(sprintf("'%s' não configurada — retornando NULL.", var_nome))
    return(NULL)
  }

  valor
}

# =========================================================
# 5. SISTEMA DE LOGGING
# =========================================================

iniciar_log <- function(nome_sessao = NULL) {

  if (is.null(nome_sessao)) {
    nome_sessao <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }

  dir.create("logs", showWarnings = FALSE)

  caminho <- file.path("logs", paste0("sessao_", nome_sessao, ".log"))

  # Escrever cabeçalho
  cat(
    sprintf(
      "=== Sessão de Scraping ===\nInício: %s\nR: %s\n\n",
      format(Sys.time()),
      R.version.string
    ),
    file = caminho
  )

  cat("Log iniciado:", caminho, "\n")
  caminho
}

registrar <- function(log_file, nivel = "INFO", mensagem) {

  linha <- sprintf(
    "[%s] [%s] %s\n",
    format(Sys.time(), "%H:%M:%S"),
    nivel,
    mensagem
  )

  cat(linha)
  cat(linha, file = log_file, append = TRUE)
}

# Usar:
log_atual <- iniciar_log("aula11_exemplo")
registrar(log_atual, "INFO", "Iniciando coleta da Agência Brasil")
registrar(log_atual, "WARN", "Página 3 retornou 0 títulos")
registrar(log_atual, "ERRO", "Timeout na página 7")

# =========================================================
# 6. CABEÇALHO PADRÃO PARA SCRIPTS
# =========================================================
#
# Todo script deve começar com:
#
# # =========================================================
# # Projeto: [Nome do Projeto]
# # Script:  [número]_[descricao].R
# # Autor:   [nome]
# # Data:    [YYYY-MM-DD]
# #
# # Entrada: [arquivos de entrada]
# # Saída:   [arquivos de saída]
# #
# # Dependências:
# # - [pacote 1]
# # - [pacote 2]
# # =========================================================
#
# Isso garante que qualquer pessoa entende o propósito
# do script sem precisar ler o código.
#
# =========================================================

# =========================================================
# 7. FUNÇÕES UTILITÁRIAS REUTILIZÁVEIS
# =========================================================
#
# Coloque funções genéricas em scripts/utils/
# e use source() para importá-las.
#
# =========================================================

# Conteúdo de scripts/utils/scraper_utils.R:

# Raspar página com tratamento completo
raspar_pagina <- function(
    url,
    seletor_titulo = "h2",
    seletor_link   = "h2 a",
    delay          = 1.5
) {

  Sys.sleep(delay)

  pagina <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      message("ERRO: ", url, " — ", e$message)
      return(NULL)
    }
  )

  if (is.null(pagina)) return(tibble())

  titulos <- pagina |>
    rvest::html_elements(seletor_titulo) |>
    rvest::html_text2() |>
    stringr::str_squish()

  links <- tryCatch(
    pagina |>
      rvest::html_elements(seletor_link) |>
      rvest::html_attr("href"),
    error = function(e) character(0)
  )

  n <- min(length(titulos), length(links))
  if (n == 0) return(tibble())

  tibble(
    titulo      = titulos[1:n],
    link        = links[1:n],
    url_fonte   = url,
    coletado_em = Sys.time()
  )
}

# Limpar dataset
limpar_dataset <- function(df) {
  df |>
    dplyr::mutate(titulo = stringr::str_squish(titulo)) |>
    dplyr::filter(nchar(titulo) > 20) |>
    dplyr::distinct(link, .keep_all = TRUE)
}

# Salvar em arquivo se tiver dados
salvar_se_nao_vazio <- function(df, caminho) {
  if (nrow(df) == 0) {
    warning("Dataset vazio — nada salvo.")
    return(invisible(NULL))
  }
  write_csv(df, caminho)
  cat(sprintf("Salvo: %s (%d obs.)\n", caminho, nrow(df)))
}

# =========================================================
# 8. REPRODUTIBILIDADE COM RENV
# =========================================================
#
# renv garante que os pacotes usados são sempre
# os mesmos, em qualquer computador.
#
# =========================================================

# Fluxo de trabalho com renv:

# 1. No início do projeto:
#    renv::init()

# 2. Após instalar novos pacotes:
#    renv::snapshot()

# 3. Em outro computador ou após clonar:
#    renv::restore()

# 4. Ver pacotes registrados:
#    renv::status()

# Registrar informações do ambiente atual
registrar_ambiente <- function(caminho = "docs/ambiente.txt") {

  dir.create(dirname(caminho), showWarnings = FALSE)

  info <- c(
    paste("Data:", format(Sys.Date())),
    paste("R:", R.version.string),
    paste("Sistema:", Sys.info()["sysname"]),
    "",
    "Pacotes carregados:",
    paste(" -", names(sessionInfo()$otherPkgs))
  )

  writeLines(info, caminho)
  cat("Ambiente registrado em:", caminho, "\n")
}

registrar_ambiente()
readLines("docs/ambiente.txt")

# =========================================================
# 9. .GITIGNORE RECOMENDADO
# =========================================================
#
# Arquivo .gitignore na raiz do projeto:
#
# # R
# .Rhistory
# .RData
# .Rproj.user/
# renv/library/
#
# # Dados (geralmente grandes demais para Git)
# data/raw/
# *.csv
# *.xlsx
# *.parquet
#
# # Credenciais
# .Renviron
# *.env
# *_key.txt
#
# # Outputs renderizados
# *.html
# *.pdf
# outputs/
# screenshots/
#
# # Sistema
# .DS_Store
# Thumbs.db
#
# =========================================================

# Criar .gitignore programaticamente
criar_gitignore <- function() {

  conteudo <- c(
    "# R",
    ".Rhistory",
    ".RData",
    ".Rproj.user/",
    "renv/library/",
    "",
    "# Dados",
    "data/raw/",
    "*.csv",
    "*.xlsx",
    "",
    "# Credenciais",
    ".Renviron",
    "*.env",
    "",
    "# Outputs",
    "*.html",
    "outputs/",
    "screenshots/",
    "logs/",
    "",
    "# Sistema",
    ".DS_Store",
    "Thumbs.db"
  )

  writeLines(conteudo, ".gitignore")
  cat(".gitignore criado.\n")
}

# criar_gitignore()

# =========================================================
# 10. README TEMPLATE
# =========================================================

gerar_readme <- function(
    titulo,
    pergunta_pesquisa,
    fontes,
    periodo_coleta,
    autor
) {

  conteudo <- glue::glue(
    "# {titulo}",
    "",
    "## Pergunta de Pesquisa",
    "{pergunta_pesquisa}",
    "",
    "## Fontes de Dados",
    paste("-", fontes, collapse = "\n"),
    "",
    "## Período de Coleta",
    "{periodo_coleta}",
    "",
    "## Como Reproduzir",
    "```",
    "# 1. Clone o repositório",
    "# 2. Instale R >= 4.3",
    "# 3. Execute: renv::restore()",
    "# 4. Configure credenciais em ~/.Renviron",
    "# 5. Execute scripts em ordem: 01 → 02 → 03",
    "```",
    "",
    "## Estrutura",
    "```",
    "data/raw/        → dados brutos originais",
    "data/processed/  → dados limpos",
    "scripts/         → código de coleta e análise",
    "outputs/         → figuras e tabelas",
    "docs/            → documentação metodológica",
    "```",
    "",
    "## Autor",
    "{autor}",
    .sep = "\n"
  )

  writeLines(conteudo, "README.md")
  cat("README.md criado.\n")
}

# install.packages("glue")
library(glue)

gerar_readme(
  titulo            = "Cobertura Internacional na Mídia Brasileira",
  pergunta_pesquisa = "Como a mídia brasileira cobre temas de política internacional?",
  fontes            = c(
    "Agência Brasil (agenciabrasil.ebc.com.br)",
    "ONU Brasil (brasil.un.org/pt-br/news)",
    "Google News RSS"
  ),
  periodo_coleta = "Jan/2025 – Mai/2026",
  autor          = "Vinicius Santos, PPGRI"
)

cat(readLines("README.md"), sep = "\n")

# =========================================================
# 11. PIPELINE ORGANIZADO: EXEMPLO COMPLETO
# =========================================================

# Este seria o script 01_coleta.R em um projeto organizado:
executar_coleta <- function(n_paginas = 5) {

  log_f <- iniciar_log("coleta")

  registrar(log_f, "INFO", sprintf("Iniciando coleta: %d páginas", n_paginas))

  urls <- paste0(
    "https://agenciabrasil.ebc.com.br/internacional?page=",
    0:(n_paginas - 1)
  )

  df_raw <- purrr::map_dfr(urls, function(url) {
    registrar(log_f, "INFO", paste("Acessando:", url))
    raspar_pagina(url)
  })

  registrar(log_f, "INFO", sprintf("Coleta concluída: %d observações brutas", nrow(df_raw)))

  # Salvar raw
  if (nrow(df_raw) > 0) {
    nome_raw <- nomear_arquivo("agencia_brasil", "noticias_internacionais")
    write_csv(df_raw, file.path("data/raw", nome_raw))
    registrar(log_f, "INFO", paste("Raw salvo:", nome_raw))
  }

  df_raw
}

df_coletado <- executar_coleta(n_paginas = 3)
nrow(df_coletado)

# =========================================================
# 12. EXERCÍCIO
# =========================================================
#
# Reorganize o projeto do curso seguindo a estrutura:
#
# 1. Crie a estrutura de pastas com fs::dir_create();
# 2. Mova dados existentes para data/raw/;
# 3. Renomeie todos os scripts com numeração;
# 4. Crie um README.md com gerar_readme();
# 5. Crie .gitignore com criar_gitignore();
# 6. Registre o ambiente com registrar_ambiente();
# 7. Instale renv e execute renv::snapshot().
#
# =========================================================
# 13. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - criar estrutura de pastas profissional;
# - naming conventions para dados e scripts;
# - separar raw de processed (imutabilidade do raw);
# - gerenciar credenciais com .Renviron;
# - criar sistema de logging;
# - escrever cabeçalhos de script informativos;
# - organizar funções utilitárias reutilizáveis;
# - usar renv para reprodutibilidade de pacotes;
# - criar .gitignore adequado;
# - gerar README documentado.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 12: Pipelines Automatizados com targets
#
# - o problema da re-execução desnecessária;
# - targets: fluxo declarativo de dependências;
# - caching inteligente;
# - pipeline completo scraping → limpeza → output;
# - reprodutibilidade garantida.
#
# =========================================================
# FIM DA AULA
# =========================================================
