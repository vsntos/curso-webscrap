# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 12: Pipelines Automatizados com targets
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - o que é um pipeline declarativo;
# - criar e gerenciar targets com o pacote targets;
# - usar caching inteligente para evitar re-scraping;
# - detectar mudanças em URLs com format = "url";
# - executar targets em paralelo;
# - integrar scraping, limpeza e análise em um fluxo único;
# - visualizar o grafo de dependências.
#
# =========================================================
# INSTALAÇÃO
# =========================================================
#
# install.packages("targets")
# install.packages("tarchetypes")  # helpers úteis
# install.packages("visNetwork")   # para tar_visnetwork()
#
# =========================================================
# 1. CONCEITO: O PROBLEMA DA RE-EXECUÇÃO
# =========================================================
#
# Pipeline típico SEM targets:
#
# source("01_coleta.R")    # raspa o site (lento!)
# source("02_limpeza.R")   # processa dados
# source("03_analise.R")   # análise
#
# Problemas:
# - se mudar 02_limpeza.R, precisamos re-executar tudo?
# - como saber o que está desatualizado?
# - como evitar re-raspar sem necessidade?
#
# targets resolve isso com um grafo de dependências.
#
# =========================================================
# 2. FUNÇÕES DO PIPELINE
# =========================================================
#
# Antes de criar o _targets.R, definimos as funções.
# Cada etapa é uma função pura.
#
# =========================================================

library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)

# --- Função de coleta ---
coletar_noticias_ab <- function(paginas = 0:4) {

  map_dfr(paginas, function(p) {

    url <- paste0(
      "https://agenciabrasil.ebc.com.br/internacional?page=", p
    )

    cat(sprintf("[p%d] Coletando...\n", p))
    Sys.sleep(1.5)

    pagina <- tryCatch(
      read_html(url),
      error = function(e) return(NULL)
    )

    if (is.null(pagina)) return(tibble())

    titulos <- pagina |>
      html_elements("h2") |>
      html_text2() |>
      str_squish()

    links <- pagina |>
      html_elements("h2 a") |>
      html_attr("href")

    n <- min(length(titulos), length(links))
    if (n == 0) return(tibble())

    tibble(
      titulo      = titulos[1:n],
      link        = links[1:n],
      pagina      = p,
      portal      = "Agência Brasil",
      coletado_em = Sys.time()
    )
  })
}

# --- Função de limpeza ---
limpar_corpus <- function(df) {
  df |>
    mutate(titulo = str_squish(titulo)) |>
    filter(nchar(titulo) > 20) |>
    distinct(link, .keep_all = TRUE) |>
    mutate(
      n_words = str_count(titulo, "\\S+"),
      data_coleta = as.Date(coletado_em)
    )
}

# --- Função de análise ---
calcular_frequencia <- function(df, n_top = 20) {
  df |>
    pull(titulo) |>
    str_to_lower() |>
    str_split("\\s+") |>
    unlist() |>
    str_remove_all("[[:punct:]]") |>
    tibble(palavra = _) |>
    filter(nchar(palavra) > 4) |>
    count(palavra, sort = TRUE) |>
    slice_head(n = n_top)
}

# --- Função de visualização ---
plotar_frequencia <- function(df_freq, caminho = "outputs/figures/frequencia.png") {

  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)

  p <- df_freq |>
    slice_head(n = 15) |>
    ggplot(aes(x = reorder(palavra, n), y = n)) +
    geom_col(fill = "#003366") +
    coord_flip() +
    labs(
      title   = "Palavras mais frequentes",
      x       = "Palavra",
      y       = "Frequência",
      caption = "Fonte: Agência Brasil"
    ) +
    theme_minimal()

  ggsave(caminho, p, width = 8, height = 5)
  caminho  # retornar caminho para targets rastrear
}

# =========================================================
# 3. O ARQUIVO _targets.R
# =========================================================
#
# Este arquivo deve existir na raiz do projeto.
# Crie com: file.create("_targets.R")
#
# =========================================================

# Conteúdo do _targets.R:
targets_conteudo <- '
library(targets)
library(tarchetypes)
library(tidyverse)
library(rvest)

# Carregar funções
source("scripts/utils/scraper.R")
source("scripts/utils/limpeza.R")
source("scripts/utils/analise.R")

# Opções globais
tar_option_set(
  packages = c("rvest", "tidyverse", "lubridate", "stringr"),
  format   = "rds"
)

# Definição do pipeline
list(

  # ETAPA 1: Coleta
  tar_target(
    name    = dados_brutos,
    command = coletar_noticias_ab(paginas = 0:4)
  ),

  # ETAPA 2: Limpeza
  tar_target(
    name    = corpus_limpo,
    command = limpar_corpus(dados_brutos)
  ),

  # ETAPA 3: Análise de frequência
  tar_target(
    name    = frequencia_palavras,
    command = calcular_frequencia(corpus_limpo)
  ),

  # ETAPA 4: Visualização (salva arquivo)
  tar_target(
    name    = grafico_frequencia,
    command = plotar_frequencia(frequencia_palavras),
    format  = "file"
  )
)
'

# Escrever o arquivo
writeLines(targets_conteudo, "_targets.R")
cat("_targets.R criado.\n")

# =========================================================
# 4. EXECUTAR O PIPELINE
# =========================================================

library(targets)

# Ver o que será executado
tar_manifest()

# Visualizar o grafo de dependências
# tar_visnetwork()

# Executar o pipeline
tar_make()

# =========================================================
# 5. ACESSAR RESULTADOS CACHEADOS
# =========================================================

# Ler target específico
corpus <- tar_read(corpus_limpo)
head(corpus)

# Carregar múltiplos targets
tar_load(c(corpus_limpo, frequencia_palavras))

# Ver status
tar_progress()

# =========================================================
# 6. INVALIDAR E RE-EXECUTAR PARCIALMENTE
# =========================================================

# Modificar apenas a função de limpeza
# → targets detecta mudança e re-executa apenas
#   corpus_limpo, frequencia_palavras e grafico_frequencia
# → dados_brutos NÃO é re-executado (scraping economizado!)

# Para forçar re-execução de um target:
# tar_invalidate("corpus_limpo")
# tar_make()

# =========================================================
# 7. MONITORAR MUDANÇAS EM URLS
# =========================================================
#
# format = "url" faz o targets checar o conteúdo da URL.
# Se mudar, re-executa os targets dependentes.
#
# =========================================================

targets_com_url <- '
library(targets)
library(tarchetypes)

list(

  # Monitorar a URL
  tar_url(
    name = url_agencia,
    command = "https://agenciabrasil.ebc.com.br/internacional"
  ),

  # Só re-coleta se a URL mudar
  tar_target(
    name    = dados_brutos,
    command = coletar_noticias_ab(url_agencia),
    deps    = url_agencia
  )
)
'

# =========================================================
# 8. PIPELINE COM MÚLTIPLOS PORTAIS
# =========================================================

# Funções adicionais
coletar_onu <- function(paginas = 0:2) {

  map_dfr(paginas, function(p) {

    url <- paste0("https://brasil.un.org/pt-br/news?page=", p)

    Sys.sleep(2)

    pagina <- tryCatch(read_html(url), error = function(e) NULL)
    if (is.null(pagina)) return(tibble())

    tibble(
      titulo = pagina |>
        html_elements("h3") |>
        html_text2() |>
        str_squish(),
      portal = "ONU Brasil"
    )
  })
}

integrar_portais <- function(df1, df2) {
  bind_rows(df1, df2) |>
    filter(nchar(titulo) > 20) |>
    distinct(titulo, .keep_all = TRUE)
}

# _targets.R com múltiplos portais:
targets_multi <- '
library(targets)

source("scripts/utils/scraper.R")

list(
  tar_target(agencia_brasil, coletar_noticias_ab(0:4)),
  tar_target(onu_brasil,     coletar_onu(0:2)),
  tar_target(corpus_integrado, integrar_portais(agencia_brasil, onu_brasil)),
  tar_target(corpus_limpo,   limpar_corpus(corpus_integrado)),
  tar_target(frequencia,     calcular_frequencia(corpus_limpo)),
  tar_target(grafico,        plotar_frequencia(frequencia), format = "file")
)
'

# =========================================================
# 9. PARALELISMO COM CREW
# =========================================================
#
# Para pipelines com muitos targets independentes,
# o pacote crew permite execução paralela.
#
# install.packages("crew")
#
# =========================================================

targets_paralelo <- '
library(targets)
library(crew)

tar_option_set(
  controller = crew_controller_local(workers = 4)
)

list(
  tar_target(urls, paste0("https://portal.com?page=", 0:9)),

  tar_target(
    paginas,
    raspar_pagina(urls),
    pattern = map(urls)  # um worker por URL
  ),

  tar_target(corpus, bind_rows(paginas))
)
'

# =========================================================
# 10. INTEGRAÇÃO COM QUARTO
# =========================================================
#
# Relatórios Quarto como targets finais.
# O relatório re-renderiza automaticamente quando
# qualquer target que usa muda.
#
# install.packages("tarchetypes")
#
# =========================================================

targets_quarto <- '
library(targets)
library(tarchetypes)

list(
  tar_target(corpus_limpo, limpar_corpus(dados_brutos)),
  tar_target(frequencia,   calcular_frequencia(corpus_limpo)),

  # Relatório Quarto como target
  tar_quarto(
    name = relatorio,
    path = "docs/relatorio_final.qmd"
  )
)
'

# O arquivo relatorio_final.qmd usa tar_read() para acessar targets:
# ```{r}
# library(targets)
# tar_load(frequencia)
# ```

# =========================================================
# 11. PIPELINE COMPLETO DEMONSTRADO SEM targets
# =========================================================
#
# Para entender a lógica antes de usar targets,
# execute manualmente as etapas:
#
# =========================================================

cat("=== Etapa 1: Coleta ===\n")
dados_brutos_manual <- coletar_noticias_ab(paginas = 0:2)
cat(nrow(dados_brutos_manual), "observações coletadas.\n\n")

cat("=== Etapa 2: Limpeza ===\n")
corpus_manual <- limpar_corpus(dados_brutos_manual)
cat(nrow(corpus_manual), "observações após limpeza.\n\n")

cat("=== Etapa 3: Frequência ===\n")
freq_manual <- calcular_frequencia(corpus_manual)
print(head(freq_manual, 10))

cat("=== Etapa 4: Visualização ===\n")
plotar_frequencia(freq_manual, "outputs/figures/freq_manual.png")

# =========================================================
# 12. EXERCÍCIO
# =========================================================
#
# Crie um pipeline completo com targets:
#
# 1. Defina as funções em scripts/utils/;
# 2. Crie _targets.R com pelo menos 4 etapas;
# 3. Execute tar_make();
# 4. Leia um resultado com tar_read();
# 5. Modifique a função de limpeza;
# 6. Execute tar_make() novamente;
# 7. Observe quais targets re-executaram.
#
# Bônus: adicione tar_url() para monitorar o portal.
#
# =========================================================
# 13. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - o problema da re-execução manual de pipelines;
# - o conceito de pipeline declarativo;
# - criar _targets.R com tar_target();
# - executar com tar_make();
# - usar caching (targets só re-executa o necessário);
# - acessar resultados com tar_read() e tar_load();
# - invalidar targets para forçar re-execução;
# - monitorar mudanças em URLs com format = "url";
# - usar paralelismo com crew;
# - integrar com Quarto via tar_quarto().
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 13: Introdução ao NLP com quanteda
#
# - tokenização;
# - document-feature matrix (DFM);
# - análise de frequência;
# - nuvem de palavras;
# - keyness e comparação de corpora;
# - aplicações em política externa e mídia.
#
# =========================================================
# FIM DA AULA
# =========================================================
