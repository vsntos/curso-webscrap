# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 15: Desenvolvimento do Projeto Final
# =========================================================
#
# OBJETIVO
#
# Este script serve como TEMPLATE para o projeto final.
#
# Ele demonstra:
#
# - como organizar todas as etapas do projeto;
# - como documentar metodologia de coleta;
# - como construir pipeline completo e reproduzível;
# - como produzir análises conectadas à teoria de RI;
# - como gerar outputs para o relatório final.
#
# Adapte cada seção ao seu tema e fontes específicas.
#
# =========================================================
# CONFIGURAÇÃO DO PROJETO
# =========================================================
#
# Preencha antes de começar:
#
PERGUNTA_PESQUISA <- "Como portais brasileiros e internacionais
                      enquadram conflitos internacionais?"

FONTES <- c(
  "Agência Brasil (agenciabrasil.ebc.com.br)",
  "ONU Brasil (brasil.un.org)"
)

PERIODO_INICIO <- as.Date("2025-01-01")
PERIODO_FIM    <- as.Date("2026-05-12")

PESQUISADOR <- "Vinicius Santos"
DATA_COLETA <- Sys.Date()

# =========================================================
# 1. PACOTES
# =========================================================

library(rvest)
library(httr2)
library(xml2)
library(jsonlite)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyverse)
library(lubridate)
library(stringr)
library(fs)

# =========================================================
# 2. ESTRUTURA DE PASTAS
# =========================================================

# Criar estrutura padrão (se não existir)
pastas <- c(
  "data/raw",
  "data/processed",
  "scripts/utils",
  "outputs/figures",
  "outputs/tables",
  "docs",
  "logs"
)

walk(pastas, ~dir_create(.x, recurse = TRUE))
cat("Estrutura de pastas verificada.\n")

# =========================================================
# 3. INICIAR LOG DA SESSÃO
# =========================================================

log_file <- paste0(
  "logs/projeto_",
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  ".log"
)

log <- function(nivel = "INFO", msg) {
  linha <- sprintf("[%s] [%s] %s\n",
                   format(Sys.time(), "%H:%M:%S"), nivel, msg)
  cat(linha)
  cat(linha, file = log_file, append = TRUE)
}

log("INFO", "Iniciando projeto final")
log("INFO", paste("Pergunta:", str_squish(PERGUNTA_PESQUISA)))

# =========================================================
# 4. FUNÇÕES UTILITÁRIAS
# =========================================================

# Raspar portal com tratamento completo
raspar_portal <- function(
    url_base,
    portal_nome,
    n_paginas      = 10,
    seletor_titulo = "h2",
    seletor_link   = "h2 a",
    delay          = 1.5
) {

  log("INFO", sprintf("Iniciando coleta: %s (%d páginas)", portal_nome, n_paginas))

  resultados <- map_dfr(0:(n_paginas - 1), function(p) {

    url <- paste0(url_base, p)

    Sys.sleep(runif(1, delay, delay * 1.5))

    pagina <- tryCatch(
      read_html(url),
      error = function(e) {
        log("ERRO", sprintf("Página %d de %s: %s", p, portal_nome, e$message))
        return(NULL)
      }
    )

    if (is.null(pagina)) return(tibble())

    titulos <- tryCatch(
      pagina |> html_elements(seletor_titulo) |> html_text2() |> str_squish(),
      error = function(e) character(0)
    )

    links <- tryCatch(
      pagina |> html_elements(seletor_link) |> html_attr("href"),
      error = function(e) character(0)
    )

    n <- min(length(titulos), length(links))

    log("INFO", sprintf("  Página %d: %d títulos", p, n))

    if (n == 0) return(tibble())

    tibble(
      titulo      = titulos[1:n],
      link        = links[1:n],
      portal      = portal_nome,
      pagina      = p,
      url_base    = url_base,
      coletado_em = Sys.time()
    )
  })

  log("INFO", sprintf("Coleta de %s concluída: %d observações", portal_nome, nrow(resultados)))
  resultados
}

# Limpar corpus
limpar_corpus <- function(df) {

  n_antes <- nrow(df)

  df_limpo <- df |>
    mutate(titulo = str_squish(titulo)) |>
    filter(
      nchar(titulo) > 20,
      !str_detect(titulo, "^(Internacional|Nacional|Veja também|Leia mais)")
    ) |>
    distinct(link, .keep_all = TRUE) |>
    mutate(
      n_words     = str_count(titulo, "\\S+"),
      data_coleta = as.Date(coletado_em)
    )

  log("INFO", sprintf("Limpeza: %d → %d observações (-%d)",
                      n_antes, nrow(df_limpo), n_antes - nrow(df_limpo)))

  df_limpo
}

# Salvar com nome padronizado
salvar_dados <- function(df, tipo, nome_base) {

  caminho <- file.path(
    paste0("data/", tipo),
    paste0(nome_base, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
  )

  write_csv(df, caminho)
  log("INFO", sprintf("Salvo: %s (%d obs.)", caminho, nrow(df)))
  caminho
}

# =========================================================
# 5. COLETA DE DADOS
# =========================================================

log("INFO", "=== ETAPA 1: COLETA ===")

# Portal 1: Agência Brasil
df_ab <- raspar_portal(
  url_base    = "https://agenciabrasil.ebc.com.br/internacional?page=",
  portal_nome = "Agência Brasil",
  n_paginas   = 5,
  delay       = 1.5
)

# Pausa entre portais
Sys.sleep(3)

# Portal 2: ONU Brasil
df_onu <- raspar_portal(
  url_base       = "https://brasil.un.org/pt-br/news?page=",
  portal_nome    = "ONU Brasil",
  n_paginas      = 3,
  seletor_titulo = "h3",
  seletor_link   = "h3 a",
  delay          = 2.0
)

# Integrar
df_raw <- bind_rows(df_ab, df_onu)

log("INFO", sprintf("Total raw: %d observações de %d portais",
                    nrow(df_raw), n_distinct(df_raw$portal)))

# Salvar raw (nunca sobrescrever)
salvar_dados(df_raw, "raw", "corpus_bruto")

# =========================================================
# 6. LIMPEZA E ESTRUTURAÇÃO
# =========================================================

log("INFO", "=== ETAPA 2: LIMPEZA ===")

df_limpo <- limpar_corpus(df_raw)

# Enriquecer
df_final <- df_limpo |>
  mutate(
    # Variáveis derivadas
    mes         = floor_date(data_coleta, "month"),
    dia_semana  = wday(data_coleta, label = TRUE),

    # Atores mencionados
    menciona_onu    = str_detect(titulo, regex("ONU|Nações Unidas", ignore_case = TRUE)),
    menciona_otan   = str_detect(titulo, regex("OTAN|NATO", ignore_case = TRUE)),
    menciona_brics  = str_detect(titulo, regex("BRICS", ignore_case = TRUE)),
    menciona_china  = str_detect(titulo, regex("China", ignore_case = TRUE)),
    menciona_russia = str_detect(titulo, regex("Rússia|Russia", ignore_case = TRUE)),
    menciona_brasil = str_detect(titulo, regex("Brasil", ignore_case = TRUE)),
    menciona_eua    = str_detect(titulo, regex("EUA|Estados Unidos", ignore_case = TRUE))
  )

# Salvar processed
salvar_dados(df_final, "processed", "corpus_final")

# =========================================================
# 7. ESTATÍSTICAS DESCRITIVAS
# =========================================================

log("INFO", "=== ETAPA 3: ANÁLISE DESCRITIVA ===")

cat("\n=== RESUMO DO CORPUS ===\n")
cat(sprintf("Total de notícias:  %d\n", nrow(df_final)))
cat(sprintf("Portais:            %s\n", paste(unique(df_final$portal), collapse = ", ")))
cat(sprintf("Período:            %s a %s\n",
            min(df_final$data_coleta, na.rm = TRUE),
            max(df_final$data_coleta, na.rm = TRUE)))

# Por portal
df_final |>
  count(portal) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

# Menções por ator
df_final |>
  summarise(
    onu    = sum(menciona_onu,    na.rm = TRUE),
    otan   = sum(menciona_otan,   na.rm = TRUE),
    brics  = sum(menciona_brics,  na.rm = TRUE),
    china  = sum(menciona_china,  na.rm = TRUE),
    russia = sum(menciona_russia, na.rm = TRUE),
    brasil = sum(menciona_brasil, na.rm = TRUE),
    eua    = sum(menciona_eua,    na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "ator", values_to = "mencoes") |>
  arrange(desc(mencoes)) |>
  print()

# =========================================================
# 8. ANÁLISE NLP COM QUANTEDA
# =========================================================

log("INFO", "=== ETAPA 4: NLP ===")

# Criar corpus
corp <- corpus(df_final, text_field = "titulo")

# Tokenizar
toks <- corp |>
  tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
  tokens_remove(c(
    stopwords("pt"),
    "após", "sobre", "entre", "sendo", "também", "ainda",
    "mais", "menos", "além", "desde", "novo", "nova"
  )) |>
  tokens_tolower()

# DFM
dfm_proj <- dfm(toks)

# Frequência
freq <- textstat_frequency(dfm_proj, n = 20)

log("INFO", "Top 10 palavras mais frequentes:")
print(head(freq, 10))

# =========================================================
# 9. VISUALIZAÇÕES
# =========================================================

log("INFO", "=== ETAPA 5: VISUALIZAÇÕES ===")

# Figura 1: Frequência de palavras
fig_freq <- freq |>
  slice_head(n = 15) |>
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_col(fill = "#003366") +
  coord_flip() +
  labs(
    title    = "Palavras mais frequentes no corpus",
    subtitle = str_wrap(PERGUNTA_PESQUISA, 60),
    x        = NULL,
    y        = "Frequência",
    caption  = paste("Fonte:", paste(FONTES, collapse = " | "))
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/figures/fig01_frequencia.png", fig_freq,
       width = 8, height = 5, dpi = 150)
log("INFO", "Figura 1 salva: fig01_frequencia.png")

# Figura 2: Volume por portal
fig_portal <- df_final |>
  count(portal) |>
  ggplot(aes(x = reorder(portal, n), y = n, fill = portal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("#003366", "#CC0000", "#006600")) +
  labs(
    title   = "Notícias coletadas por portal",
    x       = NULL,
    y       = "Número de notícias",
    caption = paste("Coleta em:", format(DATA_COLETA))
  ) +
  theme_minimal()

ggsave("outputs/figures/fig02_portais.png", fig_portal,
       width = 6, height = 4, dpi = 150)
log("INFO", "Figura 2 salva: fig02_portais.png")

# Figura 3: Menções por ator
fig_atores <- df_final |>
  summarise(across(starts_with("menciona_"), sum, na.rm = TRUE)) |>
  pivot_longer(everything(), names_to = "ator", values_to = "n") |>
  mutate(ator = str_remove(ator, "menciona_")) |>
  arrange(desc(n)) |>
  ggplot(aes(x = reorder(ator, n), y = n)) +
  geom_col(fill = "#336699") +
  coord_flip() +
  labs(
    title   = "Menções por ator internacional",
    x       = NULL,
    y       = "Número de notícias",
    caption = paste("Fonte:", paste(FONTES, collapse = " | "))
  ) +
  theme_minimal()

ggsave("outputs/figures/fig03_atores.png", fig_atores,
       width = 7, height = 5, dpi = 150)
log("INFO", "Figura 3 salva: fig03_atores.png")

# =========================================================
# 10. ANÁLISE DE FRAMING (KEYNESS)
# =========================================================

if (n_distinct(df_final$portal) >= 2) {

  dfm_por_portal <- dfm_group(dfm_proj, groups = docvars(corp, "portal"))

  portais <- rownames(dfm_por_portal)
  target  <- portais[1]

  keyness <- textstat_keyness(dfm_por_portal, target = target)

  # Salvar tabela
  write_csv(as_tibble(head(keyness, 30)), "outputs/tables/tab_keyness.csv")
  log("INFO", "Tabela keyness salva: tab_keyness.csv")

  # Figura keyness
  fig_key <- textplot_keyness(keyness, n = 10, color = c("#CC0000", "#003366"))

  ggsave("outputs/figures/fig04_keyness.png", fig_key,
         width = 8, height = 5, dpi = 150)
  log("INFO", "Figura 4 salva: fig04_keyness.png")
}

# =========================================================
# 11. ANÁLISE TEMÁTICA COM DICIONÁRIO
# =========================================================

dict_ri <- dictionary(list(

  multilateralismo = c(
    "onu", "otan", "brics", "g20", "g7", "fmi",
    "multilateral", "conselho", "assembleia", "cúpula", "organização"
  ),

  conflito = c(
    "guerra", "conflito", "sanção", "sanções", "militar",
    "ataque", "bombardeio", "invasão", "tropas", "armamento"
  ),

  economia = c(
    "comércio", "exportação", "importação", "pib", "tarifa",
    "moeda", "parceiro", "bilateral", "mercado", "investimento"
  ),

  clima = c(
    "clima", "climático", "emissão", "carbono", "cop",
    "meta", "ambiental", "floresta", "aquecimento"
  ),

  diplomacia_brasil = c(
    "brasil", "itamaraty", "diplomacia", "lula",
    "presidência", "política externa", "mediação"
  )
))

dfm_temas <- dfm_lookup(dfm_proj, dictionary = dict_ri)

df_temas <- convert(dfm_temas, to = "data.frame") |>
  as_tibble() |>
  bind_cols(select(docvars(corp), portal))

# Temas por portal
tab_temas <- df_temas |>
  pivot_longer(
    cols      = multilateralismo:diplomacia_brasil,
    names_to  = "tema",
    values_to = "n"
  ) |>
  group_by(portal, tema) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(portal, desc(total))

write_csv(tab_temas, "outputs/tables/tab_temas.csv")
log("INFO", "Tabela de temas salva: tab_temas.csv")

# Visualizar
fig_temas <- tab_temas |>
  ggplot(aes(x = reorder(tema, total), y = total, fill = portal)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("#003366", "#CC0000")) +
  labs(
    title   = "Temas de RI por portal",
    x       = NULL,
    y       = "Menções",
    fill    = "Portal",
    caption = paste("Fonte:", paste(FONTES, collapse = " | "))
  ) +
  theme_minimal()

ggsave("outputs/figures/fig05_temas.png", fig_temas,
       width = 8, height = 5, dpi = 150)
log("INFO", "Figura 5 salva: fig05_temas.png")

# =========================================================
# 12. REGISTRAR METADADOS DE COLETA
# =========================================================

metadados <- tibble(
  item  = c(
    "Pergunta de Pesquisa",
    "Fontes",
    "Período de Coleta",
    "Data de Execução",
    "R Version",
    "Observações Raw",
    "Observações Finais",
    "Portais",
    "Script"
  ),
  valor = c(
    str_squish(PERGUNTA_PESQUISA),
    paste(FONTES, collapse = "; "),
    paste(PERIODO_INICIO, "a", PERIODO_FIM),
    format(DATA_COLETA),
    R.version.string,
    as.character(nrow(df_raw)),
    as.character(nrow(df_final)),
    paste(unique(df_final$portal), collapse = "; "),
    "exemplo_aula15.R"
  )
)

write_csv(metadados, "docs/metadados_coleta.csv")
cat("\n=== Metadados de Coleta ===\n")
print(metadados)

# =========================================================
# 13. RELATÓRIO FINAL (TEMPLATE)
# =========================================================
#
# O relatório final deve ser um arquivo Quarto (.qmd)
# com as seguintes seções:
#
# 1. Introdução e Pergunta de Pesquisa
# 2. Enquadramento Teórico
# 3. Metodologia de Coleta
# 4. Análise Descritiva do Corpus
# 5. Análise Temática / Framing
# 6. Resultados e Discussão
# 7. Limitações
# 8. Conclusão
# 9. Referências
# 10. Apêndice: Informações de Reprodutibilidade
#
# Use tar_read() para carregar os targets no Quarto.
#
# =========================================================

# Template de relatório
relatorio_template <- '---
title: "TÍTULO DO SEU PROJETO"
subtitle: "Web Scraping e Dados Digitais em RI"
author: "SEU NOME"
date: today
format:
  html:
    toc: true
    code-fold: true
bibliography: refs.bib
---

## 1. Pergunta de Pesquisa

*Descreva sua pergunta aqui.*

## 2. Enquadramento Teórico

*Como o conceito de framing / agenda-setting / etc.
se aplica ao seu objeto de pesquisa?*

## 3. Metodologia

*Descreva sua estratégia de coleta, fontes,
período, método e critérios de inclusão/exclusão.*

```{r}
metadados <- read_csv("docs/metadados_coleta.csv")
knitr::kable(metadados)
```

## 4. Análise Descritiva

```{r}
df_final <- read_csv("data/processed/corpus_final_YYYYMMDD.csv")
# análise aqui
```

## 5. Análise Temática

```{r}
# Figuras e tabelas aqui
```

## 6. Resultados

*O que os dados mostram?*

## 7. Limitações

*Viés de seleção, validade de medida, etc.*

## 8. Conclusão

*Como isso responde à pergunta? Que pesquisas futuras sugere?*
'

writeLines(relatorio_template, "docs/relatorio_template.qmd")
log("INFO", "Template de relatório criado: docs/relatorio_template.qmd")

# =========================================================
# 14. SUMÁRIO FINAL
# =========================================================

cat("\n")
cat("============================================\n")
cat(" PROJETO FINAL — SUMÁRIO\n")
cat("============================================\n")
cat(sprintf("Pergunta: %s\n", str_squish(PERGUNTA_PESQUISA)))
cat(sprintf("Notícias coletadas:  %d\n", nrow(df_raw)))
cat(sprintf("Após limpeza:        %d\n", nrow(df_final)))
cat(sprintf("Portais:             %d\n", n_distinct(df_final$portal)))
cat(sprintf("Figuras geradas:     5\n"))
cat(sprintf("Tabelas geradas:     2\n"))
cat(sprintf("Log disponível:      %s\n", log_file))
cat("============================================\n")

log("INFO", "Projeto concluído com sucesso.")

# =========================================================
# 15. CHECKLIST ANTES DE ENTREGAR
# =========================================================
#
# □ Pergunta de pesquisa clara e específica?
# □ Fontes verificadas (robots.txt)?
# □ Dataset com 100+ observações?
# □ Raw preservado em data/raw/?
# □ Pipeline reproduzível (todos os scripts numerados)?
# □ README.md explicando como re-executar?
# □ Metadados de coleta documentados?
# □ Análise conectada com teoria de RI?
# □ Limitações discutidas no relatório?
# □ .Renviron no .gitignore?
# □ renv::snapshot() executado?
# □ Relatório renderizado sem erros?
#
# =========================================================
# FIM DO TEMPLATE
# =========================================================
#
# Adapte este template ao seu projeto específico.
# Bom trabalho!
#
# =========================================================
