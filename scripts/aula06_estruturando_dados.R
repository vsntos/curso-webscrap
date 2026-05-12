# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 6: Estruturando Dados
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - transformar listas em dataframes;
# - limpar texto com stringr;
# - normalizar datas com lubridate;
# - filtrar ruído e duplicatas;
# - enriquecer datasets com variáveis derivadas;
# - construir um dataset de 100+ observações.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Como construímos um corpus de notícias internacionais
# limpo, tipado e pronto para análise?
#
# =========================================================
# 1. PACOTES
# =========================================================

library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)

# =========================================================
# 2. O PROBLEMA DAS LISTAS
# =========================================================
#
# Quando raspamos múltiplas páginas,
# cada chamada retorna um tibble.
#
# Precisamos combinar todos em um único dataframe.
#
# =========================================================

# Simulando scraping de 3 páginas
pagina1 <- tibble(
  titulo = c("ONU discute reforma", "BRICS avança em moeda"),
  link   = c("/a1", "/a2"),
  data   = c("2026-05-12", "2026-05-11")
)

pagina2 <- tibble(
  titulo = c("OTAN amplia presença", "China supera EUA"),
  link   = c("/b1", "/b2"),
  data   = c("2026-05-10", "2026-05-09")
)

pagina3 <- tibble(
  titulo = c("Irã rejeita acordo nuclear", "Brasil lidera COP30"),
  link   = c("/c1", "/c2"),
  data   = c("2026-05-08", "2026-05-07")
)

lista_paginas <- list(pagina1, pagina2, pagina3)

# =========================================================
# 3. COMBINANDO LISTAS COM bind_rows()
# =========================================================

df_combinado <- bind_rows(lista_paginas)

df_combinado

# =========================================================
# 4. LIMPEZA DE TEXTO COM stringr
# =========================================================

textos_sujos <- c(
  "  \n Diplomacia brasileira \t ",
  "ONU debate crise\n\n",
  "China supera    EUA em exportações   "
)

# str_squish: colapsa espaços internos e remove bordas
str_squish(textos_sujos)

# str_trim: apenas remove bordas
str_trim(textos_sujos)

# str_to_lower / str_to_upper / str_to_title
str_to_title("acordos multilaterais em genebra")

# str_remove: remove PRIMEIRA ocorrência de padrão
str_remove("Exclusivo: Brasil firma acordo", "^[A-Za-záéíóú]+: ")

# str_remove_all: remove TODAS as ocorrências
str_remove_all("R$1.500,00 em exportações", "[R$\\.,]")

# str_replace: substitui PRIMEIRA ocorrência
str_replace("Trump | Biden", " \\| ", " e ")

# =========================================================
# 5. EXPRESSÕES REGULARES ÚTEIS EM RI
# =========================================================

titulos <- c(
  "1. Acordo firmado em Genebra em 2015",
  "Trump ameaça sair da OTAN novamente",
  "Brasil exportou US$ 45 bi em 2025",
  "Secretário-Geral da ONU visita Kiev"
)

# Detectar menções a organizações
str_detect(titulos, "ONU|OTAN|BRICS|FMI|OMC")

# Extrair anos
str_extract_all(titulos, "\\d{4}")

# Extrair valores monetários
str_extract(titulos, "US\\$\\s*[\\d,.]+\\s*\\w+")

# Remover numeração inicial
str_remove(titulos, "^\\d+\\.\\s*")

# =========================================================
# 6. NORMALIZAÇÃO DE DATAS
# =========================================================
#
# Datas chegam em formatos diferentes dependendo do site:
#
# - "12 mai 2026"     → dmy() com locale PT
# - "2026-05-12"      → ymd()
# - "May 12, 2026"    → mdy()
# - "12/05/2026"      → dmy()
#
# =========================================================

# ISO: funciona direto
ymd("2026-05-12")

# Dia-mês-ano
dmy("12/05/2026")

# Mês-dia-ano (padrão americano)
mdy("May 12, 2026")

# Com horário
ymd_hms("2026-05-12 14:30:00")

# Formato misto com parse_date_time
datas_brutas <- c("12 mai 2026", "11 mai 2026", "10 mai 2026")

# Ajustar locale para português
old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

datas_parsed <- dmy(datas_brutas)

Sys.setlocale("LC_TIME", old_locale)

datas_parsed

# =========================================================
# 7. CONVERTENDO TIPOS APÓS SCRAPING
# =========================================================

df_bruto <- tibble(
  titulo     = c("Notícia A", "Notícia B", "Notícia C"),
  data_texto = c("2026-05-12", "2026-05-11", "2026-05-10"),
  n_comentarios = c("45", "120", "7"),
  destaque   = c("sim", "não", "sim")
)

df_tipado <- df_bruto |>
  mutate(
    data          = ymd(data_texto),
    n_comentarios = as.integer(n_comentarios),
    destaque      = destaque == "sim"
  ) |>
  select(-data_texto)

glimpse(df_tipado)

# =========================================================
# 8. DEDUPLICAR
# =========================================================

df_com_dup <- tibble(
  titulo = c(
    "ONU aprova resolução",
    "ONU aprova resolução",
    "OTAN amplia presença",
    "Brasil lidera COP30"
  ),
  link = c("/a1", "/a1", "/b1", "/c1")
)

# Deduplica pelo link (mais confiável)
df_dedup <- distinct(df_com_dup, link, .keep_all = TRUE)

df_dedup

# =========================================================
# 9. FILTRAR RUÍDO
# =========================================================
#
# Páginas contêm elementos que não são notícias:
#
# - itens de menu;
# - botões ("Leia mais", "Compartilhar");
# - textos de seção ("Internacional", "Política");
# - strings muito curtas.
#
# =========================================================

df_sujo <- tibble(
  titulo = c(
    "Internacional",
    "ONU aprova resolução histórica sobre clima",
    "Leia mais",
    "China e EUA debatem tarifas em Genebra",
    "Compartilhar",
    "Veja também",
    "Brasil assume presidência do G20"
  )
)

stopwords_ui <- c(
  "Internacional", "Nacional", "Política", "Economia",
  "Leia mais", "Saiba mais", "Compartilhar", "Veja também",
  "Voltar", "Menu", "Topo", "Buscar"
)

df_limpo <- df_sujo |>
  filter(
    nchar(titulo) > 20,
    !str_trim(titulo) %in% stopwords_ui
  )

df_limpo

# =========================================================
# 10. ENRIQUECER COM VARIÁVEIS DERIVADAS
# =========================================================

df_base <- tibble(
  titulo = c(
    "ONU discute reforma do Conselho de Segurança",
    "BRICS avança em alternativa ao dólar",
    "COP30 define metas de emissão para 2035",
    "OTAN amplia presença militar no Leste Europeu",
    "Brasil e China assinam acordos comerciais"
  ),
  data = ymd(c(
    "2026-05-12", "2026-05-11", "2026-05-10",
    "2026-05-09", "2026-05-08"
  ))
)

df_enriquecido <- df_base |>
  mutate(

    # Componentes de data
    ano     = year(data),
    mes     = month(data),
    mes_lab = month(data, label = TRUE, abbr = FALSE),
    dia_sem = wday(data, label = TRUE, abbr = FALSE),

    # Métricas de texto
    n_chars = nchar(titulo),
    n_words = str_count(titulo, "\\S+"),

    # Menção a atores internacionais
    menciona_onu    = str_detect(titulo, regex("ONU|Nações Unidas", ignore_case = TRUE)),
    menciona_otan   = str_detect(titulo, regex("OTAN|NATO", ignore_case = TRUE)),
    menciona_brics  = str_detect(titulo, regex("BRICS", ignore_case = TRUE)),
    menciona_brasil = str_detect(titulo, regex("Brasil", ignore_case = TRUE)),
    menciona_china  = str_detect(titulo, regex("China", ignore_case = TRUE)),

    # Extrato de ano mencionado
    ano_mencionado = str_extract(titulo, "\\d{4}")
  )

glimpse(df_enriquecido)

df_enriquecido |>
  select(titulo, n_words, menciona_onu, menciona_brics)

# =========================================================
# 11. CONSTRUINDO DATASET DE 100+ OBSERVAÇÕES
# =========================================================
#
# Estratégia: raspar múltiplas páginas de um portal.
#
# =========================================================

raspar_agencia_brasil <- function(pagina_num) {

  url <- paste0(
    "https://agenciabrasil.ebc.com.br/internacional?page=",
    pagina_num
  )

  Sys.sleep(1.5)

  pagina <- tryCatch(
    read_html(url),
    error = function(e) {
      message("Erro na página ", pagina_num)
      return(NULL)
    }
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
    pagina_num  = pagina_num,
    coletado_em = Sys.time()
  )
}

# Raspar páginas 0 a 9 (10 páginas)
df_100 <- purrr::map_dfr(0:9, raspar_agencia_brasil)

nrow(df_100)

# =========================================================
# 12. PIPELINE COMPLETO DE LIMPEZA
# =========================================================

df_final <- df_100 |>

  # Limpar texto
  mutate(titulo = str_squish(titulo)) |>

  # Filtrar ruído
  filter(
    nchar(titulo) > 20,
    !titulo %in% c("Internacional", "Leia mais", "Compartilhar")
  ) |>

  # Deduplicar
  distinct(link, .keep_all = TRUE) |>

  # Enriquecer
  mutate(
    n_words         = str_count(titulo, "\\S+"),
    menciona_onu    = str_detect(titulo, regex("ONU|Nações Unidas", ignore_case = TRUE)),
    menciona_brasil = str_detect(titulo, regex("Brasil", ignore_case = TRUE))
  )

glimpse(df_final)
nrow(df_final)

# =========================================================
# 13. ANÁLISE EXPLORATÓRIA DO DATASET
# =========================================================

# Distribuição do tamanho dos títulos
summary(df_final$n_words)

# Quantas mencionam ONU?
df_final |>
  count(menciona_onu) |>
  mutate(pct = round(n / sum(n) * 100, 1))

# Quantas mencionam Brasil?
df_final |>
  count(menciona_brasil) |>
  mutate(pct = round(n / sum(n) * 100, 1))

# Histograma do número de palavras
ggplot(df_final, aes(x = n_words)) +
  geom_histogram(bins = 15, fill = "#003366") +
  labs(
    title   = "Distribuição do tamanho dos títulos",
    x       = "Número de palavras",
    y       = "Frequência",
    caption = "Fonte: Agência Brasil"
  ) +
  theme_minimal()

# =========================================================
# 14. EXPORTAR
# =========================================================

# Criar pasta se não existir
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/processed")) dir.create("data/processed")

write_csv(
  df_final,
  "data/processed/noticias_internacionais.csv"
)

message("Exportado: ", nrow(df_final), " observações.")

# =========================================================
# 15. EXERCÍCIO
# =========================================================
#
# Construa um dataset de 100+ observações com:
#
# 1. Coleta de pelo menos 5 páginas de um portal;
# 2. Limpeza de texto (str_squish);
# 3. Filtro de ruído (nchar > 20);
# 4. Deduplicação (distinct por link);
# 5. Variáveis derivadas:
#    - n_words;
#    - menciona_[ator] (3 atores à sua escolha);
# 6. Exportação para CSV;
# 7. Análise de frequência por ator.
#
# =========================================================
# 16. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - combinar listas com bind_rows() e map_dfr();
# - limpar texto com str_squish, str_remove, str_detect;
# - usar expressões regulares para extrair padrões;
# - normalizar datas com lubridate;
# - converter tipos (as.integer, as.Date, lógico);
# - deduplicar com distinct();
# - filtrar ruído de scraping;
# - enriquecer com variáveis derivadas;
# - construir um dataset de 100+ observações;
# - exportar para CSV.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 7: Paginação e Loops
#
# - como navegar entre páginas automaticamente;
# - loops for e while;
# - purrr como alternativa moderna;
# - scraping em escala;
# - monitoramento de progresso.
#
# =========================================================
# FIM DA AULA
# =========================================================
