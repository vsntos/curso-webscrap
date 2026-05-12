# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 14: Aplicações em Relações Internacionais
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - operacionalizar conceitos de RI com dados web;
# - medir framing com keyness;
# - analisar agenda-setting com séries temporais;
# - usar o GDELT Project como fonte de eventos;
# - construir redes de co-menções de atores;
# - integrar análise textual com dados quantitativos;
# - produzir inferências válidas para pesquisa em RI.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Como portais brasileiros e internacionais enquadram
# de forma diferente os conflitos e crises globais?
#
# =========================================================
# 1. PACOTES
# =========================================================

library(rvest)
library(httr2)
library(jsonlite)
library(xml2)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyverse)
library(lubridate)

# =========================================================
# 2. COLETAR CORPUS MULTIPORTAL
# =========================================================

coletar_portal <- function(url, portal_nome, seletor = "h2", n_paginas = 3) {

  map_dfr(0:(n_paginas - 1), function(p) {

    url_p <- if (str_detect(url, "\\?")) {
      paste0(url, "&page=", p)
    } else {
      paste0(url, "?page=", p)
    }

    Sys.sleep(1.5)

    pagina <- tryCatch(read_html(url_p), error = function(e) NULL)
    if (is.null(pagina)) return(tibble())

    titulos <- pagina |>
      html_elements(seletor) |>
      html_text2() |>
      str_squish()

    titulos <- titulos[nchar(titulos) > 20]

    if (length(titulos) == 0) return(tibble())

    tibble(
      titulo      = titulos,
      portal      = portal_nome,
      pagina      = p,
      coletado_em = Sys.time()
    )
  })
}

# Coletar de dois portais
portais <- list(
  list(
    url   = "https://agenciabrasil.ebc.com.br/internacional",
    nome  = "Agência Brasil",
    setor = "h2"
  ),
  list(
    url   = "https://brasil.un.org/pt-br/news",
    nome  = "ONU Brasil",
    setor = "h3"
  )
)

df_corpus <- map_dfr(portais, function(p) {
  cat(sprintf("Coletando: %s\n", p$nome))
  coletar_portal(p$url, p$nome, p$setor, n_paginas = 2)
})

cat(sprintf("Total: %d notícias de %d portais\n",
            nrow(df_corpus), n_distinct(df_corpus$portal)))

# =========================================================
# 3. ANÁLISE DE FRAMING COM KEYNESS
# =========================================================
#
# Framing = como diferentes portais enquadram os mesmos eventos.
#
# Keyness mede quais palavras são estatisticamente mais
# características de um corpus em relação a outro.
#
# =========================================================

corp_multi <- corpus(df_corpus, text_field = "titulo")

toks_multi <- corp_multi |>
  tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
  tokens_remove(c(stopwords("pt"), "após", "sobre", "entre", "sendo"))

dfm_multi <- toks_multi |> dfm()
dfm_portais <- dfm_group(dfm_multi, groups = docvars(corp_multi, "portal"))

# Keyness: Agência Brasil vs ONU Brasil
if (ndoc(dfm_portais) >= 2) {

  key_ab <- textstat_keyness(dfm_portais, target = "Agência Brasil")

  cat("\nPalavras mais características da Agência Brasil:\n")
  print(head(key_ab[key_ab$chi2 > 0, c("feature", "chi2")], 10))

  cat("\nPalavras mais características da ONU Brasil:\n")
  print(head(key_ab[key_ab$chi2 < 0, c("feature", "chi2")], 10))
}

# =========================================================
# 4. AGENDA-SETTING: MONITORAR ATENÇÃO POR TEMA
# =========================================================
#
# Agenda-setting: a mídia define o que é "importante"
# ao alocar espaço para certos temas.
#
# Medimos com a frequência de menções ao longo do tempo.
#
# =========================================================

# Usando Google News RSS para monitorar temas
monitorar_tema_rss <- function(tema, n_dias_atras = 30) {

  url <- paste0(
    "https://news.google.com/rss/search?q=",
    URLencode(tema),
    "&hl=pt-BR&gl=BR&ceid=BR:pt-419"
  )

  rss <- tryCatch(read_xml(url), error = function(e) NULL)
  if (is.null(rss)) return(tibble())

  items <- xml_find_all(rss, "//item")
  if (length(items) == 0) return(tibble())

  lc_orig <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", lc_orig))

  tibble(
    titulo = xml_text(xml_find_first(items, "title")),
    data   = xml_text(xml_find_first(items, "pubDate")),
    tema   = tema
  ) |>
    mutate(
      data = as.POSIXct(
        strptime(sub(" [A-Z]+$", "", data), "%a, %d %b %Y %H:%M:%S", tz = "UTC")
      )
    ) |>
    filter(!is.na(data))
}

# Monitorar múltiplos temas
temas_ri <- c("OTAN Ucrânia", "BRICS", "COP clima", "Oriente Médio")

df_agenda <- map_dfr(temas_ri, function(t) {
  cat(sprintf("Buscando: %s\n", t))
  Sys.sleep(1)
  monitorar_tema_rss(t)
})

# Volume de cobertura por tema
df_agenda |>
  count(tema, sort = TRUE) |>
  ggplot(aes(x = reorder(tema, n), y = n, fill = tema)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title   = "Volume de cobertura por tema",
    x       = NULL,
    y       = "Número de notícias (RSS)",
    caption = "Fonte: Google News RSS"
  ) +
  theme_minimal()

# =========================================================
# 5. GDELT PROJECT: EVENTOS INTERNACIONAIS
# =========================================================
#
# GDELT = Global Database of Events, Language, and Tone
#
# Monitora notícias em 100+ idiomas e codifica eventos
# usando o esquema CAMEO (Conflict and Mediation Event Observations).
#
# API: https://api.gdeltproject.org/api/v2/doc/doc
#
# =========================================================

buscar_gdelt <- function(
    query,
    max_records = 25,
    mode        = "artlist",
    idioma      = NULL
) {

  url_params <- paste0(
    "?query=", URLencode(query),
    "&mode=", mode,
    "&maxrecords=", max_records,
    "&format=json",
    if (!is.null(idioma)) paste0("&sourcelang=", idioma) else ""
  )

  url <- paste0(
    "https://api.gdeltproject.org/api/v2/doc/doc",
    url_params
  )

  resp <- tryCatch(
    request(url) |>
      req_user_agent("Curso Web Scraping em RI / pesquisa academica") |>
      req_timeout(20) |>
      req_perform(),
    error = function(e) {
      message("Erro no GDELT: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) return(tibble())
  if (resp_status(resp) != 200) return(tibble())

  dados <- resp |> resp_body_string() |> fromJSON()

  if (is.null(dados$articles)) return(tibble())

  as_tibble(dados$articles) |>
    select(any_of(c("title", "url", "domain", "language",
                    "seendate", "socialimage", "sourcecountry")))
}

# Busca: cobertura do Brasil em inglês
df_gdelt_en <- buscar_gdelt(
  query       = "Brazil foreign policy diplomacy BRICS",
  max_records = 20
)

cat(sprintf("GDELT (inglês): %d artigos\n", nrow(df_gdelt_en)))
head(df_gdelt_en, 5)

# Busca: cobertura em português
df_gdelt_pt <- buscar_gdelt(
  query       = "Brasil diplomacia política externa",
  max_records = 20,
  idioma      = "portuguese"
)

cat(sprintf("GDELT (português): %d artigos\n", nrow(df_gdelt_pt)))

# =========================================================
# 6. ANÁLISE DE TOM (GDELT TONE)
# =========================================================
#
# GDELT calcula tom (positivo/negativo) de artigos.
# Modo "tonechart" retorna série temporal de tom.
#
# =========================================================

buscar_tom_gdelt <- function(query, timespan = "1m") {

  url <- paste0(
    "https://api.gdeltproject.org/api/v2/doc/doc",
    "?query=", URLencode(query),
    "&mode=tonechart",
    "&timespan=", timespan,
    "&format=json"
  )

  resp <- tryCatch(
    request(url) |>
      req_user_agent("Curso RI") |>
      req_timeout(20) |>
      req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp)) return(tibble())

  dados <- resp |> resp_body_string() |> fromJSON()
  as_tibble(dados$tonechart)
}

tom_brasil <- buscar_tom_gdelt("Brazil diplomacy")

if (nrow(tom_brasil) > 0) {
  cat("Tom médio da cobertura sobre o Brasil:\n")
  print(head(tom_brasil))
}

# =========================================================
# 7. REDE DE CO-MENÇÕES DE ATORES
# =========================================================
#
# Modelar quais atores são mencionados juntos
# captura estrutura de alinhamentos e conflitos.
#
# =========================================================

atores_ri <- c(
  "ONU", "OTAN", "NATO", "BRICS", "EUA", "China",
  "Rússia", "Brasil", "Irã", "Ucrânia", "G20", "FMI"
)

# Corpus para análise de rede
corpus_rede <- if (nrow(df_corpus) > 0) {
  df_corpus$titulo
} else {
  c(
    "EUA e OTAN discutem apoio à Ucrânia no G20",
    "China e Rússia fortalecem parceria estratégica no BRICS",
    "Brasil e China assinam acordo de comércio no G20",
    "OTAN e EUA pressionam aliados sobre Rússia",
    "Rússia e Irã coordenam posição na ONU",
    "ONU cobra Brasil sobre desmatamento na Amazônia",
    "FMI e BRICS debatem reforma do sistema financeiro",
    "Ucrânia e OTAN planejam nova estratégia de defesa"
  )
}

# Extrair co-menções
extrair_co_mencoes <- function(titulos, atores) {

  map_dfr(titulos, function(t) {

    presentes <- atores[str_detect(t, fixed(atores, ignore_case = TRUE))]

    if (length(presentes) < 2) return(tibble())

    combn(presentes, 2, simplify = FALSE) |>
      map_dfr(~tibble(ator1 = .x[1], ator2 = .x[2], titulo = t))
  })
}

df_mencoes <- extrair_co_mencoes(corpus_rede, atores_ri)

cat(sprintf("Co-menções encontradas: %d pares\n", nrow(df_mencoes)))
print(df_mencoes)

# Frequência de co-menções
df_mencoes |>
  count(ator1, ator2, sort = TRUE) |>
  head(10)

# =========================================================
# 8. VISUALIZAR REDE COM IGRAPH
# =========================================================

if (nrow(df_mencoes) > 0 && requireNamespace("igraph", quietly = TRUE)) {

  library(igraph)

  arestas <- df_mencoes |>
    count(ator1, ator2, name = "peso")

  g <- graph_from_data_frame(arestas, directed = FALSE)

  E(g)$weight <- arestas$peso
  V(g)$grau   <- degree(g)

  plot(
    g,
    vertex.size       = V(g)$grau * 5,
    vertex.color      = "#003366",
    vertex.label.cex  = 0.8,
    vertex.label.color = "white",
    edge.width        = E(g)$weight * 2,
    edge.color        = "#999999",
    main              = "Rede de co-menções entre atores internacionais"
  )
}

# =========================================================
# 9. FRAMING DE CRISES: DICIONÁRIO MULTIFRAME
# =========================================================

frames_crise <- dictionary(list(

  humanitario = c(
    "civis", "refugiados", "vítimas", "ajuda humanitária",
    "deslocados", "fome", "crise humanitária", "sofrimento"
  ),

  geopolitico = c(
    "soberania", "segurança nacional", "aliança",
    "deterrência", "expansão", "hegemonia", "esfera de influência"
  ),

  diplomatico = c(
    "negociação", "cessar-fogo", "mediação", "acordo",
    "diálogo", "cúpula", "proposta", "resolução"
  ),

  economico = c(
    "sanção", "comércio", "energia", "petróleo",
    "exportação", "mercado", "inflação", "crise econômica"
  )
))

# Aplicar ao corpus
if (nrow(df_corpus) > 0) {

  corp_frames <- corpus(df_corpus, text_field = "titulo")

  dfm_frames <- corp_frames |>
    tokens(remove_punct = TRUE) |>
    tokens_remove(stopwords("pt")) |>
    dfm() |>
    dfm_lookup(dictionary = frames_crise)

  df_frames <- convert(dfm_frames, to = "data.frame") |>
    as_tibble() |>
    bind_cols(docvars(corp_frames))

  # Frame dominante por portal
  df_frames |>
    pivot_longer(
      cols      = humanitario:economico,
      names_to  = "frame",
      values_to = "n"
    ) |>
    group_by(portal, frame) |>
    summarise(total = sum(n), .groups = "drop") |>
    arrange(portal, desc(total)) |>
    print()
}

# =========================================================
# 10. ANÁLISE LONGITUDINAL DE TOM
# =========================================================

# Simular série temporal de tom por semana
set.seed(42)

df_tom_simulado <- tibble(
  semana = seq.Date(as.Date("2025-10-01"), by = "week", length.out = 20),
  tom_agencia = rnorm(20, mean = -0.5, sd = 1.5),
  tom_bbc     = rnorm(20, mean = -1.2, sd = 1.2)
)

df_tom_long <- df_tom_simulado |>
  pivot_longer(
    cols      = starts_with("tom_"),
    names_to  = "portal",
    values_to = "tom",
    names_prefix = "tom_"
  )

ggplot(df_tom_long, aes(x = semana, y = tom, color = portal)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title    = "Tom da cobertura ao longo do tempo",
    subtitle = "Positivo > 0 | Negativo < 0",
    x        = "Semana",
    y        = "Tom médio",
    color    = "Portal",
    caption  = "Simulação para fins didáticos"
  ) +
  theme_minimal()

# =========================================================
# 11. INTEGRAR COM DADOS ECONÔMICOS
# =========================================================
#
# Cruzar cobertura midiática com indicadores do Banco Mundial.
#
# Hipótese: maior peso econômico → mais cobertura?
#
# =========================================================

buscar_pib <- function(pais, ano_inicio = 2015) {

  url <- paste0(
    "https://api.worldbank.org/v2/country/",
    pais,
    "/indicator/NY.GDP.MKTP.CD?format=json&per_page=100"
  )

  resp <- tryCatch(
    request(url) |> req_user_agent("Curso RI") |> req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp)) return(tibble())

  dados <- resp |> resp_body_string() |> fromJSON()

  if (length(dados) < 2 || is.null(dados[[2]])) return(tibble())

  dados[[2]] |>
    filter(!is.na(value)) |>
    mutate(
      ano  = as.integer(date),
      pib  = value / 1e12,
      pais = pais
    ) |>
    filter(ano >= ano_inicio) |>
    select(pais, ano, pib)
}

paises_brics <- c("BRA", "CHN", "IND", "RUS", "ZAF")

df_pib <- map_dfr(paises_brics, function(p) {
  Sys.sleep(0.5)
  buscar_pib(p)
})

# Correlação entre PIB e menções no corpus
mencoes_pais <- df_corpus |>
  mutate(
    china  = str_detect(titulo, regex("china", ignore_case = TRUE)),
    russia = str_detect(titulo, regex("rússia|russia", ignore_case = TRUE)),
    india  = str_detect(titulo, regex("índia|india", ignore_case = TRUE)),
    brasil = str_detect(titulo, regex("brasil", ignore_case = TRUE))
  ) |>
  summarise(across(china:brasil, sum)) |>
  pivot_longer(everything(), names_to = "pais_lower", values_to = "mencoes")

cat("Menções no corpus por país:\n")
print(mencoes_pais)

# =========================================================
# 12. EXERCÍCIO
# =========================================================
#
# Analise o framing de uma crise internacional recente.
#
# 1. Colete 30+ notícias sobre um conflito atual
#    de dois portais diferentes;
#
# 2. Crie corpus com metadados de portal e data;
#
# 3. Calcule keyness — o que diferencia cada portal?
#
# 4. Crie dicionário com 3 frames relevantes para a crise;
#
# 5. Aplique dfm_lookup() e compare frames por portal;
#
# 6. Extraia co-menções dos principais atores;
#
# 7. Responda:
#    - Qual portal enfatiza mais o frame humanitário?
#    - Qual enfatiza mais o frame geopolítico?
#    - O Brasil aparece mais em que tipo de contexto?
#
# =========================================================
# 13. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - operacionalizar framing com keyness;
# - medir agenda-setting com série temporal de menções;
# - usar o GDELT Project como fonte de eventos;
# - analisar tom de cobertura com GDELT Tone;
# - construir redes de co-menções de atores;
# - criar dicionários de frames para análise crítica;
# - integrar análise textual com dados econômicos;
# - produzir inferências teóricas a partir de dados web.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 15: Desenvolvimento do Projeto Final
#
# - construir dataset original completo;
# - documentar metodologia de coleta;
# - aplicar pipeline targets completo;
# - apresentar resultados iniciais;
# - preparar entregável final.
#
# =========================================================
# FIM DA AULA
# =========================================================
