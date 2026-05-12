# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 9: JavaScript e Conteúdo Dinâmico
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - identificar quando rvest não é suficiente;
# - usar DevTools para inspecionar APIs ocultas;
# - replicar chamadas AJAX com httr2;
# - usar headers para autenticar requisições;
# - usar chromote para renderização headless;
# - trabalhar com dados JSON de APIs internas.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Como coletamos dados de portais que dependem
# de JavaScript para renderizar o conteúdo?
#
# =========================================================
# 1. PACOTES
# =========================================================

library(rvest)
library(httr2)
library(jsonlite)
library(tidyverse)

# =========================================================
# 2. DIAGNÓSTICO: ESTÁTICO OU DINÂMICO?
# =========================================================
#
# Antes de escolher a estratégia, diagnostique o site.
#
# =========================================================

diagnosticar_site <- function(url, seletor_esperado = "article, h2, h3") {

  cat(sprintf("Diagnosticando: %s\n\n", url))

  pagina <- tryCatch(
    {
      Sys.sleep(1)
      read_html(url)
    },
    error = function(e) {
      cat("ERRO ao acessar:", e$message, "\n")
      return(NULL)
    }
  )

  if (is.null(pagina)) return(invisible(NULL))

  n_elementos <- pagina |>
    html_elements(seletor_esperado) |>
    length()

  n_scripts <- pagina |>
    html_elements("script") |>
    length()

  n_links_api <- pagina |>
    html_elements("script[src]") |>
    html_attr("src") |>
    str_detect("api|bundle|chunk|main|app") |>
    sum(na.rm = TRUE)

  cat(sprintf("Elementos de conteúdo encontrados: %d\n", n_elementos))
  cat(sprintf("Tags <script> na página: %d\n", n_scripts))
  cat(sprintf("Scripts de bundle/app: %d\n\n", n_links_api))

  if (n_elementos == 0 && n_scripts > 5) {
    cat("⚠ Provável conteúdo DINÂMICO.\n")
    cat("  → Use DevTools (F12 > Network > XHR) para inspecionar APIs.\n")
    cat("  → Se necessário, use RSelenium.\n")
  } else if (n_elementos > 0) {
    cat("✓ Conteúdo ESTÁTICO detectado — rvest deve funcionar.\n")
  } else {
    cat("? Inconclusivo — inspecione manualmente.\n")
  }
}

# Testar
diagnosticar_site("https://agenciabrasil.ebc.com.br/internacional")
diagnosticar_site("https://brasil.un.org/pt-br/news")

# =========================================================
# 3. EXEMPLO: API PÚBLICA DO GDELT PROJECT
# =========================================================
#
# GDELT é uma das maiores bases de eventos internacionais.
# Tem API pública documentada — sem necessidade de Selenium.
#
# Documentação: https://blog.gdeltproject.org/
#
# =========================================================

# Buscar artigos sobre diplomacia brasileira
url_gdelt <- paste0(
  "https://api.gdeltproject.org/api/v2/doc/doc",
  "?query=Brazil+diplomacy+OR+Brazil+foreign+policy",
  "&mode=artlist",
  "&maxrecords=25",
  "&format=json",
  "&sourcelang=portuguese"
)

resp_gdelt <- tryCatch(
  {
    request(url_gdelt) |>
      req_user_agent("Curso Web Scraping em RI / pesquisa academica") |>
      req_timeout(20) |>
      req_perform()
  },
  error = function(e) {
    message("Erro no GDELT: ", e$message)
    return(NULL)
  }
)

if (!is.null(resp_gdelt) && resp_status(resp_gdelt) == 200) {

  gdelt_json <- resp_gdelt |> resp_body_string()

  gdelt_dados <- fromJSON(gdelt_json)

  # Ver estrutura
  str(gdelt_dados, max.level = 2)

  # Extrair artigos se disponíveis
  if (!is.null(gdelt_dados$articles)) {
    df_gdelt <- as_tibble(gdelt_dados$articles)
    head(df_gdelt)
  }
}

# =========================================================
# 4. EXEMPLO: API PÚBLICA DO BANCO MUNDIAL (JSON)
# =========================================================
#
# Esta é uma API oculta apenas no sentido de que
# não é a interface principal — mas é documentada.
#
# =========================================================

# Buscar indicadores disponíveis para um tema
url_indicadores <- paste0(
  "https://api.worldbank.org/v2/indicator",
  "?format=json&per_page=10&source=2",
  "&topic=19"  # tópico 19 = Infraestrutura
)

resp_ind <- tryCatch(
  request(url_indicadores) |>
    req_user_agent("Curso RI") |>
    req_perform(),
  error = function(e) NULL
)

if (!is.null(resp_ind)) {
  ind_dados <- resp_ind |> resp_body_string() |> fromJSON()
  head(ind_dados[[2]][, c("id", "name")])
}

# =========================================================
# 5. REPLICAR REQUISIÇÃO AJAX COM httr2
# =========================================================
#
# Fluxo de trabalho:
#
# 1. Abrir o site no navegador;
# 2. F12 → Network → Recarregar;
# 3. Filtrar por XHR / Fetch;
# 4. Identificar requisição com dados;
# 5. Copiar URL e headers;
# 6. Replicar com httr2.
#
# Exemplo genérico de como replicar:
#
# =========================================================

replicar_ajax <- function(
    url_api,
    headers_extras = list(),
    parametros     = list()
) {

  req <- request(url_api) |>
    req_user_agent("Pesquisa academica") |>
    req_headers(
      Accept             = "application/json, text/javascript, */*; q=0.01",
      `X-Requested-With` = "XMLHttpRequest"
    )

  # Adicionar headers extras (ex: Referer, Authorization)
  if (length(headers_extras) > 0) {
    req <- req |> req_headers(!!!headers_extras)
  }

  # Adicionar parâmetros de query
  if (length(parametros) > 0) {
    req <- req |> req_url_query(!!!parametros)
  }

  resp <- tryCatch(
    req |> req_timeout(15) |> req_retry(max_tries = 3) |> req_perform(),
    error = function(e) {
      message("Erro: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) return(NULL)
  if (resp_status(resp) != 200) {
    message("Status ", resp_status(resp))
    return(NULL)
  }

  resp |> resp_body_json()
}

# =========================================================
# 6. EXEMPLO: UN DATA API
# =========================================================
#
# O sistema das Nações Unidas expõe dados via API pública.
#
# =========================================================

url_un <- "https://data.un.org/ws/rest/data/DF_UNData_UNFCC/A.../ALL/?format=jsondata"

# Alternativa: UN Comtrade API (requer registro gratuito)
# https://comtradeplus.un.org/

# Exemplo com API do WTO (OMC)
url_wto <- paste0(
  "https://api.wto.org/timeseries/v1/data",
  "?i=TP_A_0010&r=BRA&p=000&ps=2020,2021,2022,2023",
  "&fmt=json&max=50&lang=1"
)

# (Requer API key gratuita do WTO)
# resp_wto <- request(url_wto) |>
#   req_headers(`Ocp-Apim-Subscription-Key` = Sys.getenv("WTO_KEY")) |>
#   req_perform()

# =========================================================
# 7. CHROMOTE: RENDERIZAÇÃO HEADLESS
# =========================================================
#
# Para sites que realmente exigem JS,
# chromote controla o Chrome sem interface gráfica.
#
# Instalação:
# install.packages("chromote")
#
# =========================================================

# Exemplo de uso do chromote (requer Chrome instalado)
usar_chromote <- function(url, seletor, aguardar = 3) {

  # Verificar se chromote está disponível
  if (!requireNamespace("chromote", quietly = TRUE)) {
    message("chromote não instalado. Use: install.packages('chromote')")
    return(NULL)
  }

  library(chromote)

  b <- tryCatch(
    ChromoteSession$new(),
    error = function(e) {
      message("Chrome não encontrado: ", e$message)
      return(NULL)
    }
  )

  if (is.null(b)) return(NULL)

  on.exit(b$close())

  # Navegar para a URL
  b$Page$navigate(url)

  # Aguardar JS renderizar
  Sys.sleep(aguardar)

  # Capturar o DOM final
  html_renderizado <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value

  # Parsear com rvest
  pagina_renderizada <- read_html(html_renderizado)

  # Extrair dados
  pagina_renderizada |>
    html_elements(seletor) |>
    html_text2()
}

# Exemplo de chamada (requer Chrome):
# titulos_js <- usar_chromote(
#   url      = "https://site-dinamico.com",
#   seletor  = ".noticia-titulo",
#   aguardar = 4
# )

# =========================================================
# 8. SCROLL INFINITO COM CHROMOTE
# =========================================================

scroll_e_coletar <- function(url, n_scrolls = 5, seletor = "article") {

  if (!requireNamespace("chromote", quietly = TRUE)) return(NULL)

  library(chromote)

  b <- tryCatch(ChromoteSession$new(), error = function(e) NULL)
  if (is.null(b)) return(NULL)
  on.exit(b$close())

  b$Page$navigate(url)
  Sys.sleep(3)

  for (i in seq_len(n_scrolls)) {
    # Scrollar até o fim da página
    b$Runtime$evaluate(
      "window.scrollTo(0, document.body.scrollHeight)"
    )
    Sys.sleep(2)
    cat(sprintf("Scroll %d/%d realizado.\n", i, n_scrolls))
  }

  # Capturar HTML após scrolls
  html_final <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value

  read_html(html_final) |>
    html_elements(seletor) |>
    html_text2()
}

# =========================================================
# 9. TRABALHAR COM JSON COMPLEXO
# =========================================================
#
# APIs retornam JSON aninhado.
# Precisamos navegar pela estrutura.
#
# =========================================================

# JSON típico de API de notícias
json_exemplo <- '{
  "status": "ok",
  "totalResults": 3,
  "articles": [
    {
      "title": "ONU aprova resolução sobre Gaza",
      "publishedAt": "2026-05-12T14:30:00Z",
      "source": {"id": "reuters", "name": "Reuters"},
      "url": "https://reuters.com/article/1"
    },
    {
      "title": "OTAN amplia presença no Báltico",
      "publishedAt": "2026-05-11T09:15:00Z",
      "source": {"id": "bbc", "name": "BBC"},
      "url": "https://bbc.com/article/2"
    },
    {
      "title": "China lidera cúpula do BRICS",
      "publishedAt": "2026-05-10T11:00:00Z",
      "source": {"id": "ft", "name": "Financial Times"},
      "url": "https://ft.com/article/3"
    }
  ]
}'

# Parsear
dados_json <- fromJSON(json_exemplo)

# Navegar pela estrutura
str(dados_json, max.level = 2)

# Extrair artigos como dataframe
df_artigos <- as_tibble(dados_json$articles)
df_artigos

# Acessar campos aninhados (source é um dataframe dentro)
df_artigos |>
  mutate(
    fonte_nome = source$name,
    data       = lubridate::ymd_hms(publishedAt)
  ) |>
  select(title, fonte_nome, data, url)

# =========================================================
# 10. NEWS API (EXEMPLO COM API KEY)
# =========================================================
#
# News API (newsapi.org) tem plano gratuito para pesquisa.
# Requer registro.
#
# =========================================================

buscar_news_api <- function(
    query,
    api_key  = Sys.getenv("NEWS_API_KEY"),
    language = "pt",
    n_max    = 20
) {

  if (api_key == "") {
    message("Defina NEWS_API_KEY em ~/.Renviron")
    return(NULL)
  }

  url <- "https://newsapi.org/v2/everything"

  resp <- tryCatch(
    request(url) |>
      req_url_query(
        q        = query,
        language = language,
        pageSize = n_max,
        apiKey   = api_key
      ) |>
      req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp)) return(NULL)

  dados <- resp |> resp_body_string() |> fromJSON()

  if (is.null(dados$articles)) return(tibble())

  as_tibble(dados$articles) |>
    select(title, publishedAt, source, url) |>
    mutate(
      fonte = source$name,
      data  = lubridate::ymd_hms(publishedAt)
    ) |>
    select(-source, -publishedAt)
}

# Uso (com API key configurada):
# df_news <- buscar_news_api("BRICS diplomacia")

# =========================================================
# 11. EXERCÍCIO
# =========================================================
#
# Parte A — Diagnóstico:
#
# Escolha 3 sites relevantes para RI e aplique
# diagnosticar_site() em cada um.
# Classifique: estático ou dinâmico.
#
# Parte B — API oculta:
#
# Nos sites dinâmicos identificados:
# 1. Abra o DevTools > Network;
# 2. Procure chamadas XHR ou Fetch;
# 3. Identifique URLs com dados JSON;
# 4. Tente replicar com httr2;
# 5. Extraia e estruture os dados.
#
# =========================================================
# 12. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - a diferença entre conteúdo estático e dinâmico;
# - como diagnosticar um site com R;
# - usar DevTools > Network para inspecionar requisições;
# - replicar chamadas AJAX com httr2;
# - usar headers (Accept, X-Requested-With, Referer);
# - trabalhar com JSON aninhado;
# - usar chromote para renderização headless;
# - implementar scroll infinito automatizado.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 10: Automação com Selenium
#
# - quando chromote não é suficiente;
# - instalar e configurar RSelenium;
# - navegar, clicar, preencher formulários;
# - lidar com login;
# - scraping de portais legislativos complexos.
#
# =========================================================
# FIM DA AULA
# =========================================================
