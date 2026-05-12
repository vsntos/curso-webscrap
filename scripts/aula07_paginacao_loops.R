# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 7: Paginação e Loops
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - navegar entre páginas automaticamente;
# - usar loops for e purrr para coletar em escala;
# - gerar vetores de URLs programaticamente;
# - monitorar progresso durante a coleta;
# - parar loops com condições de saída;
# - combinar múltiplos portais e seções;
# - salvar dados em lotes para evitar perda.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Qual é o padrão de cobertura internacional
# em portais brasileiros ao longo do tempo?
#
# Para responder, precisamos de volume:
# centenas de notícias de múltiplas páginas.
#
# =========================================================
# 1. PACOTES
# =========================================================

library(rvest)
library(httr2)
library(tidyverse)
library(lubridate)
library(stringr)

# =========================================================
# 2. TIPOS DE PAGINAÇÃO
# =========================================================
#
# Antes de iterar, identifique o padrão de URL do site:
#
# A) Numérica:  portal.com/noticias?page=1
# B) Offset:    portal.com/noticias?start=0
# C) Path:      portal.com/noticias/pagina/1
# D) Cursor:    portal.com/noticias?cursor=abc123
# E) Link rel:  <a rel="next" href="...">
#
# =========================================================

# Exemplos de construção de URLs

# A) Numérica (página começa em 0 ou 1 — verifique!)
urls_numerica <- paste0(
  "https://agenciabrasil.ebc.com.br/internacional?page=",
  0:9
)
head(urls_numerica, 3)

# B) Offset de 20 em 20
offsets <- seq(0, 180, by = 20)
urls_offset <- paste0(
  "https://api.exemplo.com/dados?offset=",
  offsets,
  "&limit=20"
)
head(urls_offset, 3)

# C) Path
urls_path <- paste0(
  "https://portal.com/noticias/pagina/",
  1:10
)
head(urls_path, 3)

# =========================================================
# 3. LOOP FOR BÁSICO
# =========================================================

resultados_for <- list()

for (p in 0:2) {

  url <- paste0(
    "https://agenciabrasil.ebc.com.br/internacional?page=",
    p
  )

  cat(sprintf("[%d/3] Acessando: %s\n", p + 1, url))

  pagina <- tryCatch(
    {
      Sys.sleep(1.5)
      read_html(url)
    },
    error = function(e) {
      message("Erro na página ", p, ": ", e$message)
      return(NULL)
    }
  )

  if (is.null(pagina)) next

  titulos <- pagina |>
    html_elements("h2") |>
    html_text2() |>
    str_squish()

  links <- pagina |>
    html_elements("h2 a") |>
    html_attr("href")

  n <- min(length(titulos), length(links))
  if (n == 0) next

  resultados_for[[length(resultados_for) + 1]] <- tibble(
    titulo = titulos[1:n],
    link   = links[1:n],
    pagina = p
  )
}

df_for <- bind_rows(resultados_for)
nrow(df_for)
df_for

# =========================================================
# 4. EQUIVALENTE COM purrr
# =========================================================
#
# purrr::map_dfr() é mais limpo e idiomático no tidyverse.
# Evita variáveis de estado (resultados[[i]]).
#
# =========================================================

raspar_pagina_ab <- function(p) {

  url <- paste0(
    "https://agenciabrasil.ebc.com.br/internacional?page=",
    p
  )

  cat(sprintf("Raspando página %d...\n", p))

  Sys.sleep(1.5)

  pagina <- tryCatch(
    read_html(url),
    error = function(e) {
      message("Falha: ", e$message)
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
    pagina      = p,
    coletado_em = Sys.time()
  )
}

# Raspar 5 páginas com purrr
df_purrr <- map_dfr(0:4, raspar_pagina_ab)

nrow(df_purrr)
df_purrr

# =========================================================
# 5. MONITORAR PROGRESSO
# =========================================================
#
# Em coletas longas, é útil ver o progresso.
#
# =========================================================

raspar_com_progresso <- function(paginas) {

  total <- length(paginas)

  map_dfr(seq_along(paginas), function(i) {

    p <- paginas[i]

    cat(sprintf(
      "[%d/%d] Página %d...\n",
      i, total, p
    ))

    raspar_pagina_ab(p)
  })
}

df_monitorado <- raspar_com_progresso(0:3)

# =========================================================
# 6. PARAR QUANDO NÃO HÁ MAIS RESULTADOS
# =========================================================
#
# Alguns sites não retornam erro quando a página não existe.
# Retornam HTML com seção vazia.
#
# Estratégia: parar quando não encontrar notícias.
#
# =========================================================

raspar_ate_vazio <- function(url_base, max_paginas = 50) {

  resultados <- list()

  for (p in 0:(max_paginas - 1)) {

    url <- paste0(url_base, p)

    Sys.sleep(1.5)

    pagina <- tryCatch(
      read_html(url),
      error = function(e) return(NULL)
    )

    if (is.null(pagina)) {
      cat("Erro na página", p, "— encerrando.\n")
      break
    }

    titulos <- pagina |>
      html_elements("h2") |>
      html_text2() |>
      str_squish()

    # Parar se não encontrou nada
    if (length(titulos) == 0) {
      cat("Página vazia na página", p, "— encerrando.\n")
      break
    }

    cat(sprintf("[p%d] %d títulos encontrados.\n", p, length(titulos)))

    resultados[[length(resultados) + 1]] <- tibble(
      titulo = titulos,
      pagina = p
    )
  }

  bind_rows(resultados)
}

df_auto <- raspar_ate_vazio(
  "https://agenciabrasil.ebc.com.br/internacional?page=",
  max_paginas = 5
)

nrow(df_auto)

# =========================================================
# 7. SEGUIR LINK DE PRÓXIMA PÁGINA
# =========================================================
#
# Alguns sites usam <a rel="next"> para indicar
# a próxima página.
#
# Essa é a forma mais robusta de navegar.
#
# =========================================================

raspar_seguindo_links <- function(url_inicial, max_paginas = 20) {

  url_atual <- url_inicial
  resultados <- list()
  i <- 1

  while (!is.null(url_atual) && i <= max_paginas) {

    cat(sprintf("[p%d] %s\n", i, url_atual))

    Sys.sleep(1.5)

    pagina <- tryCatch(
      read_html(url_atual),
      error = function(e) return(NULL)
    )

    if (is.null(pagina)) break

    # Extrair dados
    titulos <- pagina |>
      html_elements("h2") |>
      html_text2() |>
      str_squish()

    if (length(titulos) > 0) {
      resultados[[i]] <- tibble(titulo = titulos, pagina = i)
    }

    # Procurar link para próxima página
    proxima_rel <- pagina |>
      html_element("a[rel='next']") |>
      html_attr("href")

    # Parar se não há próxima
    url_atual <- proxima_rel
    i <- i + 1
  }

  bind_rows(resultados)
}

# =========================================================
# 8. MÚLTIPLAS SEÇÕES EM PARALELO
# =========================================================
#
# Para cobrir múltiplas editorias de um portal.
#
# =========================================================

secoes <- c(
  "internacional",
  "economia",
  "direitos-humanos"
)

urls_secoes <- paste0(
  "https://agenciabrasil.ebc.com.br/",
  secoes,
  "?page=0"
)

df_secoes <- map_dfr(
  seq_along(urls_secoes),
  function(i) {

    Sys.sleep(2)

    pagina <- tryCatch(
      read_html(urls_secoes[i]),
      error = function(e) return(NULL)
    )

    if (is.null(pagina)) return(tibble())

    tibble(
      titulo = pagina |>
        html_elements("h2") |>
        html_text2() |>
        str_squish(),
      secao = secoes[i]
    )
  }
)

df_secoes |>
  count(secao)

# =========================================================
# 9. COMBINAR MÚLTIPLOS PORTAIS
# =========================================================

portais <- tibble(
  nome = c("Agência Brasil", "ONU Brasil"),
  url  = c(
    "https://agenciabrasil.ebc.com.br/internacional?page=0",
    "https://brasil.un.org/pt-br/news?page=0"
  )
)

df_portais <- map_dfr(
  seq_len(nrow(portais)),
  function(i) {

    Sys.sleep(2)

    pagina <- tryCatch(
      read_html(portais$url[i]),
      error = function(e) return(NULL)
    )

    if (is.null(pagina)) return(tibble())

    tibble(
      titulo = pagina |>
        html_elements("h2, h3") |>
        html_text2() |>
        str_squish(),
      portal = portais$nome[i]
    )
  }
)

df_portais |> count(portal)

# =========================================================
# 10. SALVAR EM LOTES PARA EVITAR PERDA
# =========================================================
#
# Em coletas longas, salvar periodicamente é essencial.
# Se o script quebrar na página 45, você não perde tudo.
#
# =========================================================

if (!dir.exists("data/raw")) dir.create("data/raw", recursive = TRUE)

raspar_em_lotes <- function(url_base, n_paginas = 30, lote = 10) {

  todos <- list()

  for (p in 0:(n_paginas - 1)) {

    Sys.sleep(1.5)

    pagina <- tryCatch(
      read_html(paste0(url_base, p)),
      error = function(e) NULL
    )

    if (!is.null(pagina)) {

      titulos <- pagina |>
        html_elements("h2") |>
        html_text2() |>
        str_squish()

      if (length(titulos) > 0) {
        todos[[length(todos) + 1]] <- tibble(titulo = titulos, pagina = p)
      }
    }

    # Salvar a cada `lote` páginas
    if ((p + 1) %% lote == 0) {
      arquivo <- paste0("data/raw/lote_p", p, ".csv")
      write_csv(bind_rows(todos), arquivo)
      cat("Lote salvo:", arquivo, "\n")
    }
  }

  bind_rows(todos)
}

df_lotes <- raspar_em_lotes(
  "https://agenciabrasil.ebc.com.br/internacional?page=",
  n_paginas = 5,
  lote = 3
)

nrow(df_lotes)

# =========================================================
# 11. DELAYS ADAPTATIVOS
# =========================================================
#
# Variar o delay reduz o risco de ser bloqueado.
#
# =========================================================

# Delay fixo
Sys.sleep(1.5)

# Delay aleatório entre 1 e 3 segundos
Sys.sleep(runif(1, min = 1, max = 3))

# Delay exponencial em caso de erro (backoff)
esperar_com_backoff <- function(tentativa) {
  delay <- 2 ^ tentativa  # 2, 4, 8, 16...
  cat(sprintf("Aguardando %ds antes de tentar novamente...\n", delay))
  Sys.sleep(delay)
}

# =========================================================
# 12. PIPELINE COMPLETO EM ESCALA
# =========================================================

coletar_noticias_escala <- function(
    url_base,
    n_paginas    = 20,
    seletor_titulo = "h2",
    seletor_link   = "h2 a"
) {

  total <- n_paginas

  map_dfr(0:(n_paginas - 1), function(p) {

    cat(sprintf("[%d/%d] Página %d\n", p + 1, total, p))

    Sys.sleep(runif(1, 1, 2))

    pagina <- tryCatch(
      read_html(paste0(url_base, p)),
      error = function(e) {
        message("Erro: ", e$message)
        return(NULL)
      }
    )

    if (is.null(pagina)) return(tibble())

    titulos <- pagina |>
      html_elements(seletor_titulo) |>
      html_text2() |>
      str_squish()

    links <- pagina |>
      html_elements(seletor_link) |>
      html_attr("href")

    n <- min(length(titulos), length(links))
    if (n == 0) return(tibble())

    tibble(
      titulo      = titulos[1:n],
      link        = links[1:n],
      pagina      = p,
      coletado_em = Sys.time()
    )

  }) |>
    filter(nchar(titulo) > 20) |>
    distinct(link, .keep_all = TRUE)
}

df_escala <- coletar_noticias_escala(
  url_base  = "https://agenciabrasil.ebc.com.br/internacional?page=",
  n_paginas = 5
)

nrow(df_escala)

write_csv(df_escala, "data/processed/corpus_internacional.csv")

# =========================================================
# 13. EXERCÍCIO
# =========================================================
#
# Objetivo: construir corpus com 200+ observações.
#
# 1. Escolha um portal com paginação por URL;
# 2. Identifique o padrão (numérico, offset, path);
# 3. Gere vetor de URLs com paste0();
# 4. Escreva função de extração com tratamento de erro;
# 5. Use map_dfr() com Sys.sleep(1.5);
# 6. Monitore o progresso;
# 7. Salve em CSV;
# 8. Conte por mês ou seção.
#
# =========================================================
# 14. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - tipos de paginação (numérica, offset, link rel);
# - loops for e while em R;
# - purrr::map_dfr() como alternativa idiomática;
# - gerar vetores de URLs com paste0();
# - monitorar progresso durante coleta;
# - parar loops com condições de saída;
# - seguir links de próxima página automaticamente;
# - combinar múltiplos portais e seções;
# - salvar em lotes para evitar perda de progresso;
# - usar delays adaptativos.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 8: Robustez e Tratamento de Erros
#
# - tryCatch em detalhe;
# - retentativas automáticas;
# - logging de erros;
# - validação de resultados;
# - scraping resiliente para coletas longas.
#
# =========================================================
# FIM DA AULA
# =========================================================
