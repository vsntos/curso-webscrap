# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 10: Automação com Selenium
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - instalar e configurar RSelenium;
# - iniciar e encerrar sessões de forma segura;
# - navegar, clicar e digitar com Selenium;
# - extrair conteúdo renderizado por JavaScript;
# - automatizar scroll infinito;
# - navegar entre páginas clicando;
# - lidar com login em portais;
# - usar aguardar elementos com timeout.
#
# =========================================================
# QUANDO USAR SELENIUM
# =========================================================
#
# Selenium é necessário quando:
#
# ✓ O conteúdo depende de JavaScript obrigatório;
# ✓ Há login necessário;
# ✓ Interações (cliques, formulários) são exigidas;
# ✓ Scroll infinito não pode ser interceptado via API;
# ✓ CAPTCHA não está presente (Selenium não resolve CAPTCHA).
#
# Selenium NÃO é necessário quando:
#
# ✗ O site tem API pública (use httr2);
# ✗ O HTML já contém os dados (use rvest);
# ✗ Há API oculta acessível (use httr2 + headers).
#
# =========================================================
# 1. INSTALAÇÃO
# =========================================================
#
# install.packages("RSelenium")
#
# Você também precisa do ChromeDriver compatível com
# a versão do Chrome instalado no seu sistema.
#
# Verificar versão do Chrome:
# chrome://settings/help
#
# Baixar ChromeDriver:
# https://chromedriver.chromium.org/downloads
#
# Alternativa automática:
# RSelenium::rsDriver(chromever = "auto")
#
# =========================================================

library(RSelenium)
library(rvest)
library(tidyverse)

# =========================================================
# 2. INICIANDO UMA SESSÃO
# =========================================================

iniciar_sessao <- function(
    browser   = "chrome",
    headless  = TRUE,
    port      = 4444L
) {

  # Opções do Chrome
  chrome_opts <- list(
    chromeOptions = list(
      args = if (headless) {
        list("--headless", "--no-sandbox", "--disable-dev-shm-usage",
             "--disable-gpu", "--window-size=1400,900")
      } else {
        list("--window-size=1400,900")
      }
    )
  )

  driver <- tryCatch(
    rsDriver(
      browser      = browser,
      chromever    = "auto",
      verbose      = FALSE,
      port         = port,
      extraCapabilities = chrome_opts
    ),
    error = function(e) {
      message("Erro ao iniciar Selenium: ", e$message)
      message("Verifique se o ChromeDriver está instalado.")
      return(NULL)
    }
  )

  if (is.null(driver)) return(NULL)

  list(
    driver = driver,
    remDr  = driver$client
  )
}

# =========================================================
# 3. TEMPLATE DE SCRAPER SELENIUM
# =========================================================
#
# Sempre use on.exit() para garantir que o driver feche,
# mesmo se ocorrer um erro durante a coleta.
#
# =========================================================

scraper_selenium_template <- function(url, seletor_titulo = "h2") {

  # Iniciar sessão
  sessao <- iniciar_sessao(headless = TRUE)
  if (is.null(sessao)) return(tibble())

  remDr <- sessao$remDr

  # SEMPRE fechar ao sair
  on.exit({
    tryCatch(remDr$close(), error = function(e) NULL)
    tryCatch(sessao$driver$server$stop(), error = function(e) NULL)
  })

  # Navegar
  remDr$navigate(url)
  Sys.sleep(3)

  # Capturar HTML renderizado
  html <- tryCatch(
    remDr$getPageSource()[[1]],
    error = function(e) NULL
  )

  if (is.null(html)) return(tibble())

  # Extrair com rvest
  pagina <- read_html(html)

  titulos <- pagina |>
    html_elements(seletor_titulo) |>
    html_text2() |>
    str_squish()

  tibble(titulo = titulos, url_fonte = url)
}

# Executar (requer Chrome instalado)
# df <- scraper_selenium_template(
#   "https://agenciabrasil.ebc.com.br/internacional"
# )

# =========================================================
# 4. ENCONTRAR E INTERAGIR COM ELEMENTOS
# =========================================================

demonstrar_interacoes <- function() {

  sessao <- iniciar_sessao(headless = FALSE)  # visível para debug
  if (is.null(sessao)) return(NULL)

  remDr <- sessao$remDr
  on.exit({
    tryCatch(remDr$close(), error = function(e) NULL)
    tryCatch(sessao$driver$server$stop(), error = function(e) NULL)
  })

  # Navegar
  remDr$navigate("https://duckduckgo.com")
  Sys.sleep(2)

  # Encontrar campo de busca por CSS selector
  campo_busca <- remDr$findElement(
    using = "css selector",
    value = "input[name='q']"
  )

  # Limpar e digitar
  campo_busca$clearElement()
  campo_busca$sendKeysToElement(list("Brasil BRICS diplomacia"))
  Sys.sleep(0.5)

  # Pressionar Enter
  campo_busca$sendKeysToElement(list(key = "enter"))
  Sys.sleep(3)

  # Extrair resultados
  html <- remDr$getPageSource()[[1]]
  pagina <- read_html(html)

  resultados <- pagina |>
    html_elements("h2") |>
    html_text2() |>
    str_squish()

  head(resultados, 10)
}

# demonstrar_interacoes()

# =========================================================
# 5. AGUARDAR ELEMENTO (ESPERA EXPLÍCITA)
# =========================================================
#
# Em vez de Sys.sleep() fixo, aguardar até o elemento aparecer.
#
# =========================================================

aguardar_elemento <- function(remDr, seletor, timeout = 15, intervalo = 0.5) {

  inicio <- Sys.time()

  while (TRUE) {

    elemento <- tryCatch(
      remDr$findElement("css selector", seletor),
      error = function(e) NULL
    )

    if (!is.null(elemento)) {
      return(elemento)
    }

    elapsed <- as.numeric(Sys.time() - inicio, units = "secs")

    if (elapsed > timeout) {
      message(sprintf(
        "Timeout: '%s' não apareceu em %ds.", seletor, timeout
      ))
      return(NULL)
    }

    Sys.sleep(intervalo)
  }
}

# =========================================================
# 6. SCROLL INFINITO
# =========================================================

coletar_scroll_infinito <- function(url, n_scrolls = 10, seletor = "article") {

  sessao <- iniciar_sessao()
  if (is.null(sessao)) return(tibble())

  remDr <- sessao$remDr
  on.exit({
    tryCatch(remDr$close(), error = function(e) NULL)
    tryCatch(sessao$driver$server$stop(), error = function(e) NULL)
  })

  remDr$navigate(url)
  Sys.sleep(3)

  todos_titulos <- character(0)
  sem_novos <- 0

  for (i in seq_len(n_scrolls)) {

    # Capturar estado atual
    html   <- remDr$getPageSource()[[1]]
    pagina <- read_html(html)

    titulos_atuais <- pagina |>
      html_elements(seletor) |>
      html_text2() |>
      str_squish()

    novos <- setdiff(titulos_atuais, todos_titulos)
    todos_titulos <- unique(c(todos_titulos, titulos_atuais))

    cat(sprintf("[scroll %d/%d] +%d novos | %d total\n",
                i, n_scrolls, length(novos), length(todos_titulos)))

    if (length(novos) == 0) {
      sem_novos <- sem_novos + 1
      if (sem_novos >= 3) {
        cat("Sem novos elementos por 3 scrolls — encerrando.\n")
        break
      }
    } else {
      sem_novos <- 0
    }

    # Scrollar até o fim
    remDr$executeScript(
      "window.scrollTo(0, document.body.scrollHeight);"
    )
    Sys.sleep(2)
  }

  tibble(titulo = todos_titulos)
}

# =========================================================
# 7. NAVEGAÇÃO PAGINADA COM CLIQUE
# =========================================================

navegar_paginas_clique <- function(
    url,
    seletor_dados   = "h2",
    seletor_proxima = "a[rel='next'], .next, .proxima",
    max_paginas     = 20
) {

  sessao <- iniciar_sessao()
  if (is.null(sessao)) return(tibble())

  remDr <- sessao$remDr
  on.exit({
    tryCatch(remDr$close(), error = function(e) NULL)
    tryCatch(sessao$driver$server$stop(), error = function(e) NULL)
  })

  remDr$navigate(url)
  Sys.sleep(2)

  resultados <- list()

  for (p in seq_len(max_paginas)) {

    html   <- remDr$getPageSource()[[1]]
    pagina <- read_html(html)

    titulos <- pagina |>
      html_elements(seletor_dados) |>
      html_text2() |>
      str_squish()

    cat(sprintf("[p%d] %d títulos\n", p, length(titulos)))

    if (length(titulos) > 0) {
      resultados[[p]] <- tibble(titulo = titulos, pagina = p)
    }

    # Procurar botão de próxima página
    btn_prox <- tryCatch(
      remDr$findElement("css selector", seletor_proxima),
      error = function(e) NULL
    )

    if (is.null(btn_prox)) {
      cat("Última página encontrada.\n")
      break
    }

    # Scrollar até o botão antes de clicar
    remDr$executeScript(
      "arguments[0].scrollIntoView(true);",
      list(btn_prox)
    )
    Sys.sleep(0.5)

    btn_prox$clickElement()
    Sys.sleep(2)
  }

  bind_rows(resultados)
}

# =========================================================
# 8. LOGIN AUTOMATIZADO
# =========================================================
#
# ATENÇÃO ÉTICA:
#
# Automatizar login implica:
# - ter autorização para usar a conta;
# - não violar termos de uso;
# - não expor credenciais no código.
#
# Use variáveis de ambiente (.Renviron):
# PORTAL_USER=seu_usuario
# PORTAL_SENHA=sua_senha
#
# =========================================================

fazer_login <- function(
    remDr,
    url_login,
    seletor_user,
    seletor_senha,
    seletor_botao,
    usuario = Sys.getenv("PORTAL_USER"),
    senha   = Sys.getenv("PORTAL_SENHA")
) {

  if (usuario == "" || senha == "") {
    stop("Defina PORTAL_USER e PORTAL_SENHA em ~/.Renviron")
  }

  remDr$navigate(url_login)
  Sys.sleep(2)

  # Preencher usuário
  campo_user <- aguardar_elemento(remDr, seletor_user)
  if (!is.null(campo_user)) {
    campo_user$clearElement()
    campo_user$sendKeysToElement(list(usuario))
  }

  # Preencher senha
  campo_senha <- remDr$findElement("css selector", seletor_senha)
  campo_senha$clearElement()
  campo_senha$sendKeysToElement(list(senha))

  # Clicar em entrar
  botao <- remDr$findElement("css selector", seletor_botao)
  botao$clickElement()

  Sys.sleep(3)

  cat("Login realizado. URL atual:", remDr$getCurrentUrl()[[1]], "\n")
}

# =========================================================
# 9. CAPTURAR SCREENSHOT PARA DOCUMENTAÇÃO
# =========================================================

tirar_screenshot <- function(remDr, nome = NULL) {

  if (is.null(nome)) {
    nome <- paste0("screenshots/screen_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
  }

  dir.create(dirname(nome), showWarnings = FALSE, recursive = TRUE)

  remDr$screenshot(file = nome)
  cat("Screenshot salvo:", nome, "\n")
}

# =========================================================
# 10. EXEMPLO COMPLETO: CÂMARA DOS DEPUTADOS
# =========================================================
#
# A Câmara tem API pública REST — prefira ela.
# Mas o exemplo abaixo mostra como usaríamos Selenium
# se a API não existisse.
#
# API oficial: https://dadosabertos.camara.leg.br/swagger/api.html
#
# =========================================================

# Com a API oficial (sem Selenium):
buscar_votacoes_api_camara <- function(ano = 2025, pagina = 1) {

  url <- paste0(
    "https://dadosabertos.camara.leg.br/api/v2/votacoes",
    "?dataInicio=", ano, "-01-01",
    "&dataFim=", ano, "-12-31",
    "&pagina=", pagina,
    "&itens=20",
    "&ordem=DESC&ordenarPor=dataHoraRegistro"
  )

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent("Curso RI") |>
      httr2::req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp)) return(tibble())

  dados <- resp |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()

  if (is.null(dados$dados)) return(tibble())

  as_tibble(dados$dados) |>
    select(id, siglaOrgao, descricao, dataHoraRegistro, aprovacao)
}

df_votacoes <- buscar_votacoes_api_camara(ano = 2025)

df_votacoes

# =========================================================
# 11. EXERCÍCIO
# =========================================================
#
# Parte A — Com API (sem Selenium):
#
# Use a API da Câmara (dadosabertos.camara.leg.br)
# para coletar proposições legislativas sobre RI:
# - tema = 220 (Relações Internacionais)
# - colete 3 páginas
# - salve em CSV
#
# Parte B — Com Selenium (avançado):
#
# Escolha um portal governamental sem API.
# Automatize:
# 1. Navegação até a seção de documentos;
# 2. Clique em um filtro ou formulário;
# 3. Extração de resultados;
# 4. Navegação para próxima página.
#
# =========================================================
# 12. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - quando Selenium é (e não é) necessário;
# - instalar e configurar RSelenium;
# - iniciar e encerrar sessões com segurança (on.exit);
# - encontrar elementos por CSS selector e XPath;
# - clicar, digitar e pressionar teclas;
# - scrollar com executeScript();
# - aguardar elementos com espera explícita;
# - automatizar scroll infinito;
# - navegar paginação via clique;
# - realizar login automatizado;
# - usar a API da Câmara como alternativa superior.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 11: Organização de Projetos
#
# - estrutura de pastas para projetos de scraping;
# - naming conventions;
# - separação raw / processed;
# - README e documentação;
# - reprodutibilidade como padrão científico.
#
# =========================================================
# FIM DA AULA
# =========================================================
