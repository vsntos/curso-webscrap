# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 8: Robustez e Tratamento de Erros
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - usar tryCatch() para capturar erros sem quebrar o script;
# - usar possibly() e safely() do purrr;
# - implementar retentativas automáticas;
# - registrar erros em arquivo de log;
# - validar resultados do scraping;
# - lidar com timeouts e encoding;
# - construir scrapers prontos para coletas longas.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Como coletamos dados de forma confiável
# em coletas longas, sem interrupções?
#
# Um scraper robusto é um scraper científico.
#
# =========================================================
# 1. PACOTES
# =========================================================

library(rvest)
library(httr2)
library(tidyverse)
library(purrr)
library(stringr)

# =========================================================
# HELPER: EXTRATOR DA AGÊNCIA BRASIL
# =========================================================
#
# A Agência Brasil separa <h2> dos <a> — não há <a> dentro
# de <h2>. Extraímos pelo padrão da URL das notícias.
#

extrair_ab <- function(pagina, secao = "internacional") {
  padrao <- paste0("/", secao, "/noticia/")
  nos    <- pagina |> html_elements(paste0("a[href*='", padrao, "']"))
  if (length(nos) == 0) return(tibble(titulo = character(), link = character()))
  links   <- nos |> html_attr("href")
  titulos <- nos |> html_text2() |> str_squish()
  validos <- nchar(titulos) > 20 & !grepl("^scald=", titulos)
  links   <- links[validos]; titulos <- titulos[validos]
  idx     <- !duplicated(links)
  tibble(
    titulo = titulos[idx],
    link   = paste0("https://agenciabrasil.ebc.com.br", links[idx])
  )
}

# =========================================================
# 2. O PROBLEMA: ERROS QUEBRAM O LOOP
# =========================================================

# Este script quebraria ao encontrar uma URL inválida:
urls_exemplo <- c(
  "https://agenciabrasil.ebc.com.br/internacional",
  "https://url-inventada-que-nao-existe.xyz",
  "https://brasil.un.org/pt-br/news"
)

# SEM tratamento — para no segundo item:
# for (url in urls_exemplo) {
#   pagina <- read_html(url)  # ERRO aqui
# }

# =========================================================
# 3. tryCatch() BÁSICO
# =========================================================
#
# tryCatch() captura erros, avisos e mensagens.
# Em vez de parar, executa o handler e continua.
#
# =========================================================

resultado <- tryCatch(
  {
    # Tentar algo que pode falhar
    read_html("https://url-inventada.xyz")
  },
  error = function(e) {
    cat("Erro capturado:", conditionMessage(e), "\n")
    return(NULL)  # Retornar NULL em vez de parar
  }
)

# Script continua — resultado é NULL
is.null(resultado)

# =========================================================
# 4. tryCatch() PARA SCRAPING
# =========================================================

raspar_com_trycatch <- function(url) {

  pagina <- tryCatch(
    {
      Sys.sleep(1)
      read_html(url)
    },
    error = function(e) {
      message(sprintf("ERRO em %s: %s", url, e$message))
      return(NULL)
    }
  )

  if (is.null(pagina)) return(tibble())

  titulos <- tryCatch(
    pagina |> html_elements("h2") |> html_text2() |> str_squish(),
    error = function(e) character(0)
  )

  if (length(titulos) == 0) return(tibble())

  tibble(titulo = titulos, url_fonte = url)
}

# Testar com URLs válidas e inválidas
df_teste <- map_dfr(urls_exemplo, raspar_com_trycatch)

df_teste

# =========================================================
# 5. possibly() DO PURRR
# =========================================================
#
# possibly() envolve uma função para retornar
# um valor padrão (otherwise) em caso de erro.
# Mais limpo que tryCatch() para uso com map().
#
# =========================================================

# Criar versão segura do read_html
ler_seguro <- possibly(read_html, otherwise = NULL)

# Agora podemos usar no map sem tryCatch explícito
paginas <- map(urls_exemplo, function(url) {
  Sys.sleep(1)
  ler_seguro(url)
})

# Quantas funcionaram?
n_ok <- sum(!map_lgl(paginas, is.null))
cat(sprintf("%d/%d páginas carregadas com sucesso.\n", n_ok, length(urls_exemplo)))

# Filtrar apenas as que funcionaram
paginas_validas <- compact(paginas)
length(paginas_validas)

# =========================================================
# 6. safely() DO PURRR
# =========================================================
#
# safely() retorna uma lista com $result e $error.
# Permite inspecionar erros depois.
#
# =========================================================

resultados_safe <- map(urls_exemplo, function(url) {
  Sys.sleep(1)
  safely(read_html)(url)
})

# Separar sucessos e falhas
paginas_ok    <- map(resultados_safe, "result")
erros         <- map(resultados_safe, "error")

# Quais falharam?
falhou <- map_lgl(erros, ~!is.null(.x))
urls_com_erro <- urls_exemplo[falhou]

cat("URLs com erro:\n")
print(urls_com_erro)

# Mensagens de erro
map_chr(
  erros[falhou],
  ~conditionMessage(.x)
)

# =========================================================
# 7. RETENTATIVAS MANUAIS
# =========================================================
#
# Para erros temporários (timeout, 503),
# faz sentido tentar novamente.
#
# =========================================================

raspar_com_retry <- function(url, max_tentativas = 3, delay_base = 2) {

  for (tentativa in seq_len(max_tentativas)) {

    pagina <- tryCatch(
      {
        Sys.sleep(runif(1, 0.8, 1.5))
        read_html(url)
      },
      error = function(e) {
        cat(sprintf(
          "Tentativa %d/%d falhou: %s\n",
          tentativa, max_tentativas, e$message
        ))
        return(NULL)
      }
    )

    if (!is.null(pagina)) return(pagina)

    # Espera exponencial antes da próxima tentativa
    if (tentativa < max_tentativas) {
      delay <- delay_base ^ tentativa
      cat(sprintf("Aguardando %ds antes da tentativa %d...\n",
                  delay, tentativa + 1))
      Sys.sleep(delay)
    }
  }

  message("Todas as tentativas falharam para: ", url)
  return(NULL)
}

# Testar
pagina_retry <- raspar_com_retry(
  "https://agenciabrasil.ebc.com.br/internacional"
)

!is.null(pagina_retry)

# =========================================================
# 8. RETRY COM HTTR2
# =========================================================
#
# httr2 tem retry embutido — mais elegante.
#
# =========================================================

raspar_httr2_robusto <- function(url) {

  resp <- tryCatch(
    request(url) |>
      req_user_agent("Curso Web Scraping em RI / pesquisa academica") |>
      req_timeout(15) |>
      req_retry(
        max_tries = 4,
        backoff   = ~ 2 ^ .x  # 2, 4, 8 segundos
      ) |>
      req_perform(),
    error = function(e) {
      message("Falha definitiva em ", url, ": ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) return(NULL)

  # Verificar status
  status <- resp_status(resp)

  if (status == 429) {
    cat("Rate limit atingido. Aguardando 60s...\n")
    Sys.sleep(60)
    return(NULL)
  }

  if (status != 200) {
    message(sprintf("Status %d para %s", status, url))
    return(NULL)
  }

  resp |> resp_body_string() |> read_html()
}

pagina_robusta <- raspar_httr2_robusto(
  "https://agenciabrasil.ebc.com.br/internacional"
)

!is.null(pagina_robusta)

# =========================================================
# 9. SISTEMA DE LOGGING
# =========================================================
#
# Registrar erros em arquivo é essencial
# para coletas longas e reprodutíveis.
#
# =========================================================

# Criar diretório de logs
if (!dir.exists("logs")) dir.create("logs")

log_file <- paste0("logs/scraping_", format(Sys.Date(), "%Y%m%d"), ".txt")

# Função de log
log_evento <- function(tipo, url, mensagem = "") {

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  linha <- sprintf(
    "[%s] [%s] %s%s\n",
    timestamp,
    tipo,
    url,
    if (nchar(mensagem) > 0) paste0(" | ", mensagem) else ""
  )

  cat(linha)
  cat(linha, file = log_file, append = TRUE)
}

# Usar no scraper
raspar_com_log <- function(url) {

  pagina <- tryCatch(
    {
      Sys.sleep(1)
      p <- read_html(url)
      log_evento("OK", url)
      p
    },
    error = function(e) {
      log_evento("ERRO", url, e$message)
      return(NULL)
    }
  )

  pagina
}

# raspar_com_log() é um WRAPPER de carregamento com log.
# Ela retorna o objeto HTML bruto — não extrai dados.
# Use o retorno com uma função de extração em seguida.

pagina_ab  <- raspar_com_log("https://agenciabrasil.ebc.com.br/internacional")
pagina_inv <- raspar_com_log("https://url-invalida.xyz")

# Extrair notícias da página carregada com sucesso
if (!is.null(pagina_ab)) {
  dados_ab <- extrair_ab(pagina_ab)
  cat(sprintf("%d notícias encontradas.\n", nrow(dados_ab)))
  print(dados_ab)
}

# Ver o log gerado
readLines(log_file)

# =========================================================
# 10. VALIDAR RESULTADOS
# =========================================================
#
# Não assumir que o scraping funcionou.
# Verificar antes de prosseguir.
#
# =========================================================

validar_pagina <- function(pagina, seletor = "h2", min_elementos = 1) {

  if (is.null(pagina)) {
    return(list(ok = FALSE, motivo = "pagina NULL"))
  }

  n <- length(html_elements(pagina, seletor))

  if (n < min_elementos) {
    return(list(
      ok     = FALSE,
      motivo = sprintf("Apenas %d elemento(s) para '%s'", n, seletor)
    ))
  }

  list(ok = TRUE, n_elementos = n)
}

pagina_ab <- tryCatch(
  read_html("https://agenciabrasil.ebc.com.br/internacional"),
  error = function(e) NULL
)

validacao <- validar_pagina(pagina_ab, seletor = "h2")
validacao

# =========================================================
# 11. VALIDAR DATASET FINAL
# =========================================================

validar_dataset <- function(df, n_min = 10) {

  problemas <- character(0)

  if (nrow(df) == 0) {
    problemas <- c(problemas, "Dataset vazio")
  }

  if (nrow(df) < n_min) {
    problemas <- c(problemas,
      sprintf("Apenas %d observações (mínimo: %d)", nrow(df), n_min))
  }

  if ("titulo" %in% names(df)) {
    n_na <- sum(is.na(df$titulo))
    if (n_na > 0) {
      problemas <- c(problemas,
        sprintf("%d títulos com NA", n_na))
    }

    n_curtos <- sum(nchar(df$titulo) <= 10, na.rm = TRUE)
    if (n_curtos > 0) {
      problemas <- c(problemas,
        sprintf("%d títulos muito curtos (≤10 chars)", n_curtos))
    }
  }

  if (length(problemas) > 0) {
    warning("Problemas no dataset:\n", paste("-", problemas, collapse = "\n"))
    return(FALSE)
  }

  cat(sprintf("Dataset OK: %d observações.\n", nrow(df)))
  return(TRUE)
}

# Testar
df_ok <- tibble(titulo = c("Notícia longa o suficiente", "Outra notícia aqui"))
validar_dataset(df_ok, n_min = 1)

df_ruim <- tibble(titulo = c(NA, "ok"))
validar_dataset(df_ruim, n_min = 5)

# =========================================================
# 12. LIDAR COM ENCODING
# =========================================================

# rvest detecta automaticamente na maioria dos casos
pagina_enc <- tryCatch(
  read_html("https://agenciabrasil.ebc.com.br/internacional",
            encoding = "UTF-8"),
  error = function(e) NULL
)

# Remover caracteres de controle se necessário
#
# Nota: \x00 (NUL) não é suportado em strings R —
# o parser rejeita antes de qualquer função ser chamada.
# O padrão abaixo cobre os demais caracteres de controle
# que podem aparecer em texto raspado de sites.
#
limpar_encoding <- function(texto) {
  texto |>
    str_remove_all("[\x01-\x08\x0B\x0C\x0E-\x1F]") |>
    str_squish()
}

# Caracteres de controle válidos em R: \x01–\x1F exceto \x00
limpar_encoding("Texto\x01com\x1Fcaracteres\x0Bestranhos")

# =========================================================
# 13. SCRIPT COMPLETO ROBUSTO
# =========================================================

scraper_robusto <- function(
    url_base,
    n_paginas      = 10,
    secao          = "internacional",
    delay_min      = 1.0,
    delay_max      = 2.5,
    max_tentativas = 3
) {
  #
  # Usa extrair_ab() definida no topo do script —
  # funciona para qualquer seção da Agência Brasil.
  #

  if (!dir.exists("data/raw")) dir.create("data/raw", recursive = TRUE)

  relatorio <- list(ok = 0, erros = 0)

  df <- map_dfr(0:(n_paginas - 1), function(p) {

    url <- paste0(url_base, p)

    cat(sprintf("[%d/%d] %s\n", p + 1, n_paginas, url))

    # Retentativas com backoff exponencial
    pagina <- NULL
    for (t in seq_len(max_tentativas)) {

      Sys.sleep(runif(1, delay_min, delay_max))

      pagina <- tryCatch(
        read_html(url),
        error = function(e) NULL
      )

      if (!is.null(pagina)) break

      if (t < max_tentativas) Sys.sleep(2 ^ t)
    }

    if (is.null(pagina)) {
      relatorio$erros <<- relatorio$erros + 1
      cat(sprintf("  ✗ Falhou após %d tentativas.\n", max_tentativas))
      return(tibble())
    }

    # Extrair com helper robusto para a Agência Brasil
    dados <- tryCatch(
      extrair_ab(pagina, secao = secao),
      error = function(e) tibble()
    )

    relatorio$ok <<- relatorio$ok + 1
    cat(sprintf("  ✓ %d notícias encontradas.\n", nrow(dados)))

    if (nrow(dados) == 0) return(tibble())

    dados |> mutate(pagina = p, coletado_em = Sys.time())
  })

  # Limpeza final
  df <- df |>
    mutate(titulo = str_squish(titulo)) |>
    filter(nchar(titulo) > 20) |>
    distinct(link, .keep_all = TRUE)

  # Relatório
  cat(sprintf(
    "\n=== Relatório ===\nPáginas OK: %d | Erros: %d | Observações: %d\n",
    relatorio$ok, relatorio$erros, nrow(df)
  ))

  df
}

# Executar
df_robusto <- scraper_robusto(
  url_base  = "https://agenciabrasil.ebc.com.br/internacional?page=",
  n_paginas = 5,
  secao     = "internacional",
  delay_min = 1.5,
  delay_max = 3.0
)

nrow(df_robusto)

write_csv(df_robusto, "data/processed/corpus_robusto.csv")

# =========================================================
# 14. EXERCÍCIO
# =========================================================
#
# Modifique o seu scraper das aulas anteriores para:
#
# 1. Adicionar tryCatch() em todas as chamadas de rede;
# 2. Usar possibly() na função principal;
# 3. Implementar retry com espera exponencial;
# 4. Criar log de sessão em arquivo .txt;
# 5. Validar o dataset final com validar_dataset();
# 6. Gerar relatório: OK vs erros vs total;
# 7. Salvar resultado com timestamp no nome do arquivo.
#
# =========================================================
# 15. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - tryCatch() para capturar erros sem quebrar loops;
# - possibly() e safely() do purrr;
# - implementar retentativas com backoff exponencial;
# - req_retry() do httr2;
# - criar sistema de logging em arquivo;
# - validar páginas e datasets resultantes;
# - lidar com encoding e caracteres especiais;
# - construir scrapers prontos para produção.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 9: JavaScript e Conteúdo Dinâmico
#
# - por que rvest falha em alguns sites;
# - identificar conteúdo renderizado por JS;
# - usar DevTools > Network para inspecionar;
# - encontrar e usar APIs ocultas;
# - estratégias alternativas ao Selenium.
#
# =========================================================
# FIM DA AULA
# =========================================================
