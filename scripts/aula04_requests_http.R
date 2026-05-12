# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 4: Requests e Respostas HTTP
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - como funciona o protocolo HTTP;
# - como fazer requisições GET com httr2;
# - como inspecionar status codes e headers;
# - como consumir APIs públicas;
# - como lidar com rate limiting e erros;
# - como conectar dados de APIs com pesquisa em RI.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Quais padrões econômicos distinguem os países do BRICS?
#
# Usaremos a API do Banco Mundial para investigar:
#
# - PIB;
# - exportações;
# - gastos militares.
#
# =========================================================
# 1. PACOTES
# =========================================================

# Fazer requisições HTTP
library(httr2)

# Trabalhar com JSON
library(jsonlite)

# Manipulação de dados
library(dplyr)
library(tidyr)

# Visualização
library(ggplot2)

# =========================================================
# 2. ANATOMIA DE UMA URL
# =========================================================
#
# URL = Uniform Resource Locator
#
# Partes de uma URL:
#
# https://api.worldbank.org/v2/country/BRA/indicator/NY.GDP.MKTP.CD?format=json&per_page=100
#  │         │                   │                    │               │
#  │         │                   │                    │               └── query string
#  │         │                   │                    └── path do recurso
#  │         │                   └── endpoint
#  │         └── host (servidor)
#  └── protocolo (https)
#
# Query string: parâmetros separados por &
#
# ?format=json → retornar JSON
# &per_page=100 → até 100 resultados
#
# =========================================================
# 3. PRIMEIRA REQUISIÇÃO HTTP
# =========================================================
#
# Vamos buscar o PIB do Brasil.
#
# Indicador NY.GDP.MKTP.CD = PIB em US$ correntes
#
# =========================================================

url_brasil <- paste0(
  "https://api.worldbank.org/v2/country/",
  "BRA",
  "/indicator/",
  "NY.GDP.MKTP.CD",
  "?format=json&per_page=100"
)

# Ver a URL construída
url_brasil

# Fazer a requisição
resp_brasil <- request(url_brasil) |>

  # Identificar quem somos (boa prática ética)
  req_user_agent("Curso Web Scraping em RI / pesquisa academica") |>

  # Tentar até 3 vezes em caso de falha temporária
  req_retry(max_tries = 3) |>

  # Enviar a requisição
  req_perform()

# =========================================================
# 4. INSPECIONANDO A RESPOSTA
# =========================================================
#
# O objeto de resposta contém:
#
# - status code (200 = sucesso)
# - headers (metadados)
# - body (os dados)
#
# =========================================================

# Status code
resp_status(resp_brasil)

# Descrição do status
resp_status_desc(resp_brasil)

# =========================================================
# 5. HEADERS DA RESPOSTA
# =========================================================
#
# Headers informam:
#
# - tipo de conteúdo (Content-Type)
# - tamanho da resposta
# - servidor
# - informações de cache
#
# =========================================================

# Ver os headers
resp_headers(resp_brasil)

# Um header específico
resp_header(resp_brasil, "Content-Type")

# =========================================================
# 6. CORPO DA RESPOSTA
# =========================================================
#
# A API retorna JSON.
#
# JSON é um formato de dados estruturado:
#
# {
#   "key": "value",
#   "lista": [1, 2, 3]
# }
#
# =========================================================

# Extrair como texto
json_texto <- resp_brasil |>
  resp_body_string()

# Ver as primeiras 500 caracteres
substr(json_texto, 1, 500)

# =========================================================
# 7. CONVERTENDO JSON PARA R
# =========================================================
#
# fromJSON() converte JSON em objetos R.
#
# A API do Banco Mundial retorna uma lista de dois elementos:
#
# [[1]] = metadados da requisição
# [[2]] = os dados em si
#
# =========================================================

dados <- fromJSON(json_texto)

# Estrutura geral
str(dados, max.level = 2)

# Metadados
dados[[1]]

# Dados (primeiras linhas)
head(dados[[2]])

# =========================================================
# 8. LIMPANDO OS DADOS
# =========================================================

pib_brasil <- dados[[2]] |>

  # Remover anos sem valor
  filter(!is.na(value)) |>

  mutate(
    ano         = as.integer(date),
    pib_trilhoes = value / 1e12
  ) |>

  select(
    ano,
    pib_trilhoes
  ) |>

  filter(ano >= 2000) |>

  arrange(ano)

pib_brasil

# =========================================================
# 9. VISUALIZANDO O PIB DO BRASIL
# =========================================================

ggplot(
  pib_brasil,
  aes(x = ano, y = pib_trilhoes)
) +

  geom_line(size = 1.2, color = "#006600") +
  geom_point(size = 2.5, color = "#006600") +

  labs(
    title    = "PIB do Brasil",
    subtitle = "2000–2024, em trilhões de USD",
    x        = "Ano",
    y        = "PIB (trilhões de USD)",
    caption  = "Fonte: Banco Mundial"
  ) +

  theme_minimal()

# =========================================================
# 10. CRIANDO UMA FUNÇÃO REUTILIZÁVEL
# =========================================================
#
# Boa prática: encapsular a lógica em uma função.
#
# Recebe: código do país e código do indicador.
# Retorna: dataframe pronto.
#
# =========================================================

buscar_indicador <- function(
    pais,
    indicador,
    ano_inicio = 2000
) {

  # Construir URL
  url <- paste0(
    "https://api.worldbank.org/v2/country/",
    pais,
    "/indicator/",
    indicador,
    "?format=json&per_page=100"
  )

  # Fazer requisição com tratamento de erro
  resp <- tryCatch(
    {
      request(url) |>
        req_user_agent("Curso Web Scraping em RI") |>
        req_retry(max_tries = 3) |>
        req_perform()
    },
    error = function(e) {
      message("Erro ao acessar ", url, ": ", e$message)
      return(NULL)
    }
  )

  # Retornar vazio se falhou
  if (is.null(resp)) {
    return(tibble(
      ano = integer(), pais = character(), valor = numeric()
    ))
  }

  # Verificar status
  if (resp_status(resp) != 200) {
    message("Status ", resp_status(resp), " para ", pais)
    return(tibble(
      ano = integer(), pais = character(), valor = numeric()
    ))
  }

  # Converter JSON
  dados <- resp |>
    resp_body_string() |>
    fromJSON()

  # Verificar estrutura
  if (length(dados) < 2 || is.null(dados[[2]])) {
    return(tibble(
      ano = integer(), pais = character(), valor = numeric()
    ))
  }

  # Limpar e retornar
  dados[[2]] |>
    filter(!is.na(value)) |>
    mutate(
      ano  = as.integer(date),
      pais = country$value,
      valor = value
    ) |>
    select(ano, pais, valor) |>
    filter(ano >= ano_inicio) |>
    arrange(ano)
}

# =========================================================
# 11. CONSULTANDO MÚLTIPLOS PAÍSES
# =========================================================
#
# BRICS: Brasil, Rússia, Índia, China, África do Sul
#
# Códigos ISO:
# BRA, RUS, IND, CHN, ZAF
#
# =========================================================

paises_brics <- c("BRA", "RUS", "IND", "CHN", "ZAF")

# Esperar entre requisições (boa prática)
pib_brics <- purrr::map_dfr(
  paises_brics,
  function(p) {
    Sys.sleep(0.5)
    buscar_indicador(p, "NY.GDP.MKTP.CD")
  }
)

pib_brics

# =========================================================
# 12. PADRONIZANDO OS DADOS
# =========================================================

pib_brics <- pib_brics |>
  mutate(pib_trilhoes = valor / 1e12)

pib_brics

# =========================================================
# 13. VISUALIZAÇÃO COMPARATIVA
# =========================================================

ggplot(
  pib_brics,
  aes(
    x     = ano,
    y     = pib_trilhoes,
    color = pais
  )
) +

  geom_line(size = 1.1) +
  geom_point(size = 2) +

  labs(
    title    = "PIB dos Países do BRICS",
    subtitle = "2000–2024, em trilhões de USD",
    x        = "Ano",
    y        = "PIB (trilhões de USD)",
    color    = "País",
    caption  = "Fonte: Banco Mundial"
  ) +

  theme_minimal() +

  theme(
    legend.position = "bottom"
  )

# =========================================================
# 14. MÚLTIPLOS INDICADORES
# =========================================================
#
# Indicadores do Banco Mundial:
#
# PIB:            NY.GDP.MKTP.CD
# Exportações:    NE.EXP.GNFS.CD
# Importações:    NE.IMP.GNFS.CD
# Gasto militar:  MS.MIL.XPND.CD
# População:      SP.POP.TOTL
#
# =========================================================

# Exportações do Brasil
exportacoes_brasil <- buscar_indicador(
  "BRA",
  "NE.EXP.GNFS.CD"
)

Sys.sleep(1)

# Gasto militar do Brasil
militar_brasil <- buscar_indicador(
  "BRA",
  "MS.MIL.XPND.CD"
)

exportacoes_brasil |>
  mutate(exp_bilhoes = valor / 1e9)

militar_brasil |>
  mutate(mil_bilhoes = valor / 1e9)

# =========================================================
# 15. STATUS CODES NA PRÁTICA
# =========================================================
#
# Vamos intencionalmente testar diferentes situações.
#
# =========================================================

# URL válida → 200
resp_ok <- request(
  "https://api.worldbank.org/v2/country/BRA?format=json"
) |>
  req_perform()

resp_status(resp_ok)

# URL de país inválido → pode retornar 200 com dados vazios
resp_invalido <- request(
  "https://api.worldbank.org/v2/country/XYZ?format=json"
) |>
  req_perform()

resp_status(resp_invalido)

# =========================================================
# 16. INSPECIONANDO UMA API DE NOTICIAS INTERNACIONAIS
# =========================================================
#
# The Guardian Open Platform
#
# API gratuita de um dos maiores jornais do mundo.
# Cobertura extensa de temas de RI: diplomacia, conflitos,
# política internacional, clima, economia global.
#
# Chave "test" permite buscas sem cadastro (limite de taxa).
# Para uso intensivo: registre-se em:
# https://open-platform.theguardian.com/access/
#
# =========================================================

url_guardian <- paste0(
  "https://content.guardianapis.com/search",
  "?q=Brazil+diplomacy",
  "&show-fields=headline,trailText,webPublicationDate",
  "&page-size=10",
  "&api-key=test"
)

resp_guardian <- tryCatch(
  {
    request(url_guardian) |>
      req_user_agent("Curso Web Scraping em RI") |>
      req_timeout(10) |>
      req_perform()
  },
  error = function(e) NULL
)

if (!is.null(resp_guardian) && resp_status(resp_guardian) == 200) {

  guardian_dados <- resp_guardian |>
    resp_body_string() |>
    fromJSON()

  # Estrutura da resposta
  str(guardian_dados$response, max.level = 2)

  # Tabela com manchetes e datas
  resultados <- guardian_dados$response$results
  resultados[, c("webTitle", "webPublicationDate", "webUrl")]

}

# =========================================================
# 17. EXERCÍCIO
# =========================================================
#
# Escolha um cenário de pesquisa em RI:
#
# A) Gastos militares após 2014 (Ucrânia / Crimeia)
#    Países: RUS, UKR, POL, DEU, USA
#    Indicador: MS.MIL.XPND.CD
#
# B) PIB per capita dos BRICS vs G7
#    Países: BRA, CHN, IND + USA, DEU, JPN
#    Indicador: NY.GDP.PCAP.CD
#
# C) Comércio após COVID-19
#    Países: dois à sua escolha
#    Indicadores: NE.EXP.GNFS.CD + NE.IMP.GNFS.CD
#
# Para cada análise:
#
# 1. Construa a URL;
# 2. Faça a requisição;
# 3. Verifique o status code;
# 4. Limpe os dados;
# 5. Visualize;
# 6. Interprete.
#
# =========================================================
# 18. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - o que é HTTP e como funciona;
# - GET vs POST;
# - status codes (200, 403, 404, 429, 500);
# - headers e User-Agent;
# - como fazer requisições com httr2;
# - como inspecionar respostas;
# - como converter JSON em dataframes;
# - como criar funções reutilizáveis;
# - como respeitar rate limits;
# - como aplicar tudo isso em RI.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Agora que entendemos como a web funciona,
# vamos aprender a raspar páginas HTML diretamente.
#
# Aula 5: Introdução ao Scraping com rvest
#
# - read_html();
# - html_elements() e html_element();
# - html_text2() e html_attr();
# - scraping de listas de notícias;
# - construção de datasets.
#
# =========================================================
# FIM DA AULA
# =========================================================
