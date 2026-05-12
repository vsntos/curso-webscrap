# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula: APIs, Comércio Internacional e Sanções Econômicas
# =========================================================
#
# OBJETIVO DA AULA
#
# Aprender:
#
# - o que é uma API;
# - como coletar dados internacionais;
# - como trabalhar com JSON;
# - como construir séries temporais;
# - como analisar efeitos de sanções econômicas.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# As sanções ocidentais contra a Rússia produziram
# efeitos duradouros sobre sua capacidade exportadora?
#
# =========================================================
# OBSERVAÇÃO METODOLÓGICA
# =========================================================
#
# NÃO vamos parar em 2022.
#
# Por quê?
#
# Porque:
#
# - 2022 representa apenas o choque inicial;
# - efeitos econômicos possuem defasagem;
# - países podem adaptar suas estratégias;
# - a Rússia redirecionou comércio para Ásia.
#
# Isso nos ajuda a pensar:
#
# - causalidade;
# - adaptação estratégica;
# - resiliência econômica;
# - limites das sanções.
#
# =========================================================
# 1. PACOTES
# =========================================================

# Comunicação com APIs
library(httr2)

# Trabalhar com JSON
library(jsonlite)

# Manipulação de dados
library(dplyr)

# Visualização
library(ggplot2)

# =========================================================
# 2. O QUE É UMA API?
# =========================================================
#
# API = Application Programming Interface
#
# APIs permitem comunicação entre sistemas.
#
# Em vez de baixar planilhas manualmente,
# fazemos pedidos diretamente ao servidor.
#
# Hoje usaremos a API do Banco Mundial:
#
# https://api.worldbank.org
#
# =========================================================
# 3. PRIMEIRA REQUISIÇÃO
# =========================================================

# Código ISO:
# RUS = Rússia

# Indicador:
# NE.EXP.GNFS.CD
#
# Exportações de bens e serviços
# (US$ correntes)

url <- paste0(
  "https://api.worldbank.org/v2/country/",
  "RUS",
  "/indicator/",
  "NE.EXP.GNFS.CD",
  "?format=json&per_page=100"
)

# Visualizar URL
url

# =========================================================
# 4. ENVIANDO A REQUISIÇÃO
# =========================================================

resp <- request(url) |>
  
  # Identifica quem acessa a API
  req_user_agent("Curso de Web Scraping em RI") |>
  
  # Tentar novamente em caso de erro
  req_retry(max_tries = 3) |>
  
  # Enviar requisição
  req_perform()

# Status HTTP
resp_status(resp)

# =========================================================
# 5. TRANSFORMANDO JSON EM DADOS
# =========================================================

dados_json <- resp |>
  resp_body_string()

dados <- fromJSON(dados_json)

# =========================================================
# 6. ENTENDENDO A ESTRUTURA
# =========================================================
#
# A resposta possui:
#
# [[1]] = metadados
# [[2]] = dados
#
# =========================================================

str(dados, max.level = 2)

# Metadados
dados[[1]]

# Primeiras linhas
head(dados[[2]])

# =========================================================
# 7. LIMPEZA DOS DADOS
# =========================================================

exportacoes_russia <- dados[[2]] |>
  
  # Remover valores ausentes
  filter(!is.na(value)) |>
  
  mutate(
    
    # Converter ano para número
    ano = as.integer(date),
    
    # Converter para bilhões de dólares
    exportacoes_bilhoes = value / 1e9
    
  ) |>
  
  select(
    ano,
    exportacoes_bilhoes
  ) |>
  
  # Manter período desejado
  filter(ano >= 2015) |>
  
  arrange(ano)

# Visualizar resultado
exportacoes_russia

# =========================================================
# 8. VISUALIZAÇÃO
# =========================================================
#
# Vamos destacar:
#
# - o início da guerra em 2022;
# - o período pós-sanções.
#
# =========================================================

ggplot(
  exportacoes_russia,
  aes(
    x = ano,
    y = exportacoes_bilhoes
  )
) +
  
  # Área pós-2022
  annotate(
    "rect",
    xmin = 2022,
    xmax = max(exportacoes_russia$ano),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.1,
    fill = "red"
  ) +
  
  geom_line(
    size = 1.3,
    color = "#003399"
  ) +
  
  geom_point(
    size = 3,
    color = "#003399"
  ) +
  
  geom_vline(
    xintercept = 2022,
    linetype = "dashed",
    color = "red"
  ) +
  
  annotate(
    "text",
    x = 2022.2,
    y = max(exportacoes_russia$exportacoes_bilhoes) * 0.95,
    label = "Início das sanções\nocidentais",
    hjust = 0,
    color = "red"
  ) +
  
  labs(
    title = "Exportações da Rússia",
    subtitle = "Antes e depois das sanções de 2022",
    x = "Ano",
    y = "Bilhões de dólares",
    caption = "Fonte: Banco Mundial"
  ) +
  
  theme_minimal()

# =========================================================
# 9. INTERPRETAÇÃO
# =========================================================
#
# Perguntas importantes:
#
# - Houve queda imediata?
# - Houve recuperação?
# - O impacto foi duradouro?
# - A Rússia encontrou novos mercados?
# - Sanções econômicas funcionam?
#
# =========================================================
# 10. LIMITAÇÕES IMPORTANTES
# =========================================================
#
# ESTE INDICADOR MOSTRA:
#
# - exportações totais da economia
#
# ELE NÃO MOSTRA:
#
# - destino das exportações;
# - comércio bilateral;
# - setores específicos;
# - petróleo vs manufaturas.
#
# Portanto:
#
# Mesmo que as exportações totais permaneçam altas,
# a estrutura do comércio pode ter mudado profundamente.
#
# Exemplo:
#
# - queda nas exportações para Europa;
# - aumento das exportações para China e Índia.
#
# =========================================================
# 11. CRIANDO UMA FUNÇÃO
# =========================================================

buscar_exportacoes <- function(
    pais,
    ano_inicio = 2015
) {
  
  # Construir URL
  url <- paste0(
    "https://api.worldbank.org/v2/country/",
    pais,
    "/indicator/NE.EXP.GNFS.CD",
    "?format=json&per_page=100"
  )
  
  # Fazer requisição
  resp <- request(url) |>
    req_user_agent("Curso de Web Scraping em RI") |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  # Converter resposta
  dados <- resp |>
    resp_body_string() |>
    fromJSON()
  
  # Verificar existência dos dados
  if(length(dados) < 2 || is.null(dados[[2]])) {
    
    return(
      tibble(
        ano = integer(),
        pais = character(),
        exportacoes_bilhoes = numeric()
      )
    )
    
  }
  
  # Limpeza
  resultado <- dados[[2]] |>
    
    filter(!is.na(value)) |>
    
    mutate(
      ano = as.integer(date),
      exportacoes_bilhoes = value / 1e9,
      pais = country$value
    ) |>
    
    select(
      ano,
      pais,
      exportacoes_bilhoes
    ) |>
    
    filter(ano >= ano_inicio) |>
    
    arrange(ano)
  
  return(resultado)
}

# =========================================================
# 12. COMPARANDO PAÍSES
# =========================================================
#
# Vamos comparar:
#
# - Rússia
# - China
# - Brasil
#
# =========================================================

russia <- buscar_exportacoes("RUS")
china  <- buscar_exportacoes("CHN")
brasil <- buscar_exportacoes("BRA")

# Combinar bases
comparacao <- bind_rows(
  russia,
  china,
  brasil
)

# Visualizar
comparacao

# =========================================================
# 13. VISUALIZAÇÃO COMPARATIVA
# =========================================================

ggplot(
  comparacao,
  aes(
    x = ano,
    y = exportacoes_bilhoes,
    color = pais
  )
) +
  
  annotate(
    "rect",
    xmin = 2022,
    xmax = max(comparacao$ano),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.08,
    fill = "red"
  ) +
  
  geom_line(size = 1.2) +
  
  geom_point(size = 2) +
  
  geom_vline(
    xintercept = 2022,
    linetype = "dashed",
    color = "black"
  ) +
  
  labs(
    title = "Exportações Comparadas",
    subtitle = "Rússia, China e Brasil",
    x = "Ano",
    y = "Bilhões de dólares",
    color = "País",
    caption = "Fonte: Banco Mundial"
  ) +
  
  scale_color_manual(
    values = c(
      "Russia" = "#003399",
      "China"  = "#CC0000",
      "Brazil" = "#006600"
    )
  ) +
  
  theme_minimal()

# =========================================================
# 14. INTERPRETAÇÃO COMPARATIVA
# =========================================================
#
# Perguntas:
#
# - O padrão russo difere dos demais?
# - A Rússia sofreu mais?
# - O choque foi temporário?
# - Países emergentes responderam de forma parecida?
#
# =========================================================
# 15. OUTROS INDICADORES
# =========================================================
#
# PIB:
# NY.GDP.MKTP.CD
#
# Importações:
# NE.IMP.GNFS.CD
#
# População:
# SP.POP.TOTL
#
# Gastos militares:
# MS.MIL.XPND.CD
#
# =========================================================
# 16. EXERCÍCIO
# =========================================================
#
# Escolha um país e investigue:
#
# - exportações;
# - PIB;
# - população;
# - gastos militares.
#
# Sugestões:
#
# USA = Estados Unidos
# IND = Índia
# IRN = Irã
# TUR = Turquia
#
# Pergunta:
#
# O comportamento econômico mudou após 2020?
#
# =========================================================
# 17. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - o que é uma API;
# - como acessar dados internacionais;
# - como trabalhar com JSON;
# - como construir gráficos;
# - como pensar causalidade;
# - como analisar sanções econômicas;
# - como conectar métodos computacionais
#   à pesquisa em Relações Internacionais.
#
# =========================================================
# FIM DA AULA
# =========================================================

