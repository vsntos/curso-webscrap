# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 1: RSS, Google News e Monitoramento Internacional
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - o que é RSS;
# - como coletar notícias automaticamente;
# - como monitorar temas internacionais;
# - como construir pipelines de coleta;
# - como transformar notícias em dados.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Como podemos monitorar automaticamente
# temas internacionais em larga escala?
#
# Exemplo:
#
# - diplomacia;
# - conflitos;
# - eleições;
# - sanções;
# - líderes políticos.
#
# =========================================================
# O QUE É RSS?
# =========================================================
#
# RSS = Really Simple Syndication
#
# RSS é um formato usado por sites de notícias
# para distribuir atualizações automaticamente.
#
# Em vez de visitar manualmente dezenas de sites,
# podemos coletar notícias automaticamente.
#
# Isso é extremamente útil para:
#
# - monitoramento internacional;
# - análise de mídia;
# - pesquisa em RI;
# - inteligência política;
# - construção de datasets.
#
# =========================================================
# 1. PACOTES
# =========================================================

# Manipulação de dados
library(tidyverse)

# Leitura de XML
library(xml2)

# Web scraping
library(rvest)

# Trabalhar com datas
library(lubridate)

# =========================================================
# 2. ENTENDENDO O GOOGLE NEWS RSS
# =========================================================
#
# O Google News possui feeds RSS para buscas.
#
# Exemplo:
#
# https://news.google.com/rss/search?q=Trump
#
# A ideia:
#
# Fazemos uma busca →
# Google retorna notícias em formato RSS →
# Transformamos em dataframe.
#
# =========================================================
# 3. PRIMEIRA URL RSS
# =========================================================

query <- "Trump"

url <- paste0(
  "https://news.google.com/rss/search?q=",
  URLencode(query),
  "&hl=pt-BR&gl=BR&ceid=BR:pt-419"
)

# Visualizar URL
url

# =========================================================
# 4. LENDO O RSS
# =========================================================

rss <- read_xml(url)

# Visualizar estrutura XML
rss

# =========================================================
# 5. EXTRAINDO ITENS
# =========================================================
#
# Cada notícia aparece como:
#
# <item>
#
# Vamos extrair todos os itens.
#
# =========================================================

items <- xml_find_all(rss, "//item")

# Quantas notícias foram encontradas?
length(items)

# =========================================================
# 6. EXTRAINDO TÍTULOS
# =========================================================

titulos <- xml_text(
  xml_find_first(items, "title")
)

head(titulos)

# =========================================================
# 7. EXTRAINDO LINKS
# =========================================================

links <- xml_text(
  xml_find_first(items, "link")
)

head(links)

# =========================================================
# 8. EXTRAINDO DATAS
# =========================================================

datas <- xml_text(
  xml_find_first(items, "pubDate")
)

head(datas)

# =========================================================
# 9. CONSTRUINDO UM DATAFRAME
# =========================================================

df_exemplo <- tibble(
  titulo   = titulos,
  link     = links,
  pub_date = datas
)

df_exemplo

# =========================================================
# 10. CRIANDO UMA FUNÇÃO
# =========================================================
#
# Agora vamos automatizar o processo.
#
# A função:
#
# - recebe uma busca;
# - acessa o RSS;
# - coleta notícias;
# - devolve um dataframe.
#
# =========================================================

get_google_news <- function(query) {
  
  # Construir URL RSS
  url <- paste0(
    "https://news.google.com/rss/search?q=",
    URLencode(query),
    "&hl=pt-BR&gl=BR&ceid=BR:pt-419"
  )
  
  # Ler RSS
  rss <- tryCatch(
    read_xml(url),
    error = function(e) NULL
  )
  
  # Caso falhe
  if (is.null(rss)) {
    return(tibble())
  }
  
  # Extrair notícias
  items <- xml_find_all(rss, "//item")
  
  # Caso não encontre notícias
  if (length(items) == 0) {
    return(tibble())
  }
  
  # Ajustar locale para datas em inglês
  lc_orig <- Sys.getlocale("LC_TIME")
  
  Sys.setlocale("LC_TIME", "C")
  
  on.exit(
    Sys.setlocale("LC_TIME", lc_orig)
  )
  
  # Construir dataframe
  tibble(
    
    titulo = xml_text(
      xml_find_first(items, "title")
    ),
    
    link = xml_text(
      xml_find_first(items, "link")
    ),
    
    pub_date = xml_text(
      xml_find_first(items, "pubDate")
    ),
    
    fonte = xml_text(
      xml_find_first(items, "source")
    ),
    
    busca = query
    
  ) %>%
    
    mutate(
      
      # Converter datas
      pub_date = as.POSIXct(
        strptime(
          sub(" [A-Z]+$", "", pub_date),
          "%a, %d %b %Y %H:%M:%S",
          tz = "UTC"
        )
      )
      
    )
}

# =========================================================
# 11. TESTANDO A FUNÇÃO
# =========================================================

noticias_trump <- get_google_news("Trump")

noticias_trump

# =========================================================
# 12. MÚLTIPLAS BUSCAS
# =========================================================
#
# Agora vamos monitorar múltiplos temas.
#
# =========================================================

queries <- c(
  "diplomacia",
  "Irã",
  "Trump",
  "China",
  "Ucrânia"
)

queries

# =========================================================
# 13. COLETANDO TODAS AS NOTÍCIAS
# =========================================================

df_news <- purrr::map_dfr(
  queries,
  get_google_news
)

# Visualizar
df_news

# =========================================================
# 14. FILTRANDO PERÍODO
# =========================================================
#
# O RSS retorna notícias recentes.
#
# Podemos filtrar por data
# diretamente no R.
#
# =========================================================

df_news <- df_news %>%
  
  filter(
    
    !is.na(pub_date),
    
    pub_date >= as.POSIXct(
      "2025-01-01",
      tz = "UTC"
    ),
    
    pub_date <= as.POSIXct(
      "2026-12-31",
      tz = "UTC"
    )
    
  )

# Visualizar
df_news

# =========================================================
# 15. REMOVENDO DUPLICATAS
# =========================================================
#
# Uma notícia pode aparecer
# em múltiplas buscas.
#
# Exemplo:
#
# "Trump" + "diplomacia"
#
# Vamos remover duplicatas.
#
# =========================================================

df_news <- df_news %>%
  
  distinct(
    link,
    .keep_all = TRUE
  )

# =========================================================
# 16. EXPLORANDO OS DADOS
# =========================================================

glimpse(df_news)

# =========================================================
# 17. CONTAGEM POR TEMA
# =========================================================

df_news %>%
  count(busca, sort = TRUE)

# =========================================================
# 18. VISUALIZAÇÃO
# =========================================================

df_news %>%
  
  count(busca) %>%
  
  ggplot(
    aes(
      x = reorder(busca, n),
      y = n
    )
  ) +
  
  geom_col(fill = "#003366") +
  
  coord_flip() +
  
  labs(
    title = "Número de notícias por tema",
    subtitle = "Google News RSS",
    x = "Tema",
    y = "Número de notícias",
    caption = "Fonte: Google News RSS"
  ) +
  
  theme_minimal()

# =========================================================
# 19. ANÁLISE SUBSTANTIVA
# =========================================================
#
# Perguntas importantes:
#
# - Quais temas recebem maior cobertura?
# - Quais eventos internacionais dominam o noticiário?
# - Como a cobertura muda ao longo do tempo?
# - Existem picos de atenção?
# - A mídia brasileira cobre certos países mais?
#
# =========================================================
# 20. LIMITAÇÕES IMPORTANTES
# =========================================================
#
# RSS NÃO É O UNIVERSO COMPLETO DA MÍDIA.
#
# O feed depende:
#
# - do algoritmo do Google;
# - da indexação das notícias;
# - da língua;
# - da região.
#
# Além disso:
#
# - notícias podem desaparecer;
# - resultados mudam ao longo do tempo;
# - o RSS retorna apenas notícias recentes.
#
# =========================================================
# 21. SALVANDO OS DADOS
# =========================================================

write_csv(
  df_news,
  "noticias_google_news.csv"
)

# =========================================================
# 22. EXERCÍCIO
# =========================================================
#
# Escolha um tema internacional:
#
# - eleições;
# - China;
# - ONU;
# - OTAN;
# - Amazônia;
# - imigração;
# - comércio internacional.
#
# Depois:
#
# - colete notícias;
# - filtre por período;
# - visualize resultados;
# - interprete padrões.
#
# =========================================================
# 23. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - o que é RSS;
# - como coletar notícias automaticamente;
# - como trabalhar com XML;
# - como construir pipelines;
# - como monitorar temas internacionais;
# - como transformar notícias em dados.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Próximo passo:
#
# Web scraping de HTML com rvest.
#
# Vamos aprender:
#
# - CSS selectors;
# - scraping de páginas;
# - coleta de tabelas;
# - coleta de textos;
# - automação de monitoramento.
#
# =========================================================
# FIM DA AULA
# =========================================================


