# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 5: Introdução ao Scraping com rvest
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - a lógica completa do web scraping estático;
# - a usar read_html(), html_elements(), html_text2();
# - a extrair texto, atributos e tabelas;
# - a construir pipelines de coleta;
# - a limpar e estruturar o dataset coletado;
# - a raspar múltiplas páginas com purrr.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Quais temas de política internacional dominam
# a cobertura da Agência Brasil e da ONU?
#
# Podemos construir um dataset de notícias
# para responder isso empiricamente.
#
# =========================================================
# 1. PACOTES
# =========================================================

# Web scraping
library(rvest)

# Manipulação de dados
library(tidyverse)

# Strings
library(stringr)

# =========================================================
# 2. PRIMEIRO SCRAPING: HTML LOCAL
# =========================================================
#
# Antes de raspar sites reais,
# vamos praticar com HTML controlado.
#
# =========================================================

html_noticias <- read_html('
<html>
<body>

<section class="lista-noticias">

  <article class="noticia">
    <h2 class="titulo">
      <a href="/noticias/onu-securidade">ONU discute reforma do Conselho de Segurança</a>
    </h2>
    <time class="data" datetime="2026-05-12">12 mai 2026</time>
    <p class="resumo">Proposta de ampliação inclui Brasil e Índia entre candidatos.</p>
    <span class="categoria">Multilateralismo</span>
  </article>

  <article class="noticia">
    <h2 class="titulo">
      <a href="/noticias/brics-moeda">BRICS avança em discussão sobre moeda comum</a>
    </h2>
    <time class="data" datetime="2026-05-11">11 mai 2026</time>
    <p class="resumo">Reunião em Xangai debateu alternativas ao dólar.</p>
    <span class="categoria">Economia Internacional</span>
  </article>

  <article class="noticia">
    <h2 class="titulo">
      <a href="/noticias/cop30-belem">COP30 em Belém define metas climáticas</a>
    </h2>
    <time class="data" datetime="2026-05-10">10 mai 2026</time>
    <p class="resumo">Países emergentes exigem financiamento climático do Norte.</p>
    <span class="categoria">Meio Ambiente</span>
  </article>

  <article class="noticia">
    <h2 class="titulo">
      <a href="/noticias/russia-esfera">Rússia amplia esfera de influência no Sahel</a>
    </h2>
    <time class="data" datetime="2026-05-09">09 mai 2026</time>
    <p class="resumo">Wagner Group substituído por força estatal russa.</p>
    <span class="categoria">Segurança Internacional</span>
  </article>

</section>

</body>
</html>
')

html_noticias

# =========================================================
# 3. EXTRAINDO TÍTULOS
# =========================================================

titulos <- html_noticias |>
  html_elements(".titulo") |>
  html_text2()

titulos

# =========================================================
# 4. EXTRAINDO LINKS
# =========================================================

links <- html_noticias |>
  html_elements(".titulo a") |>
  html_attr("href")

links

# =========================================================
# 5. EXTRAINDO DATAS
# =========================================================
#
# O atributo datetime contém a data em formato ISO 8601.
# É mais confiável do que o texto visível.
#
# =========================================================

datas <- html_noticias |>
  html_elements("time") |>
  html_attr("datetime")

datas

# =========================================================
# 6. EXTRAINDO CATEGORIAS
# =========================================================

categorias <- html_noticias |>
  html_elements(".categoria") |>
  html_text2()

categorias

# =========================================================
# 7. CONSTRUINDO O DATAFRAME
# =========================================================

df_noticias <- tibble(
  titulo    = titulos,
  link      = links,
  data      = as.Date(datas),
  categoria = categorias
)

df_noticias

# =========================================================
# 8. SCRAPING REAL: AGÊNCIA BRASIL
# =========================================================
#
# A Agência Brasil é um portal público.
# Verifique o robots.txt antes de qualquer scraping.
#
# https://agenciabrasil.ebc.com.br/robots.txt
#
# =========================================================

url_ab <- "https://agenciabrasil.ebc.com.br/internacional"

pagina_ab <- tryCatch(
  {
    Sys.sleep(1)
    read_html(url_ab)
  },
  error = function(e) {
    message("Não foi possível acessar: ", e$message)
    return(NULL)
  }
)

if (!is.null(pagina_ab)) {

  # Inspecionar tags disponíveis
  # (ajuste os seletores após inspeção no navegador)

  # Tentativa com h2
  titulos_ab <- pagina_ab |>
    html_elements("h2") |>
    html_text2() |>
    str_squish()

  head(titulos_ab, 10)

}

# =========================================================
# 9. FUNÇÃO DE EXTRAÇÃO
# =========================================================
#
# Boas práticas:
#
# - encapsular em função;
# - tratar erros com tryCatch;
# - usar Sys.sleep() entre requisições;
# - retornar tibble() em caso de falha.
#
# =========================================================

extrair_noticias <- function(url, seletor_titulo = "h2") {

  # Respeitar o servidor
  Sys.sleep(1.5)

  # Acessar a página
  pagina <- tryCatch(
    read_html(url),
    error = function(e) {
      message("Erro em ", url, ": ", e$message)
      return(NULL)
    }
  )

  # Retornar vazio se falhou
  if (is.null(pagina)) {
    return(tibble(
      titulo   = character(),
      link     = character(),
      url_base = character()
    ))
  }

  # Extrair títulos
  titulos <- pagina |>
    html_elements(seletor_titulo) |>
    html_text2() |>
    str_squish()

  # Extrair links
  links <- pagina |>
    html_elements(paste0(seletor_titulo, " a")) |>
    html_attr("href")

  # Garantir mesmo comprimento
  n <- min(length(titulos), length(links))

  if (n == 0) {
    return(tibble(
      titulo   = character(),
      link     = character(),
      url_base = character()
    ))
  }

  tibble(
    titulo   = titulos[1:n],
    link     = links[1:n],
    url_base = url
  )
}

# Testar
resultado <- extrair_noticias(
  "https://agenciabrasil.ebc.com.br/internacional"
)

resultado

# =========================================================
# 10. SCRAPING DA ONU BRASIL
# =========================================================

url_onu <- "https://brasil.un.org/pt-br/news"

pagina_onu <- tryCatch(
  {
    Sys.sleep(1)
    read_html(url_onu)
  },
  error = function(e) NULL
)

if (!is.null(pagina_onu)) {

  titulos_onu <- pagina_onu |>
    html_elements("h3") |>
    html_text2() |>
    str_squish()

  datas_onu <- pagina_onu |>
    html_elements("time") |>
    html_text2() |>
    str_squish()

  head(titulos_onu, 5)
  head(datas_onu, 5)

}

# =========================================================
# 11. SCRAPING DE TABELAS: WIKIPEDIA
# =========================================================
#
# Wikipedia contém tabelas ricas sobre RI:
#
# - membros de organizações internacionais;
# - lista de conflitos;
# - acordos bilaterais;
# - histórico de sanções.
#
# =========================================================

url_wiki_otan <- "https://pt.wikipedia.org/wiki/OTAN"

pagina_otan <- tryCatch(
  {
    Sys.sleep(1)
    read_html(url_wiki_otan)
  },
  error = function(e) NULL
)

if (!is.null(pagina_otan)) {

  tabelas <- pagina_otan |>
    html_table(fill = TRUE)

  # Quantas tabelas?
  length(tabelas)

  # Ver a primeira
  head(tabelas[[1]])

}

# =========================================================
# 12. LIMPEZA DE TEXTO
# =========================================================
#
# Textos extraídos precisam de limpeza:
#
# - str_squish()      → remove espaços extras e \n
# - str_trim()        → remove espaços nas bordas
# - str_remove()      → remove padrão
# - str_remove_all()  → remove todas as ocorrências
# - str_replace()     → substitui padrão
# - str_to_title()    → capitalizar
#
# =========================================================

titulos_sujos <- c(
  "  \n DIPLOMACIA brasileira \t",
  "ONU debate crise\n\n",
  "  China supera EUA em exportações   "
)

# Limpar
titulos_limpos <- str_squish(titulos_sujos)

titulos_limpos

# Remover texto indesejado
titulos_sem_prefixo <- str_remove(
  titulos_limpos,
  "^DIPLOMACIA "
)

titulos_sem_prefixo

# =========================================================
# 13. SCRAPING DE MÚLTIPLAS PÁGINAS
# =========================================================
#
# Muitos portais paginam conteúdo.
#
# Estratégia:
#
# 1. Identificar o padrão de URL das páginas;
# 2. Gerar vetor de URLs;
# 3. Iterar com purrr::map_dfr();
# 4. Combinar os resultados.
#
# =========================================================

# Gerar URLs de paginação
urls_agencia_brasil <- paste0(
  "https://agenciabrasil.ebc.com.br/internacional?page=",
  0:2   # páginas 0, 1 e 2
)

urls_agencia_brasil

# Iterar sobre as páginas
df_multiplas <- purrr::map_dfr(
  urls_agencia_brasil,
  extrair_noticias
)

# Ver resultado combinado
df_multiplas

# Quantas notícias coletamos?
nrow(df_multiplas)

# =========================================================
# 14. DEDUPLICAR E NORMALIZAR
# =========================================================

df_limpo <- df_multiplas |>

  # Remover duplicatas por título
  distinct(titulo, .keep_all = TRUE) |>

  # Remover linhas com título muito curto (menus, etc.)
  filter(nchar(titulo) > 15) |>

  # Adicionar timestamp de coleta
  mutate(
    coletado_em = Sys.time()
  )

df_limpo

# =========================================================
# 15. ANALISANDO O DATASET
# =========================================================

# Quantas notícias?
nrow(df_limpo)

# Palavras mais frequentes nos títulos
palavras <- df_limpo |>
  pull(titulo) |>
  str_to_lower() |>
  str_split("\\s+") |>
  unlist() |>
  str_remove_all("[[:punct:]]") |>
  tibble(palavra = _) |>
  filter(nchar(palavra) > 4) |>
  count(palavra, sort = TRUE)

head(palavras, 20)

# =========================================================
# 16. VISUALIZAÇÃO
# =========================================================

palavras |>
  slice_head(n = 15) |>
  ggplot(
    aes(
      x = reorder(palavra, n),
      y = n
    )
  ) +
  geom_col(fill = "#003366") +
  coord_flip() +
  labs(
    title    = "Palavras mais frequentes",
    subtitle = "Títulos de notícias internacionais",
    x        = "Palavra",
    y        = "Frequência",
    caption  = "Fonte: Agência Brasil"
  ) +
  theme_minimal()

# =========================================================
# 17. SALVANDO O DATASET
# =========================================================

write_csv(
  df_limpo,
  "noticias_internacionais.csv"
)

message("Dataset salvo: ", nrow(df_limpo), " notícias.")

# =========================================================
# 18. QUANDO RVEST NÃO FUNCIONA
# =========================================================
#
# Alguns sites usam JavaScript para carregar conteúdo.
#
# Sintoma:
#
# read_html() retorna HTML "vazio" ou com divs vazias.
#
# Sinal de alerta:
#
# Se html_elements() retornar character(0) para
# seletores que funcionam no navegador,
# o conteúdo é provavelmente dinâmico.
#
# Solução: RSelenium (Aulas 9 e 10).
#
# =========================================================

# Diagnóstico simples
diagnosticar_pagina <- function(url) {

  pagina <- tryCatch(
    read_html(url),
    error = function(e) return(NULL)
  )

  if (is.null(pagina)) {
    return("Falha na requisição")
  }

  n_elementos <- pagina |>
    html_elements("article, .noticia, .post, h2, h3") |>
    length()

  n_scripts <- pagina |>
    html_elements("script") |>
    length()

  cat(
    "Elementos encontrados:", n_elementos, "\n",
    "Scripts JS na página:", n_scripts, "\n"
  )

  if (n_elementos == 0 && n_scripts > 5) {
    cat("⚠ Provável conteúdo dinâmico — use RSelenium.\n")
  } else {
    cat("✓ Conteúdo estático — rvest deve funcionar.\n")
  }
}

# Testar
diagnosticar_pagina("https://agenciabrasil.ebc.com.br/internacional")

# =========================================================
# 19. EXERCÍCIO
# =========================================================
#
# Construa um dataset de notícias internacionais
# com pelo menos 50 observações.
#
# Passos:
#
# 1. Escolha um portal:
#    - https://agenciabrasil.ebc.com.br/internacional
#    - https://brasil.un.org/pt-br/news
#    - https://www.bbc.com/portuguese/internacional
#
# 2. Inspecione a estrutura HTML (F12);
#
# 3. Identifique os seletores de título e link;
#
# 4. Escreva a função de extração;
#
# 5. Raspe pelo menos 3 páginas;
#
# 6. Limpe o texto;
#
# 7. Salve em CSV;
#
# 8. Identifique os 10 termos mais frequentes.
#
# =========================================================
# 20. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - a lógica completa do web scraping;
# - read_html() para carregar páginas;
# - html_elements() e html_element();
# - html_text2() para extrair texto;
# - html_attr() para extrair atributos;
# - html_table() para extrair tabelas;
# - como limpar texto com stringr;
# - como raspar múltiplas páginas com purrr;
# - como diagnosticar páginas dinâmicas;
# - como construir um dataset de notícias.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 6: Estruturando Dados
#
# Vamos transformar listas de scraping em datasets:
#
# - listas → dataframes;
# - limpeza de texto avançada;
# - normalização de datas;
# - construção de corpus com 100+ observações;
# - exportação para análise.
#
# =========================================================
# FIM DA AULA
# =========================================================
