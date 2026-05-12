# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 3: HTML, CSS e Estrutura de Páginas
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - o que é HTML e como ele se estrutura;
# - as principais tags e atributos;
# - como usar CSS selectors para localizar elementos;
# - como extrair conteúdo de páginas com rvest;
# - como inspecionar e mapear a estrutura de um site.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Como encontramos dados dentro de uma página web?
#
# Exemplo:
#
# - quais são os títulos das últimas notícias?
# - quando foram publicadas?
# - quais são os links para cada matéria?
#
# =========================================================
# 1. PACOTES
# =========================================================

# Web scraping
library(rvest)

# Manipulação de dados
library(tidyverse)

# =========================================================
# 2. O QUE É HTML?
# =========================================================
#
# HTML = HyperText Markup Language
#
# É a linguagem que descreve o CONTEÚDO de uma página.
#
# Estrutura básica:
#
# <html>
#   <head>
#     <title>Título</title>
#   </head>
#   <body>
#     <h1>Meu conteúdo</h1>
#     <p>Parágrafo de texto.</p>
#   </body>
# </html>
#
# =========================================================
# 3. LENDO HTML NO R
# =========================================================
#
# read_html() lê qualquer documento HTML.
#
# Pode receber:
#
# - uma URL (https://...)
# - uma string com HTML
# - um arquivo local
#
# =========================================================

# HTML de exemplo com notícias internacionais
html_exemplo <- read_html('
<html>
<body>

  <article class="noticia">
    <h2 class="titulo">
      <a href="/noticias/onu-oriente-medio">ONU debate crise no Oriente Médio</a>
    </h2>
    <span class="data">12 mai 2026</span>
    <p class="resumo">Conselho de Segurança se reuniu para discutir escalada...</p>
    <span class="fonte">Agência ONU</span>
  </article>

  <article class="noticia">
    <h2 class="titulo">
      <a href="/noticias/otan-leste-europeu">OTAN amplia presença no Leste Europeu</a>
    </h2>
    <span class="data">11 mai 2026</span>
    <p class="resumo">Aliança anuncia novas bases militares em três países...</p>
    <span class="fonte">Reuters</span>
  </article>

  <article class="noticia">
    <h2 class="titulo">
      <a href="/noticias/china-comercio">China registra superávit comercial recorde</a>
    </h2>
    <span class="data">10 mai 2026</span>
    <p class="resumo">Exportações chinesas batem novo recorde em abril...</p>
    <span class="fonte">Bloomberg</span>
  </article>

</body>
</html>
')

# Visualizar o objeto
html_exemplo

# =========================================================
# 4. SELECIONANDO ELEMENTOS POR TAG
# =========================================================
#
# html_elements() seleciona TODOS os elementos que combinam.
# html_element()  seleciona apenas o PRIMEIRO.
#
# =========================================================

# Todos os artigos
artigos <- html_exemplo |>
  html_elements("article")

# Quantos artigos existem?
length(artigos)

# Todos os títulos h2
titulos_h2 <- html_exemplo |>
  html_elements("h2")

titulos_h2

# =========================================================
# 5. SELECIONANDO POR CLASSE CSS
# =========================================================
#
# Sintaxe: .nome-da-classe
#
# =========================================================

# Selecionar por classe .titulo
titulos <- html_exemplo |>
  html_elements(".titulo")

titulos

# Selecionar por classe .data
datas <- html_exemplo |>
  html_elements(".data")

datas

# Selecionar por classe .fonte
fontes <- html_exemplo |>
  html_elements(".fonte")

fontes

# =========================================================
# 6. EXTRAINDO TEXTO COM html_text2()
# =========================================================
#
# html_text2() extrai o texto limpo do elemento.
# (html_text() existe mas html_text2() lida melhor com espaços)
#
# =========================================================

titulos_texto <- html_exemplo |>
  html_elements(".titulo") |>
  html_text2()

titulos_texto

datas_texto <- html_exemplo |>
  html_elements(".data") |>
  html_text2()

datas_texto

fontes_texto <- html_exemplo |>
  html_elements(".fonte") |>
  html_text2()

fontes_texto

# =========================================================
# 7. EXTRAINDO ATRIBUTOS COM html_attr()
# =========================================================
#
# Links ficam no atributo href dos elementos <a>.
#
# html_attr("href") extrai esse atributo.
#
# =========================================================

links <- html_exemplo |>
  html_elements(".titulo a") |>
  html_attr("href")

links

# =========================================================
# 8. CONSTRUINDO UM DATAFRAME
# =========================================================

df_noticias <- tibble(
  titulo = titulos_texto,
  data   = datas_texto,
  fonte  = fontes_texto,
  link   = links
)

df_noticias

# =========================================================
# 9. SELETORES COMBINADOS
# =========================================================
#
# Podemos combinar seletores para ser mais específicos:
#
# ".noticia .data"  → .data dentro de .noticia
# "article h2"      → h2 dentro de article
# "h2.titulo"       → h2 com classe titulo
#
# =========================================================

# Selecionar resumos dentro de artigos
resumos <- html_exemplo |>
  html_elements("article p") |>
  html_text2()

resumos

# =========================================================
# 10. INSPECIONANDO TABELAS HTML
# =========================================================
#
# html_table() converte <table> diretamente em dataframe.
#
# Muito útil para sites institucionais (ONU, legislativos).
#
# =========================================================

html_tabela <- read_html('
<table>
  <thead>
    <tr>
      <th>País</th>
      <th>Assentos no CSONU</th>
      <th>Status</th>
    </tr>
  </thead>
  <tbody>
    <tr><td>Estados Unidos</td><td>Permanente</td><td>P5</td></tr>
    <tr><td>China</td><td>Permanente</td><td>P5</td></tr>
    <tr><td>Rússia</td><td>Permanente</td><td>P5</td></tr>
    <tr><td>França</td><td>Permanente</td><td>P5</td></tr>
    <tr><td>Reino Unido</td><td>Permanente</td><td>P5</td></tr>
    <tr><td>Brasil</td><td>Rotativo</td><td>E10</td></tr>
  </tbody>
</table>
')

# html_table() retorna uma lista de dataframes
tabelas <- html_tabela |>
  html_table()

# Acessar a primeira tabela
conselho_seguranca <- tabelas[[1]]

conselho_seguranca

# =========================================================
# 11. EXEMPLO REAL: WIKIPEDIA
# =========================================================
#
# Wikipedia usa tabelas HTML (wikitables).
#
# html_table() funciona muito bem.
#
# ATENÇÃO: use Sys.sleep() para não sobrecarregar servidores.
#
# =========================================================

# Lista de membros da OTAN
url_otan <- "https://pt.wikipedia.org/wiki/OTAN"

pagina_otan <- tryCatch(
  read_html(url_otan),
  error = function(e) NULL
)

if (!is.null(pagina_otan)) {

  # Extrair todas as tabelas da página
  tabelas_otan <- pagina_otan |>
    html_table(fill = TRUE)

  # Quantas tabelas há na página?
  length(tabelas_otan)

  # Ver a primeira tabela
  head(tabelas_otan[[1]])

}

# =========================================================
# 12. MAPEANDO A ESTRUTURA DE UMA PÁGINA
# =========================================================
#
# Antes de raspar, precisamos entender a estrutura.
#
# Passos:
#
# 1. Abrir o site no navegador;
# 2. Usar F12 para inspecionar;
# 3. Identificar tags, classes e IDs;
# 4. Testar seletores no R.
#
# =========================================================

# Exemplo: Agência Senado
url_senado <- "https://www12.senado.leg.br/noticias"

pagina_senado <- tryCatch(
  {
    Sys.sleep(1)
    request(url_senado) |>
      req_headers("User-Agent" = "Mozilla/5.0") |>
      req_perform() |>
      resp_body_html()
  },
  error = function(e) NULL
)

if (!is.null(pagina_senado)) {

  # O Senado usa <article><a> para cada manchete
  # h3 não aparece nesta página — o seletor correto é "article a"
  noticias_senado <- pagina_senado |>
    html_elements("article a")

  titulos <- html_text2(noticias_senado)
  links   <- paste0("https://www12.senado.leg.br",
                    html_attr(noticias_senado, "href"))

  data.frame(titulo = titulos, url = links)

}

# =========================================================
# 13. TRATANDO ESTRUTURAS AUSENTES
# =========================================================
#
# Nem toda página tem a estrutura esperada.
#
# Quando um seletor não encontra elementos,
# html_elements() retorna um vetor vazio.
#
# =========================================================

# Seletor que não existe
ausente <- html_exemplo |>
  html_elements(".categoria-inexistente") |>
  html_text2()

# Resultado: character(0)
ausente

# Verificar se encontrou algo
length(ausente) > 0

# =========================================================
# 14. EXTRAINDO MÚLTIPLOS ATRIBUTOS
# =========================================================
#
# Podemos extrair qualquer atributo HTML.
#
# Exemplos:
#
# - href   → endereço do link
# - src    → endereço da imagem
# - class  → classes CSS do elemento
# - id     → identificador único
# - alt    → texto alternativo de imagem
#
# =========================================================

html_imagens <- read_html('
<div class="galeria">
  <img src="diplomacia.jpg" alt="Reunião diplomática" class="foto">
  <img src="onu.jpg" alt="Sede da ONU" class="foto">
  <img src="otan.jpg" alt="Cúpula da OTAN" class="foto">
</div>
')

# Extrair atributos src e alt
srcs <- html_imagens |>
  html_elements("img") |>
  html_attr("src")

alts <- html_imagens |>
  html_elements("img") |>
  html_attr("alt")

tibble(
  arquivo   = srcs,
  descricao = alts
)

# =========================================================
# 15. EXERCÍCIO
# =========================================================
#
# Escolha um portal de notícias internacionais:
#
# - https://agenciabrasil.ebc.com.br
# - https://www.bbc.com/portuguese
# - https://brasil.un.org/pt-br/news
#
# Depois:
#
# 1. Abra no navegador e inspecione (F12);
# 2. Identifique a tag e classe dos títulos;
# 3. Identifique a tag e classe das datas;
# 4. Leia a página com read_html();
# 5. Extraia títulos e datas com html_elements();
# 6. Construa um dataframe.
#
# =========================================================
# 16. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - a estrutura de documentos HTML;
# - as principais tags (h1-h6, p, a, div, article, table);
# - o papel dos atributos (id, class, href, src);
# - CSS selectors (tag, .classe, #id, combinados);
# - como usar html_elements() e html_element();
# - como extrair texto com html_text2();
# - como extrair atributos com html_attr();
# - como converter tabelas com html_table().
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Na próxima aula aprenderemos como as páginas chegam até nós.
#
# HTTP: Requests e Respostas
#
# - GET e POST;
# - status codes (200, 404, 403);
# - headers e autenticação;
# - httr2;
# - APIs públicas.
#
# =========================================================
# FIM DA AULA
# =========================================================
