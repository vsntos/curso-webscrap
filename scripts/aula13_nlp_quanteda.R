# =========================================================
# CURSO: WEB SCRAPING E DADOS DIGITAIS EM RI
# Aula 13: Introdução ao NLP com quanteda
# =========================================================
#
# OBJETIVO
#
# Aprender:
#
# - criar corpus com quanteda;
# - tokenizar e remover stopwords;
# - construir Document-Feature Matrix (DFM);
# - analisar frequência de palavras;
# - criar nuvens de palavras;
# - usar keyness para comparar corpora;
# - criar dicionários temáticos;
# - analisar co-ocorrências;
# - aplicar NLP a pesquisa em RI.
#
# =========================================================
# PERGUNTA DE PESQUISA
# =========================================================
#
# Quais atores e temas dominam a cobertura
# de política internacional na mídia brasileira?
#
# =========================================================
# 1. PACOTES
# =========================================================

# NLP
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)

# Dados e visualização
library(tidyverse)
library(lubridate)

# =========================================================
# 2. CARREGAR DADOS COLETADOS
# =========================================================
#
# Usamos o dataset gerado nas aulas anteriores.
# Se não tiver, criamos um corpus de exemplo.
#
# =========================================================

# Tentar carregar dataset real
if (file.exists("data/processed/noticias_internacionais.csv")) {

  df_noticias <- read_csv("data/processed/noticias_internacionais.csv")
  cat("Dataset carregado:", nrow(df_noticias), "notícias\n")

} else {

  # Corpus de exemplo para a aula
  df_noticias <- tibble(
    titulo = c(
      "ONU debate reforma do Conselho de Segurança da ONU",
      "BRICS avança em moeda alternativa ao dólar norte-americano",
      "OTAN amplia presença militar no Leste Europeu após cúpula",
      "Brasil e China assinam acordos de comércio bilateral em Brasília",
      "Irã rejeita proposta nuclear do Ocidente e ameaça enriquecer urânio",
      "Rússia intensifica bombardeios enquanto OTAN debate apoio à Ucrânia",
      "G20 discute reforma do sistema financeiro internacional em Buenos Aires",
      "EUA impõem novas sanções econômicas contra aliados do regime russo",
      "Cúpula climática define metas de emissão para países emergentes",
      "Itamaraty divulga nota sobre posição brasileira no conflito em Gaza",
      "Mercosul retoma negociações com União Europeia após anos de impasse",
      "China supera EUA como maior parceiro comercial do Brasil em 2025",
      "Conselho de Segurança da ONU vota resolução sobre crise humanitária",
      "OTAN anuncia criação de fundo de defesa coletiva de 100 bilhões",
      "Brasil assume presidência do G20 e anuncia prioridades diplomáticas",
      "Crise no Oriente Médio leva ONU a convocar reunião de emergência",
      "Ucrânia pede mais armamentos à OTAN para resistir ao avanço russo",
      "Acordo de Paris: países emergentes cobram financiamento climático",
      "Dilema nuclear: Irã avança em capacidade de enriquecimento de urânio",
      "Diplomacia brasileira busca papel de mediador no conflito ucraniano"
    ),
    portal      = rep(c("Agência Brasil", "ONU Brasil"), 10),
    data        = seq.Date(as.Date("2026-04-23"), by = "day", length.out = 20),
    coletado_em = Sys.time()
  )

  cat("Usando corpus de exemplo:", nrow(df_noticias), "notícias\n")
}

head(df_noticias)

# =========================================================
# 3. CRIAR CORPUS
# =========================================================

corp <- corpus(
  df_noticias,
  text_field = "titulo"
)

# Ver resumo
summary(corp, 5)

# =========================================================
# 4. ADICIONAR DOCVARS (METADADOS)
# =========================================================

# Metadados já vêm do dataframe quando usamos corpus()
# Confirmar:
head(docvars(corp))

# Adicionar variável derivada
docvars(corp, "mes") <- format(docvars(corp, "data"), "%Y-%m")
docvars(corp, "ano") <- year(docvars(corp, "data"))

# =========================================================
# 5. TOKENIZAÇÃO
# =========================================================

toks <- tokens(
  corp,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  remove_url     = TRUE
)

# Ver tokens do primeiro documento
toks[1]

# =========================================================
# 6. REMOVER STOPWORDS
# =========================================================
#
# Stopwords: palavras funcionais sem conteúdo analítico.
# Ex: "de", "que", "em", "para", "com"...
#
# =========================================================

# Stopwords em português do quanteda
sw_pt <- stopwords("pt")
head(sw_pt, 20)

# Adicionar stopwords específicas do domínio
sw_custom <- c(
  sw_pt,
  "após", "sobre", "entre", "desde", "além",
  "ainda", "sendo", "tendo", "sendo", "também",
  "mais", "menos", "apenas", "grande", "novo"
)

toks_limpos <- tokens_remove(toks, sw_custom)

toks_limpos[1:3]

# =========================================================
# 7. STEMMING (OPCIONAL)
# =========================================================
#
# Stemming reduz palavras à sua raiz.
# "acordos", "acordo", "acordou" → "acord"
#
# Útil para aumentar correspondências, mas pode distorcer.
# Use com cautela em análises de RI.
#
# =========================================================

toks_stem <- tokens_wordstem(toks_limpos, language = "pt")

# Comparar
toks_limpos[1]
toks_stem[1]

# =========================================================
# 8. NGRAMS
# =========================================================

# Bigramas (pares de palavras)
toks_bi <- tokens_ngrams(toks_limpos, n = 2)
toks_bi[1:3]

# Trigramas
toks_tri <- tokens_ngrams(toks_limpos, n = 3)
toks_tri[1:2]

# =========================================================
# 9. DOCUMENT-FEATURE MATRIX (DFM)
# =========================================================
#
# DFM: matriz documentos × palavras
# Cada célula = frequência da palavra no documento.
#
# =========================================================

dfm_corp <- dfm(toks_limpos)

# Dimensões
dim(dfm_corp)

# Ver estrutura
dfm_corp

# =========================================================
# 10. FREQUÊNCIA DE FEATURES
# =========================================================

freq <- textstat_frequency(dfm_corp)

# Top 20 palavras
head(freq, 20)

# =========================================================
# 11. VISUALIZAÇÃO: FREQUÊNCIA
# =========================================================

freq |>
  slice_head(n = 15) |>
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_col(fill = "#003366") +
  coord_flip() +
  labs(
    title    = "Palavras mais frequentes",
    subtitle = "Títulos de notícias internacionais",
    x        = NULL,
    y        = "Frequência",
    caption  = "Fonte: Agência Brasil / ONU Brasil"
  ) +
  theme_minimal(base_size = 13)

# =========================================================
# 12. NUVEM DE PALAVRAS
# =========================================================

textplot_wordcloud(
  dfm_corp,
  max_words  = 40,
  min_size   = 0.8,
  max_size   = 4,
  color      = c("#001f4d", "#003366", "#336699", "#6699CC", "#99BBDD")
)

# =========================================================
# 13. KEYNESS: COMPARAR PORTAIS
# =========================================================
#
# Keyness identifica quais palavras são mais características
# de um grupo em relação a outro.
#
# =========================================================

# Agrupar DFM por portal
dfm_portais <- dfm_group(dfm_corp, groups = docvars(corp, "portal"))

dfm_portais

# Calcular keyness: Agência Brasil vs ONU Brasil
keyness_ab <- textstat_keyness(
  dfm_portais,
  target = "Agência Brasil"
)

head(keyness_ab, 10)
tail(keyness_ab, 10)

# Visualizar keyness
textplot_keyness(
  keyness_ab,
  n = 10,
  color = c("#CC0000", "#003366")
)

# =========================================================
# 14. DICIONÁRIOS TEMÁTICOS
# =========================================================
#
# Dicionários classificam documentos por tema.
# Criados com base em palavras-chave do domínio de RI.
#
# =========================================================

dicionario_ri <- dictionary(list(

  multilateralismo = c(
    "onu", "otan", "nato", "brics", "g20", "g7",
    "mercosul", "conselho", "assembleia", "multilateral",
    "organização", "tratado", "acordo", "cúpula"
  ),

  conflito_seguranca = c(
    "guerra", "conflito", "sanção", "sanções", "militar",
    "ataque", "defesa", "arma", "armamento", "bombardeio",
    "invasão", "tropas", "ucrânia", "rússia", "otan"
  ),

  economia_comercio = c(
    "comércio", "exportação", "importação", "pib",
    "tarifa", "moeda", "dólar", "parceiro", "bilateral",
    "mercado", "investimento", "acordo", "econômico"
  ),

  clima_meio_ambiente = c(
    "clima", "climático", "emissão", "carbono", "cop",
    "paris", "meta", "renovável", "aquecimento",
    "ambiental", "floresta", "amazônia"
  ),

  diplomacia_brasil = c(
    "brasil", "itamaraty", "diplomacia", "diplomático",
    "presidência", "lula", "política externa", "mediador"
  )
))

# Aplicar dicionário
dfm_temas <- dfm_lookup(dfm_corp, dictionary = dicionario_ri)

# Converter para dataframe
df_temas <- convert(dfm_temas, to = "data.frame") |>
  as_tibble() |>
  bind_cols(docvars(corp))

df_temas |> select(doc_id, multilateralismo:diplomacia_brasil)

# =========================================================
# 15. ANÁLISE TEMÁTICA
# =========================================================

# Tema dominante por documento
df_tema_dominante <- df_temas |>
  pivot_longer(
    cols = multilateralismo:diplomacia_brasil,
    names_to  = "tema",
    values_to = "n"
  ) |>
  group_by(doc_id) |>
  slice_max(n, n = 1) |>
  ungroup()

df_tema_dominante |>
  count(tema, sort = TRUE)

# Tema por portal
df_temas |>
  pivot_longer(
    cols = multilateralismo:diplomacia_brasil,
    names_to  = "tema",
    values_to = "n"
  ) |>
  group_by(portal, tema) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(portal, desc(total))

# =========================================================
# 16. CO-OCORRÊNCIAS
# =========================================================
#
# Feature Co-occurrence Matrix (FCM):
# quais palavras aparecem juntas?
#
# =========================================================

fcm_corp <- fcm(
  toks_limpos,
  context   = "window",
  window    = 3,
  tri       = TRUE
)

# Top features
top_feat <- names(topfeatures(fcm_corp, 15))

# FCM reduzida às top features
fcm_top <- fcm_select(fcm_corp, pattern = top_feat)

# Visualizar rede de co-ocorrências
textplot_network(
  fcm_top,
  min_freq   = 1,
  vertex_size = 2,
  edge_color  = "#003366",
  edge_alpha  = 0.5
)

# =========================================================
# 17. ANÁLISE LONGITUDINAL
# =========================================================
#
# Frequência de temas ao longo do tempo.
#
# =========================================================

# Agrupar por mês
dfm_por_mes <- dfm_group(dfm_corp, groups = docvars(corp, "mes"))

# Frequência de palavras-chave por mês
palavras_alvo <- c("onu", "otan", "brics", "guerra", "clima")

df_longitudinal <- dfm_select(dfm_por_mes, pattern = palavras_alvo) |>
  convert(to = "data.frame") |>
  as_tibble() |>
  pivot_longer(
    cols = all_of(palavras_alvo),
    names_to  = "palavra",
    values_to = "frequencia"
  ) |>
  rename(mes = doc_id)

ggplot(df_longitudinal, aes(x = mes, y = frequencia, color = palavra, group = palavra)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title    = "Frequência de palavras-chave por mês",
    x        = "Mês",
    y        = "Frequência",
    color    = "Palavra-chave",
    caption  = "Fonte: corpus coletado"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =========================================================
# 18. ANÁLISE DE SENTIMENTO COM DICIONÁRIO
# =========================================================

dict_valence <- dictionary(list(
  positivo = c(
    "acordo", "cooperação", "parceria", "paz",
    "progresso", "avanço", "negociação", "aliança"
  ),
  negativo = c(
    "conflito", "sanção", "crise", "tensão",
    "ruptura", "guerra", "ameaça", "bombardeio"
  )
))

dfm_sent <- dfm_lookup(dfm_corp, dictionary = dict_valence)

df_sentimento <- convert(dfm_sent, to = "data.frame") |>
  as_tibble() |>
  mutate(escore = positivo - negativo) |>
  bind_cols(select(docvars(corp), portal, data))

# Distribuição por portal
df_sentimento |>
  group_by(portal) |>
  summarise(
    escore_medio = mean(escore),
    positivo_total = sum(positivo),
    negativo_total = sum(negativo)
  )

# =========================================================
# 19. EXERCÍCIO
# =========================================================
#
# Use o corpus coletado nas aulas anteriores.
#
# 1. Crie corpus com metadados (portal, data);
# 2. Tokenize e remova stopwords;
# 3. Construa DFM;
# 4. Visualize top 20 palavras;
# 5. Crie dicionário com 3 temas;
# 6. Aplique dfm_lookup() e analise temas por portal;
# 7. Calcule keyness entre dois portais;
# 8. Interprete: o framing difere entre portais?
#
# =========================================================
# 20. O QUE APRENDEMOS?
# =========================================================
#
# Hoje aprendemos:
#
# - criar corpus com docvars no quanteda;
# - tokenizar com tokens();
# - remover stopwords e criar ngrams;
# - construir DFM com dfm();
# - analisar frequência com textstat_frequency();
# - criar nuvens de palavras com textplot_wordcloud();
# - calcular keyness com textstat_keyness();
# - criar dicionários temáticos com dictionary();
# - aplicar dfm_lookup() para classificação;
# - construir FCM e visualizar redes de co-ocorrências;
# - analisar séries longitudinais de frequência;
# - medir sentimento com dicionário de valência.
#
# =========================================================
# PRÓXIMA AULA
# =========================================================
#
# Aula 14: Aplicações em RI
#
# - framing na cobertura de política externa;
# - agenda-setting e atenção midiática;
# - GDELT Project: eventos internacionais em escala;
# - conectar análise textual com teorias de RI.
#
# =========================================================
# FIM DA AULA
# =========================================================
