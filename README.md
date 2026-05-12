# Web Scraping e Dados Digitais em Relações Internacionais

Datamundi · 2026  
**Docente:** Vinicius Santos

---

## Sobre o curso

Introdução ao uso de métodos computacionais para coleta, organização e análise de dados digitais aplicados à pesquisa em Relações Internacionais. O curso cobre desde fundamentos de HTML/HTTP até pipelines reprodutíveis com `targets` e análise textual com `quanteda`.

## Estrutura do repositório

```
curso_webscrap/
├── _quarto.yml              # configuração do site Quarto
├── index.qmd                # página inicial
├── syllabus.qmd             # ementa detalhada
├── schedule.qmd             # cronograma semanal
│
├── lectures/                # slides de cada aula
│   ├── week01/              #   Semana 1 — Fundamentos
│   │   ├── aula01.qmd       #     Aula 1: Dados Digitais em RI
│   │   └── aula02.qmd       #     Aula 2: Ética e Legalidade
│   ├── week02/              #   Semana 2 — Web Foundations
│   ├── week03/              #   Semanas 3–4 — Scraping Estático
│   ├── week04/
│   ├── week05/              #   Semana 5 — Scraping Dinâmico
│   ├── week06/              #   Semana 6 — Pipelines
│   ├── week07/              #   Semana 7 — Texto e NLP
│   └── week08/              #   Semana 8 — Projeto Final
│
├── scripts/                 # scripts R comentados por aula
│   ├── aula01_rss_google_news.R
│   ├── aula02_apis_banco_mundial.R
│   └── ...                  # (aula03 a aula15)
│
├── data/                    # dados de exemplo
│   ├── raw/                 # downloads originais (não versionados)
│   └── processed/           # dados limpos (versionados)
│
└── assets/                  # recursos visuais
    └── custom.scss          # estilos institucionais
```

## Pré-requisitos

- R ≥ 4.3 e RStudio / Positron
- Quarto CLI ≥ 1.5 (`quarto --version`)
- Pacotes R: `tidyverse`, `rvest`, `httr2`, `jsonlite`, `RSelenium`, `chromote`, `targets`, `quanteda`

Instale todos de uma vez com:

```r
install.packages(c(
  "tidyverse", "rvest", "httr2", "jsonlite", "xml2",
  "RSelenium", "chromote", "targets", "tarchetypes",
  "quanteda", "quanteda.textstats", "quanteda.textplots",
  "renv", "fs", "usethis", "lubridate", "purrr"
))
```

## Renderizar o site

```bash
# Pré-visualização local (hot-reload)
quarto preview

# Build completo
quarto render

# Site gerado em _site/
```

## Licença

Material didático de uso livre para fins acadêmicos não-comerciais.  
Scripts de exemplo são disponibilizados sob [MIT License](LICENSE).
