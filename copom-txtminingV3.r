# --- 0. CARREGANDO PACOTES ---
library(GetBCBData)
library(tidyverse)
library(tidytext)
library(textdata)
library(pdftools)
library(stringr)
library(beepr)
library(writexl)
library(showtext)
library(scales)
library(rstudioapi)

# --- 0.1. CONFIGURAÇÃO DE FONTES ---
font_add(
  "plusjakarta",
  regular = "C:/Users/GabrielHenriqueMarti/AppData/Local/Microsoft/Windows/Fonts/PLUSJAKARTASANS-Regular.ttf"
)
showtext_auto()
showtext_opts(dpi = 300)

# --- 0.2. CONFIGURAÇÃO DE ANÁLISE DE DATA ESPECÍFICA ---
# Para analisar uma data específica, defina abaixo (formato: "YYYY-MM-DD")
# Se deixar NULL, o script continuará normalmente
data_personalizada <- NULL  # Exemplo: "2024-12-18" ou NULL para desabilitar

# ====================================================================
# --- 1. CONFIGURAÇÕES GERAIS ---
# ====================================================================

TOP_N_PALAVRAS <- 10
FAMILIA_FONTE <- "plusjakarta"
CORES_TOM <- c("Dovish" = "#2ca02c", "Hawkish" = "#d62728")
CORES_REGIME <- c("Hawkish" = "#d62728", "Neutro" = "gray60", "Dovish" = "#2ca02c")
COR_SERIE <- "#189CD8"

log_info <- function(...) {
  cat(..., "\n")
}

garantir_diretorio <- function(path_dir) {
  if (!dir.exists(path_dir)) {
    dir.create(path_dir, recursive = TRUE)
  }
}

tema_copom <- function(
  angle_x = FALSE,
  legend_position = "none",
  bold_x = FALSE,
  margin_plot = TRUE
) {
  theme_minimal(base_family = FAMILIA_FONTE, base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 13),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.text.x = element_text(
        angle = if (angle_x) 45 else 0,
        hjust = if (angle_x) 1 else 0.5,
        size = if (bold_x) 11 else 10,
        face = if (bold_x) "bold" else "plain"
      ),
      axis.text.y = element_text(size = 10),
      legend.position = legend_position,
      legend.text = element_text(size = 11),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = if (margin_plot) margin(t = 10, r = 20, b = 0, l = 10, unit = "pt") else margin()
    )
}

save_plot_all <- function(plot, fixed_path, local_ts_dir, width = 8, height = 7, dpi = 300) {
  garantir_diretorio(dirname(fixed_path))
  garantir_diretorio(local_ts_dir)
  ggplot2::ggsave(filename = fixed_path, plot = plot, bg = "transparent", width = width, height = height, dpi = dpi)
  ggplot2::ggsave(
    filename = file.path(local_ts_dir, basename(fixed_path)),
    plot = plot,
    bg = "transparent",
    width = width,
    height = height,
    dpi = dpi
  )
}

save_csv_all <- function(df, fixed_path, local_ts_dir) {
  garantir_diretorio(dirname(fixed_path))
  garantir_diretorio(local_ts_dir)
  readr::write_csv(df, fixed_path)
  readr::write_csv(df, file.path(local_ts_dir, basename(fixed_path)))
}

normalizar_data_copom <- function(data_texto) {
  if (is.na(data_texto)) {
    return(as.Date(NA))
  }

  data_ajustada <- data_texto |>
    stringr::str_replace("([A-Za-z]+\\s+)\\d+-(\\d+)(,\\s+\\d{4})", "\\1\\2\\3") |>
    stringr::str_replace("\\d+-(\\d+)(\\s+[A-Za-z]+\\s+\\d{4})", "\\1\\2")

  parsed <- lubridate::mdy(data_ajustada, quiet = TRUE)
  if (is.na(parsed)) {
    parsed <- lubridate::dmy(data_ajustada, quiet = TRUE)
  }

  if (!is.na(parsed)) {
    ano <- lubridate::year(parsed)
    if (ano < 2000 || ano > 2035) {
      parsed <- as.Date(NA)
    }
  }

  parsed
}

calcular_top_palavras <- function(tokens, dicionario, custom_stopwords, id_documento = NULL, top_n = TOP_N_PALAVRAS) {
  tokens_filtrados <- tokens

  if (!is.null(id_documento)) {
    tokens_filtrados <- tokens_filtrados |>
      filter(id_documento == !!id_documento)
  }

  palavras_com_score <- tokens_filtrados |>
    inner_join(dicionario, by = "word", relationship = "many-to-many") |>
    anti_join(custom_stopwords, by = "word") |>
    filter(value != 0) |>
    group_by(word, value, tipo) |>
    summarise(frequencia = n(), .groups = "drop") |>
    mutate(contribuicao_total = value * frequencia) |>
    arrange(desc(abs(contribuicao_total)))

  top_hawkish <- palavras_com_score |>
    filter(value > 0) |>
    slice_head(n = top_n) |>
    arrange(contribuicao_total)

  top_dovish <- palavras_com_score |>
    filter(value < 0) |>
    slice_head(n = top_n) |>
    arrange(desc(contribuicao_total))

  bind_rows(top_dovish, top_hawkish)
}

plotar_top_palavras <- function(top_palavras, titulo, subtitulo, fill_label = "Tom da Comunicação:") {
  ggplot(top_palavras, aes(x = reorder(word, contribuicao_total), y = contribuicao_total, fill = tipo)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = CORES_TOM) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "",
      y = "Contribuição Total",
      fill = fill_label
    ) +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
    tema_copom(legend_position = "top")
}

gerar_analise_reuniao <- function(reuniao_df, tokens, dicionario, custom_stopwords, output_path, output_local_ts_dir) {
  if (nrow(reuniao_df) == 0) {
    return(invisible(NULL))
  }

  top_palavras <- calcular_top_palavras(
    tokens = tokens,
    dicionario = dicionario,
    custom_stopwords = custom_stopwords,
    id_documento = reuniao_df$id_documento[[1]]
  )

  if (nrow(top_palavras) == 0) {
    return(invisible(NULL))
  }

  grafico <- plotar_top_palavras(
    top_palavras = top_palavras,
    titulo = paste("Top Palavras - Reunião", format(reuniao_df$data[[1]], "%d/%m/%Y")),
    subtitulo = paste("Net Sentiment:", round(reuniao_df$net_sentiment[[1]], 3))
  )

  print(grafico)
  save_plot_all(grafico, output_path, output_local_ts_dir, width = 8, height = 8)

  invisible(top_palavras)
}

# ====================================================================
# --- 2. GERENCIAMENTO DE CAMINHOS E ARQUIVAMENTO LOCAL ---
# ====================================================================

tryCatch({
  script_path <- rstudioapi::getSourceEditorContext()$path
  script_dir <- dirname(script_path)
  setwd(script_dir)
  log_info("✓ Diretório de trabalho definido como:", getwd())
}, error = function(e) {
  script_dir <- getwd()
  log_info("Atenção: Não foi possível obter o diretório do script via rstudioapi. Usando getwd().")
  log_info("Diretório de trabalho (Local):", script_dir)
})

path_base_projeto <- "C:/Users/GabrielHenriqueMarti/Desktop/Projeto_Macro/dados/latest"
path_csv_lexico <- file.path(path_base_projeto, "lm-copom_lexico_customizado.csv")
path_png_reuniao <- file.path(path_base_projeto, "copom_sentimento_por_reuniao.png")
path_png_data <- file.path(path_base_projeto, "copom_sentimento_por_data.png")
path_png_selic_sentimento <- file.path(path_base_projeto, "copom_selic_vs_sentimento.png")
path_png_top_palavras <- file.path(path_base_projeto, "copom_top_palavras_sentimento.png")
path_png_top_palavras_recente <- file.path(path_base_projeto, "copom_top_palavras_reuniao_recente.png")
path_png_top_palavras_personalizada <- file.path(path_base_projeto, "copom_top_palavras_data_personalizada.png")
path_png_regime_timeline <- file.path(path_base_projeto, "copom_regime_timeline.png")
path_png_regime_distribuicao <- file.path(path_base_projeto, "copom_regime_distribuicao.png")
path_excel_analise <- file.path(path_base_projeto, "copom_analise_completa.xlsx")
path_csv_comparacao_lexicos <- file.path(path_base_projeto, "copom_comparacao_lexicos.csv")
path_png_comparacao_lexicos <- file.path(path_base_projeto, "copom_comparacao_lexicos_facet.png")
path_excel_scorecard_lexicos <- file.path(path_base_projeto, "copom_scorecard_lexicos.xlsx")
path_png_scatter_lexicos <- file.path(path_base_projeto, "copom_scatter_sentimento_vs_selic_t1.png")

timestamp_run <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_local_ts_dir <- file.path(script_dir, paste0("COPOMSENTIMENT_", timestamp_run))

garantir_diretorio(path_base_projeto)
garantir_diretorio(output_local_ts_dir)

log_info("✓ Pasta de arquivamento LOCAL:", output_local_ts_dir)
log_info("Arquivos finais (latest) serão salvos em:", path_base_projeto)

caminho_pasta_pdfs <- file.path(script_dir, "atas")

if (!dir.exists(caminho_pasta_pdfs)) {
  stop("Diretório não encontrado: ", caminho_pasta_pdfs, "\nCrie a pasta 'atas' ou ajuste o caminho")
}

arquivos_pdf <- list.files(path = caminho_pasta_pdfs, pattern = "\\.pdf$", full.names = TRUE)

if (length(arquivos_pdf) == 0) {
  stop("Nenhum arquivo PDF encontrado em: ", caminho_pasta_pdfs)
}

# ====================================================================
# --- 3. INGESTÃO DE DADOS (PDFs das Atas do Copom) ---
# ====================================================================

ler_ata_pdf_avancado <- function(caminho_arquivo) {
  tryCatch({
    texto_bruto <- pdftools::pdf_text(caminho_arquivo) |>
      stringr::str_c(collapse = " ") |>
      stringr::str_squish()

    reuniao_id <- stringr::str_extract(texto_bruto, "\\d{3}(st|nd|rd|th) Meeting|\\d{3}(st|nd|rd|th) Minutes")
    data_texto <- stringr::str_extract(
      texto_bruto,
      "\\d{1,2}(-\\d{1,2})?\\s+[A-Z][a-z]+\\s+\\d{4}|[A-Z][a-z]+\\s+\\d{1,2}(-\\d{1,2})?,\\s+\\d{4}"
    )

    texto_analitico <- stringr::str_extract(texto_bruto, "A\\) Update of the economic outlook.*")
    if (is.na(texto_analitico)) {
      texto_analitico <- texto_bruto
    }

    nome_arquivo <- basename(caminho_arquivo)
    data_arquivo <- stringr::str_extract(nome_arquivo, "\\d{4}-\\d{2}-\\d{2}")

    data_final <- if (!is.na(data_arquivo)) {
      as.Date(data_arquivo)
    } else {
      normalizar_data_copom(data_texto)
    }

    reuniao_num <- as.integer(stringr::str_extract(nome_arquivo, "\\d+"))

    tibble(
      id_documento = nome_arquivo,
      reuniao_num = reuniao_num,
      data = data_final,
      reuniao = reuniao_id,
      data_identificada = data_texto,
      texto = texto_analitico
    )
  }, error = function(e) {
    warning("Erro ao processar ", basename(caminho_arquivo), ": ", e$message)
    NULL
  })
}

log_info("--- Iniciando Ingestão de Dados ---")
log_info("Processando", length(arquivos_pdf), "arquivo(s) PDF...")

atas_copom <- purrr::map_dfr(arquivos_pdf, ler_ata_pdf_avancado)

log_info("✓ Leitura concluída:", nrow(atas_copom), "ata(s) processada(s)")

datas_validas <- sum(!is.na(atas_copom$data))
if (datas_validas == 0) {
  warning("Nenhuma data foi extraída dos PDFs")
  print(head(atas_copom |> select(id_documento, data_identificada, data), 3))
} else if (datas_validas < nrow(atas_copom)) {
  log_info("⚠", datas_validas, "de", nrow(atas_copom), "datas extraídas com sucesso")
} else {
  log_info("✓ Todas as", datas_validas, "datas extraídas com sucesso")
}

# ====================================================================
# --- 4. ANÁLISE DE SENTIMENTO (ABORDAGEM BIS: construção de léxico híbrido / DICIONÁRIO LOUGHRAM) ---
# ====================================================================

if (nrow(atas_copom) == 0) {
  stop("Nenhuma ata foi processada com sucesso. Verifique os PDFs.")
}

log_info("--- Iniciando Análise de Sentimento (Léxico Customizado LOUGHRAM) ---")

lm_lexicon <- get_sentiments("loughran") |>
  filter(sentiment %in% c("positive", "negative")) |>
  mutate(
    value = if_else(sentiment == "positive", 1, -1),
    tipo = if_else(sentiment == "positive", "Hawkish", "Dovish")
  ) |>
  select(word, value, tipo)

mp_overrides <- tribble(
  ~word, ~value, ~tipo,
  
  # Hawkish
  "guidance_two_more_hikes", 3, "Hawkish",
  "hike_100bps", 2, "Hawkish",
  "hike_75bps", 1.5, "Hawkish",
  "even_more_contractionary", 2, "Hawkish",
  "significantly_contractionary", 2, "Hawkish",
  "very_prolonged_period", 1.5, "Hawkish",
  "resume_the_rate_hiking", 1.5, "Hawkish",
  "deanchoring", 1, "Hawkish",
  "deanchored", 1, "Hawkish",
  "further_adjustments", 1, "Hawkish",
  "more_adverse", 1, "Hawkish",
  "firm_commitment", 1, "Hawkish",
  "depreciated", 1, "Hawkish",
  "contractionary", 1, "Hawkish",
  "vigilant", 1, "Hawkish",
  "adverse", 1, "Hawkish",
  "upside", 1, "Hawkish",
  "strength", 1, "Hawkish",
  "tightening", 1, "Hawkish",
  "restrictive", 1, "Hawkish",
  "restriction", 0.6, "Hawkish",
  "inflationary", 1, "Hawkish",
  "stimulaing", 0.5, "Hawkish",
  "pressured", 0.8, "Hawkish",
  "resilient", 0.8, "Hawkish",
  "persistence", 0.4, "Hawkish",
  "challenging", 0.6, "Hawkish",
  "red", 0.5, "Hawkish",
  "dynamic", 0.6, "Hawkish",
  "overheating", 1, "Hawkish",
  "expanding", 1, "Hawkish",
  "strong_growth", 1, "Hawkish",
  "discipline", 1, "Hawkish",
  "tight_labor_market", 1, "Hawkish",
  
  # Dovish

  "cut", -0.8, "Dovish",
  "downside", -0.6, "Dovish",
  "accommodative", -0.6, "Dovish",
  "easing", -0.5, "Dovish",
  "slack", -0.5, "Dovish",
  "benign", -0.5, "Dovish",
  "disinflation", -0.5, "Dovish",
  "green", -0.5, "Dovish",
  "moderation", -0.8, "Dovish",
  "disinflationary", -0.8, "Dovish",
  "guidance_easing_cycle", -3, "Dovish",
  "initiate_flexibilization", -3, "Dovish",
  "improved_inflation_outlook", -1.5, "Dovish",
  "expectations_less_distant", -1.5, "Dovish"
)

dicionario_mp <- bind_rows(mp_overrides, lm_lexicon) |>
  distinct(word, .keep_all = TRUE)

atas_copom_tratado <- atas_copom |>
  mutate(
    texto_limpo = str_to_lower(texto),

    # Hawkish Overrides (N-grams e termos específicos)

    texto_limpo = str_replace_all(texto_limpo, "1\\.00 p\\.p\\.", "hike_100bps"),
    texto_limpo = str_replace_all(texto_limpo, "0\\.75 p\\.p\\.", "hike_75bps"),
    texto_limpo = str_replace_all(texto_limpo, "0\\.50 p\\.p\\.", "hike_50bps"),
    texto_limpo = str_replace_all(texto_limpo, "same magnitude at the next two meetings", "guidance_two_more_hikes"),
    texto_limpo = str_replace_all(texto_limpo, "same magnitude in the next two meetings", "guidance_two_more_hikes"),
    texto_limpo = str_replace_all(texto_limpo, "resume the rate hiking", "resume_the_rate_hiking"),
    texto_limpo = str_replace_all(texto_limpo, "even more contractionary", "even_more_contractionary"),
    texto_limpo = str_replace_all(texto_limpo, "significantly contractionary", "significantly_contractionary"),
    texto_limpo = str_replace_all(texto_limpo, "very prolonged period", "very_prolonged_period"),
    texto_limpo = str_replace_all(texto_limpo, "firm commitment", "firm_commitment"),
    texto_limpo = str_replace_all(texto_limpo, "more adverse", "more_adverse"),
    texto_limpo = str_replace_all(texto_limpo, "further adjustments", "further_adjustments"),
    texto_limpo = str_replace_all(texto_limpo, "strong growth", "strong_growth"),
    texto_limpo = str_replace_all(texto_limpo, "activity moderation", "activity_moderation"),
    texto_limpo = str_replace_all(texto_limpo, "disinflationary gains", "disinflationary_gains"),
    texto_limpo = str_replace_all(texto_limpo, "fiscal discipline", "fiscal_discipline"),
    
    # Dovish Overrides (N-grams e termos específicos)

    texto_limpo = str_replace_all(texto_limpo, "beginning of an interest-rate easing cycle", "guidance_easing_cycle"),
    texto_limpo = str_replace_all(texto_limpo, "magnitude of the easing cycle", "guidance_easing_cycle"),
    texto_limpo = str_replace_all(texto_limpo, "initiate the flexibilization", "initiate_flexibilization"),
    texto_limpo = str_replace_all(texto_limpo, "improved current inflation outlook", "improved_inflation_outlook"),
    texto_limpo = str_replace_all(texto_limpo, "less distant from the target", "expectations_less_distant")
  )

# Palavras para serem desconsideradas pelo algoritmo:

custom_stopwords <- tibble(
  word = c(
    "committee",
    "prospective",
    "relevant",
    "volatility",
    "fomc",
    "meeting",
    "members",
    "participants",
    "staff",
    "funding",
    "support",
    "persists",
    "consistent",
    "goal",
    "issuance",
    "compensation",
    "progress",
    "caution",
    "effective",
    "achieve",
    "board",
    "preferred",
    "clarify",
    "ample",
    "importance",
    "standing",
    "notable",
    "information",
    "data",
    "financing",
    "confidence",
    "broad",
    "volatile",
    "volatility",
    "pandemic",
    "stabilize",
    "repurchase",
    "favored",
    "approval",
    "commitment"
  ),
  lexicon = "custom"
)

all_stopwords <- bind_rows(stop_words, custom_stopwords)

copom_tokens <- atas_copom_tratado |>
  unnest_tokens(word, texto_limpo) |>
  mutate(word = str_replace_all(word, "[^a-z_]", "")) |>
  filter(word != "") |>
  anti_join(all_stopwords, by = "word")

copom_indice <- copom_tokens |>
  inner_join(dicionario_mp, by = "word", relationship = "many-to-many") |>
  group_by(id_documento, reuniao_num, data) |>
  summarise(
    total_palavras = n(),
    soma_scores = sum(value),
    net_sentiment = soma_scores / total_palavras,
    .groups = "drop"
  ) |>
  arrange(reuniao_num)

log_info("✓ Análise de sentimento concluída:", nrow(copom_indice), "reuniões processadas")



# ====================================================================
# --- 5. IMPORTAÇÃO DA TAXA SELIC (BCB - SGS 432) ---
# ====================================================================

log_info("--- Importando Série da Taxa Selic ---")

data_min_sentimento <- min(copom_indice$data, na.rm = TRUE)

selic_data <- GetBCBData::gbcbd_get_series(
  id = c("SELIC" = 432),
  first.date = data_min_sentimento,
  last.date = Sys.Date(),
  format.data = "wide"
)

log_info(
  "✓ Selic importada:",
  nrow(selic_data),
  "observações de",
  min(selic_data$ref.date),
  "a",
  max(selic_data$ref.date)
)

# ====================================================================
# --- 6. PREPARAÇÃO DA SÉRIE E EXPORTAÇÃO DO LÉXICO ---
# ====================================================================

log_info("--- Iniciando Preparação da Série e Exportação do Léxico ---")

serie_sentimento <- copom_indice |>
  select(id_documento, reuniao_num, data, net_sentiment, soma_scores, total_palavras)

save_csv_all(dicionario_mp, path_csv_lexico, output_local_ts_dir)
log_info("✓ Léxico personalizado salvo em:", path_csv_lexico)

# ====================================================================
# --- 7. GERAÇÃO DE GRÁFICOS ---
# ====================================================================

log_info("--- Iniciando Geração de Gráficos ---")

p1 <- ggplot(serie_sentimento, aes(x = reuniao_num, y = net_sentiment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = COR_SERIE, linewidth = 1.5) +
  geom_point(color = COR_SERIE, size = 1.5) +
  labs(
    title = "Sentimento das Atas do Copom (Léxico BIS)",
    subtitle = "Hawkish: >0, Dovish: <0.",
    x = "Número da Reunião",
    y = "Sentimento Líquido (Net Sentiment Score)"
  ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
  tema_copom()

print(p1)
save_plot_all(p1, path_png_reuniao, output_local_ts_dir)

serie_com_data <- serie_sentimento |>
  filter(!is.na(data)) |>
  arrange(data)

if (nrow(serie_com_data) > 0) {
  p2 <- ggplot(serie_com_data, aes(x = data, y = net_sentiment)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(color = COR_SERIE, linewidth = 1.5) +
    geom_point(color = COR_SERIE, size = 1.5) +
    geom_smooth(method = "loess", se = FALSE, color = "firebrick", linewidth = 0.9, span = 0.6) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
    labs(
      title = "Sentimento das Atas do Copom (Léxico BIS)",
      subtitle = "Hawkish: >0, Dovish: <0.",
      x = "",
      y = "Sentimento Líquido (Net Sentiment Score)"
    ) +
    tema_copom(angle_x = TRUE)

  print(p2)
  save_plot_all(p2, path_png_data, output_local_ts_dir)
}

if (nrow(serie_com_data) > 0 && nrow(selic_data) > 0) {
  dados_combinados <- serie_com_data |>
    select(data, net_sentiment) |>
    left_join(selic_data |> select(ref.date, SELIC) |> rename(data = ref.date), by = "data") |>
    filter(!is.na(SELIC))

  if (nrow(dados_combinados) > 0) {
    min_sentiment <- min(dados_combinados$net_sentiment, na.rm = TRUE)
    max_sentiment <- max(dados_combinados$net_sentiment, na.rm = TRUE)
    max_selic <- max(dados_combinados$SELIC, na.rm = TRUE)

    if (max_sentiment > min_sentiment) {
      coef_escala <- max_selic / (max_sentiment - min_sentiment)
      intercepto <- -min_sentiment * coef_escala
    } else {
      coef_escala <- 1
      intercepto <- 0
    }

    p3 <- ggplot(dados_combinados, aes(x = data)) +
      geom_line(aes(y = net_sentiment, color = "Sentimento (Hawkish/Dovish)"), linewidth = 1.3) +
      geom_point(aes(y = net_sentiment, color = "Sentimento (Hawkish/Dovish)"), size = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      geom_line(aes(y = (SELIC - intercepto) / coef_escala, color = "Selic"), linewidth = 1.3) +
      geom_point(aes(y = (SELIC - intercepto) / coef_escala, color = "Selic"), size = 1.5) +
      scale_y_continuous(
        name = "Net Sentiment (Score)",
        labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01),
        sec.axis = sec_axis(
          ~ . * coef_escala + intercepto,
          name = "Taxa Selic (%)",
          labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)
        )
      ) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
      scale_color_manual(values = c("Sentimento (Hawkish/Dovish)" = COR_SERIE, "Selic" = "black"), name = "") +
      labs(
        title = "Sentimento das Atas do Copom vs Taxa Selic",
        subtitle = "Correlação entre o tom da comunicação (Léxico BIS) e a taxa básica",
        x = "Data da Reunião"
      ) +
      tema_copom(angle_x = TRUE, legend_position = "top")

    print(p3)
    save_plot_all(p3, path_png_selic_sentimento, output_local_ts_dir)
  }
}

# ====================================================================
# --- 8. ANÁLISES AVANÇADAS DE TEXT MINING ---
# ====================================================================

quantis <- quantile(serie_sentimento$net_sentiment, probs = c(1 / 3, 2 / 3), na.rm = TRUE)

serie_com_regime <- serie_sentimento |>
  mutate(
    regime = case_when(
      net_sentiment >= quantis[2] ~ "Hawkish",
      net_sentiment >= quantis[1] & net_sentiment < quantis[2] ~ "Neutro",
      net_sentiment < quantis[1] ~ "Dovish",
      TRUE ~ NA_character_
    ),
    regime = factor(regime, levels = c("Hawkish", "Neutro", "Dovish"))
  )

top_palavras_combinadas <- calcular_top_palavras(
  tokens = copom_tokens,
  dicionario = dicionario_mp,
  custom_stopwords = custom_stopwords,
  top_n = TOP_N_PALAVRAS
)

if (nrow(top_palavras_combinadas) == 0) {
  warning("Nenhuma palavra com sentimento encontrada para análise")
}

metricas_regime <- atas_copom |>
  left_join(serie_com_regime |> select(id_documento, regime), by = "id_documento") |>
  mutate(
    total_palavras_doc = str_count(texto, "\\S+"),
    total_caracteres = nchar(texto)
  ) |>
  group_by(regime) |>
  summarise(
    n_reunioes = n(),
    tamanho_medio_palavras = mean(total_palavras_doc, na.rm = TRUE),
    tamanho_medio_caracteres = mean(total_caracteres, na.rm = TRUE),
    .groups = "drop"
  )

diversidade_regime <- copom_tokens |>
  left_join(serie_com_regime |> select(id_documento, regime), by = "id_documento") |>
  group_by(regime, id_documento) |>
  summarise(total_palavras = n(), palavras_unicas = n_distinct(word), .groups = "drop") |>
  group_by(regime) |>
  summarise(diversidade_lexical_media = mean(palavras_unicas / total_palavras, na.rm = TRUE), .groups = "drop")

densidade_regime <- copom_tokens |>
  left_join(serie_com_regime |> select(id_documento, regime), by = "id_documento") |>
  group_by(regime, id_documento) |>
  summarise(total_tokens = n(), .groups = "drop") |>
  left_join(copom_indice |> select(id_documento, total_palavras), by = "id_documento") |>
  group_by(regime) |>
  summarise(densidade_sentimento_media = mean(total_palavras / total_tokens, na.rm = TRUE), .groups = "drop")

metricas_regime <- metricas_regime |>
  left_join(diversidade_regime, by = "regime") |>
  left_join(densidade_regime, by = "regime")

top_palavras_por_regime <- copom_tokens |>
  left_join(serie_com_regime |> select(id_documento, regime), by = "id_documento") |>
  inner_join(dicionario_mp, by = "word", relationship = "many-to-many") |>
  filter(!is.na(regime)) |>
  group_by(regime, word, value) |>
  summarise(frequencia = n(), contribuicao = sum(value), .groups = "drop") |>
  group_by(regime) |>
  slice_max(order_by = abs(contribuicao), n = 5) |>
  arrange(regime, desc(contribuicao))

# ====================================================================
# --- 9. GRÁFICOS AVANÇADOS ---
# ====================================================================

log_info("--- Gerando Gráficos Avançados ---")

if (nrow(top_palavras_combinadas) > 0) {
  g_top_palavras <- plotar_top_palavras(
    top_palavras = top_palavras_combinadas,
    titulo = "Contribuições ao score de Sentimento do Copom",
    subtitulo = "Impacto baseado no Dicionário Monetário (Frequência × Score)"
  )

  print(g_top_palavras)
  save_plot_all(g_top_palavras, path_png_top_palavras, output_local_ts_dir, width = 8, height = 8)
}

serie_com_regime_data <- serie_com_regime |>
  filter(!is.na(data) & !is.na(regime)) |>
  arrange(data)

if (nrow(serie_com_regime_data) > 0) {
  g_regime_timeline <- ggplot(serie_com_regime_data, aes(x = data, y = net_sentiment, color = regime)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(linewidth = 1.3, alpha = 0.7) +
    geom_point(size = 3, alpha = 0.8) +
    scale_color_manual(values = CORES_REGIME, name = "Postura:") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
    labs(
      title = "Ciclos de Comunicação: Hawkish vs Dovish",
      subtitle = "Classificação baseada em tercis do sentimento (Léxico BIS)",
      x = "Data da Reunião",
      y = "Net Sentiment"
    ) +
    tema_copom(angle_x = TRUE, legend_position = "top", margin_plot = FALSE)

  print(g_regime_timeline)
  save_plot_all(g_regime_timeline, path_png_regime_timeline, output_local_ts_dir)
}

g_regime_dist <- ggplot(
  serie_com_regime |> filter(!is.na(regime)),
  aes(x = regime, y = net_sentiment, fill = regime)
) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 3) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  scale_fill_manual(values = CORES_REGIME) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
  labs(
    title = "Distribuição do Sentimento por Postura",
    subtitle = "Boxplot + dispersão das reuniões",
    x = "Postura do Copom",
    y = "Net Sentiment"
  ) +
  tema_copom(bold_x = TRUE)

print(g_regime_dist)
save_plot_all(g_regime_dist, path_png_regime_distribuicao, output_local_ts_dir)

# ====================================================================
# --- 10. ANÁLISES POR DATA ESPECÍFICA ---
# ====================================================================

reuniao_recente <- serie_sentimento |>
  filter(!is.na(data)) |>
  arrange(desc(data)) |>
  slice(1)

resultado_recente <- gerar_analise_reuniao(
  reuniao_df = reuniao_recente,
  tokens = copom_tokens,
  dicionario = dicionario_mp,
  custom_stopwords = custom_stopwords,
  output_path = path_png_top_palavras_recente,
  output_local_ts_dir = output_local_ts_dir
)

if (!is.null(data_personalizada)) {
  data_target <- as.Date(data_personalizada)

  if (is.na(data_target)) {
    warning("'data_personalizada' não está em formato válido YYYY-MM-DD.")
  } else {
    reuniao_personalizada <- serie_sentimento |>
      filter(!is.na(data)) |>
      filter(data == data_target)

    resultado_personalizado <- gerar_analise_reuniao(
      reuniao_df = reuniao_personalizada,
      tokens = copom_tokens,
      dicionario = dicionario_mp,
      custom_stopwords = custom_stopwords,
      output_path = path_png_top_palavras_personalizada,
      output_local_ts_dir = output_local_ts_dir
    )

    if (nrow(reuniao_personalizada) == 0) {
      datas_disponiveis <- serie_sentimento |>
        filter(!is.na(data)) |>
        pull(data) |>
        sort()

      cat("\nDatas disponíveis:\n")
      print(format(datas_disponiveis, "%Y-%m-%d"))
    }
  }
}

# ====================================================================
# --- 11. EXPORTAÇÃO PARA EXCEL ---
# ====================================================================

log_info("--- Gerando Excel Compilado ---")

aba_serie_regime <- serie_com_regime |>
  select(reuniao_num, data, net_sentiment, soma_scores, total_palavras, regime) |>
  arrange(reuniao_num)

aba_top_palavras <- top_palavras_combinadas |>
  select(tipo, word, value, frequencia, contribuicao_total) |>
  arrange(desc(abs(contribuicao_total)))

aba_metricas_regime <- metricas_regime |>
  mutate(across(where(is.numeric), ~ round(., 2)))

aba_top_regime <- top_palavras_por_regime |>
  select(regime, word, value, frequencia, contribuicao) |>
  arrange(regime, desc(abs(contribuicao)))

aba_stats <- serie_com_regime |>
  summarise(
    n_observacoes = n(),
    sentimento_medio = mean(net_sentiment, na.rm = TRUE),
    sentimento_mediano = median(net_sentiment, na.rm = TRUE),
    sentimento_sd = sd(net_sentiment, na.rm = TRUE),
    sentimento_min = min(net_sentiment, na.rm = TRUE),
    sentimento_max = max(net_sentiment, na.rm = TRUE),
    hawkish_pct = sum(regime == "Hawkish", na.rm = TRUE) / n() * 100,
    neutro_pct = sum(regime == "Neutro", na.rm = TRUE) / n() * 100,
    dovish_pct = sum(regime == "Dovish", na.rm = TRUE) / n() * 100
  ) |>
  mutate(across(where(is.numeric), ~ round(., 3))) |>
  pivot_longer(everything(), names_to = "metrica", values_to = "valor")

lista_abas <- list(
  "Serie_Regime" = aba_serie_regime,
  "Top_Palavras" = aba_top_palavras,
  "Metricas_Regime" = aba_metricas_regime,
  "Top_Palavras_Regime" = aba_top_regime,
  "Estatisticas" = aba_stats
)

writexl::write_xlsx(lista_abas, path_excel_analise)
file.copy(path_excel_analise, file.path(output_local_ts_dir, basename(path_excel_analise)), overwrite = TRUE)
log_info("✓ Excel de análise compilado salvo em:", path_excel_analise)



# ====================================================================
# --- 12. COMPARAÇÃO DE DIFERENTES LÉXICOS ---
# ====================================================================

log_info("--- Gerando Comparação entre Léxicos ---")

carregar_economic_lexicon <- function(script_dir) {
  caminho_lexico_economico <- file.path(script_dir, "Economic_Lexicon.csv")

  if (!file.exists(caminho_lexico_economico)) {
    stop("ERRO: O arquivo 'Economic_Lexicon.csv' não foi encontrado no diretório do script.")
  }

  el_lexicon_bruto <- readr::read_csv(caminho_lexico_economico, show_col_types = FALSE)

  colunas_necessarias_padrao <- c("word", "value", "tipo")
  colunas_necessarias_csv <- c("token", "sentiment", "polarity")

  if (all(colunas_necessarias_padrao %in% names(el_lexicon_bruto))) {
    el_lexicon <- el_lexicon_bruto |>
      select(word, value, tipo)
  } else if (all(colunas_necessarias_csv %in% names(el_lexicon_bruto))) {
    el_lexicon <- el_lexicon_bruto |>
      transmute(
        word = token,
        value = sentiment,
        tipo = case_when(
          polarity > 0 ~ "Hawkish",
          polarity < 0 ~ "Dovish",
          TRUE ~ "Neutro"
        )
      )
  } else {
    stop(
      "ERRO: O arquivo 'Economic_Lexicon.csv' precisa conter as colunas 'word', 'value', 'tipo' ou 'token', 'sentiment', 'polarity'."
    )
  }

  el_lexicon |>
    mutate(
      word = str_to_lower(word),
      word = str_replace_all(word, "\\s+", "_"),
      tipo = case_when(
        value > 0 ~ "Hawkish",
        value < 0 ~ "Dovish",
        TRUE ~ "Neutro"
      )
    ) |>
    distinct(word, .keep_all = TRUE)
}

aplicar_overrides_bis <- function(df_atas) {
  df_atas |>
    mutate(
      texto_limpo = str_to_lower(texto),
      texto_limpo = str_replace_all(texto_limpo, "1\\.00 p\\.p\\.", "hike_100bps"),
      texto_limpo = str_replace_all(texto_limpo, "0\\.75 p\\.p\\.", "hike_75bps"),
      texto_limpo = str_replace_all(texto_limpo, "0\\.50 p\\.p\\.", "hike_50bps"),
      texto_limpo = str_replace_all(texto_limpo, "same magnitude at the next two meetings", "guidance_two_more_hikes"),
      texto_limpo = str_replace_all(texto_limpo, "same magnitude in the next two meetings", "guidance_two_more_hikes"),
      texto_limpo = str_replace_all(texto_limpo, "resume the rate hiking", "resume_the_rate_hiking"),
      texto_limpo = str_replace_all(texto_limpo, "even more contractionary", "even_more_contractionary"),
      texto_limpo = str_replace_all(texto_limpo, "significantly contractionary", "significantly_contractionary"),
      texto_limpo = str_replace_all(texto_limpo, "very prolonged period", "very_prolonged_period"),
      texto_limpo = str_replace_all(texto_limpo, "firm commitment", "firm_commitment"),
      texto_limpo = str_replace_all(texto_limpo, "more adverse", "more_adverse"),
      texto_limpo = str_replace_all(texto_limpo, "further adjustments", "further_adjustments"),
      texto_limpo = str_replace_all(texto_limpo, "strong growth", "strong_growth"),
      texto_limpo = str_replace_all(texto_limpo, "activity moderation", "activity_moderation"),
      texto_limpo = str_replace_all(texto_limpo, "disinflationary gains", "disinflationary_gains"),
      texto_limpo = str_replace_all(texto_limpo, "fiscal discipline", "fiscal_discipline"),
      texto_limpo = str_replace_all(texto_limpo, "beginning of an interest-rate easing cycle", "guidance_easing_cycle"),
      texto_limpo = str_replace_all(texto_limpo, "magnitude of the easing cycle", "guidance_easing_cycle"),
      texto_limpo = str_replace_all(texto_limpo, "initiate the flexibilization", "initiate_flexibilization"),
      texto_limpo = str_replace_all(texto_limpo, "improved current inflation outlook", "improved_inflation_outlook"),
      texto_limpo = str_replace_all(texto_limpo, "less distant from the target", "expectations_less_distant")
    )
}

aplicar_termos_compostos <- function(df_atas, termos_compostos) {
  df_atas |>
    mutate(
      texto_limpo = purrr::map_chr(texto, function(texto_atual) {
        txt <- str_to_lower(texto_atual)

        if (length(termos_compostos) > 0) {
          for (termo in termos_compostos) {
            termo_espaco <- str_replace_all(termo, "_", " ")
            txt <- str_replace_all(txt, fixed(termo_espaco), termo)
          }
        }

        txt
      })
    )
}

calcular_serie_lexico <- function(df_atas, dicionario, nome_lexico, preprocessador, stopwords_extra = NULL) {
  stopwords_base <- stop_words
  stopwords_analise <- if (is.null(stopwords_extra)) {
    stopwords_base
  } else {
    bind_rows(stopwords_base, stopwords_extra)
  }

  tokens <- preprocessador(df_atas) |>
    unnest_tokens(word, texto_limpo) |>
    mutate(word = str_replace_all(word, "[^a-z_]", "")) |>
    filter(word != "") |>
    anti_join(stopwords_analise, by = "word")

  tokens |>
    inner_join(dicionario, by = "word", relationship = "many-to-many") |>
    group_by(id_documento, reuniao_num, data) |>
    summarise(
      total_palavras = n(),
      soma_scores = sum(value),
      net_sentiment = soma_scores / total_palavras,
      .groups = "drop"
    ) |>
    arrange(reuniao_num) |>
    mutate(lexico = nome_lexico, .before = id_documento)
}

el_lexicon <- carregar_economic_lexicon(script_dir)

el_overrides <- tribble(
  ~word, ~value, ~tipo,
  "guidance_two_more_hikes", 3, "Hawkish",
  "hike_100bps", 2, "Hawkish",
  "hike_75bps", 1.5, "Hawkish",
  "even_more_contractionary", 2, "Hawkish",
  "significantly_contractionary", 2, "Hawkish",
  "very_prolonged_period", 1.5, "Hawkish",
  "resume_the_rate_hiking", 1.5, "Hawkish",
  "deanchoring", 1, "Hawkish",
  "deanchored", 1, "Hawkish",
  "further_adjustments", 1, "Hawkish",
  "more_adverse", 1, "Hawkish",
  "depreciated", 1, "Hawkish",
  "vigilant", 1, "Hawkish",
  "upside", 1, "Hawkish",
  "strength", 1, "Hawkish",
  "tightening", 1, "Hawkish",
  "restrictive", 1, "Hawkish",
  "restriction", 0.6, "Hawkish",
  "inflationary", 1, "Hawkish",
  "pressured", 0.8, "Hawkish",
  "resilient", 0.8, "Hawkish",
  "challenging", 0.5, "Hawkish",
  "red", 0.5, "Hawkish",
  "dynamic", 0.6, "Hawkish",
  "overheating", 1, "Hawkish",
  "expanding", 0.5, "Hawkish",
  "strong_growth", 1, "Hawkish",
  "discipline", 1, "Hawkish",
  "tight_labor_market", 1, "Hawkish",
  "cut", -1, "Dovish",
  "downside", -1, "Dovish",
  "accommodative", -1, "Dovish",
  "easing", -1, "Dovish",
  "slack", -1, "Dovish",
  "benign", -1, "Dovish",
  "disinflation", -1, "Dovish",
  "green", -0.5, "Dovish",
  "moderation", -1, "Dovish",
  "disinflationary", -1, "Dovish",
  "guidance_easing_cycle", -2, "Dovish",
  "initiate_flexibilization", -2, "Dovish",
  "improved_inflation_outlook", -1.5, "Dovish",
  "expectations_less_distant", -1.5, "Dovish"
)

el_custom_stopwords <- tibble(
  word = c(
    "committee",
    "prospective",
    "relevant",
    "volatility",
    "meeting",
    "members",
    "participants",
    "staff",
    "funding",
    "support",
    "consistent",
    "goal",
    "issuance",
    "compensation",
    "progress",
    "effective",
    "achieve",
    "board",
    "preferred",
    "clarify",
    "ample",
    "importance",
    "standing",
    "notable",
    "information",
    "data",
    "financing",
    "confidence",
    "broad",
    "volatile",
    "volatility",
    "stabilize",
    "repurchase",
    "favored",
    "approval",
    "commitment"
  ),
  lexicon = "custom"
)

dicionario_md_puro <- get_sentiments("loughran") |>
  filter(sentiment %in% c("positive", "negative")) |>
  mutate(
    value = if_else(sentiment == "positive", 1, -1),
    tipo = if_else(sentiment == "positive", "Hawkish", "Dovish")
  ) |>
  select(word, value, tipo) |>
  distinct(word, .keep_all = TRUE)

dicionario_el <- bind_rows(el_overrides, el_lexicon) |>
  distinct(word, .keep_all = TRUE)

termos_compostos_el_puro <- el_lexicon |>
  filter(str_detect(word, "_")) |>
  pull(word)

serie_md_customizado <- serie_sentimento |>
  mutate(lexico = "MD customizado", .before = id_documento)

serie_md_puro <- calcular_serie_lexico(
  df_atas = atas_copom,
  dicionario = dicionario_md_puro,
  nome_lexico = "MD Puro",
  preprocessador = function(df_atas) df_atas |> mutate(texto_limpo = str_to_lower(texto))
)

serie_el <- calcular_serie_lexico(
  df_atas = atas_copom,
  dicionario = dicionario_el,
  nome_lexico = "EL customizado",
  preprocessador = aplicar_overrides_bis,
  stopwords_extra = el_custom_stopwords
)

serie_el_puro <- calcular_serie_lexico(
  df_atas = atas_copom,
  dicionario = el_lexicon,
  nome_lexico = "EL Puro",
  preprocessador = function(df_atas) aplicar_termos_compostos(df_atas, termos_compostos_el_puro)
)

comparacao_lexicos <- bind_rows(
  serie_md_customizado,
  serie_md_puro,
  serie_el,
  serie_el_puro
) |>
  mutate(lexico = factor(lexico, levels = c("MD customizado", "MD Puro", "EL customizado", "EL Puro"))) |>
  arrange(lexico, data, reuniao_num)

dados_comparacao_lexicos <- comparacao_lexicos |>
  filter(!is.na(data)) |>
  left_join(selic_data |> select(ref.date, SELIC) |> rename(data = ref.date), by = "data") |>
  filter(!is.na(SELIC))



if (nrow(dados_comparacao_lexicos) > 0) {
  calcular_parametros_eixo_secundario <- function(limite_primario_min, limite_primario_max, limite_selic_min, limite_selic_max) {
    if (limite_primario_max > limite_primario_min && limite_selic_max > limite_selic_min) {
      coef_escala <- (limite_selic_max - limite_selic_min) / (limite_primario_max - limite_primario_min)
      intercepto <- limite_selic_min - limite_primario_min * coef_escala
    } else {
      coef_escala <- 1
      intercepto <- 0
    }

    list(coef_escala = coef_escala, intercepto = intercepto)
  }

  construir_plot_comparacao <- function(dados_plot, limites_primarios, mostrar_titulo = FALSE, mostrar_legenda = TRUE, mostrar_eixo_x = TRUE) {
    parametros_eixo <- calcular_parametros_eixo_secundario(
      limite_primario_min = limites_primarios[1],
      limite_primario_max = limites_primarios[2],
      limite_selic_min = -0.5,
      limite_selic_max = 15.5
    )

    dados_plot <- dados_plot |>
      mutate(selic_em_escala_sentimento = (SELIC - parametros_eixo$intercepto) / parametros_eixo$coef_escala)

    ggplot(dados_plot, aes(x = data)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      geom_line(aes(y = net_sentiment, color = "Sentimento"), linewidth = 1.2) +
      geom_point(aes(y = net_sentiment, color = "Sentimento"), size = 1.4) +
      geom_line(aes(y = selic_em_escala_sentimento, color = "Selic"), linewidth = 1.1) +
      geom_point(aes(y = selic_em_escala_sentimento, color = "Selic"), size = 1.2) +
      facet_wrap(~ lexico, scales = "fixed", ncol = 2) +
      scale_x_date(date_breaks = "12 months", date_labels = "%b/%Y") +
      scale_y_continuous(
        name = "Sentimento Líquido (Net Sentiment Score)",
        limits = limites_primarios,
        expand = expansion(mult = 0),
        labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01),
        sec.axis = sec_axis(
          ~ . * parametros_eixo$coef_escala + parametros_eixo$intercepto,
          name = "Taxa Selic (%)",
          labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)
        )
      ) +
      scale_color_manual(values = c("Sentimento" = COR_SERIE, "Selic" = "black"), name = "") +
      labs(
        title = if (mostrar_titulo) "Comparação do Índice de Sentimento por Léxico" else NULL,
        subtitle = if (mostrar_titulo) "MD customizado, MD Puro, EL customizado e EL Puro com a Selic no eixo secundário" else NULL,
        x = if (mostrar_eixo_x) "Data da Reunião" else NULL
      ) +
      tema_copom(angle_x = TRUE, legend_position = if (mostrar_legenda) "top" else "none") +
      theme(
        plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0),
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.position = if (mostrar_legenda) "top" else "none",
        axis.title.x = if (mostrar_eixo_x) element_text(size = 11) else element_blank(),
        axis.text.x = if (mostrar_eixo_x) element_text(angle = 45, hjust = 1, size = 10) else element_blank(),
        axis.ticks.x = if (mostrar_eixo_x) element_line() else element_blank()
      )
  }

  salvar_grafico_comparacao_empilhado <- function(plot_superior, plot_inferior, fixed_path, local_ts_dir, width = 12, height = 11, dpi = 300) {
    garantir_diretorio(dirname(fixed_path))
    garantir_diretorio(local_ts_dir)

    caminhos_saida <- c(fixed_path, file.path(local_ts_dir, basename(fixed_path)))

    for (caminho_saida in caminhos_saida) {
      png(filename = caminho_saida, width = width, height = height, units = "in", res = dpi, bg = "transparent")
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 2, ncol = 1, heights = grid::unit(c(1, 1), "null"))))
      print(plot_superior, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(plot_inferior, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
      dev.off()
    }
  }

  dados_md <- dados_comparacao_lexicos |>
    filter(lexico %in% c("MD customizado", "MD Puro"))

  dados_el <- dados_comparacao_lexicos |>
    filter(lexico %in% c("EL customizado", "EL Puro"))

  intervalo_md <- range(dados_md$net_sentiment, na.rm = TRUE)
  amplitude_md <- diff(intervalo_md)

  if (amplitude_md > 0) {
    folga_md <- amplitude_md * 0.08
  } else {
    folga_md <- max(abs(intervalo_md), na.rm = TRUE) * 0.08
    if (!is.finite(folga_md) || folga_md == 0) {
      folga_md <- 0.05
    }
  }

  limites_md <- c(intervalo_md[1] - folga_md, intervalo_md[2] + folga_md)
  limites_el <- c(-0.25, 0.25)

  plot_comparacao_md <- construir_plot_comparacao(
    dados_plot = dados_md,
    limites_primarios = limites_md,
    mostrar_titulo = TRUE,
    mostrar_legenda = TRUE,
    mostrar_eixo_x = FALSE
  )

  plot_comparacao_el <- construir_plot_comparacao(
    dados_plot = dados_el,
    limites_primarios = limites_el,
    mostrar_titulo = FALSE,
    mostrar_legenda = FALSE,
    mostrar_eixo_x = TRUE
  )

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 2, ncol = 1, heights = grid::unit(c(1, 1), "null"))))
  print(plot_comparacao_md, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(plot_comparacao_el, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))

  salvar_grafico_comparacao_empilhado(
    plot_superior = plot_comparacao_md,
    plot_inferior = plot_comparacao_el,
    fixed_path = path_png_comparacao_lexicos,
    local_ts_dir = output_local_ts_dir,
    width = 12,
    height = 11,
    dpi = 300
  )
}

save_csv_all(comparacao_lexicos, path_csv_comparacao_lexicos, output_local_ts_dir)
log_info("✓ Comparação entre léxicos salva em:", path_csv_comparacao_lexicos)

safe_cor <- function(x, y) {
  dados_validos <- is.finite(x) & is.finite(y)

  if (sum(dados_validos) < 3) {
    return(NA_real_)
  }

  x_valid <- x[dados_validos]
  y_valid <- y[dados_validos]

  if (sd(x_valid) == 0 || sd(y_valid) == 0) {
    return(NA_real_)
  }

  cor(x_valid, y_valid)
}

base_scorecard_lexicos <- dados_comparacao_lexicos |>
  arrange(lexico, data, reuniao_num) |>
  group_by(lexico) |>
  mutate(
    selic_nivel_t1 = lead(SELIC),
    delta_selic_t = SELIC - lag(SELIC),
    delta_selic_t1 = lead(SELIC) - SELIC,
    acerto_direcional_t1 = case_when(
      is.na(delta_selic_t1) ~ NA_real_,
      delta_selic_t1 > 0 & net_sentiment > 0 ~ 1,
      delta_selic_t1 < 0 & net_sentiment < 0 ~ 1,
      delta_selic_t1 != 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) |>
  ungroup()

scorecard_lexicos <- base_scorecard_lexicos |>
  group_by(lexico) |>
  summarise(
    n_observacoes = n(),
    cor_delta_selic_t = safe_cor(net_sentiment, delta_selic_t),
    cor_delta_selic_t1 = safe_cor(net_sentiment, delta_selic_t1),
    cor_nivel_selic_t1 = safe_cor(net_sentiment, selic_nivel_t1),
    acerto_direcional_t1 = mean(acerto_direcional_t1, na.rm = TRUE),
    n_decisoes_variacao_t1 = sum(!is.na(acerto_direcional_t1)),
    .groups = "drop"
  ) |>
  arrange(desc(cor_delta_selic_t1)) |>
  mutate(
    across(c(cor_delta_selic_t, cor_delta_selic_t1, cor_nivel_selic_t1, acerto_direcional_t1), ~ round(., 3))
  )

writexl::write_xlsx(list("Scorecard" = scorecard_lexicos), path_excel_scorecard_lexicos)
file.copy(path_excel_scorecard_lexicos, file.path(output_local_ts_dir, basename(path_excel_scorecard_lexicos)), overwrite = TRUE)
log_info("✓ Scorecard dos léxicos salvo em:", path_excel_scorecard_lexicos)

dados_scatter_lexicos <- base_scorecard_lexicos |>
  filter(!is.na(delta_selic_t1))

if (nrow(dados_scatter_lexicos) > 0) {
  grafico_scatter_lexicos <- ggplot(dados_scatter_lexicos, aes(x = net_sentiment, y = delta_selic_t1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    geom_point(color = COR_SERIE, size = 2, alpha = 0.75) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.9) +
    facet_wrap(~ lexico, scales = "free") +
    labs(
      title = "Sentimento da Ata vs Proxima Variacao da Selic",
      subtitle = "Cada ponto representa uma reunião; a linha preta é o ajuste linear simples",
      x = "Sentimento Liquido da Ata",
      y = "Selic t+1 menos Selic t"
    ) +
    tema_copom(legend_position = "none") +
    theme(
      plot.title = element_text(hjust = 0, face = "bold"),
      plot.subtitle = element_text(hjust = 0),
      strip.text = element_text(face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

  print(grafico_scatter_lexicos)
  save_plot_all(grafico_scatter_lexicos, path_png_scatter_lexicos, output_local_ts_dir, width = 11, height = 8)
}

# --- 13. NOTIFICAÇÃO ---
beep(sound = 1)
log_info("===== PROCESSAMENTO CONCLUÍDO =====")
