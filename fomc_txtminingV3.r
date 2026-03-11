# --- 0. CARREGANDO PACOTES ---
library(tidyquant) # Para baixar a Fed Funds Rate
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
font_add("plusjakarta", 
    regular = "C:/Users/GabrielHenriqueMarti/AppData/Local/Microsoft/Windows/Fonts/PLUSJAKARTASANS-Regular.ttf")

showtext_auto()
showtext_opts(dpi = 300)

# --- 0.2. CONFIGURAÇÃO DE ANÁLISE DE DATA ESPECÍFICA ---
# Para analisar uma data específica, defina abaixo (formato: "YYYY-MM-DD")
# Se deixar NULL, o script continuará normalmente
data_personalizada <- "2025-12-09"  # Exemplo: "2024-12-18" ou NULL para desabilitar

# ====================================================================
# --- 1. GERENCIAMENTO DE CAMINHOS E ARQUIVAMENTO LOCAL ---
# ====================================================================

tryCatch({
  script_path <- rstudioapi::getSourceEditorContext()$path
  script_dir <- dirname(script_path)
  setwd(script_dir)
  # ✓ Diretório de trabalho definido
}, error = function(e) {
  script_dir <- getwd()
  # Atenção: Não foi possível obter o diretório do script via rstudioapi. Usando getwd().
})

path_base_projeto <- file.path(script_dir, "resultados_fomc_experimento")
path_csv_serie <- file.path(path_base_projeto, "fomc_sentimento_serie.csv")
path_csv_completo <- file.path(path_base_projeto, "fomc_sentimento_completo.csv")
path_png_reuniao <- file.path(path_base_projeto, "fomc_sentimento_por_reuniao.png")
path_png_data <- file.path(path_base_projeto, "fomc_sentimento_por_data.png")
path_png_selic_sentimento <- file.path(path_base_projeto, "fomc_fedfunds_vs_sentimento.png")
path_png_top_palavras <- file.path(path_base_projeto, "fomc_top_palavras_sentimento.png")
path_png_top_palavras_recente <- file.path(path_base_projeto, "fomc_top_palavras_reuniao_recente.png")
path_png_top_palavras_personalizada <- file.path(path_base_projeto, "fomc_top_palavras_data_personalizada.png")
path_png_regime_timeline <- file.path(path_base_projeto, "fomc_regime_timeline.png")
path_png_regime_distribuicao <- file.path(path_base_projeto, "fomc_regime_distribuicao.png")
path_excel_analise <- file.path(path_base_projeto, "fomc_analise_completa.xlsx")

timestamp_run <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_local_ts_dir <- file.path(script_dir, paste0("FOMC_EXP_", timestamp_run))

if (!dir.exists(output_local_ts_dir)) dir.create(output_local_ts_dir, recursive = TRUE)
if (!dir.exists(path_base_projeto)) dir.create(path_base_projeto, recursive = TRUE)

caminho_pasta_pdfs <- file.path(script_dir, "atas_fomc")
if(!dir.exists(caminho_pasta_pdfs)) stop("Crie a pasta 'atas_fomc' e insira os PDFs")
arquivos_pdf <- list.files(path = caminho_pasta_pdfs, pattern = "\\.pdf$", full.names = TRUE)
if(length(arquivos_pdf) == 0) stop("Nenhum arquivo PDF encontrado na pasta atas_fomc.")

save_plot_all <- function(plot, fixed_path, local_ts_dir, width = 8, height = 7, dpi = 300) {
  ggsave(filename = fixed_path, plot = plot, bg = "transparent", width = width, height = height, dpi = dpi)
  ggsave(filename = file.path(local_ts_dir, basename(fixed_path)), plot = plot, bg = "transparent", width = width, height = height, dpi = dpi)
}

save_csv_all <- function(df, fixed_path, local_ts_dir) {
  readr::write_csv(df, fixed_path)
  readr::write_csv(df, file.path(local_ts_dir, basename(fixed_path)))
}

# ====================================================================
# --- 2. INGESTÃO DE DADOS (PDFs das Atas do FOMC) ---
# ====================================================================

ler_ata_pdf_fomc <- function(caminho_arquivo) {
  tryCatch({
    texto_bruto <- pdftools::pdf_text(caminho_arquivo) %>% 
      stringr::str_c(collapse = " ") %>% stringr::str_squish() 
    
    data_texto <- stringr::str_extract(texto_bruto, "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2}(?:-\\d{1,2})?,\\s+\\d{4}")
    
    texto_analitico <- stringr::str_extract(texto_bruto, "(?i)(Developments in Financial Markets and Open Market Operations|Staff Review of the Economic Situation).*")
    if(is.na(texto_analitico)) texto_analitico <- texto_bruto
    
    nome_arquivo <- basename(caminho_arquivo)
    
    data_final <- if(!is.na(data_texto)) {
      d <- stringr::str_replace(data_texto, "([A-Za-z]+\\s+)\\d+-(\\d+)(,\\s+\\d{4})", "\\1\\2\\3")
      lubridate::mdy(d, quiet = TRUE)
    } else {
      data_arq <- stringr::str_extract(nome_arquivo, "\\d{8}")
      if(!is.na(data_arq)) lubridate::ymd(data_arq, quiet = TRUE) else NA_Date_
    }
    
    tibble::tibble(
      id_documento = nome_arquivo,
      data         = data_final,
      texto        = texto_analitico
    )
  }, error = function(e) { return(NULL) })
}

# --- Iniciando Ingestão de Dados FOMC ---
atas_fomc <- purrr::map_dfr(arquivos_pdf, ler_ata_pdf_fomc) %>% 
  arrange(data) %>%
  mutate(reuniao_num = row_number())

# ✓ Leitura concluída

# ====================================================================
# --- 3. CONSTRUÇÃO DO LÉXICO HÍBRIDO (EL + PERSONALIZADO) ---
# ====================================================================

# --- Importando Economic Lexicon (EL) ---
caminho_lexico <- file.path(script_dir, "Economic_Lexicon.csv")

if(!file.exists(caminho_lexico)) {
  stop("ERRO: O arquivo 'Economic_Lexicon.csv' não foi encontrado no diretório do script.")
}

# Lê o CSV do Léxico Econômico
el_lexicon <- readr::read_csv(caminho_lexico, show_col_types = FALSE) |>
  rename(word = 1, value = 2, tipo = 3) |>
  mutate(
    word = str_to_lower(word),
    value = as.numeric(value),
    # Converte polaridade numérica em texto para compatibilidade
    tipo = case_when(
      tipo > 0 ~ "Hawkish",
      tipo < 0 ~ "Dovish",
      TRUE ~ "Neutro"
    ),
    # Previne que o R separe bigramas que possam estar no CSV inserindo um underline
    word = str_replace_all(word, "\\s+", "_") 
  ) |>
  distinct(word, .keep_all = TRUE)

# ✓ Léxico EL carregado

# Constrói o Léxico FOMC Personalizado (originário da V1)
# --- Construindo Léxico FOMC Personalizado ---
fomc_overrides <- tribble(
  ~word, ~value, ~tipo,
  
  # --- 0. NEUTRALIZADORES DE AMBIGUIDADE (Anulam falsos sinais do LM/EL) ---
  "unemployment", 0, "Neutro",
  "risk", 0, "Neutro",
  "risks", 0, "Neutro",
  "uncertainty", 0, "Neutro",
  "inflation", 0, "Neutro",
  "growth", 0, "Neutro",
  "stable", 0, "Neutro",
  
  # --- 1. HAWKISH (+): Aperto, Inflação Alta e Mercado de Trabalho Forte ---
  "hike_75bps", 3.0, "Hawkish",
  "hike_50bps", 2.0, "Hawkish",
  "hike_25bps", 1.0, "Hawkish",
  "raise_target_range", 2.0, "Hawkish",
  "increase_target_range", 2.0, "Hawkish",
  "upward", 0.7, "Hawkish",
  "ongoing_increases", 1.5, "Hawkish",
  "sufficiently_restrictive", 1, "Hawkish",
  "restrictive_stance", 1, "Hawkish",
  "balance_sheet_runoff", 0.2, "Hawkish",
  "cautioned", 0.1, "Hawkish",
  "reducing_holdings", 0.6, "Hawkish",
  "elevated", 0.2, "Hawkish",
  "persistent", 0.6, "Hawkish",
  "entrenched", 1.0, "Hawkish",
  
  "highly_attentive_inflation_risks", 2.0, "Hawkish",
  "upside_risks_inflation", 0.5, "Hawkish",
  "inflationary_pressures", 1, "Hawkish",
  
  "tight_labor_market", 1.5, "Hawkish",
  "robust_job_gains", 1, "Hawkish",
  "strong_job_gains", 1, "Hawkish",
  "low_unemployment", 1, "Hawkish",
  "expanding_solid_pace", 0.2, "Hawkish",
  "resilient", 0.5, "Hawkish",

  # --- 2. DOVISH (-): Cortes, Desinflação, Reequilíbrio e Fraqueza ---
  "cut_75bps", -3.0, "Dovish",
  "cut_50bps", -2.5, "Dovish",
  "cut_25bps", -1.5, "Dovish",
  "lower_target_range", -1.0, "Dovish",
  "reduce_target_range", -1.0, "Dovish",
  "cut", -1.5, "Dovish",
  "accommodative", -1.5, "Dovish",
  "below", -1.0, "Dovish",
  "disinflation", -1.0, "Dovish",
  "shutdown", -0.3, "Dovish",
  "weakness", -1.0, "Dovish",
  "weak", -1.0, "Dovish",
  "outbreak", -2.0, "Dovish",
  
  # Jargões Recentes (2024-2026) de Pouso Suave e Cortes
  "inflation_has_eased", -1.0, "Dovish",
  "progress_on_inflation", -1.5, "Dovish",
  "disinflationary_process", -1.5, "Dovish",
  "disinflationaty_trend", -1.5, "Dovish",
  "less_restrictive", -1.5, "Dovish",
  "unemployment_moved_up", -2.0, "Dovish",
  "labor_market_cooling", -2.0, "Dovish",
  "severely_disrupting", -3.0, "Dovish",
  "gradual_cooling", -1, "Dovish",
  "labor_market_softening", -2.0, "Dovish",
  "easing_labor_market", -1.5, "Dovish",
  "job_gains_moderated", -1.5, "Dovish",
  "gains_remained_low", -1.0, "Dovish",
  "slowing_job_gains", -1.5, "Dovish",
  "higher_unemployment", -1.5, "Dovish",
  "downward", -1.0, "Dovish",
  "downside_risks", -0.5, "Dovish",
  "lower_the_target_range", -1.0, "Dovish",
  "low-hiring", -1.0, "Dovish",
  "job_losses_suged", -3.0, "Dovish"

)

# ✓ Léxico FOMC Personalizado criado

# Combina EL + FOMC Personalizado (prioridade para o personalizado via distinct)
dicionario_fomc <- bind_rows(fomc_overrides, el_lexicon) |> 
  distinct(word, .keep_all = TRUE)

# ✓ LÉXICO HÍBRIDO FINAL combinado

# ====================================================================
# --- 3.1. PERSONALIZAÇÃO DE STOP WORDS ---
# ====================================================================

# --- Construindo Stop Words Personalizadas ---

# Stop words customizadas específicas para análise do FOMC
# (Adicione aqui palavras que devem ser removidas além das stop words padrão)
custom_stopwords <- tribble(
  ~word,
  "committee",
  "federal",
  "reserve",
  "volatility",
  "fomc",
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
  "pandemic",
  "stabilize",
  "repurchase",
  "favored",
  "approval",
  "commitment"
  # Adicione mais palavras conforme necessário
)

# Combina stop words padrão do tidytext com as personalizadas
stopwords_completo <- bind_rows(stop_words, custom_stopwords) |>
  distinct(word, .keep_all = TRUE)

# ✓ Stop Words combinadas

# Remove stop words customizadas do dicionário de sentimento para garantir
# que não contribuam mesmo se houver variações que escapem do anti_join
palavras_removidas_do_dicionario <- dicionario_fomc |> 
  semi_join(custom_stopwords, by = "word") |> 
  pull(word)

# ℹ Removendo stop words do dicionário (se necessário)

dicionario_fomc <- dicionario_fomc |>
  anti_join(custom_stopwords, by = "word")

# ✓ Dicionário após remoção de stop words

# ====================================================================
# --- 4. ANÁLISE DE SENTIMENTO (LÉXICO HÍBRIDO COM REGEX) ---
# ====================================================================

if(nrow(atas_fomc) == 0) stop("Nenhuma ata processada com sucesso.")
# --- Iniciando Análise de Sentimento FOMC (Léxico Híbrido) ---

# Identifica termos compostos no léxico híbrido para pré-processamento
termos_compostos <- dicionario_fomc |> filter(str_detect(word, "_")) |> pull(word)
# ✓ Termos compostos identificados

# Função de limpeza estendida que combina REGEX do V1 + dinâmico do V2
# Nota: usa a variável termos_compostos do ambiente pai
limpar_texto_hibrido <- function(texto) {
  txt <- str_to_lower(texto)
  
  # Normalização Tipográfica de Frações e Artefactos de PDF
  txt <- str_replace_all(txt, "¼", "1/4")
  txt <- str_replace_all(txt, "½", "1/2")
  txt <- str_replace_all(txt, "¾", "3/4")
  txt <- str_replace_all(txt, "\\$", "")
  
  # A) Tradução de Jargões Hawkish (Subidas de Juros)
  txt <- str_replace_all(txt, "(raise|increase)(d|s)? (the )?target range (by )?3/4", "hike_75bps")
  txt <- str_replace_all(txt, "(raise|increase)(d|s)? (the )?target range (by )?1/2", "hike_50bps")
  txt <- str_replace_all(txt, "(raise|increase)(d|s)? (the )?target range (by )?1/4", "hike_25bps")
  txt <- str_replace_all(txt, "75 basis point(s)? increase", "hike_75bps")
  txt <- str_replace_all(txt, "50 basis point(s)? increase", "hike_50bps")
  txt <- str_replace_all(txt, "25 basis point(s)? increase", "hike_25bps")
  txt <- str_replace_all(txt, "(raise|increase)(d|s)? (the )?target range", "raise_target_range")
  txt <- str_replace_all(txt, "ongoing increase(s)? (in the target range)?", "ongoing_increases")
  txt <- str_replace_all(txt, "sufficiently restrictive", "sufficiently_restrictive")
  txt <- str_replace_all(txt, "restrictive stance", "restrictive_stance")
  txt <- str_replace_all(txt, "balance sheet runoff", "balance_sheet_runoff")
  txt <- str_replace_all(txt, "reduc(e|ing) (its )?holdings", "reducing_holdings")
  
  # B) Hawkish: Diagnóstico de Inflação
  txt <- str_replace_all(txt, "highly attentive to inflation risk(s)?", "highly_attentive_inflation_risks")
  txt <- str_replace_all(txt, "persistently high inflation", "persistently_high_inflation")
  txt <- str_replace_all(txt, "upside risk(s)? to inflation", "upside_risks_inflation")
  txt <- str_replace_all(txt, "firm commitment to return(ing)? inflation", "firm_commitment_inflation")


  # C) Hawkish: Diagnóstico de Emprego e Atividade
  txt <- str_replace_all(txt, "tight labor market", "tight_labor_market")
  txt <- str_replace_all(txt, "labor market remain(s|ed) tight", "tight_labor_market")
  txt <- str_replace_all(txt, "robust job gain(s)?", "robust_job_gains")
  txt <- str_replace_all(txt, "strong job gain(s)?", "strong_job_gains")
  txt <- str_replace_all(txt, "low unemployment", "low_unemployment")
  txt <- str_replace_all(txt, "expand(ing|ed|s)? (at a )?solid pace", "expanding_solid_pace")
  txt <- str_replace_all(txt, "gains remained low", "gains_remained_low")

  # D) Tradução de Jargões Dovish (Cortes de Juros)
  txt <- str_replace_all(txt, "(lower|reduce)(d|s)? (the )?target range (by )?1/2", "cut_50bps")
  txt <- str_replace_all(txt, "(lower|reduce)(d|s)? (the )?target range (by )?1/4", "cut_25bps")
  txt <- str_replace_all(txt, "(lower|reduce)(d|s)? (the )?target range", "lower_target_range")
  
  # E) Dovish: Recentes (2024-2026) e Fraqueza Económica
  txt <- str_replace_all(txt, "better balance", "better_balance")
  txt <- str_replace_all(txt, "inflation (has )?eased", "inflation_has_eased")
  txt <- str_replace_all(txt, "progress on inflation", "progress_on_inflation")
  txt <- str_replace_all(txt, "disinflationary (process|trend)", "disinflationary_process")
  txt <- str_replace_all(txt, "less restrictive", "less_restrictive")
  txt <- str_replace_all(txt, "low-hiring", "low_hiring")

  
  txt <- str_replace_all(txt, "unemployment (rate )?(has )?moved up", "unemployment_moved_up")
  txt <- str_replace_all(txt, "severly disrupting", "severely_disrupting")
  txt <- str_replace_all(txt, "job losses surged", "job_losses_surged")
  txt <- str_replace_all(txt, "labor market (has )?(cooled|cooling)", "labor_market_cooling")
  txt <- str_replace_all(txt, "soften(ing|ed) (in |in the )?labor market", "labor_market_softening")
  txt <- str_replace_all(txt, "eas(ing|ed) (in |in the )?labor market", "easing_labor_market")
  txt <- str_replace_all(txt, "(job gains|employment) (have )?moderated", "job_gains_moderated")
  txt <- str_replace_all(txt, "slow(ing|ed) job gains", "slowing_job_gains")
  txt <- str_replace_all(txt, "higher unemployment", "higher_unemployment")
  txt <- str_replace_all(txt, "downside risk(s)?", "downside_risks")
  
  # F) Processamento dinâmico: junta termos compostos que vieram do CSV EL
  if(length(termos_compostos) > 0) {
    for(termo in termos_compostos) {
      termo_espaco <- str_replace_all(termo, "_", " ")
      txt <- str_replace_all(txt, termo_espaco, termo)
    }
  }
  
  return(txt)
}

atas_fomc_tratado <- atas_fomc |>
  mutate(texto_limpo = map_chr(texto, limpar_texto_hibrido))

fomc_tokens <- atas_fomc_tratado |>
  unnest_tokens(word, texto_limpo) |>
  mutate(word = str_replace_all(word, "[^a-z0-9_]", "")) |> 
  filter(word != "") |>
  anti_join(stopwords_completo, by = "word")

fomc_indice <- fomc_tokens |>
  inner_join(dicionario_fomc, by = "word", relationship = "many-to-many") |>
  # Filtra as palavras com peso zero para evitar que dominem as tabelas de top palavras
  filter(value != 0) |>
  group_by(id_documento, reuniao_num, data) |>
  summarise(
    total_palavras_mapeadas = n(),
    soma_scores             = sum(value),
    net_sentiment           = soma_scores / total_palavras_mapeadas,
    .groups = "drop"
  ) |>
  arrange(reuniao_num)

# ✓ Análise de sentimento concluída

# ====================================================================
# --- 5. IMPORTAÇÃO DA TAXA FED FUNDS (FRED) ---
# ====================================================================

# --- Importando Série da Fed Funds Rate ---
data_min_sentimento <- min(fomc_indice$data, na.rm = TRUE) - 15

fedfunds_data <- tidyquant::tq_get("DFF", get = "economic.data", from = data_min_sentimento) |>
  rename(data = date, FEDFUNDS = price)

# ✓ Fed Funds Rate importada

# ====================================================================
# --- 6. PREPARAÇÃO DA SÉRIE HISTÓRICA E SALVAMENTO ---
# ====================================================================

# --- Iniciando Preparação e Salvamento de Dados ---

serie_sentimento <- fomc_indice |>
  select(id_documento, reuniao_num, data, net_sentiment, soma_scores, total_palavras_mapeadas)

save_csv_all(serie_sentimento, path_csv_serie, output_local_ts_dir)
save_csv_all(atas_fomc, path_csv_completo, output_local_ts_dir)
# ✓ Datasets salvos

# ====================================================================
# --- 7. GERAÇÃO DE GRÁFICOS ---
# ====================================================================

# --- Iniciando Geração de Gráficos ---

p1 <- ggplot(serie_sentimento, aes(x = reuniao_num, y = net_sentiment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = "#189CD8", linewidth = 1.5) +
  geom_point(color = "#189CD8", size = 1.5) +
  labs(
    title    = "Sentimento das Atas do FOMC (Economic Lexicon)",
    x        = "Número Cronológico da Reunião",
    y        = "Sentimento Líquido (Net Sentiment Score)"
  ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
  theme_minimal(base_size = 14, base_family = "plusjakarta") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
print(p1)
save_plot_all(p1, path_png_reuniao, output_local_ts_dir)

serie_com_data <- serie_sentimento |> filter(!is.na(data)) |> arrange(data)

if (nrow(serie_com_data) > 0) {
  p2 <- ggplot(serie_com_data, aes(x = data, y = net_sentiment)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(color = "#189CD8", linewidth = 1.5) +
    geom_point(color = "#189CD8", size = 1.5) +
    geom_smooth(method = "loess", se = FALSE, color = "firebrick", linewidth = 0.9, span = 0.6) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
    labs(
      title    = "Sentimento das Atas do FOMC (Economic Lexicon)",
      x        = "",
      y        = "Sentimento Líquido"
    ) +
    theme_minimal(base_size = 14, base_family = "plusjakarta") +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  print(p2)
  save_plot_all(p2, path_png_data, output_local_ts_dir)
}

if (nrow(serie_com_data) > 0 && nrow(fedfunds_data) > 0) {
  dados_combinados <- serie_com_data |>
    select(data, net_sentiment) |>
    left_join(fedfunds_data, by = "data") |>
    fill(FEDFUNDS, .direction = "downup") |> 
    filter(!is.na(FEDFUNDS))

  if (nrow(dados_combinados) > 0) {
    min_sentiment <- min(dados_combinados$net_sentiment, na.rm = TRUE)
    max_sentiment <- max(dados_combinados$net_sentiment, na.rm = TRUE)
    max_rate <- max(dados_combinados$FEDFUNDS, na.rm = TRUE)

    # Evita divisão por zero caso a variação de sentimento seja zero
    if(max_sentiment > min_sentiment) {
        coef_escala <- max_rate / (max_sentiment - min_sentiment)
        intercepto <- -min_sentiment * coef_escala
    } else {
        coef_escala <- 1
        intercepto <- 0
    }

    p3 <- ggplot(dados_combinados, aes(x = data)) +
      geom_line(aes(y = net_sentiment, color = "Sentimento FOMC"), linewidth = 1.3) +
      geom_point(aes(y = net_sentiment, color = "Sentimento FOMC"), size = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      geom_line(aes(y = (FEDFUNDS - intercepto) / coef_escala, color = "Fed Funds Rate"), linewidth = 1.3) +
      geom_point(aes(y = (FEDFUNDS - intercepto) / coef_escala, color = "Fed Funds Rate"), size = 1.5) +
      scale_y_continuous(
        name = "Net Sentiment Score",
        labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01),
        sec.axis = sec_axis(~ . * coef_escala + intercepto, name = "Fed Funds Rate (%)", labels = comma_format(accuracy = 0.01))
      ) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
      scale_color_manual(values = c("Sentimento FOMC" = "#189CD8", "Fed Funds Rate" = "black"), name = "") +
      labs(
        title = "Sentimento do FOMC vs Fed Funds Rate",
        subtitle = "Baseado no Economic Lexicon (Barbaglia et al. 2024) + Léxico Personalizado",
        x = "Data da Reunião"
      ) +
      theme_minimal(base_size = 14, base_family = "plusjakarta") +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
    print(p3)
    save_plot_all(p3, path_png_selic_sentimento, output_local_ts_dir)
  }
}

# ====================================================================
# --- 8. ANÁLISES AVANÇADAS E EXPORTAÇÃO ---
# ====================================================================

# --- Iniciando Análises Avançadas ---

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

palavras_com_score <- fomc_tokens |>
  inner_join(dicionario_fomc, by = "word", relationship = "many-to-many") |>
  # Filtro extra: remove qualquer palavra que possa estar nas stop words customizadas
  anti_join(custom_stopwords, by = "word") |>
  group_by(word, value, tipo) |>
  summarise(frequencia = n(), .groups = "drop") |>
  mutate(contribuicao_total = value * frequencia) |>
  arrange(desc(abs(contribuicao_total)))

top_hawkish <- palavras_com_score |> filter(value > 0) |> slice_head(n = 10) |> arrange(contribuicao_total)
top_dovish <- palavras_com_score |> filter(value < 0) |> slice_head(n = 10) |> arrange(desc(contribuicao_total))
top_palavras_combinadas <- bind_rows(top_dovish, top_hawkish)

# Verifica se os tops não estão vazios antes de tentar plotar
if(nrow(top_palavras_combinadas) > 0) {
    g_top_palavras <- ggplot(top_palavras_combinadas, aes(x = reorder(word, contribuicao_total), y = contribuicao_total, fill = tipo)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c("Dovish" = "#2ca02c", "Hawkish" = "#d62728")) +
      labs(title = "Contribuições ao Sentimento (Economic Lexicon)", subtitle = "Frequência × Score", x = "", y = "Contribuição Total", fill = "Polaridade:") +
      theme_minimal(base_size = 14, base_family = "plusjakarta") + theme(plot.title = element_text(size = 18, face = "bold"), legend.position = "top", plot.background = element_rect(fill = "transparent", color = NA))
    print(g_top_palavras)
    save_plot_all(g_top_palavras, path_png_top_palavras, output_local_ts_dir, width = 8, height = 8)
}

metricas_regime <- atas_fomc |>
  left_join(serie_com_regime |> select(id_documento, regime), by = "id_documento") |>
  mutate(total_palavras_doc = str_count(texto, "\\S+"), total_caracteres = nchar(texto)) |>
  group_by(regime) |>
  summarise(n_reunioes = n(), tamanho_medio_palavras = mean(total_palavras_doc, na.rm = TRUE), .groups = "drop")

top_palavras_por_regime <- fomc_tokens |>
  left_join(serie_com_regime |> select(id_documento, regime), by = "id_documento") |>
  inner_join(dicionario_fomc, by = "word", relationship = "many-to-many") |>
  filter(!is.na(regime)) |>
  group_by(regime, word, value) |>
  summarise(frequencia = n(), contribuicao = sum(value), .groups = "drop") |>
  group_by(regime) |>
  slice_max(order_by = abs(contribuicao), n = 5) |>
  arrange(regime, desc(contribuicao))

aba_serie_regime <- serie_com_regime |> select(reuniao_num, data, net_sentiment, soma_scores, total_palavras = total_palavras_mapeadas, regime) |> arrange(reuniao_num)
aba_top_palavras <- top_palavras_combinadas |> select(tipo, word, value, frequencia, contribuicao_total) |> arrange(desc(abs(contribuicao_total)))
aba_metricas_regime <- metricas_regime |> mutate(across(where(is.numeric), ~ round(., 2)))
aba_top_regime <- top_palavras_por_regime |> select(regime, word, value, frequencia, contribuicao) |> arrange(regime, desc(abs(contribuicao)))

aba_stats <- serie_com_regime |>
  summarise(
    n_observacoes = n(), sentimento_medio = mean(net_sentiment, na.rm = TRUE),
    sentimento_mediano = median(net_sentiment, na.rm = TRUE), hawkish_pct = sum(regime == "Hawkish", na.rm = TRUE) / n() * 100,
    neutro_pct = sum(regime == "Neutro", na.rm = TRUE) / n() * 100, dovish_pct = sum(regime == "Dovish", na.rm = TRUE) / n() * 100
  ) |> mutate(across(where(is.numeric), ~ round(., 3))) |> pivot_longer(everything(), names_to = "metrica", values_to = "valor")

# ====================================================================
# --- 9. ANÁLISES POR DATA ESPECÍFICA ---
# ====================================================================

# --- Iniciando Análises por Data Específica ---

# 9.1. ANÁLISE DA REUNIÃO MAIS RECENTE
# --- Analisando Reunião Mais Recente ---

reuniao_recente <- serie_sentimento |> 
  filter(!is.na(data)) |> 
  arrange(desc(data)) |> 
  slice(1)

if(nrow(reuniao_recente) > 0) {
  # ✓ Reunião mais recente identificada
  
  # Filtra tokens apenas dessa reunião
  palavras_recente <- fomc_tokens |>
    filter(id_documento == reuniao_recente$id_documento) |>
    inner_join(dicionario_fomc, by = "word", relationship = "many-to-many") |>
    # Filtro extra: remove qualquer palavra que possa estar nas stop words customizadas
    anti_join(custom_stopwords, by = "word") |>
    filter(value != 0) |>
    group_by(word, value, tipo) |>
    summarise(frequencia = n(), .groups = "drop") |>
    mutate(contribuicao_total = value * frequencia) |>
    arrange(desc(abs(contribuicao_total)))
  
  top_hawkish_recente <- palavras_recente |> filter(value > 0) |> slice_head(n = 10) |> arrange(contribuicao_total)
  top_dovish_recente <- palavras_recente |> filter(value < 0) |> slice_head(n = 10) |> arrange(desc(contribuicao_total))
  top_palavras_recente <- bind_rows(top_dovish_recente, top_hawkish_recente)
  
  if(nrow(top_palavras_recente) > 0) {
    g_top_palavras_recente <- ggplot(top_palavras_recente, aes(x = reorder(word, contribuicao_total), y = contribuicao_total, fill = tipo)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c("Dovish" = "#2ca02c", "Hawkish" = "#d62728")) +
      labs(
        title = paste("Top Palavras - Reunião", format(reuniao_recente$data, "%d/%m/%Y")),
        subtitle = paste("Net Sentiment:", round(reuniao_recente$net_sentiment, 3)),
        x = "", y = "Contribuição Total", fill = "Polaridade:"
      ) +
      theme_minimal(base_size = 14, base_family = "plusjakarta") + 
      theme(
        plot.title = element_text(size = 18, face = "bold"), 
        legend.position = "top", 
        plot.background = element_rect(fill = "transparent", color = NA)
      )
    print(g_top_palavras_recente)
    save_plot_all(g_top_palavras_recente, path_png_top_palavras_recente, output_local_ts_dir, width = 8, height = 8)
    # ✓ Gráfico da reunião mais recente salvo
  }
} else {
  # ⚠ Nenhuma reunião com data válida encontrada
}

# 9.2. ANÁLISE DE DATA PERSONALIZADA
if(!is.null(data_personalizada)) {
  # --- Analisando Data Personalizada ---
  
  data_target <- as.Date(data_personalizada)
  
  reuniao_personalizada <- serie_sentimento |> 
    filter(!is.na(data)) |> 
    filter(data == data_target)
  
  if(nrow(reuniao_personalizada) > 0) {
    # ✓ Reunião encontrada para data personalizada
    
    # Filtra tokens apenas dessa reunião
    palavras_personalizada <- fomc_tokens |>
      filter(id_documento == reuniao_personalizada$id_documento) |>
      inner_join(dicionario_fomc, by = "word", relationship = "many-to-many") |>
      # Filtro extra: remove qualquer palavra que possa estar nas stop words customizadas
      anti_join(custom_stopwords, by = "word") |>
      filter(value != 0) |>
      group_by(word, value, tipo) |>
      summarise(frequencia = n(), .groups = "drop") |>
      mutate(contribuicao_total = value * frequencia) |>
      arrange(desc(abs(contribuicao_total)))
    
    top_hawkish_pers <- palavras_personalizada |> filter(value > 0) |> slice_head(n = 10) |> arrange(contribuicao_total)
    top_dovish_pers <- palavras_personalizada |> filter(value < 0) |> slice_head(n = 10) |> arrange(desc(contribuicao_total))
    top_palavras_pers <- bind_rows(top_dovish_pers, top_hawkish_pers)
    
    if(nrow(top_palavras_pers) > 0) {
      g_top_palavras_pers <- ggplot(top_palavras_pers, aes(x = reorder(word, contribuicao_total), y = contribuicao_total, fill = tipo)) +
        geom_col() + coord_flip() +
        scale_fill_manual(values = c("Dovish" = "#2ca02c", "Hawkish" = "#d62728")) +
        labs(
          title = paste("Top Palavras - Reunião", format(data_target, "%d/%m/%Y")),
          subtitle = paste("Net Sentiment:", round(reuniao_personalizada$net_sentiment, 3)),
          x = "", y = "Contribuição Total", fill = "Polaridade:"
        ) +
        theme_minimal(base_size = 14, base_family = "plusjakarta") + 
        theme(
          plot.title = element_text(size = 18, face = "bold"), 
          legend.position = "top", 
          plot.background = element_rect(fill = "transparent", color = NA)
        )
      print(g_top_palavras_pers)
      save_plot_all(g_top_palavras_pers, path_png_top_palavras_personalizada, output_local_ts_dir, width = 8, height = 8)
      # ✓ Gráfico da data personalizada salvo
    }
  } else {
    # ⚠ Nenhuma reunião encontrada para a data especificada
    # Datas disponíveis:
    datas_disponiveis <- serie_sentimento |> filter(!is.na(data)) |> pull(data) |> sort()
    # Ver datas_disponiveis para conferir
  }
} else {
  # ℹ Data personalizada não definida
}

# ====================================================================
# --- 10. EXPORTAÇÃO FINAL ---
# ====================================================================

# --- Iniciando Exportação Final ---

lista_abas <- list("Serie_Regime" = aba_serie_regime, "Top_Palavras" = aba_top_palavras, "Metricas_Regime" = aba_metricas_regime, "Top_Palavras_Regime" = aba_top_regime, "Estatisticas" = aba_stats, "Lexico_Base" = dicionario_fomc)

write_xlsx(lista_abas, path_excel_analise)
file.copy(path_excel_analise, file.path(output_local_ts_dir, basename(path_excel_analise)), overwrite = TRUE)

# Exporta léxico híbrido como CSV
path_csv_lexico <- file.path(path_base_projeto, "fomc_lexico_hibrido.csv")
tryCatch({
  save_csv_all(dicionario_fomc, path_csv_lexico, output_local_ts_dir)
  if(file.exists(path_csv_lexico)) {
    cat("\n✓ Léxico CSV salvo em:", path_csv_lexico, "\n")
    cat("  Arquivo contém", nrow(dicionario_fomc), "termos\n")
  } else {
    warning("Arquivo não foi criado em: ", path_csv_lexico)
  }
}, error = function(e) {
  warning("ERRO ao salvar léxico CSV: ", e$message)
})

View(dicionario_fomc)
print(output_local_ts_dir)
beep(sound = 1)
# ===== EXPERIMENTO DO LÉXICO CONCLUÍDO =====