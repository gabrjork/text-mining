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
font_add("plusjakarta", regular = "C:/Users/GabrielHenriqueMarti/AppData/Local/Microsoft/Windows/Fonts/PLUSJAKARTASANS-Regular.ttf")
showtext_auto()
showtext_opts(dpi = 300)

# ====================================================================
# --- 1. GERENCIAMENTO DE CAMINHOS E ARQUIVAMENTO LOCAL ---
# ====================================================================

# 1.1. Define o diretório base
tryCatch({
  script_path <- rstudioapi::getSourceEditorContext()$path
  script_dir <- dirname(script_path)
  setwd(script_dir)
  print(paste("✓ Diretório de trabalho definido como:", getwd()))
}, error = function(e) {
  script_dir <- getwd()
  print("Atenção: Não foi possível obter o diretório do script via rstudioapi. Usando getwd().")
  print(paste("Diretório de trabalho (Local):", script_dir))
})

# 1.2. Definição dos caminhos FIXOS
path_base_projeto <- "C:/Users/GabrielHenriqueMarti/Desktop/Projeto_Macro/dados/latest"
path_csv_serie <- file.path(path_base_projeto, "copom_sentimento_serie.csv")
path_csv_completo <- file.path(path_base_projeto, "copom_sentimento_completo.csv")
path_png_reuniao <- file.path(path_base_projeto, "copom_sentimento_por_reuniao.png")
path_png_data <- file.path(path_base_projeto, "copom_sentimento_por_data.png")
path_png_selic_sentimento <- file.path(path_base_projeto, "copom_selic_vs_sentimento.png")
path_png_top_palavras <- file.path(path_base_projeto, "copom_top_palavras_sentimento.png")
path_png_regime_timeline <- file.path(path_base_projeto, "copom_regime_timeline.png")
path_png_regime_distribuicao <- file.path(path_base_projeto, "copom_regime_distribuicao.png")
path_excel_analise <- file.path(path_base_projeto, "copom_analise_completa.xlsx")

# 1.3. Criação da pasta de ARQUIVAMENTO LOCAL
timestamp_run <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_local_ts_dir <- file.path(script_dir, paste0("COPOMSENTIMENT_", timestamp_run))

if (!dir.exists(output_local_ts_dir)) {
  dir.create(output_local_ts_dir, recursive = TRUE)
  print(paste("✓ Pasta de arquivamento LOCAL criada:", output_local_ts_dir))
} else {
  print(paste("✓ Usando pasta local existente:", output_local_ts_dir))
}

print(paste("Arquivos finais (latest) serão salvos em:", path_base_projeto))

# 1.4. Define o caminho da pasta com os PDFs
caminho_pasta_pdfs <- file.path(script_dir, "atas")

# Validação: verifica se o diretório existe
if(!dir.exists(caminho_pasta_pdfs)) {
  stop("Diretório não encontrado: ", caminho_pasta_pdfs, 
       "\nCrie a pasta 'atas' ou ajuste o caminho")
}

# Mapeia todos os arquivos que terminam com .pdf na pasta indicada
arquivos_pdf <- list.files(path = caminho_pasta_pdfs, pattern = "\\.pdf$", full.names = TRUE)

# Validação: verifica se há PDFs na pasta
if(length(arquivos_pdf) == 0) {
  stop("Nenhum arquivo PDF encontrado em: ", caminho_pasta_pdfs)
}

# --- 2. HELPERS DE SALVAMENTO ---
save_plot_all <- function(plot, fixed_path, local_ts_dir, width = 8, height = 7, dpi = 300) {
  ggsave(filename = fixed_path, plot = plot, bg = "transparent", width = width, height = height, dpi = dpi)
  ggsave(filename = file.path(local_ts_dir, basename(fixed_path)), plot = plot, bg = "transparent", width = width, height = height, dpi = dpi)
}

save_csv_all <- function(df, fixed_path, local_ts_dir) {
  readr::write_csv(df, fixed_path)
  readr::write_csv(df, file.path(local_ts_dir, basename(fixed_path)))
}

# ====================================================================
# --- 3. INGESTÃO DE DADOS (PDFs das Atas do Copom) ---
# ====================================================================

# 3.1. Função Avançada de Extração de PDF do Copom
ler_ata_pdf_avancado <- function(caminho_arquivo) {
  
  tryCatch({
    # 1. Lê todas as páginas do PDF e colapsa em um único bloco de texto
    texto_bruto <- pdftools::pdf_text(caminho_arquivo) %>% 
      stringr::str_c(collapse = " ") %>%
      # str_squish remove excessos de espaços em branco, \n e \r que quebram o texto
      stringr::str_squish() 
    
    # 2. Extração Inteligente de Metadados de dentro do texto
    # Busca o padrão "XXXth Meeting" ou "XXXth Minutes" logo no início do documento
    reuniao_id <- stringr::str_extract(texto_bruto, "\\d{3}(st|nd|rd|th) Meeting|\\d{3}(st|nd|rd|th) Minutes")
    
    # Extrai a data no cabeçalho — suporta "January 27-28, 2026" e "27-28 January 2026"
    data_texto <- stringr::str_extract(
      texto_bruto,
      "\\d{1,2}(-\\d{1,2})?\\s+[A-Z][a-z]+\\s+\\d{4}|[A-Z][a-z]+\\s+\\d{1,2}(-\\d{1,2})?,\\s+\\d{4}"
    )
    
    # 3. Limpeza Cirúrgica do Cabeçalho (Remoção da lista de presenças)
    # A análise macroeconômica real sempre começa na Seção A. 
    # O regex abaixo recorta tudo o que está DEPOIS de "A) Update of the economic outlook"
    texto_analitico <- stringr::str_extract(texto_bruto, "A\\) Update of the economic outlook.*")
    
    # Trava de segurança: se for uma ata muito antiga que não tem a "Seção A", 
    # ele usa o texto bruto ao invés de retornar vazio.
    if(is.na(texto_analitico)) {
      texto_analitico <- texto_bruto
    }
    
    # 4. Extração de data: tenta o nome do arquivo (YYYY-MM-DD) e depois o texto
    nome_arquivo <- basename(caminho_arquivo)
    data_arquivo <- stringr::str_extract(nome_arquivo, "\\d{4}-\\d{2}-\\d{2}")
    
    data_final <- if(!is.na(data_arquivo)) {
      as.Date(data_arquivo)
    } else if(!is.na(data_texto)) {
      # Remove o primeiro dia do intervalo, mantém o último
      # "October 29-30, 2019" → "October 30, 2019"
      # "29-30 October 2019"  → "30 October 2019"
      d <- data_texto %>%
        stringr::str_replace("([A-Za-z]+\\s+)\\d+-(\\d+)(,\\s+\\d{4})", "\\1\\2\\3") %>%
        stringr::str_replace("\\d+-(\\d+)(\\s+[A-Za-z]+\\s+\\d{4})", "\\1\\2")
      # lubridate é locale-independente: tenta americano (mdy) e depois europeu (dmy)
      parsed <- lubridate::mdy(d, quiet = TRUE)
      if (is.na(parsed)) parsed <- lubridate::dmy(d, quiet = TRUE)
      # Guarda de sanidade: anos muito fora do intervalo esperado viram NA
      if (!is.na(parsed)) {
        ano <- lubridate::year(parsed)
        if (ano < 2000 | ano > 2035) {
          parsed <- NA_Date_
        }
      }
      parsed
    } else {
      NA_Date_
    }
    
    # 5. Número da reunião extraído do nome do arquivo (ex: copom_276.pdf → 276)
    reuniao_num <- as.integer(stringr::str_extract(nome_arquivo, "\\d+"))

    # 6. Retorna a linha estruturada para o data frame
    tibble::tibble(
      id_documento = nome_arquivo,
      reuniao_num  = reuniao_num,
      data         = data_final,
      reuniao      = reuniao_id,
      data_identificada = data_texto,
      texto        = texto_analitico
    )
  }, error = function(e) {
    warning("Erro ao processar ", basename(caminho_arquivo), ": ", e$message)
    return(NULL)
  })
}

# 3.2. Aplicando a função em lote para todos os PDFs da pasta
print("--- Iniciando Ingestão de Dados ---")
print(paste("Processando", length(arquivos_pdf), "arquivo(s) PDF..."))
atas_copom <- purrr::map_dfr(arquivos_pdf, ler_ata_pdf_avancado)
print(paste("✓ Leitura concluída:", nrow(atas_copom), "ata(s) processada(s)"))

# 3.3. Validação: quantas datas foram extraídas com sucesso
datas_validas <- sum(!is.na(atas_copom$data))
if(datas_validas == 0) {
  warning("Nenhuma data foi extraída dos PDFs")
  cat("\nExemplo de data_identificada extraída:\n")
  print(head(atas_copom |> select(id_documento, data_identificada, data), 3))
} else if(datas_validas < nrow(atas_copom)) {
  print(paste("⚠", datas_validas, "de", nrow(atas_copom), "datas extraídas com sucesso"))
} else {
  print(paste("✓ Todas as", datas_validas, "datas extraídas com sucesso"))
}

# ====================================================================
# --- 4. ANÁLISE DE SENTIMENTO (ABORDAGEM BIS / DICIONÁRIO MP) ---
# ====================================================================

# 4.1. Validação: verifica se há dados para processar
if(nrow(atas_copom) == 0) {
  stop("Nenhuma ata foi processada com sucesso. Verifique os PDFs.")
}

print("--- Iniciando Análise de Sentimento (Léxico Customizado MP) ---")

# 4.2. Construção do Dicionário "MP" (Refinado com Forward Guidance BCB)
lm_lexicon <- get_sentiments("loughran") |>
  filter(sentiment %in% c("positive", "negative")) |>
  mutate(
    value = if_else(sentiment == "positive", 1, -1),
    tipo = if_else(sentiment == "positive", "Hawkish", "Dovish")
  ) |>
  select(word, value, tipo)

# Overrides e N-grams específicos extraídos das Atas do Copom
mp_overrides <- tribble(
  ~word, ~value, ~tipo,
  # 1. Forward Guidance e Intensificadores (Pesos Maiores)
  "guidance_two_more_hikes", 3, "Hawkish", # Captura a sinalização de +2 reuniões
  "hike_100bps", 2, "Hawkish",             # Magnitude extrema
  "hike_75bps", 1.5, "Hawkish",            # Magnitude alta
  "even_more_contractionary", 2, "Hawkish",
  "significantly_contractionary", 2, "Hawkish",
  "very_prolonged_period", 1.5, "Hawkish",
  "resume_the_rate_hiking", 1.5, "Hawkish",
  
  # 2. Jargões Estruturais do BCB (Peso 1)
  "deanchoring", 1, "Hawkish",
  "deanchored", 1, "Hawkish",
  "further_adjustments", 1, "Hawkish",
  "more_adverse", 1, "Hawkish",
  "firm_commitment", 1, "Hawkish",
  "vigilant", 1, "Hawkish",
  "upside", 1, "Hawkish",
  "tightening", 1, "Hawkish",
  "restrictive", 1, "Hawkish",
  "inflationary", 1, "Hawkish",
  "unanchored", 1, "Hawkish",
  "overheating", 1, "Hawkish",
  "strong_growth", 1, "Hawkish",
  "tight_labor_market", 1, "Hawkish",
  
  # 3. Termos Dovish (Peso -1)
  "cut", -1, "Dovish",
  "downside", -1, "Dovish",
  "accommodative", -1, "Dovish",
  "easing", -1, "Dovish",
  "slack", -1, "Dovish",
  "stimulus", -1, "Dovish",
  "activity_moderation", -1, "Dovish",
  
  # 4. Forward Guidance de Afrouxamento (Pesos Fortemente Negativos)
  "guidance_easing_cycle", -3, "Dovish",      # Equivalente dovish de um choque
  "initiate_flexibilization", -2, "Dovish",   # Sinalização de início de cortes
  "improved_inflation_outlook", -1.5, "Dovish",
  "expectations_less_distant", -1.5, "Dovish"
)

dicionario_mp <- bind_rows(mp_overrides, lm_lexicon) |>
  distinct(word, .keep_all = TRUE)

# 4.3. Pré-processamento: Tradução de Contexto e Preservação de Magnitudes
atas_copom_tratado <- atas_copom |>
  mutate(
    texto_limpo = str_to_lower(texto),
    
    # A) Tradução de Magnitudes Numéricas
    texto_limpo = str_replace_all(texto_limpo, "1\\.00 p\\.p\\.", "hike_100bps"),
    texto_limpo = str_replace_all(texto_limpo, "0\\.75 p\\.p\\.", "hike_75bps"),
    texto_limpo = str_replace_all(texto_limpo, "0\\.50 p\\.p\\.", "hike_50bps"),
    
    # B) Tradução de Forward Guidance
    texto_limpo = str_replace_all(texto_limpo, "same magnitude at the next two meetings", "guidance_two_more_hikes"),
    texto_limpo = str_replace_all(texto_limpo, "same magnitude in the next two meetings", "guidance_two_more_hikes"),
    texto_limpo = str_replace_all(texto_limpo, "resume the rate hiking", "resume_the_rate_hiking"),
    
    # C) Tradução de Intensificadores e Jargões (Bigramas/Trigramas)
    texto_limpo = str_replace_all(texto_limpo, "even more contractionary", "even_more_contractionary"),
    texto_limpo = str_replace_all(texto_limpo, "significantly contractionary", "significantly_contractionary"),
    texto_limpo = str_replace_all(texto_limpo, "very prolonged period", "very_prolonged_period"),
    texto_limpo = str_replace_all(texto_limpo, "firm commitment", "firm_commitment"),
    texto_limpo = str_replace_all(texto_limpo, "more adverse", "more_adverse"),
    texto_limpo = str_replace_all(texto_limpo, "further adjustments", "further_adjustments"),
    texto_limpo = str_replace_all(texto_limpo, "strong growth", "strong_growth"),
    texto_limpo = str_replace_all(texto_limpo, "activity moderation", "activity_moderation"),

    # D) Tradução de Forward Guidance Dovish (Janeiro 2026)
    texto_limpo = str_replace_all(texto_limpo, "beginning of an interest-rate easing cycle", "guidance_easing_cycle"),
    texto_limpo = str_replace_all(texto_limpo, "magnitude of the easing cycle", "guidance_easing_cycle"),
    texto_limpo = str_replace_all(texto_limpo, "initiate the flexibilization", "initiate_flexibilization"),
    texto_limpo = str_replace_all(texto_limpo, "improved current inflation outlook", "improved_inflation_outlook"),
    texto_limpo = str_replace_all(texto_limpo, "less distant from the target", "expectations_less_distant")
  )




# 4.4. Tokenização
copom_tokens <- atas_copom_tratado |>
  unnest_tokens(word, texto_limpo) |>
  # Mantém o underline para não quebrar os bigramas que acabamos de criar
  mutate(word = str_replace_all(word, "[^a-z_]", "")) |> 
  filter(word != "") |>
  anti_join(stop_words, by = "word")

# 4.5. Cálculo do Índice de Sentimento (Dicionário MP)
copom_indice <- copom_tokens |>
  inner_join(dicionario_mp, by = "word", relationship = "many-to-many") |>
  group_by(id_documento, reuniao_num, data) |>
  summarise(
    total_palavras  = n(),
    soma_scores     = sum(value),
    net_sentiment   = soma_scores / total_palavras,
    .groups = "drop"
  ) |>
  arrange(reuniao_num)

print(paste("✓ Análise de sentimento concluída:", nrow(copom_indice), "reuniões processadas"))



# ====================================================================
# --- 4.6. IMPORTAÇÃO DA TAXA SELIC (BCB - SGS 432) ---
# ====================================================================

print("--- Importando Série da Taxa Selic ---")

# A série base é o copom_indice agora
data_min_sentimento <- min(copom_indice$data, na.rm = TRUE)
data_max_sentimento <- max(copom_indice$data, na.rm = TRUE)

# Importa a Selic do SGS (série 432)
selic_data <- GetBCBData::gbcbd_get_series(
  id = c("SELIC" = 432),
  first.date = data_min_sentimento,
  last.date = Sys.Date(),
  format.data = "wide"
)

print(paste("✓ Selic importada:", nrow(selic_data), "observações de", 
            min(selic_data$ref.date), "a", max(selic_data$ref.date))
)





# ====================================================================
# --- 5. PREPARAÇÃO DA SÉRIE HISTÓRICA E SALVAMENTO ---
# ====================================================================

print("--- Iniciando Preparação e Salvamento de Dados ---")

serie_sentimento <- copom_indice |>
  select(id_documento, reuniao_num, data, net_sentiment, soma_scores, total_palavras)

save_csv_all(serie_sentimento, path_csv_serie, output_local_ts_dir)
print(paste("✓ Série histórica salva em:", path_csv_serie))

save_csv_all(atas_copom, path_csv_completo, output_local_ts_dir)
print(paste("✓ Dataset completo salvo em:", path_csv_completo))


View(serie_sentimento)





# ====================================================================
# --- 6. GERAÇÃO DE GRÁFICOS ---
# ====================================================================

print("--- Iniciando Geração de Gráficos ---")

# 6.1. Gráfico 1: Por Número de Reunião
p1 <- ggplot(serie_sentimento, aes(x = reuniao_num, y = net_sentiment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = "#189CD8", linewidth = 1.5) +
  geom_point(color = "#189CD8", size = 1.5) +
  labs(
    title    = "Sentimento das Atas do Copom (Léxico BIS)",
    subtitle = "Hawkish: >0, Dovish: <0.",
    x        = "Número da Reunião",
    y        = "Sentimento Líquido (Net Sentiment Score)"
  ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
  theme_minimal(base_family = "plusjakarta", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 10, r = 20, b = 0, l = 10, unit = "pt")
  )

save_plot_all(p1, path_png_reuniao, output_local_ts_dir)
print("✓ 6.1: Gráfico por Reunião salvo.")

# 6.2. Gráfico 2: Por Data
serie_com_data <- serie_sentimento |>
  filter(!is.na(data)) |>
  arrange(data)

if (nrow(serie_com_data) > 0) {
  p2 <- ggplot(serie_com_data, aes(x = data, y = net_sentiment)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(color = "#189CD8", linewidth = 1.5) +
    geom_point(color = "#189CD8", size = 1.5) +
    geom_smooth(method = "loess", se = FALSE, color = "firebrick", linewidth = 0.9, span = 0.6) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
    labs(
      title    = "Sentimento das Atas do Copom (Léxico BIS)",
      subtitle = "Hawkish: >0, Dovish: <0.",
      x        = "",
      y        = "Sentimento Líquido (Net Sentiment Score)"
    ) +
    theme_minimal(base_family = "plusjakarta", base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 13),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(t = 10, r = 20, b = 0, l = 10, unit = "pt")
    )
  save_plot_all(p2, path_png_data, output_local_ts_dir)
  print("✓ 6.2: Gráfico por Data salvo.")
}

# 6.3. Gráfico 3: Selic vs Sentimento (eixos duplos)
if (nrow(serie_com_data) > 0 && nrow(selic_data) > 0) {
  dados_combinados <- serie_com_data |>
    select(data, net_sentiment) |>
    left_join(selic_data |> select(ref.date, SELIC) |> rename(data = ref.date), by = "data") |>
    filter(!is.na(SELIC))
  
  if (nrow(dados_combinados) > 0) {
    min_sentiment <- min(dados_combinados$net_sentiment, na.rm = TRUE)
    max_sentiment <- max(dados_combinados$net_sentiment, na.rm = TRUE)
    max_selic <- max(dados_combinados$SELIC, na.rm = TRUE)
    
    coef_escala <- max_selic / (max_sentiment - min_sentiment)
    intercepto <- -min_sentiment * coef_escala
    
    p3 <- ggplot(dados_combinados, aes(x = data)) +
      geom_line(aes(y = net_sentiment, color = "Sentimento (Hawkish/Dovish)"), linewidth = 1.3) +
      geom_point(aes(y = net_sentiment, color = "Sentimento (Hawkish/Dovish)"), size = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      geom_line(aes(y = (SELIC - intercepto) / coef_escala, color = "Selic"), linewidth = 1.3) +
      geom_point(aes(y = (SELIC - intercepto) / coef_escala, color = "Selic"), size = 1.5) +
      scale_y_continuous(
        name = "Net Sentiment (Score)",
        labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01),
        sec.axis = sec_axis(~ . * coef_escala + intercepto, 
                            name = "Taxa Selic (%)",
                            labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01))
      ) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
      scale_color_manual(values = c("Sentimento (Hawkish/Dovish)" = "#189CD8", "Selic" = "black"), name = "") +
      labs(
        title = "Sentimento das Atas do Copom vs Taxa Selic",
        subtitle = "Correlação entre o tom da comunicação (Léxico BIS) e a taxa básica",
        x = "Data da Reunião"
      ) +
      theme_minimal(base_family = "plusjakarta", base_size = 14) +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.text = element_text(size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(t = 10, r = 20, b = 0, l = 10, unit = "pt")
      )
    save_plot_all(p3, path_png_selic_sentimento, output_local_ts_dir)
    print("✓ 6.3: Gráfico Selic vs Sentimento salvo.")
  }
}

# ====================================================================
# --- 7. ANÁLISES AVANÇADAS DE TEXT MINING ---
# ====================================================================

print("--- Iniciando Análises Avançadas ---")

# 7.1. Classificação de Regimes
quantis <- quantile(serie_sentimento$net_sentiment, probs = c(1/3, 2/3), na.rm = TRUE)

serie_com_regime <- serie_sentimento |>
  mutate(
    regime = case_when(
      net_sentiment >= quantis[2] ~ "Hawkish", # Agora valores maiores são Hawkish
      net_sentiment >= quantis[1] & net_sentiment < quantis[2] ~ "Neutro",
      net_sentiment < quantis[1] ~ "Dovish",   # Valores menores são Dovish
      TRUE ~ NA_character_
    ),
    regime = factor(regime, levels = c("Hawkish", "Neutro", "Dovish"))
  )

print(paste("✓ Regimes classificados:", 
            sum(serie_com_regime$regime == "Hawkish", na.rm = TRUE), "Hawkish |",
            sum(serie_com_regime$regime == "Neutro", na.rm = TRUE), "Neutro |",
            sum(serie_com_regime$regime == "Dovish", na.rm = TRUE), "Dovish"))

# 7.2. Top 20 Palavras Mais Hawkish e Mais Dovish (Substituindo AFINN)
palavras_com_score <- copom_tokens |>
  inner_join(dicionario_mp, by = "word", relationship = "many-to-many") |>
  group_by(word, value, tipo) |>
  summarise(frequencia = n(), .groups = "drop") |>
  mutate(contribuicao_total = value * frequencia) |>
  arrange(desc(abs(contribuicao_total)))

top_hawkish <- palavras_com_score |>
  filter(value > 0) |>
  slice_head(n = 10) |>
  arrange(contribuicao_total)

top_dovish <- palavras_com_score |>
  filter(value < 0) |>
  slice_head(n = 10) |>
  arrange(desc(contribuicao_total))

top_palavras_combinadas <- bind_rows(
  top_dovish,
  top_hawkish
)

# 7.3. Métricas de Text Mining por Regime
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

# Diversidade lexical e Densidade omitidos aqui para manter concisão, mantendo sua lógica original.
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

metricas_regime <- metricas_regime |> left_join(diversidade_regime, by = "regime") |> left_join(densidade_regime, by = "regime")

# 7.4. Top 5 Palavras por Regime
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
# --- 8. GRÁFICOS AVANÇADOS ---
# ====================================================================

print("--- Gerando Gráficos Avançados ---")

# 8.1. Gráfico: Top 20 Palavras (Barras Horizontais)
g_top_palavras <- ggplot(top_palavras_combinadas, aes(
  x = reorder(word, contribuicao_total), 
  y = contribuicao_total,
  fill = tipo
)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Dovish" = "#2ca02c", "Hawkish" = "#d62728")) +
  labs(
    title = "Contribuições ao score de Sentimento do Copom",
    subtitle = "Impacto baseado no Dicionário Monetário (Frequência × Score)",
    x = "",
    y = "Contribuição Total ao Sentimento",
    fill = "Tom da Comunicação:"
  ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_family = "plusjakarta", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 10, r = 20, b = 0, l = 10, unit = "pt")
  )

save_plot_all(g_top_palavras, path_png_top_palavras, output_local_ts_dir, width = 8, height = 8)
print("✓ 8.1: Gráfico Top Palavras salvo.")

# 8.2. Gráfico: Timeline de Regimes
serie_com_regime_data <- serie_com_regime |>
  filter(!is.na(data) & !is.na(regime)) |>
  arrange(data)

if (nrow(serie_com_regime_data) > 0) {
  g_regime_timeline <- ggplot(serie_com_regime_data, aes(x = data, y = net_sentiment, color = regime)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(linewidth = 1.3, alpha = 0.7) +
    geom_point(size = 3, alpha = 0.8) +
    scale_color_manual(
      values = c("Hawkish" = "#d62728", "Neutro" = "gray60", "Dovish" = "#2ca02c"),
      name = "Postura:"
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
    labs(
      title = "Ciclos de Comunicação: Hawkish vs Dovish",
      subtitle = "Classificação baseada em tercis do sentimento (Léxico BIS)",
      x = "Data da Reunião",
      y = "Net Sentiment"
    ) +
    theme_minimal(base_family = "plusjakarta", base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 13),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "top",
      legend.text = element_text(size = 11),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  save_plot_all(g_regime_timeline, path_png_regime_timeline, output_local_ts_dir)
  print("✓ 8.2: Gráfico Timeline de Regimes salvo.")
}

# 8.3. Gráfico: Distribuição por Regime (Boxplot)
g_regime_dist <- ggplot(serie_com_regime |> filter(!is.na(regime)), 
                        aes(x = regime, y = net_sentiment, fill = regime)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 3) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  scale_fill_manual(values = c("Hawkish" = "#d62728", "Neutro" = "gray60", "Dovish" = "#2ca02c")) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)) +
  labs(
    title = "Distribuição do Sentimento por Postura",
    subtitle = "Boxplot + dispersão das reuniões",
    x = "Postura do Copom",
    y = "Net Sentiment"
  ) +
  theme_minimal(base_family = "plusjakarta", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
save_plot_all(g_regime_dist, path_png_regime_distribuicao, output_local_ts_dir)
print("✓ 8.3: Gráfico Distribuição por Regime salvo.")

# ====================================================================
# --- 9. EXPORTAÇÃO PARA EXCEL ---
# ====================================================================

print("--- Gerando Excel Compilado ---")

aba_serie_regime <- serie_com_regime |>
  select(reuniao_num, data, net_sentiment, soma_scores, total_palavras, regime) |>
  arrange(reuniao_num)

aba_top_palavras <- top_palavras_combinadas |>
  select(tipo, word, value, frequencia, contribuicao_total) |>
  arrange(desc(abs(contribuicao_total)))

aba_metricas_regime <- metricas_regime |>
  mutate(across(where(is.numeric), ~round(., 2)))

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
  mutate(across(where(is.numeric), ~round(., 3))) |>
  pivot_longer(everything(), names_to = "metrica", values_to = "valor")

lista_abas <- list(
  "Serie_Regime" = aba_serie_regime,
  "Top_Palavras" = aba_top_palavras,
  "Metricas_Regime" = aba_metricas_regime,
  "Top_Palavras_Regime" = aba_top_regime,
  "Estatisticas" = aba_stats
)

write_xlsx(lista_abas, path_excel_analise)
file.copy(path_excel_analise, file.path(output_local_ts_dir, basename(path_excel_analise)), overwrite = TRUE)
print(paste("✓ Excel de análise compilado salvo em:", path_excel_analise))

# --- 10. NOTIFICAÇÃO ---
beep(sound = 1)
print("\n===== PROCESSAMENTO CONCLUÍDO =====")

View(serie_sentimento)
