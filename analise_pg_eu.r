# --- 0. CARREGANDO PACOTES ---
library(tidyverse)
library(tidytext)
library(pdftools)
library(stringr)
library(writexl)
library(scales)
library(rstudioapi)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(Ternary)

# ====================================================================
# --- 1. GERENCIAMENTO DE CAMINHOS E DIRETORIOS ---
# ====================================================================

tryCatch({
  script_path <- rstudioapi::getSourceEditorContext()$path
  script_dir <- dirname(script_path)
  setwd(script_dir)
  print("[OK] Diretorio de trabalho definido.")
}, error = function(e) {
  script_dir <- getwd()
  print("[ATENCAO] Nao foi possivel obter o diretorio do script via rstudioapi. Usando getwd().")
})

path_base_projeto <- file.path(script_dir, "resultados_PG")

if (!dir.exists(path_base_projeto)) {
  dir.create(path_base_projeto, recursive = TRUE)
  print(paste("[OK] Pasta de resultados criada:", path_base_projeto))
} else {
  print("[OK] Usando pasta de resultados existente.")
}

# Caminhos dos outputs atualizados para incluir graficos simples
path_csv_eixos <- file.path(path_base_projeto, "pg_eixos_detalhados.csv")
path_csv_indice <- file.path(path_base_projeto, "pg_indice_desconforto.csv")

path_png_comparativo_pct <- file.path(path_base_projeto, "pg_eixos_percentual.png")   # NOVO: % Simples
path_png_comparativo_bruto <- file.path(path_base_projeto, "pg_eixos_absoluto.png")   # NOVO: Contagem Bruta
path_png_top_palavras <- file.path(path_base_projeto, "pg_top_palavras_lexico.png")   # NOVO: Top Palavras

path_png_heatmap <- file.path(path_base_projeto, "pg_heatmap_eixos.png")
path_png_wordcloud_2019 <- file.path(path_base_projeto, "pg_wordcloud_2019.png")
path_png_wordcloud_2024 <- file.path(path_base_projeto, "pg_wordcloud_2024.png")
path_png_indice <- file.path(path_base_projeto, "pg_indice_desconforto.png")
path_png_ternario <- file.path(path_base_projeto, "pg_grafico_ternario.png")
path_png_tfidf <- file.path(path_base_projeto, "pg_analise_tfidf.png")          
path_png_log_odds <- file.path(path_base_projeto, "pg_mudanca_probabilidade.png") 
path_png_network <- file.path(path_base_projeto, "pg_rede_bigramas_2024.png")     
path_excel_analise <- file.path(path_base_projeto, "pg_analise_completa.xlsx")

caminho_pasta_pdfs <- file.path(script_dir)
arquivo_pg_2019 <- list.files(path = caminho_pasta_pdfs, pattern = "PG2019", full.names = TRUE)
arquivo_pg_2024 <- list.files(path = caminho_pasta_pdfs, pattern = "PG2024", full.names = TRUE)

if(length(arquivo_pg_2019) == 0) stop("Arquivo PG2019 nao encontrado.")
if(length(arquivo_pg_2024) == 0) stop("Arquivo PG2024 nao encontrado.")

print("[OK] Arquivos PDF localizados.")

# ====================================================================
# --- 2. DICIONARIO DE ANALISE DE RELACOES INTERNACIONAIS ---
# ====================================================================

lexico_ri <- tribble(
  ~palavra, ~eixo, ~peso, ~polaridade_desconforto,
  "security", "Defesa", 1, 1,
  "threats", "Defesa", 1, 1,
  "threat", "Defesa", 1, 1,
  "military", "Defesa", 1, 1,
  "forces", "Defesa", 1, 1,
  "defense", "Defesa", 1, 1,
  "defence", "Defesa", 1, 1,
  "protection", "Defesa", 1, 1,
  "secure", "Defesa", 0.8, 1,
  "protecting", "Defesa", 1, 1,
  "defend", "Defesa", 0.8, 1,
  "defensive", "Defesa", 0.8, 1,
  "armed_forces", "Defesa", 1.5, 1,
  "defence_union", "Defesa", 1.5, 1,
  "hybrid_threats", "Defesa", 1.5, 1,
  "hybrid_attacks", "Defesa", 1.5, 1,
  "cyber_defence", "Defesa", 1.5, 1,
  "economic_security", "Defesa", 1.5, 1,
  "military_capabilities", "Defesa", 1.5, 1,
  "external_borders", "Defesa", 1.5, 1,
  
  "leadership", "Negociacao", 1, -1,
  "lead", "Negociacao", 0.8, -1,
  "leading", "Negociacao", 0.8, -1,
  "partnership", "Negociacao", 1, -1,
  "partners", "Negociacao", 1, -1,
  "partner", "Negociacao", 1, -1,
  "negotiation", "Negociacao", 1, -1,
  "negotiate", "Negociacao", 0.8, -1,
  "negotiations", "Negociacao", 1, -1,
  "treaties", "Negociacao", 1, -1,
  "treaty", "Negociacao", 1, -1,
  "diplomacy", "Negociacao", 1, -1,
  "diplomatic", "Negociacao", 1, -1,
  "dialogue", "Negociacao", 0.8, -1,
  "cooperation", "Negociacao", 0.9, -1,
  "agreement", "Negociacao", 0.9, -1,
  "free_trade", "Negociacao", 1.5, -1,
  "fair_trade", "Negociacao", 1.5, -1,
  "strategic_partnership", "Negociacao", 1.5, -1,
  "strategic_partnerships", "Negociacao", 1.5, -1,
  "international_law", "Negociacao", 1.5, -1,
  "international_order", "Negociacao", 1.5, -1,
  "multilateralism", "Negociacao", 1.5, -1,
  
  "values", "Diferenciacao", 1, 1,
  "identity", "Diferenciacao", 1, 1,
  "sovereignty", "Diferenciacao", 1, 1,
  "autonomous", "Diferenciacao", 0.9, 1,
  "autonomy", "Diferenciacao", 0.9, 1,
  "independence", "Diferenciacao", 0.9, 1,
  "unique", "Diferenciacao", 0.8, 1,
  "our_values", "Diferenciacao", 1.5, 1,
  "our_citizens", "Diferenciacao", 1.5, 1,
  "way_of_life", "Diferenciacao", 1.5, 1,
  "european_identity", "Diferenciacao", 1.5, 1,
  "technological_sovereignty", "Diferenciacao", 1.5, 1,
  "reduce_dependencies", "Diferenciacao", 1.5, 1
)

custom_stop_words <- stop_words %>%
  bind_rows(tibble(
    word = c("ursula", "von", "der", "leyen", "commission", 
             "parliament", "council", "eu", "union", "member", "states",
             "guidelines", "political", "candidate", "president", "page", "europe", "european"),
    lexicon = "custom"
  ))


# ====================================================================
# --- 3. INGESTAO E TOKENIZACAO COM N-GRAMAS ---
# ====================================================================

ler_pg_pdf <- function(caminho_arquivo) {
  tryCatch({
    texto_bruto <- pdftools::pdf_text(caminho_arquivo) %>% 
      stringr::str_c(collapse = " ") %>%
      stringr::str_squish()
    
    texto_limpo <- texto_bruto %>%
      stringr::str_remove_all("\\d{1,3}\\s*$") %>%
      stringr::str_remove_all("http[s]?://[^\\s]+") %>%
      stringr::str_remove_all("www\\.[^\\s]+") %>%
      stringr::str_remove_all("\\([^)]*\\)") %>%
      stringr::str_squish() %>%
      stringr::str_to_lower()
    return(texto_limpo)
  }, error = function(e) {
    stop("Erro ao processar PDF: ", e$message)
  })
}

print("[PROCESSANDO] Extraindo e tokenizando PDFs...")
texto_pg_2019 <- ler_pg_pdf(arquivo_pg_2019[1])
texto_pg_2024 <- ler_pg_pdf(arquivo_pg_2024[1])

processar_texto_com_ngramas <- function(texto, documento_id) {
  texto_tratado <- texto %>%
    str_replace_all("our values", "our_values") %>%
    str_replace_all("our citizens", "our_citizens") %>%
    str_replace_all("european way of life", "way_of_life") %>%
    str_replace_all("way of life", "way_of_life") %>%
    str_replace_all("armed forces", "armed_forces") %>%
    str_replace_all("defence union", "defence_union") %>%
    str_replace_all("hybrid threats", "hybrid_threats") %>%
    str_replace_all("hybrid attacks", "hybrid_attacks") %>%
    str_replace_all("cyber defence", "cyber_defence") %>%
    str_replace_all("economic security", "economic_security") %>%
    str_replace_all("military capabilities", "military_capabilities") %>%
    str_replace_all("external borders", "external_borders") %>%
    str_replace_all("free trade", "free_trade") %>%
    str_replace_all("fair trade", "fair_trade") %>%
    str_replace_all("strategic partnership", "strategic_partnership") %>%
    str_replace_all("strategic partnerships", "strategic_partnerships") %>%
    str_replace_all("international law", "international_law") %>%
    str_replace_all("international order", "international_order") %>%
    str_replace_all("technological sovereignty", "technological_sovereignty") %>%
    str_replace_all("reduce dependencies", "reduce_dependencies")
  
  tokens <- tibble(text = texto_tratado) %>%
    unnest_tokens(palavra, text) %>%
    mutate(palavra = str_replace_all(palavra, "[^a-z_]", "")) %>%
    filter(palavra != "") %>%
    anti_join(custom_stop_words, by = c("palavra" = "word")) %>%
    filter(nchar(palavra) >= 3) %>%
    filter(!str_detect(palavra, "^\\d+$")) %>%
    mutate(documento = documento_id)
  return(tokens)
}

tokens_2019 <- processar_texto_com_ngramas(texto_pg_2019, "PG2019")
tokens_2024 <- processar_texto_com_ngramas(texto_pg_2024, "PG2024")
tokens_completos <- bind_rows(tokens_2019, tokens_2024)

# ====================================================================
# --- 4. ANALISE POR EIXOS DE RELACOES INTERNACIONAIS (SIMPLIFICADA) ---
# ====================================================================

stats_gerais <- tokens_completos %>%
  group_by(documento) %>%
  summarise(total_palavras = n(), palavras_unicas = n_distinct(palavra), .groups = "drop")

# Calculamos a métrica simples: % de cada eixo em relacao ao TOTAL de palavrasMAPEADAS (do lexico) naquele documento
analise_eixos <- tokens_completos %>%
  inner_join(lexico_ri, by = "palavra") %>%
  group_by(documento, eixo) %>%
  summarise(
    freq_absoluta = n(),
    .groups = "drop"
  ) %>%
  group_by(documento) %>%
  mutate(
    # freq_relativa agora e exatamente o % do eixo dentros das palavras que deram "match"
    freq_relativa = freq_absoluta / sum(freq_absoluta) 
  ) %>%
  ungroup()

# Dados para o Gráfico de Top 10 palavras brutas
top_palavras_lexico <- tokens_completos %>%
  inner_join(lexico_ri, by = "palavra") %>%
  count(documento, palavra, eixo, sort = TRUE) %>%
  group_by(documento) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup()

# ====================================================================
# --- 5. CALCULO DO INDICE DE DESCONFORTO GEOPOLITICO ---
# ====================================================================

indice_desconforto <- tokens_completos %>%
  inner_join(lexico_ri, by = "palavra") %>%
  mutate(contribuicao = peso * polaridade_desconforto) %>%
  group_by(documento) %>%
  summarise(
    total_tokens_ri = n(),
    soma_contribuicao = sum(contribuicao),
    indice_final = soma_contribuicao / total_tokens_ri,
    .groups = "drop"
  )

dados_ternario <- analise_eixos %>%
  select(documento, eixo, freq_relativa) %>%
  pivot_wider(names_from = eixo, values_from = freq_relativa, values_fill = 0)

# ====================================================================
# --- 6. NOVAS METRICAS: TF-IDF, LOG ODDS E REDES DE BIGRAMAS ---
# ====================================================================
print("[PROCESSANDO] Calculando metricas avancadas de Text Mining...")

palavras_freq <- tokens_completos %>% count(documento, palavra, sort = TRUE)

tfidf_analise <- palavras_freq %>%
  bind_tf_idf(palavra, documento, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(documento) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()

frequencias_relativas <- palavras_freq %>%
  group_by(documento) %>%
  mutate(proporcao = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = documento, values_from = proporcao, values_fill = 0.0001) %>% 
  mutate(log_ratio = log2(PG2024 / PG2019)) %>%
  arrange(desc(abs(log_ratio)))

bigramas_2024 <- tibble(text = texto_pg_2024) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$word, !word2 %in% custom_stop_words$word) %>%
  filter(!str_detect(word1, "^\\d+$"), !str_detect(word2, "^\\d+$")) %>%
  filter(nchar(word1) >= 3, nchar(word2) >= 3) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 3) 

rede_bigramas_2024 <- graph_from_data_frame(bigramas_2024)

# ====================================================================
# --- 7. VISUALIZACOES (SIMPLES E AVANCADAS) ---
# ====================================================================
print("[PROCESSANDO] Gerando graficos de analise...")

# 7.1 Graficos Simples: Contagem Bruta (Frequencia Absoluta)
plot_comparativo_bruto <- analise_eixos %>%
  ggplot(aes(x = eixo, y = freq_absoluta, fill = documento)) +
  geom_col(position = "dodge", alpha = 0.85, color = "black", linewidth = 0.3) +
  geom_text(aes(label = freq_absoluta), position = position_dodge(width = 0.9), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("PG2019" = "#3498DB", "PG2024" = "#E74C3C")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Volume Bruto de Palavras por Eixo Geopolitico",
    subtitle = "Contagem simples das ocorrencias do lexico em cada documento",
    x = "",
    y = "Frequencia Absoluta (Contagem)"
  ) +
  theme_minimal(base_size = 14) + theme(legend.position = "top", plot.title = element_text(face = "bold"))

# 7.2 Graficos Simples: Market Share / Percentual
plot_comparativo_pct <- analise_eixos %>%
  ggplot(aes(x = eixo, y = freq_relativa, fill = documento)) +
  geom_col(position = "dodge", alpha = 0.85, color = "black", linewidth = 0.3) +
  geom_text(aes(label = scales::percent(freq_relativa, accuracy = 0.1)), position = position_dodge(width = 0.9), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("PG2019" = "#3498DB", "PG2024" = "#E74C3C")) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribuicao Tematica (% do Total Mapeado)",
    subtitle = "Peso de cada eixo em relacao ao total de palavras do lexico encontradas",
    x = "",
    y = "Percentual (%)"
  ) +
  theme_minimal(base_size = 14) + theme(legend.position = "top", plot.title = element_text(face = "bold"))

# 7.3 Graficos Simples: Top 10 Palavras Absolutas
plot_top_palavras <- top_palavras_lexico %>%
  mutate(palavra = reorder_within(palavra, n, documento)) %>%
  ggplot(aes(x = n, y = palavra, fill = eixo)) +
  geom_col(color = "black", linewidth = 0.3) +
  facet_wrap(~documento, scales = "free") +
  scale_y_reordered() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Top 10 Palavras do Lexico Mais Utilizadas",
    subtitle = "Contagem bruta dos termos mais frequentes mapeados pelo dicionario",
    x = "Contagem (Frequencia Absoluta)",
    y = "",
    fill = "Eixo Tematico"
  ) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# 7.4 Metricas Avancadas
plot_tfidf <- tfidf_analise %>%
  mutate(palavra = reorder_within(palavra, tf_idf, documento)) %>%
  ggplot(aes(x = tf_idf, y = palavra, fill = documento)) +
  geom_col(show.legend = FALSE, color = "black", linewidth = 0.3) +
  facet_wrap(~documento, scales = "free") +
  scale_y_reordered() +
  scale_fill_manual(values = c("PG2019" = "#3498DB", "PG2024" = "#E74C3C")) +
  labs(title = "Assinatura Retorica Exclusiva por Mandato (TF-IDF)", x = "Score TF-IDF", y = "") +
  theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold"))

plot_log_odds <- frequencias_relativas %>%
  filter(abs(log_ratio) > 1) %>% 
  top_n(30, abs(log_ratio)) %>%
  mutate(direcao = ifelse(log_ratio > 0, "Mais forte em 2024", "Mais forte em 2019")) %>%
  ggplot(aes(x = log_ratio, y = reorder(palavra, log_ratio), fill = direcao)) +
  geom_col(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("Mais forte em 2024" = "#E74C3C", "Mais forte em 2019" = "#3498DB")) +
  labs(title = "A Guinada de Vocabulario (Log Odds Ratio)", x = "Log Odds Ratio", y = "", fill = "") +
  theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold"), legend.position = "top")

plot_indice <- ggplot(indice_desconforto, aes(x = documento, y = indice_final, fill = indice_final > 0)) +
  geom_col(width = 0.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
  geom_text(aes(label = round(indice_final, 3)), vjust = ifelse(indice_desconforto$indice_final > 0, -0.5, 1.5), size = 5) +
  scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB")) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  labs(title = "Indice de Desconforto Geopolitico (2019 vs 2024)", x = "", y = "Score do Indice") +
  theme_minimal(base_size = 14) + theme(legend.position = "none")

# Salva TODOS os gráficos usando ggsave
ggsave(filename = path_png_comparativo_bruto, plot = plot_comparativo_bruto, bg = "white", width = 10, height = 7, dpi = 300)
ggsave(filename = path_png_comparativo_pct, plot = plot_comparativo_pct, bg = "white", width = 10, height = 7, dpi = 300)
ggsave(filename = path_png_top_palavras, plot = plot_top_palavras, bg = "white", width = 12, height = 7, dpi = 300)
ggsave(filename = path_png_tfidf, plot = plot_tfidf, bg = "white", width = 12, height = 8, dpi = 300)
ggsave(filename = path_png_log_odds, plot = plot_log_odds, bg = "white", width = 10, height = 8, dpi = 300)
ggsave(filename = path_png_indice, plot = plot_indice, bg = "white", width = 10, height = 7, dpi = 300)

set.seed(2026)
plot_network <- ggraph(rede_bigramas_2024, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "gray50", show.legend = FALSE) +
  geom_node_point(size = 5, color = "darkred", alpha = 0.8) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 4, fontface = "bold", repel = TRUE) +
  labs(title = "Arquitetura do Discurso de 2024: Rede de Bigramas") + theme_void(base_size = 14)
ggsave(filename = path_png_network, plot = plot_network, bg = "white", width = 12, height = 10, dpi = 300)

# Grafico Ternario usando o pacote Ternary
tryCatch({
  png(filename = path_png_ternario, width = 3000, height = 2400, res = 300, bg = "white")
  
  # Configura o diagrama ternario
  TernaryPlot(
    atip = "Defesa",
    btip = "Negociacao", 
    ctip = "Diferenciacao",
    alab = "Defesa →",
    blab = "← Negociacao",
    clab = "Diferenciacao →",
    main = "Evolucao Discursiva da UE: Grafico Ternario",
    grid.lines = 5,
    grid.minor.lines = 0,
    axis.labels = seq(0, 1, by = 0.2),
    point = "right"
  )
  
  # Adiciona os pontos para cada documento
  # Converte os dados para formato adequado (a, b, c devem somar 1)
  pg_2019_coords <- as.numeric(dados_ternario[dados_ternario$documento == "PG2019", c("Defesa", "Negociacao", "Diferenciacao")])
  pg_2024_coords <- as.numeric(dados_ternario[dados_ternario$documento == "PG2024", c("Defesa", "Negociacao", "Diferenciacao")])
  
  # Plota os pontos
  TernaryPoints(
    coordinates = matrix(pg_2019_coords, nrow = 1),
    pch = 19,
    cex = 3,
    col = "#3498DB"
  )
  
  TernaryPoints(
    coordinates = matrix(pg_2024_coords, nrow = 1),
    pch = 19,
    cex = 3,
    col = "#E74C3C"
  )
  
  # Adiciona labels
  TernaryText(
    coordinates = matrix(pg_2019_coords, nrow = 1),
    labels = "PG2019",
    col = "#3498DB",
    font = 2,
    cex = 1.2,
    pos = 1
  )
  
  TernaryText(
    coordinates = matrix(pg_2024_coords, nrow = 1),
    labels = "PG2024",
    col = "#E74C3C",
    font = 2,
    cex = 1.2,
    pos = 3
  )
  
  # Adiciona legenda
  legend(
    "topright",
    legend = c("PG2019", "PG2024"),
    pch = 19,
    col = c("#3498DB", "#E74C3C"),
    bty = "n",
    cex = 1.2
  )
  
  invisible(dev.off())
  print("[OK] Grafico ternario criado com sucesso usando pacote Ternary.")
  
}, error = function(e) {
  invisible(dev.off())
  print(paste("[ERRO] Nao foi possivel criar o grafico ternario:", e$message))
  print("       O script continuara sem este grafico.")
})

# Exportacao de Excel
lista_dados_excel <- list(
  "Indice_Desconforto" = indice_desconforto,
  "Analise_por_Eixos" = analise_eixos,
  "Top_10_Palavras" = top_palavras_lexico,
  "Matriz_Ternaria" = dados_ternario,
  "Analise_TF_IDF" = tfidf_analise,
  "Log_Odds_Ratio" = frequencias_relativas,
  "Stats_Gerais" = stats_gerais,
  "Lexico_RI" = lexico_ri
)

write_xlsx(lista_dados_excel, path_excel_analise)

print("============================================================")
print("[OK] PROCESSAMENTO CONCLUIDO COM SUCESSO.")