# ==============================================================================
# DOWNLOAD EM LOTE: Atas do FOMC (PDF)
# Estratégia: Usar datas conhecidas das reuniões para construir URLs diretas
# ==============================================================================

library(httr)

# Configurações
destino <- "atas"  # Pasta onde serão salvos os PDFs

# Datas das reuniões FOMC (formato: "Mês Dia, Ano")
datas_reunioes <- c(
  "Jan 28, 2026", "Dec 10, 2025", "Oct 29, 2025", "Sep 17, 2025",
  "Jul 30, 2025", "Jun 18, 2025", "May 7, 2025", "Mar 19, 2025",
  "Jan 29, 2025", "Dec 18, 2024", "Nov 7, 2024", "Sep 18, 2024",
  "Jul 31, 2024", "Jun 12, 2024", "May 1, 2024", "Mar 20, 2024",
  "Jan 31, 2024", "Dec 13, 2023", "Nov 1, 2023", "Sep 20, 2023",
  "Jul 26, 2023", "Jun 14, 2023", "May 3, 2023", "Mar 22, 2023",
  "Feb 1, 2023", "Dec 14, 2022", "Nov 2, 2022", "Sep 21, 2022",
  "Jul 27, 2022", "Jun 15, 2022", "May 4, 2022", "Mar 16, 2022",
  "Jan 26, 2022", "Dec 15, 2021", "Nov 3, 2021", "Sep 22, 2021",
  "Jul 28, 2021", "Jun 16, 2021", "Apr 28, 2021", "Mar 17, 2021",
  "Jan 27, 2021", "Dec 16, 2020", "Nov 5, 2020", "Sep 16, 2020",
  "Jul 29, 2020", "Jun 10, 2020", "Apr 29, 2020", "Mar 15, 2020",
  "Mar 3, 2020", "Jan 29, 2020"
)

# Cria a pasta 'atas' se não existir
if (!dir.exists(destino)) {
  dir.create(destino, showWarnings = FALSE)
  message("Pasta '", destino, "' criada com sucesso.")
}

# Converte datas para formato YYYYMMDD
message("\n=== Convertendo datas para formato de URL ===\n")

# Salva o locale atual e muda para inglês temporariamente
locale_original <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")

datas_formatadas <- sapply(datas_reunioes, function(data_texto) {
  # Parseia a data
  data_obj <- as.Date(data_texto, format = "%b %d, %Y")
  # Formata como YYYYMMDD
  format(data_obj, "%Y%m%d")
})

# Restaura o locale original
Sys.setlocale("LC_TIME", locale_original)

# Verifica se há NAs
datas_invalidas <- sum(is.na(datas_formatadas))
if (datas_invalidas > 0) {
  message("AVISO: ", datas_invalidas, " datas não puderam ser convertidas")
  message("Datas problemáticas: ", paste(datas_reunioes[is.na(datas_formatadas)], collapse = ", "))
}

message("Total de reuniões válidas: ", sum(!is.na(datas_formatadas)), " de ", length(datas_formatadas), "\n")

# Remove NAs antes de construir URLs
datas_formatadas <- datas_formatadas[!is.na(datas_formatadas)]

# Constrói URLs dos PDFs
base_url <- "https://www.federalreserve.gov/monetarypolicy/files/fomcminutes"
todas_urls <- paste0(base_url, datas_formatadas, ".pdf")

# Baixa os PDFs
message("\n=== Baixando PDFs das atas ===\n")
contador <- 0

for (url_pdf in todas_urls) {
  # Extrai nome do arquivo da URL
  nome_arquivo <- basename(url_pdf)
  destfile <- file.path(destino, nome_arquivo)
  
  # Pula se já foi baixado
  if (file.exists(destfile)) {
    message("Já existe: ", nome_arquivo)
    next
  }
  
  # Tenta baixar o PDF
  resp <- tryCatch({
    GET(url_pdf, write_disk(destfile, overwrite = TRUE), timeout(30))
  }, error = function(e) {
    message("Erro ao baixar: ", nome_arquivo)
    return(NULL)
  })
  
  if (!is.null(resp) && status_code(resp) == 200) {
    tamanho_kb <- round(file.size(destfile) / 1024)
    message("✓ Baixada: ", nome_arquivo, " (", tamanho_kb, " KB)")
    contador <- contador + 1
  } else {
    message("✗ Não disponível: ", nome_arquivo, 
            " (status: ", ifelse(is.null(resp), "NULL", status_code(resp)), ")")
    if (file.exists(destfile)) file.remove(destfile)
  }
  
  Sys.sleep(0.5)  # Pausa entre downloads
}

message("\n=== DOWNLOAD CONCLUÍDO ===")
message("Arquivos baixados: ", contador, " de ", length(todas_urls))
message("Salvos em: ", normalizePath(destino))