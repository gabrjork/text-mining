# ==============================================================================
# DOWNLOAD EM LOTE: Atas do Copom em Inglês (PDF)
# Estratégia: URL direto via padrão "Minutes%20NNN.pdf"
# ==============================================================================

library(httr)

# Configurações
reuniao_inicio <- 200
reuniao_fim    <- 276
destino        <- "atas"   # Pasta onde serão salvos os PDFs

# Cria a pasta 'atas' se não existir
if (!dir.exists(destino)) {
  dir.create(destino, showWarnings = FALSE)
  message("Pasta '", destino, "' criada com sucesso.")
}

base_url <- "https://www.bcb.gov.br/content/copom/copomminutes/Minutes%20"

for (n in reuniao_inicio:reuniao_fim) {
  
  url      <- paste0(base_url, n, ".pdf")
  destfile <- file.path(destino, paste0("copom_", n, ".pdf"))
  
  # Pula se já foi baixado
  if (file.exists(destfile)) {
    message("Já existe: ", destfile)
    next
  }
  
  resp <- tryCatch(
    GET(url, write_disk(destfile, overwrite = TRUE), timeout(30)),
    error = function(e) NULL
  )
  
  if (is.null(resp) || status_code(resp) != 200) {
    message("Não encontrado: ata ", n)
    file.remove(destfile)
  } else {
    message("✓ Baixada: ata ", n, " (", round(file.size(destfile) / 1024), " KB)")
  }
  
  Sys.sleep(0.5)  # Intervalo para não sobrecarregar o servidor
}

message("\nDownload concluído.")
