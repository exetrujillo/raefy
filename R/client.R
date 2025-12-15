#' Cliente RAE interno
#'
#' @param cadena_consulta La cadena de consulta de la API (por ejemplo "search?w=hola" o "wotd?callback=json").
#' @return El contenido raw de la respuesta.
#' @keywords internal
rae_client <- function(cadena_consulta) {
  base_url <- "https://dle.rae.es/data/"
  url <- paste0(base_url, cadena_consulta)
  args <- c(
    "-s",
    "-f",
    "-H", shQuote("User-Agent: Diccionario/2 CFNetwork/808.2.16 Darwin/16.3.0"),
    "-H", shQuote("Authorization: Basic cDY4MkpnaFMzOmFHZlVkQ2lFNDM0"),
    shQuote(url)
  )

  # Ejecutar curl
  # Se requiere curl instalado en el sistema (Windows 10+ lo trae por defecto)
  resultado <- tryCatch(
    {
      system2("curl", args, stdout = TRUE, stderr = NULL)
    },
    warning = function(w) {
      stop("La peticion a la API fallo (posiblemente 403 Forbidden o Error de Conexion).")
    },
    error = function(e) {
      stop("Error ejecutando curl del sistema. Asegurese de tener curl instalado y en el PATH.")
    }
  )

  contenido <- paste(resultado, collapse = "\n")

  if (nchar(contenido) == 0) {
    stop("La API retorno una respuesta vacia.")
  }

  return(contenido)
}
