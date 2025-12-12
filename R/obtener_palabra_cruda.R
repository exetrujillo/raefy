#' Obtener la definicion cruda de una palabra por su ID o busqueda
#'
#' @param query La palabra a buscar si no se provee ID (opcional).
#' @param id El ID de la palabra (opcional).
#' @return El contenido HTML de la definicion.
#' @keywords internal
obtener_palabra_cruda <- function(query = NULL, id = NULL) {
  if (is.null(id) && is.null(query)) {
    stop("Debe proveer 'id' o 'query'.")
  }
  
  if (is.null(id)) {
    resultados <- buscar_palabra(query)
    if (nrow(resultados) == 0) {
      warning(paste("No se encontraron resultados para:", query))
      return(NULL)
    }
    id <- resultados$id[1]
    message(paste("Usando el primer resultado para", query, ":", resultados$palabra[1], "(ID:", id, ")"))
  }

  encoded_id <- URLencode(id)
  endpoint <- paste0("fetch?id=", encoded_id)
  
  response_txt <- rae_client(endpoint)

  return(response_txt)
}
