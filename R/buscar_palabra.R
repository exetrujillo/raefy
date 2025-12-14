#' Buscar una palabra en el diccionario RAE
#'
#' @param consulta La palabra a buscar.
#' @return Un dataframe con columnas: palabra, resultado, id.
#' @keywords internal
buscar_palabra <- function(consulta) {
  consulta_codificada <- utils::URLencode(consulta)
  cadena_consulta <- paste0("search?w=", consulta_codificada)
  texto_respuesta <- rae_client(cadena_consulta)
  json_data <- jsonlite::fromJSON(texto_respuesta)
  
  if (length(json_data$res) == 0) {
    return(data.frame(palabra = character(), resultado = character(), id = character()))
  }
  
  resultados <- json_data$res
  
  df <- data.frame(
    palabra = consulta,
    resultado = resultados$header,
    id = resultados$id,
    stringsAsFactors = FALSE
  )
  
  return(dplyr::as_tibble(df))
}
