#' Cliente RAE interno
#'
#' @param cadena_consulta La cadena de consulta de la API (e.g., "search?w=hola&orderBy=asc").
#' @return El contenido raw de la respuesta.
#' @keywords internal
rae_client <- function(cadena_consulta) {
  base_url <- "https://dle.rae.es/data/"
  url <- paste0(base_url, cadena_consulta)
  
  headers <- httr::add_headers(
    "User-Agent" = "Diccionario/2 CFNetwork/808.2.16 Darwin/16.3.0",
    "Authorization" = "Basic cDY4MkpnaFMzOmFHZlVkQ2lFNDM0",
    "Content-Type" = "application/x-www-form-urlencoded"
  )
  
  respuesta <- httr::RETRY(
    verb = "GET",
    url = url,
    config = headers,
    times = 3,
    pause_base = 1,
    pause_cap = 5,
    terminate_on = c(404, 401, 403) 
  )
  
  if (httr::status_code(respuesta) != 200) {
    stop(paste("La peticion a la API fallo con estado:", httr::status_code(respuesta)))
  }
  
  return(httr::content(respuesta, "text", encoding = "UTF-8"))
}
