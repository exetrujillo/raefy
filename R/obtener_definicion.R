#' Obtener las definiciones de una palabra
#'
#' @param palabra_consulta La palabra a buscar (opcional).
#' @param id El ID de la palabra (opcional).
#' @return Un dataframe con las definiciones.
#' @export
obtener_definicion <- function(palabra_consulta = NULL, id = NULL) {
  html <- obtener_palabra_cruda(query = palabra_consulta, id = id)
  defs <- extraer_definiciones(html)
  return(defs)
}
