#' Obtener informacion completa de una palabra
#'
#' @param palabra_consulta La palabra a buscar (opcional).
#' @param id El ID de la palabra (opcional).
#' @param conjugaciones Si es TRUE, intenta obtener las conjugaciones (solo para verbos).
#' @return Una lista con la palabra, etimologia, definiciones y conjugaciones (si aplica).
#' @export
obtener_palabra <- function(palabra_consulta = NULL, id = NULL, conjugaciones = FALSE) {
  html <- obtener_palabra_cruda(query = palabra_consulta, id = id)
  
  if (is.null(html)) return(NULL)
  
  etimologia <- extraer_etimologia(html)
  definiciones <- extraer_definiciones(html)
  
  tabla_conjugaciones <- NULL
  if (conjugaciones) {
    conjug_id <- extraer_id_conjugacion(html)
    if (!is.na(conjug_id)) {
      html_conjug <- obtener_palabra_cruda(id = conjug_id)
      if (!is.null(html_conjug)) {
        tabla_conjugaciones <- extraer_conjugaciones(html_conjug)
      }
    }
  }
  
  # TODO: Intentar obtener la palabra exacta del header si es posible
  # Por ahora usamos palabra_consulta o id como referencia. Falta definir.
  palabra_ref <- if (!is.null(palabra_consulta)) palabra_consulta else id
  
  resultado <- list(
    palabra = palabra_ref,
    etimologia = etimologia,
    definiciones = definiciones,
    conjugaciones = tabla_conjugaciones
  )
  
  return(resultado)
}
