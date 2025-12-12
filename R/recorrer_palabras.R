#' Obtener informacion para multiples palabras
#'
#' @description
#' Esta funcion toma un vector de palabras, busca sus definiciones y retorna
#' un dataframe consolidado con toda la informacion.
#'
#' @param palabras Un vector de caracteres con las palabras a buscar.
#' @param progreso Si es TRUE, muestra una barra de progreso (requiere paquete progress o similar, por ahora simple message).
#' @return Un dataframe con las columnas: palabra, etimologia, numero, categoria, definicion, sinonimos, antonimos.
#' @importFrom dplyr bind_rows mutate select
#' @export
recorrer_palabras <- function(palabras, progreso = TRUE) {
  # Si es un solo string con espacios, dividirlo
  if (length(palabras) == 1 && grepl("\\s", palabras)) {
    palabras <- unlist(strsplit(palabras, "\\s+"))
  }

  palabras_unicas <- unique(palabras)
  palabras_unicas <- palabras_unicas[!is.na(palabras_unicas) & palabras_unicas != ""]
  
  resultados_lista <- list()
  
  total <- length(palabras_unicas)
  
  for (i in seq_along(palabras_unicas)) {
    p <- palabras_unicas[i]
    
    if (progreso) {
      message(sprintf("[%d/%d] Procesando: %s", i, total, p))
    }
    
    tryCatch({
      respuesta <- obtener_palabra(palabra_consulta = p)
      
      if (!is.null(respuesta) && nrow(respuesta$definiciones) > 0) {
        df <- respuesta$definiciones
        df$palabra <- respuesta$palabra
        df$etimologia <- respuesta$etimologia        
        df <- df[, c("palabra", "etimologia", "numero", "categoria", "definicion", "sinonimos", "antonimos")]
        
        resultados_lista[[length(resultados_lista) + 1]] <- df
      } else {
        warning(sprintf("No se encontraron resultados para: %s", p))
      }
    }, error = function(e) {
      warning(sprintf("Error procesando '%s': %s", p, e$message))
    })
  }
  
  if (length(resultados_lista) > 0) {
    final_df <- dplyr::bind_rows(resultados_lista)
    return(final_df)
  } else {
    return(dplyr::tibble())
  }
}
