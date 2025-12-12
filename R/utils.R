#' Expandir abreviaturas de categoria gramatical
#'
#' @param abrev La abreviatura (ej. "m.", "tr.").
#' @return La descripcion completa.
#' @keywords internal
expandir_categoria <- function(abrev) {
  if (is.na(abrev)) return(NA_character_)
  
  texto <- abrev
  
  # Diccionario de reemplazos
  # Ordenar por longitud para evitar reemplazos parciales incorrectos si fuera necesario
  reemplazos <- list(
    "prnl\\." = "verbo pronominal",
    "intr\\." = "verbo intransitivo",
    "tr\\." = "verbo transitivo",
    "cop\\." = "verbo copulativo",
    "impers\\." = "verbo impersonal",
    "aux\\." = "verbo auxiliar",
    "prep\\." = "preposición",
    "conj\\." = "conjunción",
    "interj\\." = "interjección",
    "pron\\." = "pronombre",
    "art\\." = "artículo",
    "adv\\." = "adverbio",
    "adj\\." = "adjetivo",
    "loc\\." = "locución",
    "verb\\." = "verbal",
    "nom\\." = "nominal",
    "m\\." = "nombre masculino",
    "f\\." = "nombre femenino",
    "U\\." = "Usado",
    "t\\." = "también",
    "c\\." = "como"
  )
  
  for (patron in names(reemplazos)) {
    texto <- gsub(patron, reemplazos[[patron]], texto)
  }
  
  return(texto)
}
