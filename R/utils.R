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
  # Diccionario de reemplazos
  # Nota: Se usan escapes unicode (\uXXXX) para pasar el check de CRAN
  reemplazos <- list(
    "prnl\\." = "verbo pronominal",
    "intr\\." = "verbo intransitivo",
    "tr\\." = "verbo transitivo",
    "cop\\." = "verbo copulativo",
    "impers\\." = "verbo impersonal",
    "aux\\." = "verbo auxiliar",
    "prep\\." = "preposici\u00f3n",
    "conj\\." = "conjunci\u00f3n",
    "interj\\." = "interjecci\u00f3n",
    "pron\\." = "pronombre",
    "art\\." = "art\u00edculo",
    "adv\\." = "adverbio",
    "adj\\." = "adjetivo",
    "loc\\." = "locuci\u00f3n",
    "verb\\." = "verbal",
    "nom\\." = "nominal",
    "m\\." = "nombre masculino",
    "f\\." = "nombre femenino",
    "U\\." = "Usado",
    "t\\." = "tambi\u00e9n",
    "c\\." = "como"
  )
  
  for (patron in names(reemplazos)) {
    texto <- gsub(patron, reemplazos[[patron]], texto)
  }
  
  return(texto)
}
