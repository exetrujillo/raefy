#' Obtener la palabra del día
#'
#' @return Un dataframe con la palabra del día y su fecha (si está disponible) o contenido HTML crudo.
#' @export
#' @examples
#' \donttest{
#' palabra_del_dia()
#' }
palabra_del_dia <- function() {
  respuesta <- rae_client("wotd?callback=json")
  texto_json <- gsub("^json\\(|\\)$", "", respuesta)
  
  datos <- tryCatch({
    jsonlite::fromJSON(texto_json)
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(datos)) {
    return(data.frame(
      palabra = datos$header,
      id = datos$id,
      stringsAsFactors = FALSE
    ))
  } else {
    return(NULL)
  }
}

#' Obtener una palabra aleatoria
#'
#' @return Un dataframe con la definicion de una palabra aleatoria.
#' @export
#' @examples
#' \donttest{
#' palabra_aleatoria()
#' }
palabra_aleatoria <- function() {
  html <- rae_client("random")
  
  # Extraer la palabra del header
  pagina <- rvest::read_html(html)
  nodo_palabra <- rvest::html_node(pagina, "header.f")
  palabra <- if (!inherits(nodo_palabra, "xml_missing")) {
    rvest::html_text(nodo_palabra, trim = TRUE)
  } else {
    NA_character_
  }
  
  definiciones <- extraer_definiciones(html)
  
  if (nrow(definiciones) > 0) {
    definiciones$palabra <- palabra
    # Reordenar columnas para que palabra sea la primera
    definiciones <- definiciones[, c("palabra", setdiff(names(definiciones), "palabra"))]
  }
  
  return(definiciones)
}

#' Buscar anagramas
#'
#' @param palabra La palabra para buscar anagramas.
#' @return Un dataframe con los anagramas encontrados.
#' @export
#' @examples
#' \donttest{
#' buscar_anagrama("amor")
#' }
buscar_anagrama <- function(palabra) {  
  if (missing(palabra) || is.null(palabra) || palabra == "") {
    stop("Debe proporcionar una palabra para buscar anagramas.")
  }
  
  endpoint <- paste0("anagram?w=", utils::URLencode(palabra))
  respuesta <- rae_client(endpoint)
  
  datos <- tryCatch({
    jsonlite::fromJSON(respuesta)
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(datos) && !is.null(datos$res)) {
    return(dplyr::as_tibble(datos$res))
  } else {
    return(dplyr::tibble())
  }
}

#' Autocompletar palabra
#'
#' @param texto El texto parcial para buscar sugerencias.
#' @return Un vector de caracteres con las sugerencias.
#' @export
#' @examples
#' \donttest{
#' autocompletar("progra")
#' }
autocompletar <- function(texto) {
  if (missing(texto) || is.null(texto) || texto == "") {
    return(character())
  }
  
  endpoint <- paste0("keys?q=", utils::URLencode(texto), "&callback=jsonp123")
  respuesta <- rae_client(endpoint)
  
  texto_json <- gsub("^jsonp123\\(|\\)$", "", respuesta)
  
  sugerencias <- tryCatch({
    jsonlite::fromJSON(texto_json)
  }, error = function(e) {
    return(character())
  })
  
  return(sugerencias)
}
