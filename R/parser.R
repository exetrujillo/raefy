#' Funciones de parseo para la respuesta de la RAE
#'
#' @description
#' Este archivo contiene funciones para procesar el HTML retornado por la API.

#' Extraer definiciones del HTML
#'
#' @param contenido_html El contenido HTML crudo.
#' @return Un dataframe con las definiciones y metadatos.
#' @importFrom rvest read_html html_nodes html_text html_attr html_node
#' @importFrom xml2 read_html xml_find_all xml_text xml_remove xml_attr xml_name
#' @importFrom dplyr tibble bind_rows
#' @keywords internal
extraer_definiciones <- function(contenido_html) {
  if (is.null(contenido_html) || contenido_html == "") {
    warning("El contenido HTML proporcionado esta vacio o es nulo. No se pueden extraer definiciones.")
    return(dplyr::tibble())
  }
  pagina <- rvest::read_html(contenido_html)
  
  articulo <- rvest::html_node(pagina, "article")
  if (inherits(articulo, "xml_missing")) {
    contenedor <- rvest::html_node(pagina, "body")
  } else {
    contenedor <- articulo
  }
  
  hijos <- xml2::xml_children(contenedor)
  
  lista_definiciones <- list()
  definicion_actual <- NULL
  
  for (i in seq_along(hijos)) {
    nodo <- hijos[[i]]
    etiqueta <- xml2::xml_name(nodo)
    atributo_clase <- xml2::xml_attr(nodo, "class")
    if (is.na(atributo_clase)) atributo_clase <- ""
    
    if (etiqueta == "p" && grepl("\\bj\\b", atributo_clase)) {
      if (!is.null(definicion_actual)) {
        lista_definiciones[[length(lista_definiciones) + 1]] <- definicion_actual
        definicion_actual <- NULL
      }
      
      nodo_copia <- nodo
      
      num_node <- rvest::html_node(nodo_copia, "span.n_acep")
      numero <- NA_integer_
      if (!inherits(num_node, "xml_missing")) {
        num_text <- rvest::html_text(num_node, trim = TRUE)
        num_clean <- sub("\\.$", "", num_text)
        numero <- suppressWarnings(as.integer(num_clean))
      }
      
      cat_node <- rvest::html_node(nodo_copia, "abbr.d, abbr.g")
      categoria <- if (!inherits(cat_node, "xml_missing")) {
        cat_text <- rvest::html_text(cat_node, trim = TRUE)
        expandir_categoria(cat_text)
      } else {
        NA_character_
      }
      
      if (!inherits(num_node, "xml_missing")) xml2::xml_remove(num_node)
      if (!inherits(cat_node, "xml_missing")) xml2::xml_remove(cat_node)      
      inner_sin <- rvest::html_node(nodo_copia, "div.sin-header")
      if (!inherits(inner_sin, "xml_missing")) xml2::xml_remove(inner_sin)
      marks <- rvest::html_nodes(nodo_copia, "mark")
      for (m in marks) {
        xml2::xml_text(m) <- paste0(xml2::xml_text(m), " ")
      }
      
      definicion <- rvest::html_text(nodo_copia, trim = TRUE)
      definicion <- gsub("\\s+\\.", ".", definicion)
      definicion <- gsub("\\s+", " ", definicion)
      
      definicion_actual <- dplyr::tibble(
        numero = numero,
        categoria = categoria,
        definicion = definicion,
        sinonimos = NA_character_,
        antonimos = NA_character_
      )
    } 
    else if (etiqueta == "div" && grepl("sin-header", atributo_clase)) {
      if (!is.null(definicion_actual)) {
        filas <- rvest::html_nodes(nodo, "tr")
        sinonimos <- NA_character_
        antonimos <- NA_character_
        
        for (fila in filas) {
          nodo_header <- rvest::html_node(fila, "abbr.sin-header-inline")
          nodo_contenido <- rvest::html_node(fila, "td:nth-child(2)") # El contenido esta en la segunda celda
          
          if (!inherits(nodo_header, "xml_missing") && !inherits(nodo_contenido, "xml_missing")) {
            texto_header <- rvest::html_text(nodo_header, trim = TRUE)
            texto_contenido <- rvest::html_text(nodo_contenido, trim = TRUE)
            
            if (grepl("Sin\\.:", texto_header)) {
              sinonimos <- texto_contenido
            } else if (grepl("Ant\\.:", texto_header)) {
              antonimos <- texto_contenido
            }
          }
        }
        
        definicion_actual$sinonimos <- sinonimos
        definicion_actual$antonimos <- antonimos        
        lista_definiciones[[length(lista_definiciones) + 1]] <- definicion_actual
        definicion_actual <- NULL
      }
    }
  }
  
  if (!is.null(definicion_actual)) {
    lista_definiciones[[length(lista_definiciones) + 1]] <- definicion_actual
  }
  
  if (length(lista_definiciones) > 0) {
    return(dplyr::bind_rows(lista_definiciones))
  } else {
    return(dplyr::tibble())
  }
}

#' Extraer etimologia del HTML
#'
#' @param contenido_html El contenido HTML crudo.
#' @return Un string con la etimologia.
#' @keywords internal
extraer_etimologia <- function(contenido_html) {
  if (is.null(contenido_html) || contenido_html == "") {
    return(NA_character_)
  }
  pagina <- rvest::read_html(contenido_html)
  etim_node <- rvest::html_node(pagina, "p.n2")
  if (is.na(etim_node)) {
    return(NA_character_)
  }
  return(rvest::html_text(etim_node, trim = TRUE))
}

#' Extraer ID de conjugacion
#'
#' @param contenido_html El contenido HTML crudo.
#' @return El ID de conjugacion o NA si no existe.
#' @keywords internal
extraer_id_conjugacion <- function(contenido_html) {
  if (is.null(contenido_html) || contenido_html == "") {
    return(NA_character_)
  }
  pagina <- rvest::read_html(contenido_html)
  link <- rvest::html_node(pagina, "a[href^='#conjugacion']")
  
  if (inherits(link, "xml_missing")) {
    return(NA_character_)
  }
  
  href <- rvest::html_attr(link, "href")
  id <- sub("#conjugacion", "", href)
  return(id)
}

#' Extraer tabla de conjugaciones
#'
#' @param contenido_html El contenido HTML crudo de la conjugacion.
#' @return Un dataframe con las conjugaciones.
#' @importFrom rvest html_nodes html_text html_attr read_html
#' @importFrom dplyr tibble bind_rows
#' @keywords internal
extraer_conjugaciones <- function(contenido_html) {
  if (is.null(contenido_html) || contenido_html == "") {
    return(dplyr::tibble())
  }
  
  pagina <- rvest::read_html(contenido_html)
  nodo_tabla <- rvest::html_node(pagina, "table")
  
  if (inherits(nodo_tabla, "xml_missing")) {
    return(dplyr::tibble())
  }
  
  filas <- rvest::html_nodes(nodo_tabla, "tr")
  
  resultados <- list()
  
  modo_actual <- "Formas no personales" # Valor inicial por defecto
  tiempos_actuales <- c(NA_character_, NA_character_)
  
  for (fila in filas) {
    ths <- rvest::html_nodes(fila, "th")
    tds <- rvest::html_nodes(fila, "td")
    
    # caso 1
    if (length(ths) > 0) {
      textos_ths <- rvest::html_text(ths, trim = TRUE)
      
      modos_conocidos <- c("Indicativo", "Subjuntivo", "Imperativo")
      match_modo <- which(textos_ths %in% modos_conocidos)
      
      if (length(match_modo) > 0) {
        modo_actual <- textos_ths[match_modo[1]]
        next
      }
    }
    
    # caso 2
    if (length(ths) >= 2) {
      textos_ths <- rvest::html_text(ths, trim = TRUE)
      no_vacios <- textos_ths[textos_ths != ""]
      if (length(no_vacios) >= 2) {
        tiempos_actuales <- utils::tail(no_vacios, 2)
      } else if (length(no_vacios) == 1) {
        t <- utils::tail(no_vacios, 1)
        tiempos_actuales <- c(t, t)
      } else if (length(textos_ths) >= 2) {
        tiempos_actuales <- utils::tail(textos_ths, 2)
      }
      
      # Fix para verbos imperativos:
      # TODO: si los tiempos estan vacios, asumir Presente. 
      # Falta tomar definiciones aqui
      if (modo_actual == "Imperativo" && all(tiempos_actuales == "")) {
        tiempos_actuales <- c("Presente", NA_character_)
      }
      next
    }
    
    # Caso 3: Datos - tds
    if (length(tds) > 0) {
      valores <- rvest::html_text(tds, trim = TRUE)
      
      # Si tenemos 5 columnas (lo estandar)
      if (length(valores) >= 5) {
        persona <- valores[3]
        forma1 <- valores[4]
        forma2 <- valores[5]
        
        if (persona == "") persona <- NA_character_
        
        # Marca para Formas no personales
        if (modo_actual == "Formas no personales") {
          persona <- "no aplica"
        }
        
        if (forma1 != "") {
          resultados[[length(resultados) + 1]] <- dplyr::tibble(
            modo = modo_actual,
            tiempo = tiempos_actuales[1],
            persona = persona,
            forma = forma1
          )
        }
        
        if (forma2 != "") {
          resultados[[length(resultados) + 1]] <- dplyr::tibble(
            modo = modo_actual,
            tiempo = tiempos_actuales[2],
            persona = persona,
            forma = forma2
          )
        }
      } 

      # Si tenemos 4 columnas por ej, Imperativo
      else if (length(valores) == 4) {
        persona <- valores[3]
        forma1 <- valores[4]
        
        if (persona == "") persona <- NA_character_
        
        # Marca para Formas no personales
        if (modo_actual == "Formas no personales") {
          persona <- "no aplica"
        }
        
        if (forma1 != "") {
          resultados[[length(resultados) + 1]] <- dplyr::tibble(
            modo = modo_actual,
            tiempo = tiempos_actuales[1], # Usamos el primer tiempo (Presente)
            persona = persona,
            forma = forma1
          )
        }
      }
    }
  }
  
  if (length(resultados) > 0) {
    df_final <- dplyr::bind_rows(resultados)
    df_final <- df_final[!is.na(df_final$forma) & df_final$forma != "", ]
    return(df_final)
  } else {
    return(dplyr::tibble())
  }
}
