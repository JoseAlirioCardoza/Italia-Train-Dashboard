# app/logic/scraper.R
box::use(
  chromote[Chromote],
  rvest[read_html, html_elements, html_element, html_text, html_attr],
  dplyr[as_tibble, bind_rows, case_when, filter, `%>%`],
  shiny[tagList, div]
)

#' @export
obtener_datos_rfi <- function(id_estacion) {
  
  scraping_rfi <- function(id, llegada_bool) {
    tipo_url <- if(llegada_bool) "True" else "False"
    url <- paste0("https://iechub.rfi.it/ArriviPartenze/ArrivalsDepartures/Monitor?placeId=", 
                  id, "&arrivals=", tipo_url)
    
    b <- Chromote$new()
    s <- b$new_session()
    
    tryCatch({
      s$Page$navigate(url)
      
      # Estabilidad de carga: 4 segundos
      Sys.sleep(4) 
      
      html_raw <- s$Runtime$evaluate("document.documentElement.outerHTML")$result$value
      s$close()
      
      doc <- read_html(html_raw)
      filas <- doc %>% html_elements("table tbody tr")
      
      if(length(filas) == 0) {
        return(data.frame(Status = "Aviso", 
                          Mensaje = "No se detectaron datos en este momento."))
      }
      
      lista_datos <- lapply(filas, function(f) {
        celdas <- f %>% html_elements("td")
        if (length(celdas) >= 6) {
          # 1. Identificación del Operador (Vettore) vía imagen
          img_node <- celdas[1] %>% html_element("img")
          img_info <- tolower(paste(html_attr(img_node, "src"), html_attr(img_node, "alt")))
          
          # 2. Identificación de Categoría (Texto o imagen)
          cat_raw <- celdas[2] %>% html_text(trim = TRUE)
          if (cat_raw == "") cat_raw <- celdas[2] %>% html_element("img") %>% html_attr("alt")
          cat_limpia <- toupper(gsub("(?i)categoria\\s+", "", cat_raw, perl = TRUE))
          
          # 3. Lógica de Vettore (Jerarquía de marcas)
          vettore_txt <- case_when(
            grepl("italo", img_info) ~ "Italo",
            grepl("trenord", img_info) ~ "Trenord",
            grepl("intercity", img_info) ~ "Intercity",
            grepl("ALTA VELOCITA|FRECCIA", cat_limpia) ~ "Frecciarossa",
            grepl("REG|RV|R", cat_limpia) ~ "Trenitalia Regionale",
            TRUE ~ "Trenitalia"
          )
          
          # 4. Limpieza de Ritardo (solo números)
          ritardo_raw <- celdas[6] %>% html_text(trim = TRUE)
          ritardo_num <- gsub("[^0-9]", "", ritardo_raw)
          if(ritardo_num == "") ritardo_num <- "0"
          
          return(data.frame(
            Vettore = vettore_txt,
            Categoria = cat_limpia,
            Treno = html_text(celdas[3], trim = TRUE),
            Destinazione_Provenienza = html_text(celdas[4], trim = TRUE),
            Orario = html_text(celdas[5], trim = TRUE),
            Ritardo = paste0(ritardo_num, "'"),
            Binario = html_text(celdas[7], trim = TRUE),
            stringsAsFactors = FALSE
          ))
        }
      })
      
      res <- bind_rows(lista_datos) %>% filter(!is.na(Treno) & Treno != "")
      
      if(nrow(res) == 0) return(data.frame(Status = "Sin datos", Mensaje = "Tabla vacía."))
      return(as_tibble(res))
      
    }, error = function(e) {
      if(exists("s")) s$close()
      return(data.frame(Error = paste("Detalle:", e$message)))
    })
  }
  
  # Estructura de retorno compatible con el sistema de actualización
  list(
    estacion_nombre = NULL, 
    timestamp = NULL, 
    Arrivi = scraping_rfi(id_estacion, TRUE), 
    Partenze = scraping_rfi(id_estacion, FALSE)
  )
}