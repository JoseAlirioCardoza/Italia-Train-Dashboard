# app/main.R
box::use(
  shiny[moduleServer, NS, tagList, div, reactiveVal, observeEvent, req, h2, hr, isolate],
  waiter[waiter_show, waiter_hide, spin_fading_circles],
  app/logic/data_manager,
  app/logic/scraper,
  app/view/map_view,
  app/view/table_view
)

# Cargamos los datos estáticos al iniciar (99 estaciones con IDs de RFI)
STATIONS_DATA <- data_manager$get_stations_df()
RAILS_DATA <- data_manager$load_infrastructure()


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container-fluid",
        div(style = "padding: 10px 0;",
            h2("Monitor Ferroviario Italia 2026", style = "color: #2c3e50; font-weight: bold;")
        ),
        hr(),
        # Cambio de proporciones: flex: 5 para mapa, flex: 7 para tablas
        div(style = "display: flex; gap: 20px;",
            div(style = "flex: 5; border: 1px solid #dee2e6; border-radius: 8px; overflow: hidden;", 
                map_view$ui(ns("map"))
            ),
            div(style = "flex: 7; background: #f8f9fa; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);", 
                table_view$ui(ns("tables"))
            )
        )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Valor reactivo para almacenar los resultados del scraping
    results <- reactiveVal(NULL)
    
    # Inicializamos los servidores de los módulos
    # 'clicked_station' devuelve el nombre de la estación clicada en el mapa
    clicked_station <- map_view$server("map", STATIONS_DATA, RAILS_DATA)
    
    # 'refresh_trigger' devuelve el evento del botón de actualización
    refresh_trigger <- table_view$server("tables", results)
    
    # Observador que se dispara al hacer clic en el mapa O al pulsar Actualizar
    observeEvent(list(clicked_station(), refresh_trigger()), {
      
      # Validamos que haya una estación seleccionada
      station_name <- clicked_station()
      req(station_name)
      
      # Mostramos el Waiter (pantalla de carga) sobre el panel de tablas
      waiter_show(
        id = ns("tables-table_container"), 
        html = tagList(
          spin_fading_circles(), 
          div(style = "margin-top: 10px; font-weight: bold;", paste("Aggiornando", station_name, "..."))
        ),
        color = "rgba(248, 249, 250, 0.8)"
      )
      
      # Buscamos la info de la estación (ID de RFI)
      st_info <- STATIONS_DATA[STATIONS_DATA$name == station_name, ]
      
      tryCatch({
        # Ejecutamos el scraper (4 segundos de espera interna)
        data <- scraper$obtener_datos_rfi(st_info$rfi_id)
        
        # Inyectamos metadatos adicionales para la vista
        data$estacion_nombre <- station_name 
        
        # Sincronizamos con la hora oficial de Italia
        data$timestamp <- format(
          Sys.time(), 
          "%d/%m/%Y %H:%M:%S", 
          tz = "Europe/Rome"
        )
        
        # Actualizamos el valor reactivo para que table_view lo renderice
        results(data)
        
      }, error = function(e) {
        message("Error en la descarga de datos: ", e$message)
      })
      
      # Ocultamos el cargador
      waiter_hide(id = ns("tables-table_container"))
      
    }, ignoreInit = TRUE)
    
  })
}