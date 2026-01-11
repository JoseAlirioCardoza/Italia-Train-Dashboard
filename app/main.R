# app/main.R
box::use(
  shiny[moduleServer, NS, tagList, div, reactiveVal, observeEvent, req, h2, hr, isolate, tags, HTML, br],
  waiter[waiter_show, waiter_hide, use_waiter],
  app/logic/data_manager,
  app/logic/scraper,
  app/view/map_view,
  app/view/table_view
)

STATIONS_DATA <- data_manager$get_stations_df()
RAILS_DATA <- data_manager$load_infrastructure()

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    use_waiter(),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@700&family=Roboto:wght@300;400&display=swap"),
      tags$style(HTML(paste0("
        body {
          background-color: #f8f9fa;
          font-family: 'Roboto', sans-serif;
          color: #2c3e50;
        }

        .container-fluid { padding: 30px 50px 10px 50px; }

        /* HEADER */
        h2 { 
          font-family: 'JetBrains Mono', monospace;
          font-weight: 700;
          color: #2980b9; 
          letter-spacing: -1px;
          margin: 0;
          font-size: 2.2rem;
        }

        .subtitle {
          font-family: 'JetBrains Mono', monospace;
          color: #2980b9; 
          font-size: 0.85rem;
          margin-top: 5px;
          opacity: 0.8;
        }

        /* TABLA Y COLORES */
        #", ns("tabla_container"), " h3, 
        #", ns("tabla_container"), " h4,
        #", ns("tabla_container"), " .station-title {
          color: #2980b9 !important;
          font-family: 'JetBrains Mono', monospace;
          font-weight: 700;
        }

        #", ns("tabla_container"), " thead th {
          background-color: #2980b9 !important;
          color: white !important;
          border-color: #2471a3 !important;
          font-family: 'JetBrains Mono', monospace;
        }

        /* CARDS */
        .r-card {
          background: #ffffff;
          border-radius: 4px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
          padding: 25px;
          border: 1px solid #e1e4e8;
          position: relative !important;
        }

        #", ns("map_card"), " {
          height: 650px; 
          flex: 0 0 auto;
        }

        /* FOOTER ACTUALIZADO */
        .footer {
          margin-top: 40px;
          padding: 20px;
          text-align: center;
          font-family: 'JetBrains Mono', monospace;
          font-size: 0.75rem;
          color: #95a5a6;
          border-top: 1px solid #e1e4e8;
        }

        /* WAITER */
        .waiter-overlay {
          position: absolute !important;
          top: 0 !important; left: 0 !important;
          width: 100% !important; height: 100% !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          background-color: rgba(255, 255, 255, 0.8) !important;
          cursor: wait !important;
        }
      ")))
    ),
    
    div(class = "container-fluid",
        div(style = "margin-bottom: 30px;",
            h2("Monitor Ferroviario"),
            div(class = "subtitle", paste("Data", format(Sys.Date(), "%d %B %Y")))
        ),
        
        div(style = "display: flex; gap: 30px; align-items: flex-start;",
            div(id = ns("map_card"), class = "r-card", style = "width: 40%;",
                map_view$ui(ns("map"))
            ),
            div(id = ns("tabla_container"), class = "r-card", style = "width: 60%; min-height: 500px;",
                table_view$ui(ns("tables"))
            )
        ),
        
        div(class = "footer",
            "Sito web creato con informazioni tratte da RFI (rete ferroviaria italiana), autore Jose Alirio Cardoza, email: cardoza.alirio@gmail.com"
        )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results <- reactiveVal(NULL)
    
    clicked_station <- map_view$server("map", STATIONS_DATA, RAILS_DATA)
    refresh_trigger <- table_view$server("tables", results)
    
    observeEvent(list(clicked_station(), refresh_trigger()), {
      station_name <- clicked_station()
      req(station_name)
      
      waiter_show(
        id = ns("tabla_container"), 
        html = div(style = "font-family: 'JetBrains Mono', monospace; font-size: 0.9em; color: #2c3e50;", 
                   "caricamento_in_corso..."),
        color = "rgba(255, 255, 255, 0.8)"
      )
      
      st_info <- STATIONS_DATA[STATIONS_DATA$name == station_name, ]
      
      tryCatch({
        data <- scraper$obtener_datos_rfi(st_info$rfi_id)
        data$estacion_nombre <- station_name 
        data$timestamp <- format(Sys.time(), "%d/%m/%Y %H:%M:%S", tz = "Europe/Rome")
        results(data)
      }, error = function(e) { message("Error: ", e$message) })
      
      waiter_hide(id = ns("tabla_container"))
      
    }, ignoreInit = TRUE)
  })
}
