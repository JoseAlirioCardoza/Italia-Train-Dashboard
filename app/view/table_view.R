# app/view/table_view.R
box::use(
  shiny[moduleServer, NS, tagList, div, h3, h4, tableOutput, renderTable, 
        renderText, textOutput, hr, req, actionButton, icon, reactive, tags],
  waiter[useWaiter]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    useWaiter(),
    tags$style(paste0("
      /* Estilo del botón */
      #", ns("refresh"), " {
        border-radius: 50%; width: 40px; height: 40px; 
        background-color: #f8f9fa; color: #2c3e50; border: 1px solid #dee2e6;
        transition: all 0.3s ease;
      }
      #", ns("refresh"), ":hover {
        background-color: #e9ecef; color: #007bff; border-color: #007bff; transform: rotate(45deg);
      }
      #", ns("refresh"), ":active { background-color: #007bff; color: white; }

      /* REDUCCIÓN DE LETRA EN TABLAS */
      #", ns("table_container"), " table {
        font-size: 0.82rem; /* Letra más pequeña */
        width: 100% !important;
      }
      #", ns("table_container"), " th {
        background-color: #2c3e50; /* Cabecera oscura estilo ferroviario */
        color: white;
        padding: 5px !important;
      }
      #", ns("table_container"), " td {
        padding: 4px !important; /* Más compacto */
        vertical-align: middle;
      }
    ")),
    
    div(
      id = ns("table_container"), 
      style = "min-height: 400px; padding: 10px; position: relative;", 
      
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
          h3(textOutput(ns("station_title")), style = "color: #2c3e50; font-weight: bold; margin: 0;"),
          actionButton(ns("refresh"), label = NULL, icon = icon("sync"))
      ),
      
      div(style = "color: #95a5a6; font-size: 0.85em; font-style: italic; margin-bottom: 15px;",
          textOutput(ns("last_update"))
      ),
      
      hr(),
      h4("🛬 Arrivi", style = "font-size: 1.1em; color: #2980b9;"),
      tableOutput(ns("table_arrivi")),
      hr(),
      h4("🛫 Partenze", style = "font-size: 1.1em; color: #c0392b;"),
      tableOutput(ns("table_partenze"))
    )
  )
}

#' @export
server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    output$station_title <- renderText({
      req(data_reactive())
      data_reactive()$estacion_nombre
    })
    
    output$last_update <- renderText({
      req(data_reactive())
      if (!is.null(data_reactive()$timestamp)) {
        paste("Aggiornato:", data_reactive()$timestamp)
      }
    })
    
    output$table_arrivi <- renderTable({
      req(data_reactive())
      data_reactive()$Arrivi
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$table_partenze <- renderTable({
      req(data_reactive())
      data_reactive()$Partenze
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    return(reactive(input$refresh))
  })
}