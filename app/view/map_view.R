# app/view/map_view.R

box::use(
  leaflet[
    leaflet, 
    addTiles, 
    setView, 
    addPolylines, 
    addCircleMarkers, 
    leafletOutput, 
    renderLeaflet
  ],
  shiny[moduleServer, NS, tagList, reactive],
  dplyr[`%>%`] # Usamos el pipe desde dplyr como prefieres
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("rail_map"), height = "600px")
  )
}

#' @export
server <- function(id, stations_df, rails_data) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Renderizado del mapa
    output$rail_map <- renderLeaflet({
      # Extraer datos si vienen como reactivos o dataframe
      df <- if (is.function(stations_df)) stations_df() else stations_df
      df <- as.data.frame(df)
      
      leaflet() %>%
        # Estilo minimalista blanco usando la URL de CartoCDN
        addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png") %>%
        setView(lng = 12.5, lat = 41.9, zoom = 6) %>%
        
        # Vías de tren en color morado
        addPolylines(
          data = rails_data, 
          color = "#7030a0", 
          weight = 2, 
          opacity = 0.6
        ) %>%
        
        # Estaciones con la jerarquía de colores y tamaños (V-N-A)
        addCircleMarkers(
          data = df,
          lng = ~lng, 
          lat = ~lat, 
          layerId = ~name, # Crucial: esto es lo que lee el servidor principal
          radius = ~ifelse(tier == 1, 10, 
                           ifelse(tier == 2, 6, 3)),
          color = ~ifelse(tier == 1, "#27ae60", 
                          ifelse(tier == 2, "#e67e22", "#f1c40f")),
          stroke = TRUE,
          weight = 1,
          fillOpacity = 0.8, 
          label = ~name
        )
    })
    
    # 2. RETORNO PARA INTERACTIVIDAD
    # Este objeto reactive permite que main.R sepa qué estación se clickeó
    return(
      reactive({
        input$rail_map_marker_click$id
      })
    )
  })
}
