# This file allows packrat (used by rsconnect during deployment) to pick up dependencies.
library(rhino)
library(treesitter)
library(treesitter.r)
library(shiny)
library(leaflet)    # Para el mapa interactivo
library(sf)         # Para manejar datos geográficos (.shp)
library(chromote)   # Para el scraping con navegador
library(rvest)      # Para procesar el HTML del scraping
library(dplyr)      # Para manipular tablas de datos
library(waiter)     # Para la pantalla de carga (loading)
library(giscoR)     # Para descargar mapas de Italia si fuera necesario


#rhino::pkg_install(c("shiny", "leaflet", "sf", "chromote", "rvest", "dplyr", "waiter", "giscoR"))
renv::install(c("shiny", "leaflet", "sf", "chromote", "rvest", "dplyr", "waiter", "giscoR"))
