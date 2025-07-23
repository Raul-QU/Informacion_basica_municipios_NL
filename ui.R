# ui.R

fluidPage(
  tags$head(
    tags$style(HTML("
      .leaflet-control { z-index: 500; }
      #leyendaCustom {
        font-size: 12px;
        background-color: rgba(255,255,255,0.8);
        padding: 5px;
        border-radius: 5px;
        width: 320px;
      }
      #leyendaCustom small {
        font-size: 10px;
        color: #333;
      }
    "))
  ),
  
  leafletOutput("mapa", height = "100vh"),
  
  absolutePanel(top = 10, left = 10, width = 340, draggable = TRUE, style = "z-index:9999;",
                wellPanel(
                  selectInput("municipio", "Selecciona un municipio:", choices = sort(unique(mun$NOMGEO))),
                  uiOutput("variable_selector"),
                  sliderInput("alpha", "Transparencia (Alpha):", min = 0, max = 1, value = 0.6, step = 0.05)
                )
  ),
  
  absolutePanel(bottom = 10, left = 10, id = "leyendaCustom", fixed = TRUE,
                HTML("Fuente: Elaboración del IEPAM con información del Censo de Población y Vivienda 2020, INEGI<br><small>Contacto: ana.cardenas@nuevoleon.gob.mx / raul.quezada@nuevoleon.gob.mx</small>")
  )
)