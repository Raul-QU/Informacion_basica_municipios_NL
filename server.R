# server.R

function(input, output, session) {
  
  fisher_vars <- c("POB_TOT", "PAM_TOT", "PAM_HOM", "PAM_MUN", 
                   "PAM_DISC", "PAM_PRIM_I", "PAM_PEA", "PAM_NOAFME")
  cont_vars <- c("PCT_PAM", "PCT_DISC", "PCT_REZEDU", "PCT_PEA", "PCT_NOAFME")
  
  var_labels <- c(
    "POB_TOT" = "Población Total",
    "PAM_TOT" = "Población Adulta Mayor",
    "PAM_HOM" = "Población Adulta Mayor Hombre",
    "PAM_MUN" = "Población Adulta Mayor Mujer",
    "PAM_DISC" = "Población Adulta Mayor con Discapacidad",
    "PAM_PRIM_I" = "Población Adulta Mayor con Rezago Educativo",
    "PAM_PEA" = "Población Adulta Mayor Económicamente Activa",
    "PAM_NOAFME" = "Población Adulta Mayor sin Afiliación Médica",
    "PCT_PAM" = "Porcentaje de Personas Adultas Mayores",
    "PCT_DISC" = "Porcentaje de Personas Adultas Mayores con Discapacidad",
    "PCT_REZEDU" = "Porcentaje de Personas Adultas Mayores con Rezago Educativo",
    "PCT_PEA" = "Porcentaje de Personas Adultas Mayores Económicamente Activas",
    "PCT_NOAFME" = "Porcentaje de Personas Adultas Mayores sin Afiliación Médica"
  )
  
  output$variable_selector <- renderUI({
    vars_numericas <- ageb %>%
      st_drop_geometry() %>%
      select(where(is.numeric)) %>%
      select(-OID) %>%
      names()
    
    selectInput("variable", "Selecciona variable para clasificar:",
                choices = setNames(vars_numericas, var_labels[vars_numericas]))
  })
  
  municipio_sel <- reactive({
    req(input$municipio)
    mun %>% filter(NOMGEO == input$municipio)
  })
  
  ageb_filtradas <- reactive({
    req(input$municipio, input$variable)
    muni_geom <- municipio_sel()
    dentro <- st_within(ageb, muni_geom)
    idx_dentro <- lengths(dentro) > 0
    
    ageb[idx_dentro, ] %>%
      mutate(valor = .[[input$variable]])
  })
  
  output$mapa <- renderLeaflet({
    req(input$municipio, input$variable, input$alpha)
    
    muni_geom <- municipio_sel()
    ageb_data <- ageb_filtradas()
    
    variable <- input$variable
    alpha <- input$alpha
    
    leaflet_base <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = muni_geom,
                  fill = FALSE,
                  color = "black",
                  weight = 3,
                  group = "Municipio seleccionado")
    
    if (variable %in% fisher_vars) {
      ageb_vals <- ageb_data$valor
      ageb_vals <- ageb_vals[!is.na(ageb_vals) & ageb_vals >= 0]
      
      n_clases <- min(5, length(unique(ageb_vals)))
      if (n_clases < 2) n_clases <- 2
      
      breaks_raw <- classIntervals(ageb_vals, n = n_clases, style = "fisher")$brks
      breaks <- pmax(0, breaks_raw)
      
      etiquetas <- c()
      for (i in 1:(length(breaks) - 1)) {
        ini <- floor(breaks[i])
        fin <- ceiling(breaks[i + 1]) - 1
        etiquetas[i] <- paste("De", ini, "a", fin)
      }
      
      ageb_data <- ageb_data %>%
        mutate(clase = cut(valor, breaks = breaks, include.lowest = TRUE, labels = etiquetas))
      
      pal <- colorFactor("YlOrRd", domain = etiquetas)
      
      leaflet_base %>%
        addPolygons(data = ageb_data,
                    fillColor = ~pal(clase),
                    color = "#444", weight = 1,
                    fillOpacity = alpha,
                    label = ~paste0(variable, ": ", round(valor)),
                    group = "AGEBs") %>%
        addLegend("bottomright", pal = pal, values = ageb_data$clase,
                  title = var_labels[[variable]], opacity = 1)
      
    } else if (variable %in% cont_vars) {
      pal <- colorNumeric(
        palette = colorRampPalette(c("green", "yellow", "red"))(100),
        domain = ageb_data$valor
      )
      
      leaflet_base %>%
        addPolygons(data = ageb_data,
                    fillColor = ~pal(valor),
                    color = "#444", weight = 1,
                    fillOpacity = alpha,
                    label = ~paste0(variable, ": ", round(valor, 1)),
                    group = "AGEBs") %>%
        addLegend("bottomright", pal = pal, values = ageb_data$valor,
                  title = var_labels[[variable]], opacity = 1)
    }
  })
}