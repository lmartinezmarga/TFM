library(shiny)
library(shinycssloaders)
library(sf)
library(leaflet)
library(dplyr)
library(scales)
library(ggplot2)

# CÀRREGA DE DADES
inc <- readRDS("data/incendis.rds") %>% st_transform(4326)
inc_pts <- readRDS("data/centroides_incendis.rds") %>% st_transform(4326)

municipis <- readRDS("data/municipis.rds") %>% st_transform(4326)
comarques <- readRDS("data/comarques.rds") %>% st_transform(4326)
provincies <- readRDS("data/provincies.rds") %>% st_transform(4326)

inc_municipi <- readRDS("data/inc_mun_agregat.rds") %>% st_transform(4326)
inc_comarca <- readRDS("data/inc_com_agregat.rds") %>% st_transform(4326)
inc_provincia <- readRDS("data/inc_prov_agregat.rds") %>% st_transform(4326)

cremes <- readRDS("data/cremes.rds") %>% st_transform(4326)

# UI
ui <- fluidPage(
  
  # Títol de l'aplicació
  titlePanel("Incendis forestals a Catalunya (2000–2024)"),
  
  # Pestanyes principals
  tabsetPanel(
    
    # Pestanya 1: Incendis i visualització
    tabPanel("Incendis i visualització",
      sidebarLayout(
        
        # Panell lateral
        sidebarPanel(
          width = 3,
          
          # Selector d'anys
          sliderInput(
            "any",
            "Rang d'anys:",
            min = min(inc$ANY),
            max = max(inc$ANY),
            value = c(min(inc$ANY), max(inc$ANY)),
            step = 1,
            sep = ""
          ),
          
          # Tipus de visualització espacial
          radioButtons(
            "visual",
            "Visualització:",
            choices = c(
              "Incendis (polígon)" = "poly",
              "Incendis (punt)" = "point",
              "Agregat per unitat administrativa" = "agregat"
            ),
            selected = "poly"
          ),
          
          # Selector d'estació (només si NO és agregat)
          conditionalPanel(
            condition = "input.visual != 'agregat'",
            checkboxGroupInput(
              "temporada",
              "Estació:",
              choices = unique(inc$TEMPORADA),
              selected = unique(inc$TEMPORADA)
            )
          ),
          
          # Opcions addicionals quan la visualització és agregada
          conditionalPanel(
            condition = "input.visual == 'agregat'",
            
            # Unitat administrativa
            selectInput(
              "nivell",
              "Unitat administrativa:",
              choices = c(
                "Municipis" = "Municipis",
                "Comarques" = "Comarques",
                "Províncies" = "Províncies"
              ),
              selected = "Municipis"
            ),
            
            # Mètrica utilitzada per al rànquing
            selectInput(
              "metrica_top",
              "Mètrica del TOP:",
              choices = c(
                "Nombre d'incendis" = "n_incendis",
                "Hectàrees cremades" = "HA_CREMADES"
              ),
              selected = "n_incendis"
            )
          ),
          
          # Indicadors dinàmics
          uiOutput("n_incendis"),
          uiOutput("ha_cremades"),
          
          # Mostrar o amagar cremes prescrites
          checkboxInput("mostrar_cremes", "Mostrar cremes prescrites", value = FALSE)
        ),
        
        # Panell principal
        mainPanel(
          fluidRow(
            
            # Mapa principal
            column(
              width = 8,
              withSpinner(
                leafletOutput("mapa", height = "80vh"),
                type = 7,
                color = "steelblue"
              )
            ),
            
            # Gràfic TOP 10 (només en vista agregada)
            column(
              width = 4,
              conditionalPanel(
                condition = "input.visual == 'agregat'",
                h4("Top 10"),
                plotOutput("plot_top", height = "70vh")
              )
            )
          ),
          
          # Pestanyes de gràfics temporals
          tabsetPanel(
            tabPanel("Nombre d'incendis",
                     plotOutput("plot_evol_any_incendis"),
                     plotOutput("plot_evol_estacio_incendis")
            ),
            tabPanel("Hectàrees cremades",
                     plotOutput("plot_evol_any_ha"),
                     plotOutput("plot_evol_estacio_ha")
            )
          )
        )
      )
    ),
    
    # Pestanya 2: Anàlisi de buffers de cremes prescrites
    tabPanel(
      "Anàlisi buffers cremes",
      sidebarLayout(
        
        # Panell lateral
        sidebarPanel(
          
          # Any base de les cremes prescrites
          selectInput("any_base", "Any base cremes prescrites:",
                      choices = sort(unique(cremes$ANY))),
          
          # Radi del buffer
          selectInput("buffer_radius", "Radi buffer (m):",
                      choices = c(500, 1000, 2000), selected = 1000),
          
          # Anys posteriors a visualitzar
          checkboxGroupInput("years_after", "Anys després a mostrar:",
                             choices = 1:3, selected = 1:3),
          hr(),
          h4("Incendis dins buffers"),
          
          # Comptador d'incendis
          tableOutput("comptador_incendis")
        ),
        
        # Mapa de buffers
        mainPanel(
          leafletOutput("mapa_buffers", height = "80vh")
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # REACTIUS DE FILTRAT
  
  # Incendis (polígons) filtrats per any i estació
  inc_filt <- reactive({
    inc %>%
      filter(
        ANY >= input$any[1],
        ANY <= input$any[2],
        TEMPORADA %in% input$temporada
      )
  })
  
  # Incendis (punts) filtrats per any i estació
  inc_pts_filt <- reactive({
    inc_pts %>%
      filter(
        ANY >= input$any[1],
        ANY <= input$any[2],
        TEMPORADA %in% input$temporada
      )
  })
  
  # Agregació d'incendis per nivell administratiu
  inc_agregat <- reactive({
    
    # Selecció de la capa segons nivell
    dades <- switch(input$nivell,
                    Municipis = inc_municipi,
                    Comarques  = inc_comarca,
                    Províncies = inc_provincia)
    
    # Nom de la columna identificadora
    nom_col <- if (input$nivell == "Municipis") "NOMMUNI" else
      if (input$nivell == "Comarques") "NOMCOMAR" else "NOMPROV"
    
    # Agregació de mètriques
    dades %>%
      filter(ANY >= input$any[1], ANY <= input$any[2]) %>%
      group_by(.data[[nom_col]]) %>%
      summarise(
        n_incendis = sum(n_incendis),
        HA_CREMADES = sum(HA_CREMADES),
        .groups = "drop"
      ) %>%
      rename(NOM_UNITAT = all_of(nom_col))
  })
  
  # Top 10 unitats administratives segons mètrica seleccionada
  top_react <- reactive({
    req(input$visual == "agregat")
    inc_agregat() %>%
      st_drop_geometry() %>%
      arrange(desc(.data[[input$metrica_top]])) %>%
      slice_head(n = 10)
  })
  
  # Taula evolució anual
  taula_any_react <- reactive({
    inc_filt() %>%
      st_drop_geometry() %>%
      group_by(ANY) %>%
      summarise(
        n_incendis = n(),
        HA_CREMADES = sum(HA_CREMADES),
        .groups = "drop"
      )
  })
  
  # Taula per estació de l'any
  taula_estacio_react <- reactive({
    inc_filt() %>%
      st_drop_geometry() %>%
      group_by(TEMPORADA) %>%
      summarise(
        n_incendis = n(),
        HA_CREMADES = sum(HA_CREMADES),
        .groups = "drop"
      )
  })
  
  # Cremes prescrites filtrades pel rang d'anys
  cremes_filt <- reactive({
    req(input$any)
    cremes %>%
      filter(ANY >= input$any[1], ANY <= input$any[2])
  })
  
  # KPIs
  
  # Nombre total d'incendis
  output$n_incendis <- renderUI({
    n <- if (input$visual == "agregat") {
      sum(inc_agregat()$n_incendis, na.rm = TRUE)
    } else {
      nrow(inc_filt())
    }
    wellPanel(
      h4("Nombre d'incendis"),
      h2(n),
      if (input$visual == "agregat")
        tags$p(
          style = "font-size:10px;color:gray;",
          "Un mateix incendi pot comptar en més d'una unitat administrativa."
        )
    )
  })
  
  # Total d'hectàrees cremades
  output$ha_cremades <- renderUI({
    ha <- if (input$visual == "agregat") {
      sum(inc_agregat()$HA_CREMADES, na.rm = TRUE)
    } else {
      sum(inc_filt()$HA_CREMADES, na.rm = TRUE)
    }
    wellPanel(
      h4("Hectàrees cremades"),
      h2(round(ha, 1))
    )
  })
  
  # MAPA PRINCIPAL
  output$mapa <- renderLeaflet({
    
    # Selecció de dades segons tipus de visualització
    dades <- switch(input$visual,
                    poly  = inc_filt(),
                    point = inc_pts_filt(),
                    agregat = inc_agregat())
    req(nrow(dades) > 0)
    
    # Definició de paleta de colors
    if (length(unique(dades$HA_CREMADES)) < 2) {
      pal <- colorNumeric("Reds", domain = dades$HA_CREMADES)
      labels <- NULL
    } else {
      quantils <- quantile(dades$HA_CREMADES,
                           probs = seq(0, 1, length.out = 6),
                           na.rm = TRUE)
      pal <- colorBin("Reds",
                      domain = dades$HA_CREMADES,
                      bins = quantils,
                      right = FALSE)
      labels <- paste0(
        format(round(quantils[-length(quantils)], 1), nsmall = 1), " - ",
        format(round(quantils[-1], 1), nsmall = 1)
      )
    }
    
    # Inicialització del mapa
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 1.90, lat = 41.7, zoom = 8)
    
    # Visualització segons tipus
    if (input$visual == "poly") {
      m <- m %>%
        addPolygons(
          data = dades,
          fillColor = ~pal(HA_CREMADES),
          fillOpacity = 0.6,
          color = "grey40",
          weight = 1,
          popup = ~paste0(
            "<b>Municipi:</b> ", MUNICIPI, "<br>",
            "<b>Any:</b> ", ANY, "<br>",
            "<b>Estació:</b> ", TEMPORADA, "<br>",
            "<b>Hectàrees:</b> ", round(HA_CREMADES, 1)
          )
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = dades$HA_CREMADES,
          title = "Hectàrees cremades",
          labFormat = function(type, cuts, p) { labels }
        )
    } else if (input$visual == "point") {
      m <- m %>%
        addCircleMarkers(
          data = dades,
          radius = ~scales::rescale(sqrt(HA_CREMADES), c(3, 15)),
          fillColor = ~pal(HA_CREMADES),
          fillOpacity = 0.8,
          color = "black",
          weight = 1,
          popup = ~paste0(
            "<b>Municipi:</b> ", MUNICIPI, "<br>",
            "<b>Any:</b> ", ANY, "<br>",
            "<b>Estació:</b> ", TEMPORADA, "<br>",
            "<b>Hectàrees:</b> ", round(HA_CREMADES, 1)
          )
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = dades$HA_CREMADES,
          title = "Hectàrees cremades",
          labFormat = function(type, cuts, p) { labels },
        )
    } else if (input$visual == "agregat") {
      limits <- switch(input$nivell,
                       Municipis = municipis,
                       Comarques = comarques,
                       Províncies = provincies)
      
      m <- m %>%
        addPolygons(
          data = limits,
          fill = FALSE,
          color = "black",
          weight = 1
        ) %>%
        addPolygons(
          data = dades,
          fillColor = ~pal(HA_CREMADES),
          fillOpacity = 0.7,
          color = "grey30",
          weight = 1,
          popup = ~paste0(
            "<b>", NOM_UNITAT, "</b><br>",
            "<b>Incendis:</b> ", n_incendis, "<br>",
            "<b>Hectàrees cremades:</b> ", round(HA_CREMADES, 1)
          )
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = dades$HA_CREMADES,
          title = "Hectàrees cremades",
          labFormat = function(type, cuts, p) { labels },
        )
    }
    
    # Afegir cremes prescrites si està activat
    if (input$mostrar_cremes) {
      m <- m %>%
        addPolygons(
          data = cremes_filt(),
          fillColor = "green",
          fillOpacity = 0.3,
          color = "green",
          weight = 1,
          popup = ~paste0("<b>Crema prescrita</b><br>Any: ", ANY)
        ) %>%
        addLegend(position = "bottomleft",
                  colors = "green",
                  labels = "Cremes prescrites")
    }
    m
  })
  
  # GRÀFICS TEMPORALS
  
  # Evolució anual del nombre d'incendis
  output$plot_evol_any_incendis <- renderPlot({
    ggplot(taula_any_react(), aes(x = ANY, y = n_incendis)) +
      geom_line(color = "#E31A1C") +
      geom_point(color = "#E31A1C") +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(title = "Evolució anual del nombre d'incendis",
           x = "Any", y = "Nombre d'incendis") +
      theme_minimal()
  })
  
  # Nombre d'incendis per estació
  output$plot_evol_estacio_incendis <- renderPlot({
    ggplot(taula_estacio_react(), aes(x = TEMPORADA, y = n_incendis)) +
      geom_col(fill = "#E31A1C") +
      labs(title = "Nombre d'incendis per estació",
           x = "Estació", y = "Nombre d'incendis") +
      theme_minimal()
  })
  
  # Evolució anual d'hectàrees cremades
  output$plot_evol_any_ha <- renderPlot({
    ggplot(taula_any_react(), aes(x = ANY, y = HA_CREMADES)) +
      geom_line(color = "#E31A1C") +  # blau diferent per distinció
      geom_point(color = "#E31A1C") +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(title = "Evolució anual d'hectàrees cremades",
           x = "Any", y = "Hectàrees cremades") +
      theme_minimal()
  })
  
  # Hectàrees cremades per estació
  output$plot_evol_estacio_ha <- renderPlot({
    ggplot(taula_estacio_react(), aes(x = TEMPORADA, y = HA_CREMADES)) +
      geom_col(fill = "#E31A1C") +
      labs(title = "Hectàrees cremades per estació",
           x = "Estació", y = "Hectàrees cremades") +
      theme_minimal()
  })
  
  # Top 10 unitats administratives (agregat)
  output$plot_top <- renderPlot({
    req(input$visual == "agregat")
    top <- top_react()
    
    # Etiqueta de l'eix Y segons mètrica seleccionada
    etiqueta <- if (input$metrica_top == "n_incendis") {
      "Nombre d'incendis"
    } else {
      "Hectàrees cremades"
    }
    
    ggplot(top,
           aes(
             x = reorder(NOM_UNITAT, .data[[input$metrica_top]]),
             y = .data[[input$metrica_top]]
           )) +
      geom_col(fill = "#E31A1C") +
      coord_flip() +
      labs(
        title = paste(input$nivell, "per", etiqueta),
        x = NULL,
        y = etiqueta
      ) +
      theme_minimal()
  })
  
  # ANÀLISI POST-CREMA AMB BUFFERS
  
  # Cremes prescrites de l'any base
  cremes_base <- reactive({
    req(input$any_base)
    cremes %>% filter(ANY == input$any_base)
  })
  
  # Buffers precomputats al voltant de les cremes
  buffers_base <- reactive({
    req(input$buffer_radius, input$any_base)
    
    # Carregar fitxer de buffers segons radi seleccionat
    cremes_buffer <- readRDS(paste0("data/cremes_buffer_", input$buffer_radius, ".rds"))
    
    # Filtrar per any base
    cremes_buffer %>% filter(ANY == input$any_base)
  })
  
  # Incendis dels anys posteriors a la crema
  inc_filtered <- reactive({
    req(input$any_base, input$years_after)
    
    # Anys a analitzar (any base + anys posteriors)
    anys_mostrar <- as.numeric(input$any_base) + as.numeric(input$years_after)
    inc %>% filter(ANY %in% anys_mostrar)
  })
  
  # Comptador d'incendis dins els buffers
  comptadors_simples <- reactive({
    buffers <- buffers_base()
    incs <- inc_filtered()
    
    # Assegurar mateix sistema de coordenades
    if (st_crs(buffers) != st_crs(incs)) {
      incs <- st_transform(incs, st_crs(buffers))
    }
    
    # Intersecció booleana incendis vs buffers
    interseccio <- st_intersects(incs, buffers, sparse = FALSE)
    
    # Anys a comptar
    anys_a_comptar <- as.numeric(input$any_base) + as.numeric(input$years_after)
    
    # Comptadors incendis que intersecten algun buffer per any
    comptadors <- sapply(anys_a_comptar, function(any_sel) {
      incs_any_sel <- which(incs$ANY == any_sel)
      if (length(incs_any_sel) == 0) return(0)
      sum(rowSums(interseccio[incs_any_sel, , drop=FALSE]) > 0) # si l'incendi entra en algun buffer
    })
    names(comptadors) <- paste0("Incendis any ", anys_a_comptar)
    comptadors
  })
  
  # Mostrar comptadors al UI
  output$comptador_incendis <- renderUI({
    cts <- comptadors_simples()
    tagList(
      lapply(names(cts), function(nom) {
        tags$p(strong(nom), ": ", cts[[nom]])
      })
    )
  })
  
  # Mapa de buffers i incendis posteriors
  output$mapa_buffers <- renderLeaflet({
    cremes_buf <- buffers_base()
    cremes_poligons <- cremes_base()
    incs <- inc_filtered()
    
    # Assignar color segons anys posteriors
    colors_anys <- c("1" = "red", "2" = "orange", "3" = "yellow")
    incs$anys_despres <- as.character(incs$ANY - as.numeric(input$any_base))
    
    pal_anys <- colorFactor(palette = colors_anys, domain = incs$anys_despres)
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 1.90, lat = 41.7, zoom = 8)
    
    m <- m %>%
      
      # Cremes prescrites
      addPolygons(data = cremes_poligons,
                  fillColor = "green", fillOpacity = 0.3,
                  color = "green", weight = 1,
                  popup = ~paste0("Crema prescrita any ", ANY)) %>%
      
      # Buffers
      addPolygons(data = cremes_buf,
                  fill = FALSE,
                  color = "green", weight = 2,
                  popup = ~paste0("Buffer ", input$buffer_radius, "m any ", ANY))

    
    # Incendis posteriors
    m <- m %>%
      addPolygons(data = incs,
                  fillColor = ~pal_anys(anys_despres),
                  fillOpacity = 0.5,
                  color = "black",
                  weight = 1,
                  popup = ~paste0("Incendi any ", ANY))
    
    # Llegenda
    m <- m %>%
      addLegend("bottomright", pal = pal_anys, values = incs$anys_despres,
                title = "Anys després del buffer base",
                labFormat = labelFormat(suffix = " any(s)"))
    m
  })
}

shinyApp(ui, server)
