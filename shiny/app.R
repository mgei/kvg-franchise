source("setup.R", encoding = "utf-8")

ui <- function(req) {dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    useShinyjs(),
    selectInput("alter", label = "Altersklasse", choices = c("Erwachsene (26+)", "Junge Erwachsene (19-25)", "Kinder (0-18)")),
    selectInput("kanton", label = "Kanton", choices = kantone, selected = "AG"),
    checkboxInput("unfall", label = "Unfall mitversichern", value = F),
    radioButtons("modell", label = NULL, choices = c("nur Standard Grundversicherungen", "auch alternative Modelle")),
    uiOutput("plzfield"),
    sliderInput("kkosten", label = "Erwartete Krankheitskosten", min = 0, max = 10000, value = 1000, step = 10)
  ),
  dashboardBody(
    fluidRow(
      box(
        plotlyOutput("graphly"),
        width = 12
      )
    ),
    fluidRow(
      box(
        DTOutput("table"),
        width = 12
      )
    )
  )
)}

server <- function(input, output) { 
  
  observeEvent(input$modell, {
    if (input$modell == "auch alternative Modelle") {
      enable("plzfield")
      print("enable")
    } else {
      disable("plzfield")
      print("disable")
    }
  })
  
  output$plzfield <- renderUI({
    disabled(
      autocomplete_input("plz", "PLZ Ort", max_options = 100,
      filter(lut_hmo, KTKZ == input$kanton, !is.na(PLZ4)) %>% .[["PlzGemeinde"]])
    )
  })
  
  praemienregion <- reactive({
    req(input$plz)
    
    lut_hmo %>% 
      filter(PlzGemeinde == input$plz) %>% 
      head(n = 1) %>% 
      pull(Region)
  })
  
  data <- reactive({
    
    if (input$modell == "nur Standard Grundversicherungen") {
      premia %>%
        filter(Unfalleinschluss == unfalleinschluss(input$unfall),
               Kanton == input$kanton,
               Tariftyp %in%  tariftyp(input$modell),
               Altersklasse == altersklasse(input$alter),
               Altersuntergruppe %in% c(NA_character_, "K1")) %>%
        mutate(KKosten = input$kkosten) %>% 
        rowwise() %>% 
        mutate(KFranchise = min(KKosten, Franchise),
               KSelbstbeh = max(min(700, (KKosten-Franchise)*0.1), 0),
               KPraemie = Prämie*12,
               KTotal = KFranchise + KSelbstbeh + KPraemie) %>% 
        ungroup() %>% 
        group_by(KKosten) %>% 
        mutate(minFranchise = if_else(KTotal == min(KTotal), "min", "z"),
               minFranchise = if_else(KTotal == max(KTotal), "max", minFranchise)) %>% 
        ungroup() %>% 
        mutate(alpha = "normal")
    } else {
      validate(
        need(input$plz, 
             "Alternative Modelle insbesondere HMO-Modelle sind teilweise unterkantonal nach Prämienregionen aufgeteilt. Bitte geben Sie links Ihre Postleitzahl ein.")
      )
      premia %>%
        filter(Unfalleinschluss == unfalleinschluss(input$unfall),
               Kanton == input$kanton,
               Tariftyp %in%  tariftyp(input$modell),
               Altersklasse == altersklasse(input$alter),
               Altersuntergruppe %in% c(NA_character_, "K1"),
               Region == praemienregion()) %>%
        mutate(KKosten = input$kkosten) %>% 
        rowwise() %>% 
        mutate(KFranchise = min(KKosten, Franchise),
               KSelbstbeh = max(min(700, (KKosten-Franchise)*0.1), 0),
               KPraemie = Prämie*12,
               KTotal = KFranchise + KSelbstbeh + KPraemie) %>% 
        ungroup() %>% 
        group_by(KKosten) %>% 
        mutate(minFranchise = if_else(KTotal == min(KTotal), "min", "z"),
               minFranchise = if_else(KTotal == max(KTotal), "max", minFranchise)) %>% 
        ungroup() %>% 
        mutate(alpha = "normal")
    }

  })
  
  plotdata <- reactive({
    
    r <- input$table_rows_selected
    
    if (length(r)) {
      d <- data() %>% 
        arrange(KTotal) %>% 
        mutate(alpha = if_else(row_number() == r, "highlight", "normal"))
    } else{
      d <- data()
    }
    
    d %<>% 
      select(Versicherung, Tarif, Franchise, KFranchise, KSelbstbeh, KPraemie, KTotal, alpha) %>%
      gather(Kosten, Wert, -Versicherung, -Tarif, -Franchise, -KTotal, -alpha)
    
    d

  })
  
  output$graphly <- renderPlotly({
    p <- plotdata() %>%
      ggplot(aes(x = reorder(paste0(Versicherung, Tarif, Franchise), KTotal),
                 y = Wert, fill = Kosten, text = paste0(Versicherung, ", ", Tarif, ", F: ", Franchise))) +
      # scale_alpha_manual(values = c(1, 0.4), guide = "none") +
      # scale_alpha_discrete(guide = "none") +
      geom_col() +
      theme(legend.position = "bottom",
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank())
    
    r <- input$table_rows_selected
    if (length(r)) {
      p <- p +
        geom_col(data = plotdata() %>% filter(alpha == "highlight"), fill = "yellow")
    }
    
    p %>% 
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.4, y = 0)) %>% 
      config(displayModeBar = F)
  
  })
  
  observe({
    d <- event_data("plotly_click")
    if (is.null(d)) print("Click events appear here (double-click to clear)") else print(d$x)
  })
  
  plotly_x <- reactive({
    event_data("plotly_click")$x
  })
  
  
  output$table <- renderDataTable({
    datatable(data() %>% 
                select(VersLink, Tariftyp, Franchise, Prämie, KFranchise, KSelbstbeh, KTotal) %>% 
                arrange(KTotal),
              # options = l
              rownames = F, escape = F, selection = "single", 
              options=list(stateSave = TRUE))
        
  })
  
  tableproxy = dataTableProxy("table")
  
  observeEvent(event_data("plotly_click"), {
    tableproxy %>% 
      selectRows(as.numeric(plotly_x())) %>% 
      selectPage(which(input$table_rows_all == plotly_x()) %/% input$table_state$length + 1)
  })
  
  observe({
     r <- input$table_rows_selected

     print(r)
  })
}

shinyApp(ui, server)