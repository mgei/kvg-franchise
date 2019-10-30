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
    # uiOutput("inputcomp"),
    sliderInput("kkosten", label = "Erwartete Krankheitskosten", min = 0, max = 10000, value = 1000, step = 10)
  ),
  dashboardBody(
    # fluidRow(
    #   box(
    #     plotOutput("graph"),
    #     width = 12
    #   )
    # ),
    fluidRow(
      box(
        # plotlyOutput("graphly"),
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
      # autocomplete_input("plz", "PLZ Ort", max_options = 100,
      #                    paste0(1:9, " Dorf"))
                         # filter(lut_hmo, KTKZ == input$kanton, !is.na(PLZ4)) %>% .[["PlzGemeinde"]])
      textInput("hans", "Dieter")
    )
  })
  
  # output$inputcomp <- renderUI({
  #   disabled(
  #     autocomplete_input("element", "PLZ Ort", max_options = 100,
  #                        paste0(1:9, " Dorf"))
  #   )
  # })
  
  data <- reactive({
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
      group_by(KKosten) %>% 
      mutate(minFranchise = if_else(KTotal == min(KTotal), "min", "z"),
             minFranchise = if_else(KTotal == max(KTotal), "max", minFranchise)) %>% 
      ungroup()
  })
  
  # output$graph <- renderPlot({
  #   data() %>% 
  #     select(Versicherung, Tarif, Franchise, KFranchise, KSelbstbeh, KPraemie, KTotal) %>% 
  #     gather(Kosten, Wert, -Versicherung, -Tarif, -Franchise, -KTotal) %>% 
  #     ggplot(aes(x = reorder(paste0(Versicherung, Tarif, Franchise), KTotal), y = Wert, fill = Kosten)) +
  #     geom_col()
  # })
  
  output$graphly <- renderPlotly({
    p <- data() %>% 
      select(Versicherung, Tarif, Franchise, KFranchise, KSelbstbeh, KPraemie, KTotal) %>% 
      gather(Kosten, Wert, -Versicherung, -Tarif, -Franchise, -KTotal) %>% 
      ggplot(aes(x = reorder(paste0(Versicherung, Tarif, Franchise), KTotal), 
                 y = Wert, fill = Kosten, text = paste0(Versicherung, ", ", Tarif, ", F: ", Franchise))) +
      geom_col() +
      theme(legend.position = "bottom",
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank())
    
    p %>% 
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.4, y = 0)) %>% 
      config(displayModeBar = F)
  
  })
  
  # observe({
  #   print(altersklasse(input$alter))
  #   print(unfalleinschluss(input$unfall))
  #   print(input$kanton)
  #   print(tariftyp(input$modell))
  # })
  # 
  # observe({
  #   print(data() %>% 
  #           select(Versicherung, Tarif, Franchise, KFranchise, KSelbstbeh, KPraemie, KTotal) %>% 
  #           gather(Kosten, Wert, -Versicherung, -Tarif, -Franchise, -KTotal) %>% 
  #           filter(KTotal == max(KTotal)))
  # })
}

shinyApp(ui, server)