source("setup.R")

kantone <- readRDS("data/2021/kantone.RDS")

premia <- readRDS("data/2021/premia.RDS")

versicherer_modell <- premia %>% 
  select(Versicherer, Versicherung, Tariftyp, Tarifbezeichnung) %>% 
  distinct() %>% 
  mutate(VersMod = paste(Versicherung, Tarifbezeichnung, sep = ", ")) %>% 
  arrange(Versicherung, Tarifbezeichnung != "Grundversicherung")

franchisen <- premia %>% 
  filter(Altersklasse == "AKL-ERW") %>% 
  select(Franchise) %>% 
  distinct() %>% 
  mutate(F = Franchise %>% str_remove("FRA-") %>% as.integer()) %>% 
  pull()

ui <- fluidPage(
  useShinydashboard(),
  useShinyjs(),
  extendShinyjs(text = jscode),
  title = "Hello, Shiny!",
  fluidRow(
    br(),
    column(12,
           column(3),
           box(id = "menubox", title = "hello", solidHeader = T, status = "danger", collapsible = T,
               column(6, 
                      numericInput(inputId = "estimated_cost", label = "Erwartete Krankheitskosten", value = 1000, min = 0, max = 1000000)),
               column(6,
                      pickerInput(inputId = "canton", label = "Wohnkanton", 
                                  choices = names(kantone),
                                  choicesOpt = list(subtext = kantone),
                                  selected = "ZH"
                      ),
                      pickerInput(inputId = "agegroup", label = "Altersklasse", 
                                  choices = c("0-18" = "AKL-KIN", "19-25" = "AKL-JUG", "26+" = "AKL-ERW"),
                                  choicesOpt = list(subtext = c("Kinder", "Jugendliche", "Erwachsene")),
                                  selected = "26+"
                      ),
                      hr(),
                      # helpText("optional:"),
                      pickerInput(inputId = "insurer", label = "Versicherer/Modell", 
                                  choices = c("keine Angabe", versicherer_modell$VersMod),
                                  options = list(title = "optional")
                      ),
                      pickerInput(inputId = "franchise", label = "Bisherige Franchise",
                                  choices = c("keine Angabe", franchisen),
                                  options = list(title = "optional")),
                      switchInput(inputId = "accident",
                                  label = "Unfall", 
                                  onLabel = "mit",
                                  offLabel = "ohne",
                                  labelWidth = "40px"
                      )
                      ), 
               footer = tags$div(style = "text-align-last: right;",
                                 actionButton("go", "Berechnen"))),
           column(3))
  ),
  fluidRow(
    br(),
    column(width = 6),
    column(width = 6)
  )
)

server <- function(input, output, session) {
  observeEvent(input$go, {
    js$collapseBox("menubox")
  })
  
  observe({
    req(input$estimated_cost)
    req(input$canton)
    req(input$agegroup)
    req(input$accident)
    
    cost_insurance <- premia %>% 
      filter(Kanton == input$canton,
             Altersklasse == input$agegroup,
             Unfalleinschluss == c("OHN-UNF", "MIT-UNF")[input$accident + 1])
    
    
    if (!is.null(input$insurer)) {
      if (input$insurer != "keine Angabe") {
        insurer_model <- split_string(input$insurer, ", ")
        
        cost_insurance <- cost_insurance %>% 
          filter(Versicherung == insurer_model[1],
                 Tarifbezeichnung == insurer_model[2])
      }
    }
    
    print(cost_insurance)
  })
  
  franchise_cost_plot <- renderPlot({
    # req(input$estimated_cost)
    # req(input$canton)
    # req(input$agegroup)
    # req(input$accident)
    # 
    # cost_insurance <- premia %>% 
    #   filter(Kanton == input$canton,
    #          Altersklasse == input$agegroup,
    #          Unfalleinschluss == c("OHN-UNF", "MIT-UNF")[input$accident + 1])
    # 
    # 
    # if (!is.null(input$insurer)) {
    #   if (input$insurer != "keine Angabe") {
    #     insurer_model <- split_string(input$insurer, ", ")
    #     
    #     cost_insurance <- cost_insurance %>% 
    #       filter(Versicherung == insurer_model[1],
    #              Tarifbezeichnung == insurer_model[2])
    #   }
    # }
    # 
    # print(cost_insurance)
      
  })
}

shinyApp(ui, server)