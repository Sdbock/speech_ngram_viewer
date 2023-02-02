
library(shiny)
library(shiny.info)

source("global.R")

# Define UI for application that draws a histogram
ui <- 
navbarPage(
  title = "U.S. Presidential Speech Ngram Viewer",
  header = display(p(style = "margin: 0;", "Created by ", a(href = "https://www.seanbock.com", 
                                                   target = "_blank", "Sean Bock")), type = "powered_by"),
  theme = my_theme,
  tabPanel("Ngram Viewer",
          fluidPage(
              selectizeInput('terms',
                             label = NULL, 
                             choices = NULL, 
                             multiple = TRUE),
              shinyWidgets::chooseSliderSkin("Modern",
                                             color = "#2a4d69"),
              inputPanel(
                theme = my_theme,
                shinyWidgets::prettyCheckbox("byparty","Trends by party", 
                                             value = TRUE,
                                             shape = "round",
                                             fill = TRUE,
                                             animation = "pulse",
                                             status = "primary",
                                             thick = TRUE,
                                             icon = icon("check")),
                shinyWidgets::prettyCheckbox("facet_party","Separate by party", 
                                             value = TRUE,
                                             shape = "round",
                                             fill = TRUE,
                                             animation = "pulse",
                                             status = "primary",
                                             thick = TRUE,
                                             icon = icon("check")),
                shinyWidgets::prettyCheckbox("facet_ngram","Separate by ngram", 
                                             value = TRUE,
                                             shape = "round",
                                             fill = TRUE,
                                             animation = "pulse",
                                             status = "primary",
                                             thick = TRUE,
                                             icon = icon("check")),
                sliderInput("years",NULL,min = 1952, max = 2020, value = c(1952,2020), step = 4, round = FALSE, sep = ""),
              ),
              mainPanel(
                   tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }", # suppress error for facet variables not loaded
                           ".shiny-output-error:before { visibility: hidden; }"),
                   plotOutput("plot",
                              width = "100%"),
                )
  )),
  tabPanel("About",
           includeMarkdown("about.md"))
           
    
  )
    



server <- function(input, output, session) {
  

      updateSelectizeInput(session, "terms", label = NULL, choices = choices$word, server = TRUE, selected = c("economy","the people"))
      output$plot <- renderPlot({
      words <- input$terms 
      plot_ngrams(terms = words, 
                  byparty = input$byparty, 
                  facet_ngram = input$facet_ngram, 
                  facet_party = input$facet_party, 
                  years = input$years
                      )

    })
      

}

# Run the application 
shinyApp(ui = ui, server = server)


