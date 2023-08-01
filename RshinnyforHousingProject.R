
library(shiny)
library(shinydashboard)



header <- dashboardHeader(title = "Housing Price Prediction")


sidebar <- dashboardSidebar(width = 250,
                            conditionalPanel(
                              condition = "input.tabselected == '1'",
                              selectInput("model", "Select a model",
                                          choices = c("Forward Selection","Backward Selection","Side-Step Selection","Custom")),
                              
                            ),
                            
                            conditionalPanel(
                              condition = "input.tabselected == '2'",
                              selectInput('select', 'Browse Didfferent Neighborhood', choices = c( "Neighborhood1","Neighborhood2","Neighborhood3")),
                              
                            ))

# Body ----
body <- dashboardBody(
  mainPanel(
    tabsetPanel(
      id = "tabselected", # Add ID here
      tabPanel("Question1", value = "1", h4("Exploring different models"), plotOutput("wordcloud")),
      tabPanel("Question2", value = "2",
               conditionalPanel(
                 condition = "input.select == 'Neighborhood1'",
                 h4("Neighborhood1"), 
                 plotOutput("Time_Series2")
               ),
               conditionalPanel(
                 condition = "input.select == 'Neighborhood2'",
                 h4("Neighborhood2"), 
                 plotOutput("barPlot")
               ),
               conditionalPanel(
                 condition = "input.select == 'Neighborhood3'",
                 h4("Neighborhood3"), 
                 plotOutput("Time_Series")
               )
      )
    )
  )
)


# UI ----
ui <- dashboardPage(header, sidebar, body, skin = "black")

# Server ----
server <- function(input, output) {
  # Here should be the logic of your application
}

# Run the app
shinyApp(ui = ui, server = server)
