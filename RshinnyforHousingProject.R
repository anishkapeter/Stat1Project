
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

data=read.csv("https://raw.githubusercontent.com/anishkapeter/Stat1Project/main/train.csv")
filtered_neighborhood = data %>% filter (Neighborhood %in% c('NAmes', 'Edwards', 'BrkSide'))
data3=data
data3$SalePrice = log(data$SalePrice)
data3$GrLivArea = log(data$GrLivArea)
filtered_data2 <- data3 %>% filter(Neighborhood %in% c('NAmes', 'Edwards', 'BrkSide'))


header <- dashboardHeader(title = "Housing Price Prediction")


sidebar <- dashboardSidebar(width = 250,
                            conditionalPanel(
                              condition = "input.tabselected == '1'",
                              selectInput("model", "Select a model",
                                          choices = c("Forward Selection","Backward Selection","Side-Step Selection","Custom")),
                              
                            ),
                            
                            conditionalPanel(
                              condition = "input.tabselected == '2'",
                              selectInput('select', 'Browse Different Neighborhood', choices = c( "BrkSide","Edwards","NAmes")),
                              
                            ))

# Body ----
body <- dashboardBody(
  mainPanel(
    tabsetPanel(
      id = "tabselected", # Add ID here
      tabPanel("Question1", value = "1", h4("Exploring different models"), plotOutput("wordcloud")),
      tabPanel("Question2", value = "2",
               conditionalPanel(
                 condition = "input.select == 'BrkSide'",
                 h4("BrkSide"), 
                 plotOutput("BrkSidePlot")
               ),
               conditionalPanel(
                 condition = "input.select == 'Edwards'",
                 h4("Edwards"), 
                 plotOutput("EdwardsPlot")
               ),
               conditionalPanel(
                 condition = "input.select == 'NAmes'",
                 h4("NAmes"), 
                 plotOutput("NAmesPlot")
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
  output$BrkSidePlot <- renderPlot({
    filtered_neighborhood %>% 
      filter(Neighborhood == "BrkSide") %>% 
      ggplot(aes(x=GrLivArea, y = SalePrice)) + 
      geom_point( color = "steelblue") + 
      ggtitle("Sale Price vs Living Area Sq.Ft in Brookside") +
      xlab("Square Footage of Living Area") +
      ylab("Sales Price in Dollars")
  })
  output$EdwardsPlot <- renderPlot({
    filtered_neighborhood %>% 
      filter(Neighborhood == "Edwards") %>% 
      ggplot(aes(x=GrLivArea, y = SalePrice)) + 
      geom_point( color = "steelblue") + 
      ggtitle("Sale Price vs Living Area Sq.Ft in Edwards") +
      xlab("Square Footage of Living Area") +
      ylab("Sales Price in Dollars")
  })
  output$NAmesPlot <- renderPlot({
    filtered_neighborhood %>% 
      filter(Neighborhood == "NAmes") %>% 
      ggplot(aes(x=GrLivArea, y = SalePrice)) + 
      geom_point(color = "steelblue") + 
      ggtitle("Sale Price vs Living Area Sq.Ft in North Ames") +
      xlab("Square Footage of Living Area") +
      ylab("Sales Price in Dollars")
  })

}

# Run the app
shinyApp(ui = ui, server = server)
