
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
                              condition = "input.tabselected == '2'",
                              selectInput('select', 'Browse Different Neighborhood', choices = c( "Brookside","Edwards","North Ames")),
                              
                            ))

# Body ----
body <- dashboardBody(
  mainPanel(
    tabsetPanel(
      id = "tabselected", # Add ID here
      tabPanel("Analysis 1", value = "2",
               conditionalPanel(
                 condition = "input.select == 'Brookside'",
                 h4("Brookside"), 
                 plotOutput("BrkSidePlot")
               ),
               conditionalPanel(
                 condition = "input.select == 'Edwards'",
                 h4("Edwards"), 
                 plotOutput("EdwardsPlot")
               ),
               conditionalPanel(
                 condition = "input.select == 'North Ames'",
                 h4("North Ames"), 
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
