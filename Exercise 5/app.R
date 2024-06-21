library(shiny)
library(ggplot2)

# Function to generate correlated data
generate_correlated_data <- function(correlation, n) {
  alpha <- correlation
  beta <- sqrt(1 - correlation^2)

  x1 <- runif(n)
  x2 <- runif(n)

  x_correlated <- alpha * x1 + beta * x2

  return(data.frame(x1, x2 = x_correlated))
}

# Define UI
ui <- fluidPage(
  titlePanel("Correlation Game by Jan & Jacob"),

  # 
  tabsetPanel(
    # 
    tabPanel(
      "Scatter Plot",
      plotOutput("scatterPlot")
    ),

    # 
    tabPanel(
      "Solution",
      verbatimTextOutput("correlationOutput")
    )
  )
)

# Define server
server <- function(input, output) {
  # 
  correlation <- runif(1, -1, 1)
  df <- generate_correlated_data(correlation = correlation, n = 100)

  # 
  output$scatterPlot <- renderPlot({
    ggplot(df, aes(x = x1, y = x2)) +
      geom_point() +
      labs(x = "x1", y = "x2") +
      theme_minimal()
  })

  # 
  output$correlationOutput <- renderPrint({
    cor(df$x1, df$x2)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
