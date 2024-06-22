# Exercise Sheet 5
# Authors: Pat Callahan (Matrikelnummer: 12672775)

# Exercise 0: Understanding the App

# The user interface (UI) is, in general, the user-facing portion of an application or website and presents to the user data, images, information, and, as needed, allows for user interactivity. The UI for this app has a general layout defined by the fluidPage function, which includes a titlePanel and a tabsetPanel. The titlePanel displays the title of the app, while the tabsetPanel contains two tabs: one for the scatter plot and one for the solution. The scatter plot tab has a plotOutput function displaying a scatter plot of the data, and the solution tab has a verbatimTextOutput function printing the correlation coefficient of the data. Note that the first argument for both of these functions is a name derived from the output$* object in the server function.

# The server is the "backend" of the application in which content for the page is held/generated, input from the user is processed, and updated output for the UI is generated. In this app, the server function runs the generate_correlated_data function on start-up to create a data frame of correlated data. The render*() functions are then used to generate the output objects, output$scatterPlot and output$correlationOutput, which are passed on the UI for display.

# The generate_correlated_data function creates a data frame of correlated data based on two arguments, a chosen numeric correlation value and the number of observations n. The correlation argument is presumably a value between -1 and 1, and n is any real number greater than 0. The function generates two random uniform variables x1 and x2 with the runif() function, and then creates a third variable x_correlated that is a linear combination of x1 and x2. The function returns a data frame with x1 and x_correlated.



library(shiny)
library(ggplot2)
library(dplyr)

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
  titlePanel("Pat's Shiny App"),
  textOutput("name_output"),
  textInput("name_entry", "Your Name"),

  #
  tabsetPanel(
    #
    tabPanel(
      "Scatter Plot",
      numericInput("correlation_guess", "Your Correlation Guess", value = 0, min = -1, max = 1),
      plotOutput("scatterPlot")
    ),

    #
    tabPanel(
      "Solution",
      verbatimTextOutput("correlationOutput"),
      verbatimTextOutput("correlation_difference"),
      textOutput("correlation_diff_text"),
      textOutput("correlation_interpretation")
    ),
    tabPanel(
      "Sandbox",
      sliderInput("corr_modifier", "Correlation Modifier",
        min = -1, max = 1, value = 0, step = 0.001
      ),
      numericInput("observations_adjuster", "Number of Observations",
        value = 100, min = 1, max = Inf
      ),
      actionButton("update_correlation", "Update Correlation")
    )
  )
)

# Define server
server <- function(input, output, session) {
  #
  observe({
    updateSliderInput(session, "corr_modifier", value = runif(1, -1, 1))
  })

  df <- eventReactive(input$update_correlation,
    {
      generate_correlated_data(
        correlation = input$corr_modifier,
        n = input$observations_adjuster
      )
    },
    ignoreNULL = FALSE
  )

  cor_empirical <- reactive({
    cor(df()$x1, df()$x2)
  })

  output$name_output <- renderText({
    paste0("Currently Playing: ", input$name_entry)
  })
  #
  output$scatterPlot <- renderPlot({
    ggplot(df(), aes(x = x1, y = x2)) +
      geom_point() +
      labs(x = "x1", y = "x2") +
      theme_minimal()
  })

  #
  output$correlationOutput <- renderPrint({
    cor_empirical()
  })

  output$correlation_interpretation <- renderPrint({
    case_when(
      cor_empirical() <= -0.7 ~ "Strong negative correlation",
      cor_empirical() <= 0.3 & cor_empirical() > -0.7 ~ "Moderate negative correlation",
      cor_empirical() < 0L & cor_empirical() > -0.3 ~ "Weak negative correlation",
      cor_empirical() >= 0L & cor_empirical() < 0.3 ~ "Weak positive correlation",
      cor_empirical() >= 0.3 & cor_empirical() < 0.7 ~ "Moderate positive correlation",
      cor_empirical() >= 0.7 ~ "Strong positive correlation"
    )
  })

  correlation_difference <- reactive({
    input$correlation_guess - cor_empirical()
  })
  output$correlation_difference <- renderPrint({
    correlation_difference()
  })

  output$correlation_diff_text <- renderText({
    if (abs(correlation_difference()) < 0.1) {
      "Great guess!"
    } else {
      "Unfortunately, not a great guess..."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# my additions:
# included the session argument which normally should be included in the server, and allowed this to be passed on to the updatedSliderInput function so that the slider input on start up is updated to a random value between -1 and 1, and then the user can also adjust this manually on the slider

# I also updated some of the functions/code to be reactives to reduce duplication of code. For example, some people may have typed code twice to calculate the difference between the correlation guess and the actual correlation when creating the basic numeric output and when writing code for generating the more informative text output e.g. "Great Guess!" or not.

# to do: create an action button for the correlation adjustment because otherwise many computations will be performed as someone moves the slider around and makes temporary adjustments. This is too much reactivity, and we want to observe the action of an input button instead.
