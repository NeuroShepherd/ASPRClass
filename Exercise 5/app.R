# Exercise Sheet 5
# Authors: Pat Callahan (Matrikelnummer: 12672775)

# Exercise 0: Understanding the App

# The user interface (UI) is, in general, the user-facing portion of an application or website and presents to the user data, images, information, and, as needed, allows for user interactivity. The UI for this app has a general layout defined by the fluidPage function, which includes a titlePanel and a tabsetPanel. The titlePanel displays the title of the app, while the tabsetPanel contains two tabs: one for the scatter plot and one for the solution. The scatter plot tab has a plotOutput function displaying a scatter plot of the data, and the solution tab has a verbatimTextOutput function printing the correlation coefficient of the data. Note that the first argument for both of these functions is a name derived from the output$* object in the server function.

# The server is the "backend" of the application in which content for the page is held/generated, input from the user is processed, and updated output for the UI is generated. In this app, the server function runs the generate_correlated_data function on start-up to create a data frame of correlated data. The render*() functions are then used to generate the output objects, output$scatterPlot and output$correlationOutput, which are passed on the UI for display.

# The generate_correlated_data function creates a data frame of correlated data based on two arguments, a chosen numeric correlation value and the number of observations n. The correlation argument is presumably a value between -1 and 1, and n is any real number greater than 0. The function generates two random uniform variables x1 and x2 with the runif() function, and then creates a third variable x_correlated that is a linear combination of x1 and x2. The function returns a data frame with x1 and x_correlated (relabeled as x2 in the data frame).



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
      br(),
      actionButton("show_function", "Show Generation Function"), br(), br(),
      sliderInput("corr_modifier", "Correlation Modifier",
        min = -1, max = 1, value = 0, step = 0.001
      ),
      numericInput("observations_adjuster", "Number of Observations",
        value = 100, min = 1, max = Inf
      ), br(),
      helpText("The app will update only after pressing the button below."),
      actionButton("update_correlation", "Update Correlation")
    )
  )
)

# Define server
server <- function(input, output, session) {

  correlation_input <- reactive({
    if (!req(input$corr_modifier)) {
      runif(1, -1, 1)
    } else {
      input$corr_modifier
    }
  })


  observe({
    updateSliderInput(session, "corr_modifier", value = correlation_input())
  })

  df <- eventReactive(input$update_correlation,
    {
      generate_correlated_data(
        correlation = correlation_input(),
        n = input$observations_adjuster
      )
    },
    ignoreNULL = FALSE
  )

  observeEvent(input$update_correlation, {
    showNotification("Data Updated!", duration = 5, type = "message")
  })

  cor_empirical <- reactive({
    cor(df()$x1, df()$x2)
  })

  output$name_output <- renderText({
    paste0("Currently Playing: ", input$name_entry)
  })
  #

  point_color <- eventReactive(input$update_correlation, {
    paste(sample(0:255,size=3,replace=TRUE),collapse=" ")
  }, ignoreNULL = FALSE)

  output$scatterPlot <- renderPlot({
    # browser()
    ggplot(df(), aes(x = x1, y = x2)) +
      geom_point(color = point_color(), size =2) +
      labs(x = "x1", y = "x2") +
      theme_minimal()
  })

  #
  output$correlationOutput <- renderPrint({
    paste0("The empirical correlation between the randomly generated variables x1 and x2 is: ",
           round(cor_empirical(), 5))
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
    paste0("You guessed a correlation of ", input$correlation_guess,
           " which is an absolute difference of ", abs(round(correlation_difference(), 5)),
           " from the correct value of ", round(cor_empirical(), 5))
  })

  output$correlation_diff_text <- renderText({
    if (abs(correlation_difference()) < 0.1) {
      "Great guess!"
    } else {
      "Unfortunately, not a great guess..."
    }
  })


  output$correlation_fxn <- renderPrint({
    generate_correlated_data
  })
  observeEvent(input$show_function, {
    showModal(
      modalDialog(
        title = "Internals of the Function for Generating Correlated Data",
        verbatimTextOutput("correlation_fxn")
      )
    )
  })


}

# Run the application
shinyApp(ui = ui, server = server)


# My additions:
# I included the `session` argument in the server function, allowed this to be passed on to the updateSliderInput function, and created a reactive element called `correlation_input`. The goal of this was to allow the value the slider takes on at start-up to be the same value passed to the generate_correlated_data function. Without this, the slider would always start at 0 (or another value I choose), but would never match the actual value used at start up which is a poor user experience.

# I also updated some of the functions/code to be reactives to reduce duplication of code. For example, some people may have typed code twice to calculate the difference between the correlation guess and the actual correlation when creating the basic numeric output and when writing code for generating the more informative text output e.g. "Great Guess!" or not. Instead, I created the correlation_difference reactive object to calculate the difference between the guess and the actual correlation, and then used this object in the renderPrint and renderText functions to generate the numeric and text outputs, respectively.

# I created an action button for explicitly executing the correlation adjustment rather than reactively recalculating the values every time there is an adjustment to either the correlation or number of observations. The reactive computation approach is computationally greedy; computations are executed every time a user moves the slider around and makes temporary adjustments. Instead, the Update Correlation button requires the user to be explicit that they have chosen their parameters, and uses eventReactive to execute only when the user presses the button.

# In addition to the Update Correlation button, I added a notification to the app that displays a message when the data is updated. This is done by using the showNotification function in the observeEvent function that triggers when the Update Correlation button is pressed. Likewise, the color of the points in the ggplot point plot will have their color changed to a random new color every time the button is pressed to provide visual feedback to the end-user that an update occurred. This is done by creating a reactive object point_color that generates an RGB code, and then using this object in the renderPlot function to change the color of the points.

# I added a Show Generation Function so that users can easily see the code for the function that generates the correlated data in case they want to see empirically how their input values yield the output data. This is done by creating a modal dialog that displays the code for the function when the button is pressed, and required the use of renderPrint for generating the text output, verbatimTextOutput in the server function, wrapped in the modalDialog function, setting the modal to be rendered with showModal, and triggering the modal dialog to open with observeEvent.

# I wrote more informative text ouptut in the Solution tab.
