library(pMineR)
library(shiny)
library(shinythemes)

#' server.test fft545t4
#' 
#' @description  implement a simple markov Model
#' @useDynLib shiny    
#' @export
server.test<- function(input, output, session) {
  observeEvent(input$button.test_01,{
    updateTextInput(session, inputId = "myresults", value = "prova") 
  })


}

#' client.test brff
#' 
#' @description  implement a simple markov Model
#' @useDynLib shiny    
#' @export
client.test <- fluidPage(theme=shinytheme("flatly"),
  
  titlePanel("Use an existing theme"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Note the button is black. This is different from the previous app."),
      tabsetPanel(
        tabPanel("simple",
          actionButton("button.test_01", "test_01")
        ),
        tabPanel("advanced",
          actionButton("button.test_02", "test_02")
        )
      )
    ), 
    
    mainPanel(
      tabsetPanel(
        tabPanel("Description",
                 textInput("mytext", "Input goes here"),
                 textInput("myresults", "Results will be printed here", "Initial value")
                 
        ), 
        tabPanel("Code"),         
        tabPanel("Data"), 
        tabPanel("Plot")
      )
    )
  )
)
server <- function(input, output, session) {
  
  output$my_output_text <- renderText({
    init <- "Your value is: "
    return(paste0(init, input$mytext))
  })
  
  # send plot to the ui as my_output_plot
  output$my_output_plot <- renderPlot({
    plot(1:10, 1:10, pch=16, col=1:10, cex=1:10, main = input$mytext)
  })
  
  
}

ui <- basicPage(
  
  h3("Now we have both text and plot output"),
  textInput("mytext", "Input goes here"),
  
  # my_output_text comes from the server
  textOutput("my_output_text"),
  # my_output_plot comes from the server
  plotOutput("my_output_plot")
  
)

#shinyApp(ui = ui, server = server)
#shinyApp(ui = client.test, server = server.test)