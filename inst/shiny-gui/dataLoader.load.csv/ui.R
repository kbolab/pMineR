

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("dataLoader"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    textInput("separator", "Separator", value = ",",width = 50),
    tags$hr(),
    selectInput("IDColumnName", "ID Column Name:",
                c()),       
    selectInput("eventColumnName", "event Column Name:",
                c()),   
    selectInput("dateColumnName", "date Column Name:",
                c()),     
    textInput("formatoData", "Date Format", value = "%d/%m/%Y %H:%M:%S",width = 200),
    checkboxInput("badDateSuppressing", "Bad Date suppressing", TRUE),
    checkboxInput("UTF8ForceConversion", "Force UTF8 Conversion", FALSE),
    
    tags$style("#separator {height:25px;}"),
    tags$style("#formatoData {height:25px;}")
  ),

  # Show a plot of the generated distribution
  mainPanel(
    useShinyjs(),
    div(tableOutput("contents"), style = "font-size:80%"),
    h3(textOutput("txtMessageArea")),
    actionButton( "importButton","Import!",width = 200,style="color: #fff; background-color: #337ab7; border-color: #2e6da4; "),
    actionButton( "quitButton","Quit!",width = 200,style="color: #fff; background-color: #e00f0f; border-color: #d00e0e; ")
  )
))
