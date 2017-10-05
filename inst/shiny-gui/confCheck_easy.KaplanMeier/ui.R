
# Prende il nome dell'oggetto confCheck_easy che mi ha invocato
parent.ID <- pMineR.IO.shiny.confCheck_easy.list$parent.ID
nomeOggetto <- register.getObjName(ID = parent.ID)

# e la lista degli stati del grafo
stringa.comando <- str_c( "array.stati<-",nomeOggetto,"$getInfo()$states")
eval(expr = parse(text = stringa.comando))

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("confCheck_easy::Kaplan-Meier"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput("stato.from.1", "1sr State From:",c(" ",array.stati)) ,
    selectInput("stato.to.1", "1sr State To:",c(" ",array.stati)),    

    selectInput("stato.from.2", "2nd State From:",c(" ",array.stati)),
    selectInput("stato.to.2", "2nd State To:",c(" ",array.stati)),

    selectInput("stato.from.3", "3nd State From:",c(" ",array.stati)),
    selectInput("stato.to.3", "3nd State To:",c(" ",array.stati)),
    tags$hr(),
    actionButton( "quitButton","Quit!",width = 200,style="color: #fff; background-color: #e00f0f; border-color: #d00e0e; ")
  ),

  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Graph", grVizOutput('diagram', width = "100%", height = "760px")  ),
      tabPanel("Kaplan-Meier", verbatimTextOutput("Kaplan"),plotOutput("Kaplan.plot") )
    )
  )
))
