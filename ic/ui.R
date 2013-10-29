library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Intervalles de confiances"),
  
  sidebarPanel(
    sliderInput("mx","µ : Moyenne de la population : ",min = 1,max = 100,value = 50, step=0.5),
    sliderInput("sx","σ : écart-type de la population : ",min = 1,max = 25,value = 10, step=1),
    sliderInput("n","n : nombre d'individus par échantillon :",min = 1,max = 25,value = 4, step=1),
    sliderInput("confidence","Confiance (1-α) :",min = 0.5,max = 1,value = 0.95, step=0.005),
    selectInput("typeic", "Type d'intervalle de confiance :",
                 list("σ² connue" = "varknown",
                      "σ² inconnue" = "varunknown")),
                      br(),
    actionButton("takeech","Echantillonner et tester"),
    actionButton("reset","Reset")
  ),
  
  mainPanel(
        plotOutput("plotReality",height = "auto"),
        plotOutput("plotSample",height = "auto")
  )
)) 
