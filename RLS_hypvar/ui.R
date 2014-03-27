library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Régression linéaire : violation de l'hypothèse d'homoscédasticité"), 
  sidebarPanel(
    sliderInput("n", "n", min = 50, max =400, value = 100, step = 5)
  #  , sliderInput("alpha0", "alpha0", min = 0, max = 10, value = 1, step = 0.1)
    
    
    ,h5("Variance conditonnelle à X: ")
    , sliderInput("alpha1", "alpha1", min = 0, max = 5, value = 0, step = 0.01)
  #  , sliderInput("intercept", "intercept", min = 0, max = 10, value = 0)
  # , sliderInput(" beta1", " beta1", min = 0, max = 10, value = 2)
    
    
    ,h5("Remèdes: ")
    ,checkboxInput("log",HTML("Transformation log"),FALSE)
    
    
    ), 
  
  mainPanel(
        
    tabsetPanel(
      tabPanel("Version1"
               ,plotOutput("doublePlot1", height = "auto")
               ,tableOutput("Test1") ), 
      tabPanel("Version2" 
               ,plotOutput("doublePlot2", height = "auto") 
               ,tableOutput("Test2"))
     
      
      )
           )  
))