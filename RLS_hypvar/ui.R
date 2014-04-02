library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Régression linéaire : violation de l'hypothèse d'homoscédasticité"), 
  sidebarPanel(
   # sliderInput("n", "n", min = 50, max =400, value = 100, step = 5)
  
    conditionalPanel(condition = "input.Tabset==1",     
    h5("Paramètres de la RLS")
    ,sliderInput("intercept", "Beta0", min = 0, max = 10, value = 0)
    ,sliderInput("beta1", "Beta1", min = 0, max = 10, value = 0)
    
    
    
    ,h5("Variance du terme d'erreur: ")
    ,sliderInput("alpha0",  "Terme constant" , min = 0, max = 5, value = 2, step = 0.1)
       ,sliderInput("alpha1", "Terme dépendant de X", min = 0, max = 5, value = 0, step = 0.01)
    
        
    ,actionButton("takeY","Générer des données")
    ,actionButton("reset","Reset")
   
    
    ,h5("Tests : hypothèse d'homoscédasticité")
    ,checkboxInput("Test",HTML("Breusch-Pagan & White"),FALSE)
    
   ),
  
  conditionalPanel(condition = "input.Tabset==2"
                   
                   # ,h5("RemÃ¨des: ")
                   #  ,checkboxInput("log",HTML("Transformation log"),FALSE)
  )
    
  ),
  
  mainPanel(
        
    tabsetPanel(id="Tabset",selected=1,
      tabPanel(
        "Violation de l'hypothèse",
        plotOutput("doublePlot1", height = "auto"),
        tableOutput("Test1"), 
        value = 1), 
      tabPanel(
        "Remèdes", 
        value = 2)
          
      )
           )  
))