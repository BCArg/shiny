library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Régression linéaire : violation de l'hypothèse d'homoscédasticité"), 
  sidebarPanel(
       
    conditionalPanel(condition = "input.Tabset==1",     
    h5("Paramètres de la RLS")
    ,sliderInput("intercept", "Beta0", min = 0, max = 10, value = 0)
    ,sliderInput("beta1", "Beta1", min = 0, max = 10, value = 0)
    
       
    ,h5("Variance du terme d'erreur: ")
    ,sliderInput("alpha0",  "Terme constant" , min = 0, max = 5, value = 2, step = 0.1)
    ,sliderInput("alpha1", "Terme dépendant de X", min = 0, max = 5, value = 0, step = 0.01)
    
      
    ,actionButton("takeY","Générer des données")
    ,actionButton("reset","Reset")
   
                        
    ,checkboxInput("Coef",HTML("Coeffictients estimés par RLS"),FALSE) 
    ,checkboxInput("Test",HTML("Tests : hypothèse d'homoscédasticité"),FALSE)        
                     
   ),
  
  conditionalPanel(condition = "input.Tabset==2"
    ,fileInput('file1', 'Choose CSV File',
               accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                   
    ,checkboxInput("Coef",HTML("Coeffictients estimés par RLS"),FALSE) 
    ,checkboxInput("Test",HTML("Tests : hypothèse d'homoscédasticité"),FALSE)        
  )
    
  ),
  
  mainPanel(
        
    tabsetPanel(id="Tabset",selected=1,
      tabPanel(
        "Violation de l'hypothèse",
        plotOutput("doublePlot1", height = "auto"),
        tableOutput("Coef1"),
        tableOutput("Test1"),         
        value = 1), 
      tabPanel(
        "Remèdes", 
        tableOutput('contents'),
        plotOutput("doublePlot2", height = "auto"),
        tableOutput("Coef2"),
        tableOutput("Test2"), 
        value = 2)
          
      )
           )  
))