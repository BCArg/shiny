library(shiny)
shinyUI(pageWithSidebar(
          headerPanel("Theoreme central limite GIT"), 
          sidebarPanel(
            selectInput("dist", "Distribution theorique:",choices = c ("Normale" = "DN", "Uniforme" = "DU", "Chi-carree" = "DC","Fisher" = "DF","Exponentielle" = "DE","Gamma" = "DG")),
            
            conditionalPanel(condition = "input.dist == 'DN'", 
              sliderInput("mean", "Moyenne :", min = 5, max = 15, value = 10, step = 0.1), 
              sliderInput("sd", "Ecart-type : ", min = 0.5, max = 3.5, value = 2, step = 0.1)),
            conditionalPanel(condition = "input.dist == 'DU'", 
              sliderInput("a", "Limite inferieure : ", min = 0, max = 10, value = 5, step = 0.1), 
              sliderInput("b", "Limite superieure : ", min = 10, max = 20, value = 15, step = 0.1)),
            conditionalPanel(condition = "input.dist == 'DC'", 
              sliderInput("df", "Degres de liberte : ", min = 1, max = 20, value = 5, step = 1)),
            conditionalPanel(condition = "input.dist == 'DF'", 
              sliderInput("df1", "DDL Numerateur : ", min = 1, max = 50, value = 5, step = 1), 
              sliderInput("df2", "DDL Denominateur : ", min = 1, max = 100, value = 20, step = 1)),
            conditionalPanel(condition = "input.dist == 'DE'", 
              sliderInput("rate", "Lambda : ", min = 0.5, max = 3.5, value = 2, step = 0.1)),
            conditionalPanel(condition = "input.dist == 'DG'", 
              sliderInput("rate2", "parametre 1 : ", min = 0.5, max = 10 , value = 0.5, step = 0.1),
              sliderInput("scale", "parametre 2 : ", min = 0.1, max = 2, value = 2, step = 0.1)),  
                                
            sliderInput("n","n : nombre d'individus par echantillon :",min = 2,max = 25,value =5, step=1),
            actionButton("takeech","Echantillonner"),
            actionButton("reset","Reset")
            ), 
          
          mainPanel(plotOutput("distPlot",height = "auto") 
                    ,plotOutput("histPlot", height = "auto")
                    )
                       ))