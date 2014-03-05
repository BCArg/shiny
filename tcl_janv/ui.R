library(shiny)
shinyUI(pageWithSidebar(
          headerPanel("Theoreme central limite"), 
          sidebarPanel(
            selectInput("dist", "Distribution theorique:",choices = c ("Normale" = "DN","Uniforme" = "DU","Exponentielle" = "DE")),
            
          #  ,"Chi-carree" = "DC","Fisher" = "DF","Gamma" = "DG"
            
            conditionalPanel(condition = "input.dist == 'DN'", 
              sliderInput("mx", "Moyenne :", min = 5, max = 15, value = 10, step = 0.1), 
              sliderInput("sx", "Ecart-type : ", min = 0.5, max = 3.5, value = 2, step = 0.1)),
           conditionalPanel(condition = "input.dist == 'DU'", 
            #  sliderInput("a", "Limite inferieure : ", min = 0, max = 10, value = 5, step = 0.1), 
              sliderInput("b", "Limite superieure : ", min = 1, max = 20, value = 10, step = 0.1)),
          #  conditionalPanel(condition = "input.dist == 'DC'", 
            #  sliderInput("df", "Degres de liberte : ", min = 1, max = 20, value = 5, step = 1)),
          #  conditionalPanel(condition = "input.dist == 'DF'", 
           #   sliderInput("df1", "DDL Numerateur : ", min = 1, max = 50, value = 5, step = 1), 
            #  sliderInput("df2", "DDL Denominateur : ", min = 1, max = 100, value = 20, step = 1)),
            conditionalPanel(condition = "input.dist == 'DE'", 
              sliderInput("Lambda", "Lambda : ", min = 0, max = 10, value = 2, step = 0.1)),
         #   conditionalPanel(condition = "input.dist == 'DG'", 
          #    sliderInput("Alpha", "parametre 1 : ", min = 0.5, max = 10 , value = 0.5, step = 0.1),
           #   sliderInput("Beta", "parametre 2 : ", min = 0.1, max = 2, value = 2, step = 0.1)),  
                                
            actionButton("takesample","Echantillonner"),
            sliderInput("n","n : nombre d'individus par echantillon :",min = 5,max = 200,value =5, step=5),
            sliderInput("ns", "Nombre d'echantillons preleves en une fois : ", min = 1, max = 50, value = 1),
            actionButton("reset","Reset"),
          
            checkboxInput("showNdensity", "Afficher la distribution normale pour l'echantillon", FALSE)
            
            
            ), 
          
          mainPanel(plotOutput("doublePlot", height = "auto")
                    )
                       ))