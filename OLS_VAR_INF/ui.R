library(shiny)


shinyUI(pageWithSidebar(
headerPanel(h3(HTML("Régression linéaire : conséquences de l'hétéroscédasticité pour l'inférence de &beta;<sub>1</sub>"))), 
sidebarPanel(
       
  actionButton("takesample",HTML("Générer des données"))
 ,actionButton("reset",HTML("Reset"))

,h5(HTML("Paramètres de l'échantillonnage"))
  ,sliderInput("n", HTML("Nombre d'observations par échantillon"), min = 20, max = 500, value = 100, step = 5)
  ,sliderInput("ns", HTML("Nombre d'échantillons prélevés par simulation"), min = 1, max = 50, value = 1, step = 1)
                                              

,h5(HTML("Paramètres du modèle : Y = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &epsilon;"))
  ,p(HTML("L'intercept &beta;<sub>0</sub> est fixé à 0"))
  ,radioButtons("beta1", HTML("Pente : &beta;<sub>1</sub>"), 
             c( "H0 : beta1 = 0"= "h0",
                "H1 : beta1 = 0.5" = "h1"))
#HTML("H<sub>0</sub> : &beta;<sub>1</sub> = 0")
#HTML("H<sub>1</sub> : &beta;<sub>1</sub> !=0")

  
  ,radioButtons("alpha1", HTML("Variance du terme d'erreur : Var(&epsilon;) = &sigma;<sup>2</sup> X<sup>&alpha;<sub>1</sub></sup>"), 
                c("Homoscédasticité : alpha1 = 0"= "homo",
                  "Hétéroscédasticité : alpha1 != 0" = "hetero"))
  
  ,conditionalPanel(condition = "input.alpha1 == 'hetero'"
                    ,HTML("&alpha;<sub>1</sub>")
                    ,sliderInput("V.alpha1","", min = 0.1, max = 10, value = 2, step = 0.1))
  ,p(HTML("Le terme constant &sigma;<sup>2</sup>  est fixé à 2"))
  
       
   ),
  

  
  mainPanel(
    conditionalPanel(
      condition= "input.alpha1 == 'homo'",
    plotOutput("doublePlot", height = "auto", width = "auto"),
    h5(textOutput("comments")),
    plotOutput("barPlot", height = "auto", width = "auto") 
      )
    
    ,conditionalPanel(
      condition = "input.alpha1 == 'hetero'",
      plotOutput("XY", height = "auto", width = "auto"),
      h5(textOutput("classique")),
      plotOutput("OLS", height = "auto", width = "auto"), 
      h5(textOutput("white")),
      plotOutput("OLSW", height = "auto", width = "auto") 
    )
    
           )))  
