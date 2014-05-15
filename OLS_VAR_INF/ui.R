library(shiny)


shinyUI(pageWithSidebar(
headerPanel("Régression linéaire : conséquences de l'hétéroskédasticité pour l'inférence de β₁"), 
sidebarPanel(
       
  actionButton("takesample",HTML("Générer des données"))
 ,actionButton("reset",HTML("Reset"))

,h5(HTML("Paramètres de l'échantillonnage"))
  ,sliderInput("n", HTML("Nombre d'observations par échantillon : n"), min = 20, max = 500, value = 100, step = 5)
  ,sliderInput("ns", HTML("Nombre d'échantillons prélevés par simulation"), min = 1, max = 50, value = 1, step = 1)
  ,p(HTML("k : Nombre total d'échantillons prélevés"))    
  

,h5(HTML("Paramètres du modèle : Y = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &epsilon;"))
  ,p(HTML("Intercept : &beta;<sub>0</sub> est fixé à 0"))
  ,radioButtons("beta1", HTML("Pente : &nbsp; &nbsp; H<sub>0</sub> : &beta;<sub>1</sub> = 0 &nbsp; &nbsp; &nbsp;H<sub>1</sub> : &beta;<sub>1</sub> = 1"), 
             c("H₀ est vraie"="h0",
               "H₁ est vraie"="h1"))#caractères unicodes copiés de http://fr.wikipedia.org/wiki/Exposants_et_indices_Unicode

  
  ,radioButtons("alpha1", HTML("Variance du terme d'erreur : Var(&epsilon;) = &sigma;<sup>2</sup> X<sup>&alpha;<sub>1</sub></sup>"), 
                c("Homoskédasticité : α₁ = 0"= "homo",
                  "Hétéroskédasticité: α₁ ≠ 0" = "hetero"))
  
  ,conditionalPanel(condition = "input.alpha1 == 'hetero'"
                    ,HTML("&alpha;<sub>1</sub>")
                    ,sliderInput("V.alpha1","", min = 0.1, max = 10, value = 2, step = 0.1))
  ,p(HTML("Le terme constant &sigma;<sup>2</sup>  est fixé à 2"))
  
  ,h5(HTML("Inférence"))
  ,checkboxInput("barplot",HTML("Visualiser le % de RH<sub>0</sub>"),FALSE) 
  ,p(HTML("<i><u>Si les hypothèses sur &epsilon; sont respectées :</u></i>")) 
  ,p(HTML("- lorsque H<sub>0</sub> est vraie, le % de RH<sub>0</sub> converge vers le risque d'erreur de type I quand k &rarr; &infin; (le niveau de signification du test est fixé à 5%)"))    
  ,p(HTML("- lorsque H<sub>1</sub> est vraie, le % de RH<sub>0</sub> converge vers la puissance qui &rarr;1 quand k &rarr; &infin;"))    
),
  

  
  mainPanel(
    conditionalPanel(
      condition= "input.alpha1 == 'homo'",
    plotOutput("doublePlot", height = "auto", width = "auto"),
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