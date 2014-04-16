library(shiny)


shinyUI(pageWithSidebar(
headerPanel(h2(HTML("Régression linéaire : violation de l'hypothèse d'homoscédasticité"))), 
sidebarPanel(
       
conditionalPanel(condition = "input.Tabset==1",     

h4(HTML("Gérérer des données"))              
   ,sliderInput("n", "Nombre d'observations par simulation", min = 5, max = 500, value = 100, step = 5)
   ,actionButton("takeY",HTML("Simuler n observations"))
   ,actionButton("reset",HTML("Reset"))
                                              
,h4(HTML("Paramètres du modèle : Y = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &epsilon;"))
    ,HTML("Intercept : &beta;<sub>0</sub>")
    ,sliderInput("intercept", "", min = 0, max = 20, value = 10)
    ,HTML("Pente : &beta;<sub>1</sub>")
    ,sliderInput("beta1", "", min = -2.5, max = 2.5, value = 0, step = 0.1)
    ,selectInput("var",h5(HTML("Var(&epsilon;) :")), 
                 list("forme 1" = "exp", "forme 2" = "lin"))
                  #forme 1 = HTML("&sigma;<sup>2</sup><sub>X</sub> = &alpha;<sub>0</sub>X<sup>&alpha;<sub>1</sub></sup>") 
                  #forme 2 = HTML("&sigma;<sup>2</sup><sub>X</sub> = &alpha;<sub>0</sub>+&alpha;<sub>1</sub>X<sup>2</sup>")
    ,br()
    ,HTML("Terme constant : &alpha;<sub>0</sub>")
    ,sliderInput("alpha0", "", min = 0, max = 5, value = 2, step = 0.1)
    ,HTML("Terme dépendant de X : &alpha;<sub>1</sub>")
    ,sliderInput("alpha1","", min = 0, max = 5, value = 0, step = 0.01)
    
,h4(HTML("Régression estimée"))
    ,checkboxInput("droite",HTML("Tracer la droite des moindres carrés"),FALSE) 
    ,checkboxInput("Coef1",HTML("Afficher les coefficients estimés"),FALSE) 
    ,checkboxInput("plotresid",HTML("Afficher le graphique des résidus Vs X"),FALSE)                   
    ,checkboxInput("Test1",HTML("Tester l'hypothèse d'homoscédasticité"),FALSE)        
                     
   ),
  
conditionalPanel(condition = "input.Tabset==2" 

,h4(HTML("Le jeu de données"))                   
,p("Les données utilisées proviennent de....")
,p("Nombre d'observations : n = 108")
,p("Variable réponse : Rent (montant du loyer)")
,p("Variable explicative : Income (en fonction du revenu)") 
,checkboxInput("obs",HTML("Voir les premières observations"),FALSE)  
                   
,h4(HTML("Régression linéaire simple"))     
,checkboxInput("droite2",HTML("Tracer la droite des moindres carrés"),FALSE) 
,checkboxInput("Coef2",HTML("Régression OLS"),FALSE)           
,checkboxInput("Test2",HTML("Tester l'hypothèse d'homoscédasticité"),FALSE)                     

,h4 (HTML("Remèdes contre l'hétéroscédasticité"))                 
,selectInput("sol",(HTML("Choisir une solution possible: ")), 
                  list("Aucune" = "aucune", 
                       "Prendre le log(Y)" = "logY", 
                       "OLS avec inférence robuste" = "OLS", 
                       "GLS avec poids estimés " = "GLS")),

conditionalPanel(condition = "input.sol == 'GLS'"
,selectInput("wi","Poids estimés :",    
                  list("1/X" = "GLS1",
                       "1/sqrt(X)" = "GLS2",
                       "1/X^2" = "GLS3")))
)),
  
  mainPanel(
        
    tabsetPanel(id="Tabset",selected=1,
      tabPanel(HTML("Hétéroscédasticité : constat"),
        plotOutput("doublePlot1", height = "auto", width = "auto"),
        value = 1), 
      tabPanel(HTML("Hétéroscédasticité : remèdes"), 
       tableOutput('contents'),
       plotOutput("InitPlot", height = "auto", width = "auto"),
       h4(textOutput("caption")),
       plotOutput("LogPlot", height = "auto", width = "auto"), 
       value = 2)
          
      )
           )  
))