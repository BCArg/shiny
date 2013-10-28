library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Test d'hypothèses de comparaison unidirectionelle de moyennes : les bases illustrées"),
  
  sidebarPanel(
    tags$head(
        #tags$style(type="text/css", "select { width: 80px; }"),
        #tags$style(type='text/css', ".well { max-width: 400px; }"),#class of the from inside sidebarPanel
        #tags$style(type='text/css', ".span4 { max-width: 400px; }")#span4 is the span of sidebarPanel (span8 is for the mainPanel)
        #tags$style(type="text/css", ".jslider { max-width: 200px; }")
      ),
      checkboxInput("freezeyaxis", " Fixer la valeur maximale des axes à densité=0.2 ", FALSE),#(permet de mieux visualiser les changement de forme des distributions)
    HTML('<hr style="border:1px solid #ccc;"/>'),
    sliderInput("mx0","µ0 : moyenne de H0 :",min = 1,max = 100,value = 40, step=0.5),
    sliderInput("mx1","µ1 : moyenne de H1 : ",min = 1,max = 100,value = 50, step=0.5),
    sliderInput("sx","σ : écart-type de la population d'origine : ",min = 1,max = 25,value = 10, step=1),
    #sliderInput("sx1","Ecart-type de H1 : σ1",min = 1,max = 25,value = 10, step=1),
    sliderInput("n","n : nombre d'individus par échantillon :",min = 2,max = 25,value = 4, step=1),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    sliderInput("confidence","Confiance (1-α) :",min = 0.5,max = 1,value = 0.95, step=0.005),
    checkboxInput("alphabetalabels", " Identifier α, 1-α, β et 1-β", TRUE),
    checkboxInput("alphabetaareas", " Identifier les surfaces correspondantes à α, 1-α, β et 1-β", TRUE),
    checkboxInput("alphabetaproject", " Masquer la projection de la valeur seuil pour α de H0 sur H1", FALSE),
    HTML('<hr style="border:1px solid #ccc;"/>'),

    actionButton("takeech","Echantillonner et tester"),
    actionButton("reset","Reset"),
    selectInput("truehyp", "Modèle considéré comme correct :",
                 list("H1" = "h1",
                      "H0" = "h0")),
    checkboxInput("showmean", " Illustrer la moyenne de l'échantillon", TRUE),
    checkboxInput("showicz", " Illustrer l'IC pour µ1 à 1-α pour σ² connue", FALSE),
    checkboxInput("showict", " Illustrer l'IC pour µ1 à 1-α pour σ² inconnue", FALSE),
    checkboxInput("showboxplot", " Illustrer la distribution des valeurs de l'échantillon avec un boxplot", TRUE),
    checkboxInput("hideh1", " Masquer H1", FALSE),
    checkboxInput("showrhotrend", " Montrer l'évolution du %RH0", FALSE),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML('<p>Détails sur <a href=""http://sites.uclouvain.be/selt/ressources/104123">Statistics eLearning Tools</a>, Code sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/testhyp">Github</a></p>')
  ),
  
  mainPanel(
    
    plotOutput("doubleplot")
  )
))
