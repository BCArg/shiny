library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Intervalles de confiances"),
  
  sidebarPanel(
    sliderInput("mx","µ : Moyenne de la population : ",min = 1,max = 100,value = 50, step=0.5),
    sliderInput("sx","σ : écart-type de la population : ",min = 1,max = 25,value = 10, step=1),
    sliderInput("n","n : nombre d'individus par échantillon :",min = 2,max = 25,value = 4, step=1),
    sliderInput("confidence","Confiance (1-α) :",min = 0.5,max = 1,value = 0.7, step=0.005),
    checkboxInput("seemu","Identifier µ", TRUE),
    checkboxInput("seedech","Illustrer la distribution d'échantillonnage", FALSE),
    checkboxInput("seeicvarknown","Voir l'intervalle de confiance pour µ quand σ² est connue", TRUE),
    checkboxInput("seeicvarunknown","Voir l'intervalle de confiance pour µ quand σ² est inconnue", FALSE),
    #checkboxInput("seeconfidence","Identifier la zone de confiance dans la population d'origine", FALSE),
    checkboxInput("seeicpcevolution","Voir l'évolution du % d'IC qui couvrent µ", FALSE),
    br(),
    actionButton("takeech","Echantillonner"),
    actionButton("reset","Reset"),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML('<p>Détails sur <a href="http://sites.uclouvain.be/selt/ressources/104133" target="_blank">Statistics eLearning Tools</a>, Code sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/ic" target="_blank">Github</a></p>')

  ),
  
  mainPanel(
        plotOutput("plotReality",height = "auto"),
        plotOutput("plotSample",height = "auto"),
        plotOutput("plotPercent",height = "auto")
  )
)) 
