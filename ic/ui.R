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
    checkboxInput("seeiconx","Voir la projection des limites des IC sur l'axe des abscisses", FALSE),
    #checkboxInput("seeconfidence","Identifier la zone de confiance dans la population d'origine", FALSE),
    checkboxInput("seeicpcevolution","Voir l'évolution du % d'IC qui couvrent µ", TRUE),
    br(),
    actionButton("takeech","Echantillonner"),
    actionButton("reset","Reset"),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="http://i.creativecommons.org/l/by/2.0/be/80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
    HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/104133" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/ic" target="_blank">Github</a></p>')
  ),
  
  mainPanel(
        plotOutput("plotReality",height = "auto"),
        plotOutput("plotSample",height = "auto"),
        plotOutput("plotPercent",height = "auto")
  )
)) 
