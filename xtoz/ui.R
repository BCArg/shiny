library(shiny)

# Define UI for zplot
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Probabilité associée à une valeur de X~N(µ;σ²)"),

  sidebarPanel(
    numericInput("mx", "µ :", 25),
    numericInput("sx", "σ :", 5),
    numericInput("x", "X :", 30),
    selectInput("sens", "Probabilité de type :",
    list("P(X≤x)" = "inf", 
	  "P(X≥x)" = "sup",
	  "P(X=x)" = "equal")),
    submitButton("Calculer la probabilité"),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="http://i.creativecommons.org/l/by/2.0/be/80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
    HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/104033" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/xtoz" target="_blank">Github</a></p>')
  ),

  mainPanel(
	plotOutput("zPlot"),htmlOutput("Text")
  )
))
