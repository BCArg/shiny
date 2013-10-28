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
    HTML('<p>Détails sur <a href="http://sites.uclouvain.be/selt/ressources/104033" target="_blank">Statistics eLearning Tools</a>, Code sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/ztox" target="_blank">Github</a></p>')
  ),

  mainPanel(
	plotOutput("zPlot"),htmlOutput("Text")
  )
))
