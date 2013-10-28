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
	      br(), a("Le code source de cette animation est disponible sur Statistics eLearning Tools",href="http://sites.uclouvain.be/selt/ressources/104033")
  ),

  mainPanel(
	plotOutput("zPlot"),htmlOutput("Text")
  )
))
