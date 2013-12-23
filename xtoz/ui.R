library(shiny)

# Define UI for zplot
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Probabilité associée à une valeur de X~N(µ;σ²)"),

  sidebarPanel(
  
    tags$head(
        tags$style(type="text/css", "label { display: inline; }"),
        tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }'),
        tags$style(type="text/css", 'input[type="number"] { width: 50px; }'),
        tags$style(type="text/css", ".jslider { max-width: 350px; }"),
        tags$style(type='text/css', ".well { max-width: 400px; }"),#class of the from inside sidebarPanel
        tags$style(type='text/css', ".span4 { max-width: 400px; }")#span4 is the span of sidebarPanel (span8 is for the mainPanel)
      ),
  
    numericInput("mx", "µ :", 25),
    br(),
    numericInput("sx", "σ :", 5),
    br(),
    conditionalPanel(condition = "input.Tabset==1",
      numericInput("x", "X :", 30),
      br(),
      selectInput("sens", "Probabilité de type :",
      list("P(X≤x)" = "inf", 
	    "P(X≥x)" = "sup",
	    "P(X=x)" = "equal"))
    ),
    conditionalPanel(condition = "input.Tabset==2",
      numericInput("x1", HTML("X<sub>1</sub> :"), 20),
      br(),
      numericInput("x2", HTML("X<sub>2</sub> :"), 30)
    ),

    HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="http://i.creativecommons.org/l/by/2.0/be/80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
    HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/104033" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/xtoz" target="_blank">Github</a></p>')
  ),

  mainPanel(
    tabsetPanel(id="Tabset",selected=2,
      tabPanel(
	"Une valeur",
	plotOutput("zPlot"),
	htmlOutput("Text"),
	value=1
      ),
      tabPanel(
	"Un intervalle",
	plotOutput("zPlotInt"),
	htmlOutput("TextInt"),
	value=2
      )
    )
  )
))
