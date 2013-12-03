## testhypic Shiny/R app ui.R                                           
##                                                                      
## Author(s) :
## -----------
## Grégoire Vincke http://www.uclouvain.be/gregoire.vincke       
## For Statistical eLearning Tools http://sites.uclouvain.be/selt/      
##                                                                      
## Licences : 
## ---------
## CC-BY for the web page http://sites.uclouvain.be/selt/shiny/testhypic
## see http://creativecommons.org/licenses/by/2.0/be/ for more informations       
##
## GPLv2 for source code on https://github.com/uclouvain-selt/shiny
## See LICENCE.tx or http://www.gnu.org/licenses/old-licenses/gpl-2.0.html for more informations

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Principes du test d'hypothèse : méthode des intervalles de confiances"),
  
  sidebarPanel(
    tags$head(
        tags$style(type="text/css", "label { display: inline; }"),
        tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }')
      ),
    actionButton("takesample","Echantillonner"),
    actionButton("reset","Reset"),
    h5("Paramètres communs aux 3 situations :"),
    HTML(" &mu;<sub>0</sub> : moyenne de H<sub>0</sub> :"),#Label put outside of sliderInput because HTML is not rendered inside sliderInput label
    sliderInput("mx0", "" ,min = 1,max = 100,value = 40, step=0.5),
    HTML(" &mu;<sub>1</sub> : moyenne de H<sub>1</sub> :"),
    sliderInput("mx1","",min = 1,max = 100,value = 50, step=0.5),
    sliderInput("n","n : nombre d'individus par échantillon :",min = 2,max = 25,value = 4, step=1),
    sliderInput("ns","Nombre d'échantillons :",min = 1,max = 100,value = 1, step=1),
    radioButtons("truehyp", "Modèle considéré comme correct :",
                 list( "H1" = "h1",
                      "H0" = "h0")),

    br(),
    h5("Paramètres spécifiques à chacune des 3 situations :"),
    conditionalPanel(condition = "input.Tabset==1",
      HTML("&nbsp;&Kappa;&nbsp;: demi amplitude de l'intervalle de confiance : [x&#772; &plusmn; &Kappa;]"),
      sliderInput("k","",min = 1,max = 50,value = 5, step=0.5)
    ),
    conditionalPanel(condition = "input.Tabset==2 || input.Tabset==3",
      HTML(" Confiance (1-&alpha;) :"),
      sliderInput("confidence","",min = 0.5,max = 1,value = 0.95, step=0.005)
    ),
    conditionalPanel(condition = "input.Tabset==2",
      HTML(" &sigma; : &eacute;cart-type de la population d'origine : "),
      sliderInput("sx","",min = 1,max = 25,value = 10, step=1)
    ),
    br(),
    h5("Paramètres du graphiques :"),
    checkboxInput("showreality",HTML("Afficher la courbe de distribution de la r&eacute;alit&eacute; X~N(&mu;,&sigma;&sup2;)"),FALSE),
    br(),
    checkboxInput("pcbp2c",HTML("R&eacute;sumer le diagramme en barre des % de couvertures &agrave; 2 classes"),FALSE),
    br(),
    checkboxInput("evolpcincmu",HTML("Afficher l'&eacute;volution du pourcentage de recouvrement de &mu;"),FALSE)
  ),
  
  mainPanel(
    tabsetPanel(id="Tabset",selected=1,
      tabPanel(
	"IC empirique",
	#verbatimTextOutput("test1"),
	plotOutput("plotRealityEmp",height = "auto"),
	plotOutput("plotH0Emp",height = "auto"),
	plotOutput("plotH1Emp",height = "auto"),
	value=1),
      tabPanel(
	"IC modèle Normal à variance connue",
	#verbatimTextOutput("test2"),
	plotOutput("plotRealityZ",height = "auto"),
	plotOutput("plotH0Z",height = "auto"),
	plotOutput("plotH1Z",height = "auto"),
	value=2),
      tabPanel(
	"IC modèle Normal à variance inconnue",
	#verbatimTextOutput("test3"),
	plotOutput("plotRealityt",height = "auto"),
	plotOutput("plotH0t",height = "auto"),
	plotOutput("plotH1t",height = "auto"),
	value=3)
     )
  )
)) 
 
