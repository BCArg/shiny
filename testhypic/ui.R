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
  headerPanel("Intervalles de confiances à la moyenne"),
  
  sidebarPanel(
    tags$head(
        tags$style(type="text/css", "label { display: inline; }"),
        tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }'),
        tags$style(type="text/css", ".jslider { max-width: 350px; }"),
        tags$style(type='text/css', ".well { max-width: 400px; }"),#class of the from inside sidebarPanel
        tags$style(type='text/css', ".span4 { max-width: 400px; }")#span4 is the span of sidebarPanel (span8 is for the mainPanel)
      ),
    actionButton("takesample","Echantillonner"),
    actionButton("reset","Reset"),
    h5("Paramètres communs aux 3 onglets :"),
    radioButtons("truehyp", HTML("Mod&egrave;le d'o&ugrave; sont pr&eacute;lev&eacute;s les &eacute;chantillons :</br>"),
	      list( "H1: X~N(µ1,σ²)" = "h1",
		  "H0: X~N(µ0,σ²)" = "h0")),
    HTML(" &mu;<sub>0</sub> : moyenne de H<sub>0</sub> :"),#Label put outside of sliderInput because HTML is not rendered inside sliderInput label
    sliderInput("mx0", "" ,min = 1,max = 100,value = 40, step=0.5),
    HTML(" &mu;<sub>1</sub> : moyenne de H<sub>1</sub> :"),
    sliderInput("mx1","",min = 1,max = 100,value = 50, step=0.5),
    sliderInput("n","n : nombre d'individus par échantillon :",min = 2,max = 25,value = 4, step=1),
    HTML(" &sigma; : &eacute;cart-type de la population d'origine : "),
    sliderInput("sx","",min = 1,max = 25,value = 10, step=1),
    sliderInput("ns","Nombre d'échantillons prélevés par échantillonnage:",min = 1,max = 100,value = 1, step=1),
    conditionalPanel(condition = "input.Tabset!=4",
      br(),
      h5("Paramètres spécifiques à chacun des 3 onglets :")
    ),
    conditionalPanel(condition = "input.Tabset==1",
      HTML("&nbsp;&Kappa;&nbsp;: demi amplitude de l'intervalle de confiance : [x&#772; &plusmn; &Kappa;]"),
      sliderInput("k","",min = 1,max = 50,value = 5, step=0.5)
    ),
    conditionalPanel(condition = "input.Tabset==2 || input.Tabset==3",
      HTML(" Confiance (1-&alpha;) :"),
      sliderInput("confidence","",min = 0.5,max = 1,value = 0.95, step=0.005)
    ),
    conditionalPanel(condition = "input.Tabset!=4",
      br(),
      h5("Paramètres graphiques :"),
      checkboxInput("showreality",HTML("Afficher la distribution d'origine X~N(&mu;,&sigma;&sup2;)"),FALSE),
      br(),
      checkboxInput("showmur",HTML("Afficher µ la moyenne de la distribution d'origine"),FALSE),
      br(),
      checkboxInput("pcbp2c",HTML("Limiter &agrave; 2 classes le barplot des %"),FALSE),
      br(),
      checkboxInput("evolpcincmu",HTML("Afficher l'&eacute;volution du % de recouvrement de &mu;<sub>1</sub> et &mu;<sub>0</sub>"),FALSE)
    ),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="http://i.creativecommons.org/l/by/2.0/be/80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
    HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/104153" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/testhypic" target="_blank">Github</a></p>')
  ),
  
  mainPanel(
    tabsetPanel(id="Tabset",selected=1,
      tabPanel(
	"IC empirique",
	plotOutput("plotEmp",height = "auto"),
	#verbatimTextOutput("test1"),
	value=1),
      tabPanel(
	"IC modèle Normal à variance connue",
	#verbatimTextOutput("test2"),
	plotOutput("plotZ",height = "auto"),
	value=2),
      tabPanel(
	"IC modèle Normal à variance inconnue",
	#verbatimTextOutput("test3"),
	plotOutput("plotT",height = "auto"),
	value=3),
     tabPanel("Données",value=4
     
     )
     )
  )
)) 
 
