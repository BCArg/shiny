library(shiny)


shinyUI(pageWithSidebar(
headerPanel("Régression linéaire : conséquences de l'hétéroskédasticité pour l'inférence de β₁"), 
  conditionalPanel(
    condition = "input.visM == false",
    sidebarPanel(
    tags$head(
		tags$style(type="text/css", "label { display: inline; }"),
		tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }'),
		tags$style(type="text/css", ".jslider { max-width: 250px; }"),
		tags$style(type='text/css', ".well { max-width: 300px; }"),#class of the from inside sidebarPanel
		tags$style(type='text/css', ".span4 { max-width: 310px; }"),#span4 is the span of sidebarPanel (span8 is for the mainPanel)
		tags$style(type='text/css', "select#display { width: 150px; }"),
		tags$style(type='text/css', "#mainInputs {margin : 0px 0px 4px 0px; }"),#maininputs is the div surrounding the action button in the main panel
		tags$script(type="text/javascript",src="js/scripts.js")
		),
    h5(HTML("Paramètres de l'échantillonnage"))
      ,HTML("n : nombre d'individus par &eacute;chantillon&nbsp;:")
      ,sliderInput("n","",min = 20,max = 500,value = 100, step=5)
      ,HTML("Nombre d'&eacute;chantillons pr&eacute;lev&eacute;s&nbsp;:")
      ,sliderInput("ns", "", min = 1, max = 50, value = 1, step = 1)
#       ,p(HTML("k : Nombre total d'échantillons prélevés"))    
      

#     ,h5(HTML("Modèle : Y = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &epsilon;"))
,h5(HTML("Param&egrave;tres de l'inf&eacute;rence"))
#       ,p(HTML("Pour simplifier &nbsp;: &beta;<sub>0</sub>=0; &beta;<sub>1</sub>=0 ou 1; &sigma;<sup>2</sup>=2."))
     # ,p(HTML("&beta;<sub>0</sub> (intercept) : pour simplifier &beta;<sub>0</sub>=0. "))
     # HTML("&beta;<sub>1</sub> (pente) : pour simplifier &beta;<sub>1</sub> = 0 ou 1&nbsp;:<br />")
      ,selectInput("beta1","", 
		c("H₀ : β₁ = 0 est vraie"="h0",
		  "H₁ : β₁ = 1 est vraie"="h1"))#caractères unicodes copiés de http://fr.wikipedia.org/wiki/Exposants_et_indices_Unicode
#       ,p(HTML("Variance d'&epsilon; = &sigma;<sup>2</sup> X<sup>&alpha;<sub>1</sub></sup>. Pour simplifier : &sigma;<sup>2</sup>=2&nbsp;: "))
      ,selectInput("alpha1","", 
		    c("Homoskédasticité : α₁ = 0"= "homo",
		      "Hétéroskédasticité: α₁ ≠ 0" = "hetero"))
      
      ,conditionalPanel(condition = "input.alpha1 == 'hetero'"
			,HTML("&alpha;<sub>1</sub>")
			,sliderInput("V.alpha1","", min = 0.1, max = 10, value = 2, step = 0.1))
      #,p(HTML("Le terme constant &sigma;<sup>2</sup>  est fixé à 2"))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#       ,h5(HTML("Inférence"))
      ,br(),checkboxInput("barplot",HTML("Visualiser le % de RH<sub>0</sub>"),FALSE) 
#       ,p(HTML("<i><u>Si les hypothèses sur &epsilon; sont respectées :</u></i>")) 
#       ,p(HTML("- lorsque H<sub>0</sub> est vraie, le % de RH<sub>0</sub> converge vers le risque d'erreur de type I quand k &rarr; &infin; (le niveau de signification du test est fixé à 5%)"))    
#       ,p(HTML("- lorsque H<sub>1</sub> est vraie, le % de RH<sub>0</sub> converge vers la puissance qui &rarr;1 quand k &rarr; &infin;")) 
	      ## Graphic parameter  
		,h5("Paramètres graphiques :")
		,selectInput("display", "Display :",
		  list("Defaut" = "default", 
			"1024x768" = "1024",
			"800x600" = "800"))
		,HTML('<hr style="border:1px solid #ccc;"/>')
		,HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="img/cc_by_80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.')
		,HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/194683" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/olsvarinf" target="_blank">Github</a></p>')
    )
  ),

  mainPanel(
    HTML("<div id='mainInputs'>")
    ,actionButton("takesample","Echantillonner"),actionButton("reset","Reset"),checkboxInput("seetheor",HTML("Afficher l'information th&eacute;orique"),FALSE),checkboxInput("visM",HTML("Plein &eacute;cran"),FALSE)
    ,HTML("</div>")
    ,HTML('<hr style="border:1px solid #ccc;"/>')
    ,conditionalPanel(condition = "input.seetheor == true",
    h5(HTML("Mod&egrave;le th&eacute;orique g&eacute;n&eacute;ral : Y = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &epsilon;"))
    ,p(HTML("Pour simplifier ce mod&egrave;le, dans le cadre de cet exemple les valeurs de l'intercept (&beta;<sub>0</sub>) et de la pente (&beta;<sub>1</sub>) ont &eacute;t&eacute; fix&eacute;es au valeurs  : &beta;<sub>0</sub>=0, et &beta;<sub>1</sub>=0 ou 1.<br />Donc quand &beta;<sub>0</sub>=0 et &beta;<sub>1</sub>=1 alors <strong>Y = X + &epsilon;</strong>, et quand &beta;<sub>0</sub>=0 et &beta;<sub>1</sub>=0 alors <strong>Y = &epsilon;</strong>.<br/>La variance du terme d'erreur est <strong>&sigma;<sub>&epsilon;</sub><sup>2</sup> = &sigma;<sup>2</sup> X<sup>&alpha;<sub>1</sub></sup></strong>. Pour simplifier ce calcul nous considerons que &sigma;<sup>2</sup>=2, donc : <strong>&sigma;<sub>&epsilon;</sub><sup>2</sup> = 2 X<sup>&alpha;<sub>1</sub></sup></strong>."))
    ,h5(HTML("Inférence : <i><u>Si les hypothèses sur &epsilon; sont respectées</u></i> :"))
    ,p(HTML("k étant le nombre total d'échantillons prélevés :")) 
    ,p(HTML("- lorsque H<sub>0</sub> est vraie, le % de RH<sub>0</sub> converge vers le risque d'erreur de type I quand k &rarr; &infin; (le niveau de signification du test est fixé à 5%)"))    
    ,p(HTML("- lorsque H<sub>1</sub> est vraie, le % de RH<sub>0</sub> converge vers la puissance qui &rarr;1 quand k &rarr; &infin;"))
    )
    ,plotOutput("mainPlot", height = "auto", width = "auto")
#     ,conditionalPanel(
#       condition= "input.alpha1 == 'homo'",
#       plotOutput("doublePlot", height = "auto", width = "auto"),
#       plotOutput("barPlot", height = "auto", width = "auto")
#     )
#     ,conditionalPanel(
#       condition = "input.alpha1 == 'hetero'",
#       plotOutput("XY", height = "auto", width = "auto"),
#       h5(textOutput("classique")),
#       plotOutput("OLS", height = "auto", width = "auto"), 
#       h5(textOutput("white")),
#       plotOutput("OLSW", height = "auto", width = "auto") 
#     )
  )
 )
)