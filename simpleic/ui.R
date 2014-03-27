## simpleIC Shiny/R app ui.R                                           
##                                                                      
## Author(s) :
## -----------
## J.J.
## Orginal version by Grégoire Vincke http://www.uclouvain.be/gregoire.vincke       
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
    headerPanel("Intervalles de confiance à la moyenne"),

    conditionalPanel(
<<<<<<< HEAD
            condition = "input.visM == false",
    sidebarPanel(
        tags$head(
            tags$style(type="text/css", "label { display: inline; }"),
            tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }'),
            tags$style(type="text/css", ".jslider { max-width: 250px; }"),
            tags$style(type='text/css', ".well { max-width: 300px; }"),#class of the from inside sidebarPanel
            tags$style(type='text/css', ".span4 { max-width: 310px; }"),#span4 is the span of sidebarPanel (span8 is for the mainPanel)
            tags$style(type='text/css', "#complementinfos { width: 150px; }"),
            tags$style(type='text/css', "#CVk { width: 150px; }"),
            tags$style(type='text/css', "select#display { width: 150px; }"),
            tags$script(type="text/javascript",src="js/scripts.js")
            ), 
            ## Take sample      
            ## Population
            h5("Paramètres de la population d'origine:"),
            checkboxInput("muKn",HTML("&mu; : moyenne de la population d'origine"),FALSE),
            br(),
        conditionalPanel(
            condition = "input.muKn == true",
            sliderInput("mx1","",min = 20,max = 60,value = sample(c(31:35),1), step=1)
            ),
            checkboxInput("sigKn",HTML(" &sigma; : &eacute;cart-type de la population d'origine"),FALSE)
            ,
        conditionalPanel(
            condition = "input.sigKn == true",
            sliderInput("sx","",min = 0,max = 10,value = sample(seq(from = 2, to = 3.5, by = 0.5),1), step=0.5)# sample(c(3:15),1) ->4
            ),
=======
        condition = "input.visM == false",
        sidebarPanel(
            tags$head(
                tags$style(type="text/css", "label { display: inline; }"),
                tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }'),
                tags$style(type="text/css", ".jslider { max-width: 250px; }"),
                tags$style(type='text/css', ".well { max-width: 300px; }"),#class of the from inside sidebarPanel
                tags$style(type='text/css', ".span4 { max-width: 310px; }"),#span4 is the span of sidebarPanel (span8 is for the mainPanel)
                tags$style(type='text/css', "#complementinfos { width: 150px; }")#span4 is the span of sidebarPanel (span8 is for the mainPanel)
                ),  
            ## Take sample
            actionButton("takesample0","Echantillonner"),
            actionButton("reset","Reset"),
            br(),
            ## Population
            h5("Paramètres de la population d'origine:"),
            checkboxInput("muKn",HTML("&mu; : moyenne de la population d'origine"),FALSE),
            conditionalPanel(
                condition = "input.muKn == true",
                sliderInput("mx1","",min = 20,max = 60,value = sample(c(31:35),1), step=1)
                ),
            checkboxInput("sigKn",HTML(" &sigma; : &eacute;cart-type de la population d'origine"),FALSE),
            conditionalPanel(
                condition = "input.sigKn == true",
                sliderInput("sx","",min = 0,max = 10,value = sample(seq(from = 2, to = 3.5, by = 0.5),1), step=0.5)# sample(c(3:15),1) ->4
                ),
>>>>>>> 22245c27bffd6aee71ba4783e5e90c2969250176
            ## Sampling
            h5("Paramètres de l'échantillonnage :"),
            sliderInput("n","n : nombre d'individus par échantillon :",min = 2,max = 50,value = 10, step=1),
            sliderInput("ns","Nombre d'échantillons prélevés :",min = 1,max = 50,value = 1, step=1),#ns:number of samples

            ## IC parameter
<<<<<<< HEAD
            h5("Paramètres des IC :"),
                        checkboxInput("icPl",HTML("Afficher les intervalles de confiance"),FALSE),
            br(),
        conditionalPanel(
            condition = "input.empPl == true && input.icPl == true",
            selectInput("thresholds", "Seuils critiques :",
                        list("Afficher les formules théoriques" = "formula", 
                             "Afficher le calcul détaillé" = "calcul",
                             "Afficher le résultat" = "result"))
            ),
        conditionalPanel(
            condition = "input.icPl == true",
            
            selectInput("CVk","Modèle de détermination :",    
                         c("empirique" = "eCVk",
                           "σ connu" = "vCVk",
                           "σ inconnu" = "sCVk"))
            ),
        conditionalPanel(
            condition = "input.CVk =='eCVk' && input.icPl == true",
            HTML("c&nbsp;: demi amplitude de l'IC : [x&#772; &plusmn; c ]")),
        conditionalPanel(
            condition = "input.CVk =='vCVk' && input.icPl == true",
            HTML("c&nbsp;: seuil critique de l'IC : [x&#772; &plusmn; c &sigma; / &radic;n ]")),
        conditionalPanel(
            condition = "input.CVk =='sCVk' && input.icPl == true",
            HTML("c&nbsp;: seuil critique de l'IC : [x&#772; &plusmn; c s / &radic;n ]")),
        conditionalPanel(
            condition = "input.icPl == true",
            sliderInput("k","",min = 1,max = 25,value = 5, step=0.5)),
        conditionalPanel(
            condition = "input.icPl == true",
            selectInput("cvPl","Indiquer la couverture par les IC :",    
                         c("non" = "non",
                           "oui" = "oui",
                           "pour μ de la population d'origine" = "parOri",
                           "pour une valeur alternative  μ''" = "parAlt"))
            ),
        conditionalPanel(
            condition = "input.cvPl == 'parAlt' && input.icPl == true",
            HTML("pour une valeur alternative &mu;'' de valeur :"),
            sliderInput("mx0","",min = 20,max = 60,value = 35, step=1)
            ),
        conditionalPanel(
            condition = "input.cvPl == 'oui' && input.icPl == true",
            HTML("par un paramètre de valeur :"),
            sliderInput("mx","",min = 20,max = 60,value = 35, step=1)
            ),  
        conditionalPanel(
            condition = "input.cvPl != 'non' && input.icPl == true",
            selectInput("freqPl","Afficher  % couverture:",
                         c("non" = "freqPlnon",
                           "oui" = "freqPloui"))
            ),
        ## Graphic parameter  
            h5("Paramètres graphiques :"),
            checkboxInput("empPl",HTML("Afficher les statistiques descriptives"),TRUE),br(),
=======
            conditionalPanel(
                condition = "input.icPl == true",
                h5("Paramètre de l'intervalle de confiance :"),
                radioButtons("CVk","",    
                             c("empirique" = "eCVk",
                               "σ connu" = "vCVk",
                               "σ inconnu" = "sCVk"))
                ),
            conditionalPanel(
                condition = "input.CVk =='eCVk' && input.icPl == true",
                HTML("c&nbsp;: demi amplitude de l'IC : [x&#772; &plusmn; c ]")),
            conditionalPanel(
                condition = "input.CVk =='vCVk' && input.icPl == true",
                HTML("c&nbsp;: seuil critique de l'IC : [x&#772; &plusmn; c &sigma; / &radic;n ]")),
            conditionalPanel(
                condition = "input.CVk =='sCVk' && input.icPl == true",
                HTML("c&nbsp;: seuil critique de l'IC : [x&#772; &plusmn; c s / &radic;n ]")),
            conditionalPanel(
                condition = "input.icPl == true",
                sliderInput("k","",min = 1,max = 25,value = 5, step=0.5)),
            conditionalPanel(
                condition = "input.icPl == true",
                selectInput("cvPl","Indiquer la couverture par l'IC :",    
                            c("non" = "non",
                              "oui" = "oui",
                              "pour μ de la population d'origine" = "parOri",
                              "pour une valeur alternative  μ''" = "parAlt"))
                ),
            conditionalPanel(
                condition = "input.cvPl == 'parAlt' && input.icPl == true",
                sliderInput("mx0","",min = 20,max = 60,value = 35, step=1)
                ),
            conditionalPanel(
                condition = "input.cvPl == 'oui' && input.icPl == true",
                sliderInput("mx","",min = 20,max = 60,value = 35, step=1)
                ),  
            conditionalPanel(
                condition = "input.cvPl != 'non' && input.icPl == true",
                radioButtons("freqPl","Afficher  % couverture:",
                             c("non" = "freqPlnon",
                               "oui" = "freqPloui"))
                ),
            ## Graphic parameter  
            h5("Paramètres graphiques :"),
            sliderInput("nss","Nombre d'échantillons affichés simultanément:",min = 1,max = 100,value = 10, step=1),#nss: number of samples to show
            checkboxInput("icPl",HTML("Afficher les intervalles de confiance"),FALSE),
            br(),
            checkboxInput("empPl",HTML("Afficher les statistiques descriptives<br />"),TRUE),
            conditionalPanel(
                condition = "input.empPl == true && input.icPl == true",
                selectInput("thresholds", "Seuils critiques :",
                            list("Afficher les formules théoriques" = "formula", 
                             "Afficher le calcul détaillé" = "calcul",
                                 "Afficher le résultat" = "result"))
                ),
>>>>>>> 22245c27bffd6aee71ba4783e5e90c2969250176
            checkboxInput("showreality",HTML("Afficher la densité d'origine"),FALSE),
            sliderInput("nss","Nombre d'échantillons affichés :",min = 1,max = 50,value = 7, step=1),#nss: number of samples to show
	    selectInput("display", "Display :",
	      list("Defaut" = "default", 
		    "1024x768" = "1024",
		    "800x600" = "800")),
            HTML('<hr style="border:1px solid #ccc;"/>'),
<<<<<<< HEAD
            HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="img/cc_by_80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
            HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/104153" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/simpleic" target="_blank">Github</a></p>')
        )
  ),
=======
            HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="http://i.creativecommons.org/l/by/2.0/be/80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
            HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/104153" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/simpleic" target="_blank">Github</a></p>'),
            br(),
            checkboxInput("visM",HTML("Masquer le menu"),FALSE)
            )
        ),
>>>>>>> 22245c27bffd6aee71ba4783e5e90c2969250176

    mainPanel(
        tabsetPanel(id="Tabset",selected=1,
                    tabPanel(
                        "Vue graphique",
<<<<<<< HEAD
                        actionButton("takesample","Echantillonner"),
			actionButton("reset","Reset"),
                        checkboxInput("visM",HTML("Plein écran"),FALSE),
                        #HTML("<hr>"),
                        plotOutput("plotEmp",height='100%'),
=======
                        conditionalPanel(
                            condition = "input.visM == true",
                            actionButton("takesample1","Echantillonner"),
                            br(),
                            checkboxInput("visM",HTML("Démasquer le menu"),FALSE)),
                        plotOutput("plotEmp",height = "auto"),
>>>>>>> 22245c27bffd6aee71ba4783e5e90c2969250176
                                        #verbatimTextOutput("test1"),
                        value=1),
                    tabPanel("Données",value=2,
                             ## conditionalPanel(
                             ##     condition = "input.visM0 == true",
                             ##     checkboxInput("visM1",HTML("Demasquer le menu"),FALSE)),
                             tableOutput("DataTable")
<<<<<<< HEAD
                             )#,
#      tabPanel("Test",value=3,
#       tableOutput("test1")
#                     )
=======
                             )
                    ## tabPanel("Test",value=3,
                    ##          checkboxInput("visM",HTML("Demasquer le menu"),FALSE),
                    ##          tableOutput("test1")
                    ##          )
>>>>>>> 22245c27bffd6aee71ba4783e5e90c2969250176
                    )
  )
)) 
 
