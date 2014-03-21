## simpleTest Shiny/R app ui.R                                           
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
    headerPanel("Test d'hypothèse sur une moyenne"),
    
    sidebarPanel(
        tags$head(
            tags$style(type="text/css", "label { display: inline; }"),
            tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }'),
            tags$style(type="text/css", ".jslider { max-width: 250px; }"),
            tags$style(type='text/css', ".well { max-width: 300px; }"),#class of the from inside sidebarPanel
            tags$style(type='text/css', ".span4 { max-width: 310px; }"),#span4 is the span of sidebarPanel (span8 is for the mainPanel)
            tags$style(type='text/css', "#complementinfos { width: 150px; }")#span4 is the span of sidebarPanel (span8 is for the mainPanel)
            ),
        conditionalPanel(
            condition = "input.visM == true",  
            ## Hypothèse
            h5(HTML("Hypothèses: H<sub>0</sub>: &mu;=&mu;<sub>0</sub> versus H<sub>1</sub>: &mu;&#x2260;&mu;<sub>0</sub>")),
            HTML(" &mu;<sub>0</sub> : moyenne de H<sub>0</sub> :"),
            sliderInput("mx0","",min = 25,max = 75,value = 33, step=1)
            ),
        conditionalPanel(
            condition = "input.visM == true && input.relPl == true  && input.mx0 == input.mx",
            h5(HTML("L'hypothèse nulle H<sub>0</sub> est vraie!"), style = "color:green")),
        conditionalPanel(
            condition = "input.visM == true && input.relPl == true  && input.mx0 != input.mx",
            h5(HTML("L'hypothèse nulle H<sub>0</sub> est fausse!"), style = "color:red")),
        conditionalPanel(
            condition = "input.visM == true",  
            ## Population
            h5("Population d'origine:")),
        conditionalPanel(
            condition = "input.relPl == true && input.visM == true",
            HTML("&mu; : moyenne de la population d'origine:"),
            sliderInput("mx","",min = 25,max = 75,value = 50, step=1)),
        conditionalPanel(
            condition = "input.visM == true",
            checkboxInput("sigKn",HTML(" &sigma; : &eacute;cart-type de la population d'origine"),FALSE)),
        conditionalPanel(
            condition = "input.sigKn == true && input.visM == true",
            sliderInput("sx","",min = 1,max = 30,value = sample(c(3:15),1), step=1)),
            ## Sampling
        conditionalPanel(
            condition = "input.visM == true",
            h5("Paramètres de l'échantillonnage :"),
            sliderInput("n","n : nombre d'individus par échantillon :",min = 2,max = 50,value = 10, step=1),
            sliderInput("ns","Nombre d'échantillons prélevés par échantillonnage:",min = 1,max = 50,value = 1, step=1),
            ## Take sample      
            actionButton("takesample","Echantillonner"),
            actionButton("reset","Reset")),#ns:number of samples       
            ## IC parameter
        conditionalPanel(
            condition = "input.testPl == true && input.visM == true",
            h5("Paramètre du test d'hypothèse :"),
            radioButtons("CVk","",    
                         c("empirique" = "eCVk",
                           "σ connue" = "vCVk",
                           "σ inconnue" = "sCVk"))
            ),
        conditionalPanel(
            condition = "input.CVk =='eCVk' && input.testPl == true && input.visM == true",
            HTML("c&nbsp;: demi amplitude de l'intervalle de confiance : [x&#772; &plusmn; c ]")),
        conditionalPanel(
            condition = "input.CVk =='vCVk' && input.testPl == true && input.visM == true",
            HTML("c&nbsp;: seuil critique de l'intervalle de confiance : [x&#772; &plusmn; c &sigma; / &radic;n ]")),
        conditionalPanel(
            condition = "input.CVk =='sCVk' && input.testPl == true && input.visM == true",
            HTML("c&nbsp;: seuil critique de l'intervalle de confiance : [x&#772; &plusmn; c s / &radic;n ]")),
        conditionalPanel(
            condition = "input.testPl == true && input.visM == true",
            sliderInput("k","",min = 1,max = 50,value = 5, step=0.5)),
        ## Graphic parameter
        conditionalPanel(
            condition = "input.visM == true",   
            h5("Paramètres graphiques :"),
            sliderInput("nss","Nombre d'échantillons affichés simultanément:",min = 1,max = 100,value = 10, step=1),#nss: number of samples to show
            checkboxInput("testPl","Afficher les tests d'hypothèses",FALSE),
            br()),
            #checkboxInput("empPl",HTML("Afficher les descriptives"),FALSE)),
        conditionalPanel(
            condition = "input.testPl == true && input.visM == true",
            selectInput("thresholds", "Seuils critiques :",
                        list("Afficher les formules théoriques" = "formula", 
                             "Afficher le calcul détaillé" = "calcul",
                             "Afficher le résultat" = "result"))
            ),
        conditionalPanel(
            condition = "input.testPl == true && input.visM == true",
            radioButtons("rejPl","Indiquer la décision:",    
                         c("non" = "rejPlnon",
                           "oui" = "rejPloui"))
            ),
        conditionalPanel(
            condition = "input.rejPl != 'rejPlnon' && input.testPl == true && input.visM == true",
            radioButtons("freqPl","Afficher  % de rejet:",
                         c("non" = "freqPlnon",
                           "oui" = "freqPloui"))
            ),
        conditionalPanel(
            condition = "input.visM == true",
            checkboxInput("relPl",HTML(" Comparer avec la realité"),FALSE),
            br(),
            checkboxInput("showreality",HTML("Afficher la densité d'origine"),FALSE),
            HTML('<hr style="border:1px solid #ccc;"/>'),
            HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="http://i.creativecommons.org/l/by/2.0/be/80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
            HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/104153" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/simopletest" target="_blank">Github</a></p>')),
        checkboxInput("visM",HTML("Show panel"),TRUE)  
        ),
  

    mainPanel(
        tabsetPanel(id="Tabset",selected=1,
                    tabPanel(
                        "Approche empirique",
                        plotOutput("plotEmp",height = "auto"),
                                        #verbatimTextOutput("test1"),
                        value=1),
                    tabPanel("Données",value=2,
                             tableOutput("DataTable")
                             )
     ## tabPanel("Test",value=3,
     ##  tableOutput("test1")
                    ## )
                    )
  )
)) 
 
