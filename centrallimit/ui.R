library(shiny)
shinyUI(pageWithSidebar(
          headerPanel("Théoreme central limite"), 
          
          conditionalPanel(
            condition = "input.visM == false",
          
          sidebarPanel(
            tags$head(
              tags$style(type="text/css", "label { display: inline; }"),
              tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }'),
              tags$style(type="text/css", ".jslider { max-width: 250px; }"),
              tags$style(type='text/css', ".well { max-width: 300px; }"),#class of the from inside sidebarPanel
              tags$style(type='text/css', ".span4 { max-width: 400px; }"),#span4 is the span of sidebarPanel (span8 is for the mainPanel)
              tags$style(type='text/css', "#complementinfos { width: 150px; }"),
              tags$style(type='text/css', "#CVk { width: 150px; }"),
              tags$style(type='text/css', "select#display { width: 150px; }"),
              tags$style(type='text/css', "#mainInputs {margin : 0px 0px 4px 0px; }"),
              tags$script(type="text/javascript",src="js/scripts.js")
            ) 
            
            
      ,h5(HTML("Paramètres de l'échantillonnage"))
            ,sliderInput("n",HTML("Nombre d'individus par échantillon : n"),min = 5,max = 200,value =50, step=5)
            ,sliderInput("ns",HTML("Nombre d'échantillons prélevés"), min = 1, max = 200, value = 1)
            
      ,h5(HTML("Paramètres de la distribution théorique"))
            ,selectInput("dist", " ",
                         choices = c ("Normale" = "DN"
                                      ,"Log-Normale" = "DLN"
                                      ,"Uniforme" = "DU"
                                      ,"Exponentielle" = "DE"
                                      ,"Chi-carree" = "DC"
                                      ,"Fisher" = "DF"
                                      ,"Bimodale"= "DB")),
            
#Normale
           conditionalPanel(condition = "input.dist == 'DN'" 
              ,HTML("&mu;"),sliderInput("mx","" , min = 5, max = 15, value = 10, step = 0.1)
              ,HTML("&sigma;"),sliderInput("sx","", min = 0.5, max = 3.5, value = 2, step = 0.1)
              )
                        
#Log-Normale
          ,conditionalPanel(condition = "input.dist == 'DLN'" 
              ,HTML("&mu;"),sliderInput("lmx","" , min = 5, max = 15, value = 10, step = 0.1) 
              ,HTML("&sigma;"),sliderInput("lsx","", min = 0.5, max = 3.5, value = 2, step = 0.1)
              )
            
#Uniforme          
          ,conditionalPanel(condition = "input.dist == 'DU'" 
             ,HTML("&theta;<sub>2</sub>") ,sliderInput("b", "", min = 1, max = 20, value = 20, step = 0.1)
             )
            
#Exponentielle          
          ,conditionalPanel(condition = "input.dist == 'DE'" 
             ,HTML("&lambda;"),sliderInput("Lambda", "", min = 0.1, max = 10, value = 2, step = 0.1)
             )

#Chi-carrée         
          ,conditionalPanel(condition = "input.dist == 'DC'"
             ,HTML("&nu;"),sliderInput("df", "", min = 1, max = 20, value = 5, step = 1)
             )
            
#Fisher          
          ,conditionalPanel(condition = "input.dist == 'DF'" 
             ,HTML("&nu;<sub>1</sub>"),sliderInput("df1", "", min = 1, max = 50, value = 5, step = 1) 
             ,HTML("&nu;<sub>2</sub>"),sliderInput("df2", "", min = 1, max = 100, value = 20, step = 1)
             )
            
#Bimodale
          ,conditionalPanel(condition="input.dist=='DB'" 
             ,HTML("&mu;<sub>1</sub>"),sliderInput("m1", "", min=8, max=12, value=8, step=0.1)
             ,HTML("&mu;<sub>2</sub>"),sliderInput("m2", "", min=1, max=5, value=4, step=0.1)
             ,HTML("&sigma;<sub>1</sub>"),sliderInput("sd1", "", min=1, max=2, value=1.5, step=0.01)
             ,HTML("&sigma;<sub>2</sub>"),sliderInput("sd2", "", min=1, max=2, value=1.1, step=0.01)  
             )
            
          
         ,h5("Paramètres graphiques :"),
            
            HTML ("Afficher :"),
            br(),
            checkboxInput("empPl",HTML("Statistiques descriptives"),TRUE),
            br(),
            checkboxInput("showreality",HTML("Distribution théorique d'origine"),TRUE),
            br(),
            checkboxInput("showNdensity", HTML("Distribution d'échantillonnage des données"), FALSE),
            br(),
            checkboxInput("showMdensity", HTML("Ajustement des moyennes d'échantillonnage par la normale"), FALSE),
            br(),
            br(),
            selectInput("range", HTML("Choix de l'étendue en abscisse"),
                                         choices = c ("Identique partout" = "SameRange"
                                                      ,"Spécifique au graphe" = "DifRange")),
            br(),                                       
                            
                            
#Normale
            conditionalPanel(condition = "input.dist == 'DN'&& input.range =='SameRange'"  
                             ,sliderInput("rangeXdn", "",
                                          min = -10, max = 40, value = c(0,20))),
          
            conditionalPanel(condition = "input.dist == 'DN' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsdn", "Observations",
                                          min = -10, max = 40, value = c(0,20))
                             ,sliderInput("rangeXbardn", "Moyennes",
                                          min = 0, max = 20, value = c(8,12))),
#Log-normale            
            conditionalPanel(condition = "input.dist == 'DLN'&& input.range =='SameRange'"   
                             ,sliderInput("rangeXdln", "",
                                          min = -10, max = 40, value = c(1,20))),
            
#Uniforme            
            conditionalPanel(condition = "input.dist == 'DU'&& input.range =='SameRange'"   
                             ,sliderInput("rangeXdu", HTML("Choix de l'étendue en abscisse"),
                                          min = -5, max = 25, value = c(-1,21))),
            
            conditionalPanel(condition = "input.dist == 'DU' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsdu", "Observations",
                                          min = -5, max = 25, value = c(-1,21))
                             ,sliderInput("rangeXbardu", "Moyennes",
                                          min = 0, max = 20, value = c(7,13))),
#Exponentielle            
            conditionalPanel(condition = "input.dist == 'DE'&& input.range =='SameRange'"  
                             ,sliderInput("rangeXde", HTML("Choix de l'étendue en abscisse"),
                                          min = -5, max = 20, value = c(0,5))),
            
            conditionalPanel(condition = "input.dist == 'DE' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsde", "Observations",
                                          min = -5, max = 20, value = c(0,5))
                             ,sliderInput("rangeXbarde", "Moyennes",
                                          min = -1, max = 5, value = c(0,1))),
#Chi-carrée            
            conditionalPanel(condition = "input.dist == 'DC'&& input.range =='SameRange'"  
                             ,sliderInput("rangeXdc", HTML("Choix de l'étendue en abscisse"),
                                          min = -5, max = 60, value = c(0,20))),
            conditionalPanel(condition = "input.dist == 'DC' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsdc", "Observations",
                                          min = -5, max = 60, value = c(0,20))
                             ,sliderInput("rangeXbardc", "Moyennes",
                                          min = 0, max = 25, value = c(3,7))),
#Fisher            
            conditionalPanel(condition = "input.dist == 'DF'&& input.range =='SameRange'"  
                             ,sliderInput("rangeXdf", HTML("Choix de l'étendue en abscisse"),
                                          min = -5, max = 10, value = c(0,5))),
            conditionalPanel(condition = "input.dist == 'DF' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsdf", "Observations",
                                          min = -5, max = 10, value = c(0,5))
                             ,sliderInput("rangeXbardf", "Moyennes",
                                          min = 0, max = 5, value = c(0,2))),
#Bimodale            
            conditionalPanel(condition="input.dist=='DB'&& input.range =='SameRange'"  
                             ,sliderInput("rangeXdb", HTML("Choix de l'étendue en abscisse"),
                                          min=-20, max=40, value=c(0,15))),
            
            selectInput("display", "Display :",
                        list("Defaut" = "default", 
                             "1024x768" = "1024",
                             "800x600" = "800")),
            HTML('<hr style="border:1px solid #ccc;"/>'),
            HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="img/cc_by_80x15.png" /></a> Cette oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
            HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/" target="_blank">Github</a></p>')
            
            
            )
        ), 
          
          mainPanel(
            HTML("<div id='mainInputs'>"),
            actionButton("takesample","Echantillonner"),actionButton("reset","Reset"),checkboxInput("visM",HTML("Plein écran"),FALSE), 
            HTML("</div>"),            
            plotOutput("doublePlot", height = "auto")
                    )
                       ))