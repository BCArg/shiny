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
              tags$style(type='text/css', "#mx, #sx, #df1, #df2, #m1, #sd1, #m2, #sd2 { width: 30px; }"),
              tags$style(type='text/css', "select#display { width: 150px; }"),
              tags$style(type='text/css', "#mainInputs {margin : 0px 0px 4px 0px; }"),
              tags$style(type='text/css', "input[type=number] {width:50px;}"),
              tags$script(type="text/javascript",src="js/scripts.js")
            ) 
            
            
      ,h5(HTML("Paramètres de l'échantillonnage"))
            ,sliderInput("n",HTML("Nombre d'individus par échantillon : n"),min = 5,max = 200,value =100, step=5)
            ,sliderInput("ns",HTML("Nombre d'échantillons prélevés"), min = 1, max = 200, value = 1)
            
      ,h5(HTML("Paramètres de la distribution théorique"))
            ,selectInput("dist", " ",
                         choices = c ("Normale" = "DN"
                                      ,"Uniforme discrète" = "DUD"
                                      ,"Uniforme continue" = "DU"
                                      ,"Exponentielle" = "DE"
                                      ,"Chi-carree" = "DC"
                                      ,"Fisher" = "DF"
                                      ,"Bimodale"= "DB"
                                      ,"Binomiale" = "DBin")),  #,"Log-Normale" = "DLN"
        
#Normale
           conditionalPanel(condition = "input.dist == 'DN'" 
              ,numericInput('mx', HTML("&mu; : "), min=5, max=15, value=10, step=0.1)
              ,numericInput('sx', HTML("&sigma; : "), min=0.5, max=3.5, value=2, step=0.1)
              #,HTML("&mu;"),sliderInput("mx","" , min = 5, max = 15, value = 10, step = 0.1)
              #,HTML("&sigma;"),sliderInput("sx","", min = 0.5, max = 3.5, value = 2, step = 0.1)
              )
                        
#Binomiale
           ,conditionalPanel(condition = "input.dist == 'DBin'" 
              ,p(HTML("n est défini par le 1er slider des paramètres de l'échantillonnage"))
              ,numericInput('p', HTML("p : "), min=0, max=1, value=0.5, step=0.01)
              )
            
            
#Log-Normale
          #,conditionalPanel(condition = "input.dist == 'DLN'" 
          #    ,HTML("&mu;"),sliderInput("lmx","" , min = 5, max = 15, value = 10, step = 0.1) 
          #    ,HTML("&sigma;"),sliderInput("lsx","", min = 0.5, max = 3.5, value = 2, step = 0.1)
          #    )

#Uniforme discrète          
          ,conditionalPanel(condition = "input.dist == 'DUD'" 
             ,sliderInput("RUD", "", min = 1, max = 12, value = c(1,6))
            )            
            
            
#Uniforme continue          
          ,conditionalPanel(condition = "input.dist == 'DU'" 
              ,p(HTML("&theta;<sub>1</sub> est fixé à 0"))
              ,numericInput('b', HTML("&theta;<sub>2</sub> : "), min=1, max=20,  value=20, step=0.1)
              #,HTML("&theta;<sub>2</sub>") ,sliderInput("b", "", min = 1, max = 20, value = 20, step = 0.1)
             )
            
#Exponentielle          
          ,conditionalPanel(condition = "input.dist == 'DE'" 
              ,numericInput('Lambda', HTML("&lambda; : "), min=0.1, max=10, value=2, step=0.1)
              #,HTML("&lambda;"),sliderInput("Lambda", "", min = 0.1, max = 10, value = 2, step = 0.1)
             )

#Chi-carrée         
          ,conditionalPanel(condition = "input.dist == 'DC'"
              ,numericInput('df', HTML("&nu; : "), min=1, max=20, value=5, step=1)
              #,HTML("&nu;"),sliderInput("df", "", min = 1, max = 20, value = 5, step = 1)
             )
            
#Fisher          
          ,conditionalPanel(condition = "input.dist == 'DF'" 
              ,numericInput('df1', HTML("&nu;<sub>1</sub> : "), min=1, max=50, value=5, step=1)
              ,numericInput('df2', HTML("&nu;<sub>2</sub> : "), min=1, max=100, value=20, step=1)
              #,HTML("&nu;<sub>1</sub>"),sliderInput("df1", "", min = 1, max = 50, value = 5, step = 1) 
              #,HTML("&nu;<sub>2</sub>"),sliderInput("df2", "", min = 1, max = 100, value = 20, step = 1)
             )
            
#Bimodale
          ,conditionalPanel(condition="input.dist=='DB'" 
             ,numericInput('m1', HTML("&mu;<sub>1</sub> : "), min=8, max=12, value=8, step=0.1)
             ,numericInput('sd1', HTML("&sigma;<sub>1</sub> : "), min=1, max=2, value=1.5, step=0.01)
             ,br()
             ,numericInput('m2', HTML("&mu;<sub>2</sub> : "), min=1, max=5, value=4, step=0.1)
             ,numericInput('sd2', HTML("&sigma;<sub>2</sub> : "), min=1, max=2, value=1.1, step=0.01) 
             #,HTML("&mu;<sub>1</sub>"),sliderInput("m1", "", min=8, max=12, value=8, step=0.1)
             #,HTML("&mu;<sub>2</sub>"),sliderInput("m2", "", min=1, max=5, value=4, step=0.1)
             #,HTML("&sigma;<sub>1</sub>"),sliderInput("sd1", "", min=1, max=2, value=1.5, step=0.01)
             #,HTML("&sigma;<sub>2</sub>"),sliderInput("sd2", "", min=1, max=2, value=1.1, step=0.01)  
             )
            
          
         ,h5("Paramètres graphiques"),
            
            HTML ("Afficher :"),
            br(),
            checkboxInput("empPl",HTML("Statistiques descriptives"),TRUE),
            br(),
            checkboxInput("showreality",HTML("Distribution théorique d'origine"),TRUE), 
            br(),
            conditionalPanel(condition = "input.dist != 'DBin'", 
            checkboxInput("showMdensity", HTML("Densité normale sur l'histogramme des moyennes"), FALSE)
            ,br()),         
            conditionalPanel(condition = "input.dist == 'DBin'", 
            checkboxInput("showNdensity", HTML("Densité normale sur les histogrammes"), FALSE)
            ,br()),
            br(),
            selectInput("range", HTML("Choix de l'étendue en abscisse"),
                                         choices = c ("Spécifique au graphe" = "DifRange"
                                                      ,"Identique pour tous les graphes" = "SameRange"
                                                      )),
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

#Binomiale
            conditionalPanel(condition = "input.dist == 'DBin'&& input.range =='SameRange'"  
                             ,sliderInput("rangeXdbin", "",
                                          min = 0, max = 200, value = c(0,100))),
            
            conditionalPanel(condition = "input.dist == 'DBin' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsdbin", "Observations",
                                          min = 0, max = 200, value = c(0,100))
                             ,sliderInput("rangeXbardbin", "Moyennes",
                                          min = 0, max = 1, value = c(0,1))),
            
            
            #Log-normale            
            #conditionalPanel(condition = "input.dist == 'DLN'&& input.range =='SameRange'"   
            #                 ,sliderInput("rangeXdln", "",
            #                              min = -10, max = 40, value = c(1,20))),

#Uniforme discrète          
            conditionalPanel(condition = "input.dist == 'DUD'&& input.range =='SameRange'"   
                             ,sliderInput("rangeXdud", HTML("Choix de l'étendue en abscisse"),
                                          min = 0, max = 12, value = c(1,6))),
            
            conditionalPanel(condition = "input.dist == 'DUD' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsdud", "Observations",
                                          min = 0, max = 12, value = c(1,6))
                             ,sliderInput("rangeXbardud", "Moyennes",
                                          min = 0, max = 12, value = c(1,6))),
            
#Uniforme continue           
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
                                          min = -5, max = 60, value = c(0,30))
                             ,sliderInput("rangeXbardc", "Moyennes",
                                          min = 0, max = 25, value = c(3,7))),
#Fisher            
            conditionalPanel(condition = "input.dist == 'DF'&& input.range =='SameRange'"  
                             ,sliderInput("rangeXdf", HTML("Choix de l'étendue en abscisse"),
                                          min = -5, max = 10, value = c(0,5))),
            conditionalPanel(condition = "input.dist == 'DF' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsdf", "Observations",
                                          min = -5, max = 50, value = c(0,10))
                             ,sliderInput("rangeXbardf", "Moyennes",
                                          min = 0, max = 5, value = c(0,2))),
#Bimodale            
            conditionalPanel(condition="input.dist=='DB'&& input.range =='SameRange'"  
                             ,sliderInput("rangeXdb", HTML("Choix de l'étendue en abscisse"),
                                          min=-20, max=40, value=c(0,15))),
            conditionalPanel(condition = "input.dist == 'DB' && input.range =='DifRange'" 
                             ,sliderInput("rangeObsdb", "Observations",
                                          min=-20, max=40, value=c(0,15))
                             ,sliderInput("rangeXbardb", "Moyennes",
                                          min=-0, max=10, value=c(4,8))),
            
            selectInput("display", "Display :",
                        list("Defaut" = "default", 
                             "1024x768" = "1024",
                             "800x600" = "800")),
            HTML('<hr style="border:1px solid #ccc;"/>'),
            HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="img/cc_by_80x15.png" /></a> Cette oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
            HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/214703" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/tcl" target="_blank">Github</a></p>')
            
            
            )
        ), 
          
          mainPanel(
            HTML("<div id='mainInputs'>"),
            actionButton("takesample","Echantillonner"),actionButton("reset","Reset"),checkboxInput("visM",HTML("Plein écran"),FALSE), 
            HTML("</div>"),            
            plotOutput("doublePlot", height = "auto")
                    )
                       ))