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
              tags$style(type='text/css', ".span4 { max-width: 310px; }"),#span4 is the span of sidebarPanel (span8 is for the mainPanel)
              tags$style(type='text/css', "#complementinfos { width: 150px; }"),
              tags$style(type='text/css', "#CVk { width: 150px; }"),
              tags$style(type='text/css', "select#display { width: 150px; }"),
              tags$style(type='text/css', "#mainInputs {margin : 0px 0px 4px 0px; }"),
              tags$script(type="text/javascript",src="js/scripts.js")
            ) 
            
                   
         
            ,h5(HTML("Paramètres de l'échantillonnage"))
            ,sliderInput("n",HTML("Nombre d'individus par échantillon : n"),min = 5,max = 200,value =5, step=5)
            ,sliderInput("ns",HTML("Nombre d'echantillons prélevés par simulation"), min = 1, max = 50, value = 1)
            
            
            ,h5(HTML("Paramètres de la distribution théorique"))
            ,selectInput("dist", HTML("Sélectionner le type de distribution :"),
                         choices = c ("Normale" = "DN"
                                      ,"Uniforme" = "DU"
                                      ,"Exponentielle" = "DE"
                                      ,"Chi-carree" = "DC"
                                      ,"Fisher" = "DF"
                                      ,"Bimodale"= "DB")),
            
          #  ,"Gamma" = "DG"
            
            conditionalPanel(condition = "input.dist == 'DN'" 
              ,p (HTML("X&sim;&Nu;(&mu;, &sigma;<sup>2</sup>)"))
              ,HTML("&mu;:")               
              ,sliderInput("mx","" , min = 5, max = 15, value = 10, step = 0.1) 
              ,HTML("&sigma;")          
              ,sliderInput("sx","", min = 0.5, max = 3.5, value = 2, step = 0.1))
          
          ,conditionalPanel(condition = "input.dist == 'DU'" 
             ,p (HTML("X&sim; U(&theta;<sub>1</sub>, &theta;<sub>2</sub>) &nbsp;&nbsp;avec &theta;<sub>1</sub>=0"))
             ,HTML("&theta;<sub>2</sub>") 
             ,sliderInput("b", "", min = 1, max = 20, value = 10, step = 0.1))
          
          ,conditionalPanel(condition = "input.dist == 'DE'" 
             ,p (HTML(" &sim; &Epsilon;(&lambda;)"))
             ,HTML("&lambda;") 
             ,sliderInput("Lambda", "", min = 0, max = 10, value = 2, step = 0.1))
         
          ,conditionalPanel(condition = "input.dist == 'DC'"
             ,p (HTML("X&sim; &Chi;<sup>2</sup><sub>&nu;</sub>"))
             ,HTML("&nu;") 
             ,sliderInput("df", "", min = 1, max = 20, value = 5, step = 1))
          
          ,conditionalPanel(condition = "input.dist == 'DF'", 
              sliderInput("df1", "ddl1 : ", min = 1, max = 50, value = 5, step = 1), 
              sliderInput("df2", "ddl2", min = 1, max = 100, value = 20, step = 1)),
           conditionalPanel(condition = "input.dist == 'DB'", 
                             sliderInput("m1", "mode1 : ", min = 8, max = 12, value = 10, step = 0.1),
                             sliderInput("m2", "mode2 : ", min = 1, max = 5, value = 2, step = 0.1),
                             sliderInput("sd1", "ectype1 : ", min = 1, max = 2, value = 1.5, step = 0.01),
                             sliderInput("sd2", "ectype2 : ", min = 1, max = 2, value = 1.1, step = 0.01)                             
            )
            
            
         #   conditionalPanel(condition = "input.dist == 'DG'", 
          #    sliderInput("Alpha", "parametre 1 : ", min = 0.5, max = 10 , value = 0.5, step = 0.1),
           #   sliderInput("Beta", "parametre 2 : ", min = 0.1, max = 2, value = 2, step = 0.1)),  
                                
           
            
            
            
          
            ,checkboxInput("showNdensity", HTML("Visualiser la densité normale pour la distribution d'échantillonnage de la moyenne"), FALSE)
            
            
            ,h5("Paramètres graphiques :"),
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
            actionButton("takesample","Echantillonner"),actionButton("reset","Reset"),checkboxInput("visM",HTML("Plein écran"),FALSE),p(HTML("k : Nombre total d'échantillons prélevés")),   
            HTML("</div>"),            
            plotOutput("doublePlot", height = "auto")
                    )
                       ))