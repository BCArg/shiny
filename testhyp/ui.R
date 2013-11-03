library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Test d'hypothèses de comparaison unidirectionelle de moyennes : les bases illustrées"),
  
  sidebarPanel(
    tags$head(
        #tags$style(type="text/css", "select { width: 80px; }"),
        #tags$style(type='text/css', ".well { max-width: 400px; }"),#class of the from inside sidebarPanel
        #tags$style(type='text/css', ".span4 { max-width: 400px; }"),#span4 is the span of sidebarPanel (span8 is for the mainPanel)
        #tags$style(type="text/css", ".jslider { max-width: 200px; }")
        tags$style(type="text/css", "label { display: inline; }"),
        tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }')
      ),
      
    #HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML(" &mu;<sub>0</sub> : moyenne de H<sub>0</sub> :"),#Label put outside of sliderInput because HTML is not rendered inside sliderInput label
    sliderInput("mx0", "" ,min = 1,max = 100,value = 40, step=0.5),
    HTML(" &mu;<sub>1</sub> : moyenne de H<sub>1</sub> :"),
    sliderInput("mx1","",min = 1,max = 100,value = 50, step=0.5),
    HTML(" &sigma; : &eacute;cart-type de la population d'origine : "),
    sliderInput("sx","",min = 1,max = 25,value = 10, step=1),
    #sliderInput("sx1","Ecart-type de H1 : σ1",min = 1,max = 25,value = 10, step=1),
    sliderInput("n","n : nombre d'individus par échantillon :",min = 2,max = 25,value = 4, step=1),
    #HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML("Confiance (1-&alpha;) :"),
    sliderInput("confidence","",min = 0.5,max = 1,value = 0.95, step=0.005),
    h5("Dans les modèles H0 et H1, identifier : "),
    span("Par des labels :"),
    checkboxInput("showmu", HTML(" &mu;<sub>r</sub> | "), TRUE),
    checkboxInput("showmu0", HTML(" &mu;<sub>0</sub> | "), TRUE),
    checkboxInput("showmu1", HTML(" &mu;<sub>1</sub> | "), TRUE),
    checkboxInput("alphabetalabels", HTML(" &alpha;, 1-&alpha;, &beta; et 1-&beta;"), TRUE),
    br(),
    span("Par des surfaces :"),
    checkboxInput("showalphaarea", HTML(" &alpha; | "), TRUE),
    checkboxInput("showbetaarea", HTML(" &beta; | "), TRUE),
    checkboxInput("showconfidencearea", HTML(" 1- &alpha; | "), TRUE),
    checkboxInput("showpowerarea", HTML(" 1-&beta; | "), TRUE),
    #checkboxInput("alphabetaproject", " Masquer la projection de la valeur seuil pour α de H0 sur H1", FALSE),
    HTML('<hr style="border:1px solid #ccc;"/>'),

    actionButton("takeech","Echantillonner"),
    actionButton("reset","Reset"),
    radioButtons("truehyp", "Modèle considéré comme correct :",
                 list( "H1" = "h1",
                      "H0" = "h0")),
    h5("Pour l'échantillon, afficher : "),
    checkboxInput("showmean",HTML("&nbsp;&nbsp;x&#772; | "), TRUE),
    checkboxInput("showpvaluearea",HTML(" La p-valeur de&nbsp;x&#772; | "), TRUE),
    checkboxInput("showboxplot", " Un boxplot", FALSE),
    br(),
    checkboxInput("showicz", HTML(" IC pour µ<sub>1</sub> à 1-&alpha; pour &sigma;&sup2; connue | "), FALSE),
    checkboxInput("showict", HTML(" IC pour µ<sub>1</sub> à 1-&alpha; pour &sigma;&sup2; inconnue"), FALSE),
    h5("Gestion des graphiques : "),
    checkboxInput("hideh1", HTML(" Masquer graph de H<sub>1</sub> seule | "), TRUE),
    checkboxInput("h1overh0", HTML(" Supperposer H<sub>1</sub> à H<sub>0</sub>"), TRUE),
    br(),
    checkboxInput("showrhotrend", HTML(" Montrer l'évolution du %RH<sub>0</sub> | "), TRUE),
    checkboxInput("freezeyaxis", " Fixer la densité max à 0.2 ", FALSE),#(permet de mieux visualiser les changement de forme des distributions)
    #checkboxInput("putrealitytotop", " Afficher la distribution Realité au dessus", FALSE),
    br(),
    checkboxInput("showpowertrend"," Montrer la courbe de puissance", FALSE),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/"><img alt="Licence Creative Commons" style="border-width:0" src="http://i.creativecommons.org/l/by/2.0/be/80x15.png" /></a> Ce(tte) oeuvre de <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Statistical eLearning Tools</span> est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/">licence Creative Commons Attribution 2.0 Belgique</a>.'),
    HTML('<p>Détails sur l\'utilisation de cette ressource sur <a href="http://sites.uclouvain.be/selt/ressources/104123" target="_blank">Statistics eLearning Tools</a><br/> Code source disponible sur <a href="https://github.com/uclouvain-selt/shiny/tree/master/testhyp" target="_blank">Github</a></p>')
  ),
  
  mainPanel(
    plotOutput("doubleplot")
  )
))
