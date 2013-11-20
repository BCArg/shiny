library(shiny)

#initiate global counters
SP <- list()
SP$l.sample.means<-list()
SP$n.ech <-0

shinyServer(function(input, output){ 
#pour créer les graphiques des distributions théoriques : 
  # axe des X
  X = seq(0,20, length = 1000) 
  # paramètres sur lesquels on pourra jouer (conditionnellement à la distribution considérée) 
  pN1 <-reactive({if (input$dist == "DN") return (input$mean)}) #paramètres de la Normale
  pN2 <-reactive({if (input$dist == "DN") return (input$sd)})
  pU1 <-reactive({if (input$dist == "DU") return (input$a)})    #paramètres de l'Uniforme
  pU2 <-reactive({if (input$dist == "DU") return (input$b)})
  pC  <-reactive({if (input$dist == "DC") return (input$df)})    #paramètre de la Chi-carrée
  pF1 <- reactive({if (input$dist == "DF") return (input$df1)})  #paramètres de la Fisher
  pF2 <- reactive({if (input$dist == "DF") return (input$df2)})
  pE  <- reactive({if (input$dist == "DE") return (input$rate)}) #paramètres de l'Exponentielle
  pG1 <- reactive({if (input$dist == "DG") return (input$rate2)})#paramètres de la Gamma
  pG2 <- reactive({if (input$dist == "DG") return (input$scale)})
  #densité Y    
  getY <-reactive({
    if (input$dist == "DN")
      return(dnorm(X, mean = pN1(), sd = pN2()))
    if (input$dist == "DU")
      return(dunif (X, min = pU1(), max = pU2()))
    if (input$dist == "DC")
      return (dchisq(X, df = pC()))
    if (input$dist == "DF")
      return(df(X,df1 = pF1(),df2 = pF2()))
    if (input$dist == "DE")
      return (dexp(X, rate = pE()))
    if (input$dist == "DG")
      return(dgamma(X, shape = pG1(), rate = pG2()))
    })
  
  output$distPlot <- renderPlot({
    Y<-getY()
    par(mai=c(1,1,1,1),bty="n")
    plot(X,Y, type = "l",ylab="density", xlab = "", main = "Distribution théorique")}, height = 250)  
  
  
  
  #pour échantillonner :
  
  # Create a reactiveValues object, to let us use settable reactive values
  rv <- reactiveValues()
  # To start out, lastAction == NULL, meaning nothing clicked yet
  rv$lastAction <- 'none'
  # An observe block for each button, to record that the action happened
  observe({
    if (input$takeech != 0) {
      rv$lastAction <- 'takeech'
    }
  })
  observe({
    if (input$reset != 0) {
      rv$lastAction <- 'reset'
    }
  })
  
  getech<-reactive({#créee n valeurs aléatoires quand input$takeech est implémenté (quand le bouton takeech est pressé)
    #don't do anything until after the first button is pushed
    if(input$takeech == 0)
      return(NULL)
    else {
      if (input$dist == "DN")
        return(rnorm(input$n, mean = pN1(), sd = pN2()))
      if (input$dist == "DU")
        return(runif (input$n, min = pU1(), max = pU2()))
      if (input$dist == "DC")
        return (rchisq(input$n, df = pC()))
      if (input$dist == "DF")
        return(rf(input$n,df1 = pF1(),df2 = pF2()))
      if (input$dist == "DE")
        return (rexp(input$n, rate = pE()))
      if (input$dist == "DG")
        return(rgamma(input$n, shape = pG1(), rate = pG2()))}   
      })
  
  
  

    output$histPlot <- renderPlot({
      if (rv$lastAction=='reset'){
        getech.m <-NULL
        SP$l.sample.means<<-list()
        SP$n.ech <<-0
      }
      if (rv$lastAction=='takeech') {
        getech.m<-mean(getech())
        SP$l.sample.means<<-c(SP$l.sample.means,list(getech.m))
        SP$n.ech <<- SP$n.ech + 1
             }
      
      n.ech <-SP$n.ech
      par(mai=c(1,1,1,1),bty="n")
      hist(unlist(SP$l.sample.means), xlim = c(0,20), xlab = '', col = 'grey',main = "Histogramme des moyennes d'échantillonnage")
      mtext(bquote(bar(x) == .(round(getech.m,2))), side = 3, adj = 1)
      mtext(bquote(nsamples == .(SP$n.ech)), side = 3, adj = 0)
      
      
  },height = 250)
})


