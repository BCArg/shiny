library(shiny)

#initiate global counters
SP <- list()
SP$l.sample.obs <- list()
SP$l.sample.means<-list()
SP$n.ech <-0
#n.tirages <-0


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
  #observe({
   # if (input$take25ech != 0) {
    #  rv$lastAction <- 'take25ech'
    #}
  #})
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
  
  #get25ech <- reactive ({
  #  if(input$take25ech == 0)
  #    return(NULL)
  #  else {
  #    SP$n.tirages<<-0
  #    for (n.tirages in 1:25)({
  #      if (input$dist == "DN")
  #        return(rnorm(input$n, mean = pN1(), sd = pN2()))
  #      if (input$dist == "DU")
  #        return(runif (input$n, min = pU1(), max = pU2()))
  #      if (input$dist == "DC")
  #       return (rchisq(input$n, df = pC()))
  #     if (input$dist == "DF")
  #       return(rf(input$n,df1 = pF1(),df2 = pF2()))
  #     if (input$dist == "DE")
  #       return (rexp(input$n, rate = pE()))
  #     if (input$dist == "DG")
  #       return(rgamma(input$n, shape = pG1(), rate = pG2()))   
  #     n.tirages<<-n.tirages+ 1})
  #         }
  #})
  

    output$doublePlot <- renderPlot({
      
      if (rv$lastAction=='reset'){
        getech <- NULL
        getech.m <-NULL
        SP$l.sample.obs <<-list()
        SP$l.sample.means<<-list()
        SP$n.ech <<-0
      }
      if (rv$lastAction=='takeech') {
        getech<-getech()
        getech.m<-mean(getech())
        SP$l.sample.obs<<-c(SP$l.sample.obs, list(getech))
        SP$l.sample.means<<-c(SP$l.sample.means,list(getech.m))
        SP$n.ech <<- SP$n.ech + 1
      }
    #  if (rv$lastAction=='take25ech') {
    #   getech.m<-mean(getech())
    #   SP$l.sample.means<<-c(SP$l.sample.means,list(getech.m))
    #   SP$n.ech <<- SP$n.ech + 25
    #  }
      
      
      par(mfrow = c(1,2))
      
      ######HIST SAMPLE OBSERVATIONS#####
      
      ech.obs<-unlist(SP$l.sample.obs)
      n.ech <-SP$n.ech
      hist(ech.obs, xlim = c(0,20), xlab = "Histogramme des données d'échantillonnage", col = 'grey',main = "", cex = 1.5)
      #afficher le nombre d'échantillons
      mtext(bquote(nsamples == .(SP$n.ech)), side = 3, adj = 0, cex = 1)
          
      
      #####HIST SAMPLE MEANS#######
      
      ech.m <- unlist(SP$l.sample.means)
      hist(ech.m, xlim = c(0,20), xlab = "Histogramme des moyennes d'échantillonnage", main = '', col = 'grey', cex = 1.5)
      # afficher les moyennes : 
      mtext(bquote(bar(x) == .(round(getech.m,2))), side = 3, adj = 1, cex = 1)
      
     
      #afficher la densité normale sur l'histogramme (option)  
      if(input$showNdensity){  
        par(mfrow = c(1,2))
       
        hist(ech.obs, xlim = c(0,20), xlab = "Histogramme des données d'échantillonnage", col = 'grey',main = "", cex = 1.5)
        mtext(bquote(nsamples == .(SP$n.ech)), side = 3, adj = 0, cex = 1)
        
        h <- hist(ech.m, xlim = c(0,20), xlab = "Histogramme des moyennes d'échantillonnage", col = 'grey',main = "", cex = 1.5)
        lim_inf <- min (ech.m)-1
        lim_sup <- max(ech.m)+1
        xfit<-seq(lim_inf,lim_sup,length=100) 
        yfit<-dnorm(xfit,mean=mean(ech.m),sd=sd(ech.m))
        yfit <- yfit*diff(h$mids[1:2])*length(ech.m) 
        lines(xfit, yfit, col="blue", type = 'l',lwd=2)
        mtext(bquote(bar(x) == .(round(getech.m,2))), side = 3, adj = 1,  cex = 1)
        }
      
     
      },height = 250)
  
})


