library(shiny)

#initiate global counters
SP <- list()
SP$l.sample.obs <- list()
SP$l.sample.means<-list()
SP$n.ech <-0




shinyServer(function(input, output){ 
#pour créer les graphiques des distributions théoriques : 
  # axe des X
  X = seq(0,20, length = 1000) 
  #densité Y    
  getY <-reactive({
    if (input$dist == "DN")
      return(dnorm(X, mean = input$mean, sd = input$sd))
    if (input$dist == "DU")
      return(dunif (X, min = input$a, max = input$b))
    if (input$dist == "DC")
      return (dchisq(X, df = input$df))
    if (input$dist == "DF")
      return(df(X,df1 = input$df1,df2 = input$df2))
    if (input$dist == "DE")
      return (dexp(X, rate = input$rate))
    if (input$dist == "DG")
      return(dgamma(X, shape = input$rate2, rate = input$scale))
    })
  

  
  ########PLOT DISTRIBUTIONS THEORIQUES#######
  
  output$distPlot <- renderPlot({
    Y<-getY()
    par (mfcol=c(1,2))
    par(mai=c(1,1,1,1),bty="n")
    plot(X,Y, type = "l",ylab="density", xlab = "", main = "Distribution théorique")}, height = 250)  
  
  

  
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
  
    if(input$takeech != 0){
    
      if (input$dist == "DN")
        return(rnorm(input$n, mean = input$mean, sd = input$sd))
      if (input$dist == "DU")
        return(runif (input$n, min = input$a, max = input$b))
      if (input$dist == "DC")
        return (rchisq(input$n, df = input$df))
      if (input$dist == "DF")
        return(rf(input$n,df1 = input$df1,df2 = input$df2))
      if (input$dist == "DE")
        return (rexp(input$n, rate = input$rate))
      if (input$dist == "DG")
        return(rgamma(input$n, shape = input$rate2, rate = input$scale))}
       
      })
  
    
    output$doublePlot <- renderPlot({
      
      
      if (rv$lastAction=='reset'){
        getech <- NULL
        getech.m <-NULL
        SP$l.sample.obs <<-list()
        SP$l.sample.means<<-list()
        SP$n.ech <<-0
      }
    
    #getech.exist <- length (getech)
    
    
    #  if (getech.exist && rv$lastAction=='takeech') 
     
    if (rv$lastAction=='takeech'){
        #i <<- 1 
        for (i in 1:input$ntirages){
          getech <-getech()
          getech.m<-mean(getech)
          SP$l.sample.obs<<-c(SP$l.sample.obs, list(getech))
          SP$l.sample.means<<-c(SP$l.sample.means, getech.m)
         # i <<- i+1
        }
        SP$n.ech <<-SP$n.ech + input$ntirages
        
        par(mfrow = c(1,2))
        
        ######HIST SAMPLE OBSERVATIONS#####
        
        ech.obs<-unlist(SP$l.sample.obs)
        hist(ech.obs, xlim = c(0,20), xlab = "Histogramme des données d'échantillonnage", col = 'grey',main = "", cex = 1.5)
        #afficher le nombre d'échantillons
        mtext(bquote(nsamples == .(SP$n.ech)), side = 3, adj = 0, cex = 1)
        
        
        #####HIST SAMPLE MEANS#######
        
        ech.m <- unlist(SP$l.sample.means)
        hist(ech.m, xlab = "Histogramme des moyennes d'échantillonnage", main = '', col = 'grey', cex = 1.5)
        
        #xlim = c(0,20),
        
        # afficher les moyennes : 
        #mtext(bquote(bar(x) == .(round(getech.m,2))), side = 3, adj = 1, cex = 1)
        
        
        #afficher la densité normale sur l'histogramme (option)  
        if(input$showNdensity){  
          par(mfrow = c(1,2))
          
          hist(ech.obs, xlim = c(0,20), xlab = "Histogramme des données d'échantillonnage", col = 'grey',main = "", cex = 1.5)
          mtext(bquote(nsamples == .(SP$n.ech)), side = 3, adj = 0, cex = 1)
          
          h <- hist(ech.m, xlab = "Histogramme des moyennes d'échantillonnage", col = 'grey',main = "", cex = 1.5)
          #xlim = c(0,20),
          lim_inf <- min (ech.m)-1
          lim_sup <- max(ech.m)+1
          xfit<-seq(lim_inf,lim_sup,length=100) 
          yfit<-dnorm(xfit,mean=mean(ech.m),sd=sd(ech.m))
          yfit <- yfit*diff(h$mids[1:2])*length(ech.m) 
          lines(xfit, yfit, col="blue", type = 'l',lwd=2)
          #mtext(bquote(bar(x) == .(round(getech.m,2))), side = 3, adj = 1,  cex = 1)
        }
        
        
       }
        
        },height = 250)
  
  })


