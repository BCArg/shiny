Sys.setlocale("LC_ALL", "fr_FR.UTF-8")
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
    if(input$takeech == 0){# If takeech button is not pressed, don't do anything
      return(NULL) 
    } 
    ## List of things to do if takeech button is pressed ##
    samples<-list()
    if (input$dist == "DN"){
	for (i in 1:input$ntirages){
	  samples[[i]]<-rnorm(input$n, mean = input$mean, sd = input$sd)
	}
      return(samples)
      }
    if (input$dist == "DU"){
	for (i in 1:input$ntirages){
	  samples[[i]]<-runif (input$n, min = input$a, max = input$b)
	}
      return(samples)
    }
    if (input$dist == "DC"){
	for (i in 1:input$ntirages){
	  samples[[i]]<-rchisq(input$n, df = input$df)
	}
      return(samples)
    }
    if (input$dist == "DF"){
	for (i in 1:input$ntirages){
	  samples[[i]]<-rf(input$n,df1 = input$df1,df2 = input$df1)
	}
      return(samples)
    }
    if (input$dist == "DE"){
	for (i in 1:input$ntirages){
	  samples[[i]]<-rexp(input$n, rate = input$rate)
	}
      return(samples)
    }
    if (input$dist == "DG"){
	for (i in 1:input$ntirages){
	  samples[[i]]<-rgamma(input$n, shape = input$rate2, rate = input$scale)
	}
      return(samples)
    }
       
  })
  
    
  output$doublePlot <- renderPlot({
      
    if (rv$lastAction=='reset'){
      getech <- NULL
      getech.m <-NULL
      SP$l.sample.obs <<-list()
      SP$l.sample.means<<-list()
      SP$n.ech <<-0
    }
     
    if (rv$lastAction=='takeech'){
	samples<-list()
        samples.m<-list()
        samples <-getech()
        for(i in 1:length(samples)){
	  samples.m[[i]]<-mean(samples[[i]])
        }
	SP$l.sample.obs<<-c(SP$l.sample.obs, samples)
        SP$l.sample.means<<-c(SP$l.sample.means, samples.m)

        SP$n.ech <<-SP$n.ech + input$ntirages
        
        par(mfrow = c(3,1))
        
        x.lim<-20
	if(max(unlist(SP$l.sample.obs))>20){
	  x.lim<-ceiling(max(unlist(SP$l.sample.obs)))
	}
	######THEORETICAL DISTRIBUTION#####
	X = seq(0,x.lim, length = 1000)
	Y<-getY()
	par(bty="n")
	plot(X,Y, type = "l",ylab="density", xlab = "", main = "Distribution théorique")

        ######HIST SAMPLE OBSERVATIONS#####
        
        ech.obs<-unlist(SP$l.sample.obs)
        hist(ech.obs, xlim = c(0,x.lim),breaks=seq(0,x.lim,by=0.1),xlab = "Histogramme des données d'échantillonnage", col = 'grey',main = "", cex = 1.5)
        mtext(bquote(nsamples == .(SP$n.ech)), side = 3, adj = 0, cex = 1)#afficher le nombre d'échantillons
        
        
        #####HIST SAMPLE MEANS#######
        
        ech.m <- unlist(SP$l.sample.means)
        h<-hist(ech.m, xlim = c(0,x.lim),breaks=seq(0,x.lim,by=0.1),xlab = "Histogramme des moyennes d'échantillonnage", main = '', col = 'grey', cex = 1.5)#xlim = c(0,20)
        
        # afficher les moyennes : 
        #mtext(bquote(bar(x) == .(round(getech.m,2))), side = 3, adj = 1, cex = 1)
        
        #afficher la densité normale sur l'histogramme (option)  
        if(input$showNdensity){  
          lim_inf <- min (ech.m)-1
          lim_sup <- max(ech.m)+1
          xfit<-seq(lim_inf,lim_sup,length=100) 
          yfit<-dnorm(xfit,mean=mean(ech.m),sd=sd(ech.m))
          yfit <- yfit*diff(h$mids[1:2])*length(ech.m) 
          lines(xfit, yfit, col="blue", type = 'l',lwd=2)
          #mtext(bquote(bar(x) == .(round(getech.m,2))), side = 3, adj = 1,  cex = 1)
        }
        
        
       }
        
        },height = 600)
  
  })


