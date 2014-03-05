library(shiny)


shinyServer(function(input, output){ 

  
 rv <- reactiveValues()  # Create a reactiveValues object, to let us use settable reactive values
  
  rv$last.takesample.value<-0
  rv$samples.z<-list()

  rv$lastAction <- 'none'# To start out, lastAction == NULL, meaning nothing clicked yet
  
  # An observe block for each button, to record that the action happened
  observe({
    if (input$takesample != 0) {
      rv$lastAction <- 'takesample'
    }
  })
  observe({
    if (input$reset != 0) {
      rv$lastAction <- 'reset'
      rv$last.takesample.value<-0
      rv$samples.z<-list()
     }
  })
  
  
 
  getSamples<-reactive({
    if(input$takesample > rv$last.takesample.value && rv$lastAction == "takesample"){
      return(isolate({#Now do the expensive stuff
        samples<-list()
        for (i in 1:input$ns){
          if (input$dist == "DN"){samples[[i]]<-rnorm(input$n)}#créee n valeurs aléatoires N(0;1)
          #if (input$dist == "DU"){samples[[i]]<-runif (input$n, min = input$a, max = input$b)}
          #if (input$dist == "DC"){samples[[i]]<-rchisq(input$n, df = input$df)}
          #if (input$dist == "DF"){samples[[i]]<-rf(input$n,df1 = input$df1,df2 = input$df1)}
          #if (input$dist == "DE"){samples[[i]]<-rexp(input$n, rate = input$lambda)}
          #if (input$dist == "DG"){samples[[i]]<-rgamma(input$n, Alpha = input$rate2, Beta = input$scale)}
        }
        return(samples)
      }))
    } else {
      return(NULL)
    }})
  
 
  getInputValues<-reactive({
    return(input)#collect all inputs
  })
  
  getComputedValues<-reactive({
    samples<-list()
    samples<-getSamples()
    rv$samples.z<-c(rv$samples.z,samples)
    
       
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
  
    
    ## Computation of sample related values ##
    
    cv$samples.x<-list()
    cv$samples.x.m<-list()
    
    cv$n.samples<-length(rv$samples.z)
      
    
    if(cv$n.samples>0){
      for(i in 1:cv$n.samples){
        cv$samples.x[[i]]<-round((rv$samples.z[[i]]*v$sx)+v$mx,2)#Then sample values are compute with theoritical mean and standard deviation
        cv$samples.x.m[[i]]<-round(mean(cv$samples.x[[i]]),2)#means of samples
            
      }}
  

    ## Last takesample value
    rv$last.takesample.value<-v$takesample
  return(cv)
      
  })
    
  
    
output$doublePlot <- renderPlot({

  
  
  
######THEORETICAL DISTRIBUTION#####
v <- getInputValues ()
cv <- getComputedValues ()
  
samples.obs <- unlist(cv$samples.x)

samples.m <-unlist(cv$samples.x.m)

par(mfrow = c(3,1))



# axe des X
X = seq(0,20, length = 1000) 
#densité Y    
getY <-reactive({
  if (input$dist == "DN")
    return(dnorm(X, mean = input$mx, sd = input$sx))
  #    if (input$dist == "DU")
  #     return(dunif (X, min = input$a, max = input$b))
  #    if (input$dist == "DC")
  #     return (dchisq(X, df = input$df))
  #    if (input$dist == "DF")
  #     return(df(X,df1 = input$df1,df2 = input$df2))
  #    if (input$dist == "DE")
  #     return (dexp(X, rate = input$Lambda))
  #   if (input$dist == "DG")
  #    return(dgamma(X, shape = input$rate2, rate = input$scale))
})


x.lim<-20
if(max(samples.obs)>20){
  x.lim<-ceiling(max(samples.obs))
}


X<- seq(0,x.lim, length = 1000)
	Y<-getY()
	par(bty="n")
	plot(X,Y, type = "l",ylab="density", xlab = "", main = "Distribution théorique")

  
  
######HIST SAMPLE OBSERVATIONS#####
       
        hist(samples.obs, freq = TRUE, xlim = c(0,x.lim),breaks=seq(0,x.lim,by=0.1),xlab = "Histogramme des données d'échantillonnage", col = 'grey',main = "", cex = 1.5)
        mtext(bquote(nsamples == .(cv$n.samples)), side = 3, adj = 0, cex = 1)#afficher le nombre d'échantillons
        
        
#####HIST SAMPLE MEANS#######
        h<-hist(samples.m, freq = TRUE, xlim = c(0,x.lim),breaks=seq(0,x.lim,by=0.1),xlab = "Histogramme des moyennes d'échantillonnage", main = '', col = 'grey', cex = 1.5)#xlim = c(0,20)
        
        # afficher les moyennes : 
        #mtext(bquote(bar(x) == .(round(getSamples.m,2))), side = 3, adj = 1, cex = 1)
        
        #afficher la densité normale sur l'histogramme (option)  
        if(input$showNdensity){  
          lim_inf <- min (samples.m)-1
          lim_sup <- max(samples.m)+1
          xfit<-seq(lim_inf,lim_sup,length=100) 
          yfit<-dnorm(xfit,mean=mean(samples.m),sd=sd(samples.m))
          yfit <- yfit*diff(h$mids[1:2])*length(samples.m) 
          lines(xfit, yfit, col="blue", type = 'l',lwd=2)
          #mtext(bquote(bar(x) == .(round(getSamples.m,2))), side = 3, adj = 1,  cex = 1)
        }
        
        
       },height = 600)
  
 
    
  })






