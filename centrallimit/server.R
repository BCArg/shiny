Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots


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
  
 bimodalDistFunc <- function (n,cpct, m1, m2, sd1, sd2) {
   y0 <- rlnorm(n,mean=m1, sd = sd1)
   y1 <- rlnorm(n,mean=m2, sd = sd2)
   
   flag <- rbinom(n,size=1, prob=cpct)
   y <- y0*(1 - flag) + y1*flag}
 
  getSamples<-reactive({
    if(input$takesample > rv$last.takesample.value && rv$lastAction == "takesample"){
      return(isolate({#Now do the expensive stuff
        samples<-list()
        for (i in 1:input$ns){
          if (input$dist == "DN"){samples[[i]]<-rnorm(input$n)}#cr?ee n valeurs al?atoires N(0;1)
          if (input$dist == "DU"){samples[[i]]<-runif (input$n)}
          if (input$dist == "DC"){samples[[i]]<-rchisq(input$n, df = input$df)}
          if (input$dist == "DF"){samples[[i]]<-rf(input$n,df1 = input$df1,df2 = input$df2)}
          if (input$dist == "DE"){samples[[i]]<-rexp(input$n, rate = input$Lambda)}
          if (input$dist == "DB"){samples[[i]]<-c(rnorm(input$n/2,input$m1, input$sd1), rnorm(input$n/2, input$m2, input$sd2))}
          }
        return(samples)
      }))
    } else {
      return(NULL)
    }})
  
 
 getPlotHeight <- function() {
   if(input$display=="default") {
     unit.height<-250 #cannot be auto because height is already "auto" in ui and double auto = conflict
   }
   if(input$display=="1024") {
     unit.height<-180
   }
   if(input$display=="800") {
     unit.height<-140 
   }
   return(2*unit.height)
 }
 
 getPlotWidth <- function() {
   if(input$display=="default") {
     full.plot.width<-1310-400#"auto"
   }
   if(input$display=="1024") {
     full.plot.width<-900-400
   }
   if(input$display=="800") {
     full.plot.width<-700-400
   }
   if(input$visM && input$display!="default"){
     full.plot.width<-full.plot.width+400
   }
   return(full.plot.width)
 }
 
  
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
    cv$samples.x.sd<-list()
    
    
    cv$n.samples<-length(rv$samples.z)
      
    
    if(cv$n.samples>0){
      for(i in 1:cv$n.samples){
        if (input$dist == "DN"){cv$samples.x[[i]]<-round((rv$samples.z[[i]]*v$sx)+v$mx,2)}
        if (input$dist == "DU"){cv$samples.x[[i]]<-round(rv$samples.z[[i]]*v$b,2)}
        if (input$dist == "DE"){cv$samples.x[[i]]<-round(rv$samples.z[[i]],2)}
        if (input$dist == "DC"){cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}
        if (input$dist == "DF"){cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}        
        if (input$dist == "DB"){cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}
        cv$samples.x.m[[i]]<-round(mean(cv$samples.x[[i]]),2)
        cv$samples.x.sd[[i]]<-round(sd(cv$samples.x[[i]]),4)}
  

    ## Last takesample value
    rv$last.takesample.value<-v$takesample
  return(cv)
      
  }
    
  })
    
output$doublePlot <- renderPlot({

v <- getInputValues ()
cv <- getComputedValues ()
  
samples.obs <- unlist(cv$samples.x)

samples.m <-unlist(cv$samples.x.m)

par(mfcol = c(2,2))
m<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
layout(m)



######Output 1 : THEORETICAL DISTRIBUTION#####

getY <-reactive({
  n <- 1000000
  if (input$dist=="DN"){
    set.seed(1)
    normData<-rnorm(n, mean=input$mx, sd=input$sx)
    d<-density(normData)}
  
  if (input$dist=="DU"){
    set.seed(1)
    unifData<-runif(n, min=0, max=input$b)
    d<-density(unifData)}
  
  if (input$dist=="DE"){
    set.seed(1)
    expData<-rexp(n, rate=input$Lambda)
    d<-density(expData)}
  
  if (input$dist=="DC"){
    set.seed(1)
    chisqData<-rchisq(n, df=input$df)
    d<-density(chisqData)}
  
  if (input$dist == "DF"){
    set.seed(1)
    fishData<-rf(n, df1=input$df1, df2=input$df2)
    d<-density(fishData)}
  
  if (input$dist == "DB"){
    set.seed(1)
    bimodalData<- c(rnorm(n/2,input$m1, input$sd1), rnorm(n/2, input$m2, input$sd2))
    d <- density(bimodalData)}
  return (d)
})


dens <- getY()
plot(dens, xlab="",bty="n",main=HTML("Distribution théorique"),type='l')


###### Output 2 : statistiques descriptives #####

if(is.null(samples.obs )){
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
}

else{ 
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
}  


###### Output 3 : HIST SAMPLE OBSERVATIONS #####

if(is.null(samples.obs)){
  Y <- c()
  X <-c()
  plot(X, Y, main=HTML("Histogramme des données d'échantillonnage"), xlim = c(0,20), ylim = c(0,10), xlab ="", bty="n", ylab = "") 
  mtext(bquote(k == .(0)), side = 3, adj = 0, cex = 1)#afficher le nombre d'échantillons
}
else{
  hist(samples.obs, freq = TRUE, xlab = "",breaks = 50, col = 'grey',main = HTML("Histogramme des données d'échantillonnage"))
  mtext(bquote(k == .(cv$n.samples)), side = 3, adj = 0, cex = 1.2)
}        


##### Output 4 : HIST SAMPLE MEANS#######

if(is.null(samples.obs)){
  Y <- c()
  X <-c()
  plot(X, Y, main=HTML("Histogramme des moyennes d'échantillonnage"), xlim = c(0,20), ylim = c(0,10), xlab ="", ylab = "", bty="n") 
  mtext(bquote(k == .(0)), side = 3, adj = 0, cex = 1)
}
else{
  h<-hist(samples.m, freq = TRUE, breaks=seq(0,20,0.1), xlab ="", main=HTML("Histogramme des moyennes d'échantillonnage"), col = 'grey')
}   

#afficher la densité normale sur l'histogramme (option)  
  if(input$showNdensity && !is.null(cv$n.samples)){  
  lim_inf <- min (samples.m)-1
  lim_sup <- max(samples.m)+1
  xfit<-seq(lim_inf,lim_sup,length=100) 
  yfit<-dnorm(xfit,mean=mean(samples.m),sd=sd(samples.m))
  yfit <- yfit*diff(h$mids[1:2])*length(samples.m) 
  lines(xfit, yfit, col="blue", type = 'l',lwd=2)
}

        
       },height = getPlotHeight, width=getPlotWidth)
  
 
    
  })
