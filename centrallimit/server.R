Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots


library(shiny)
library(plotrix)


shinyServer(function(input, output){ 

  
 y.delta<-0.1
  
 rv <- reactiveValues()  # Create a reactiveValues object, to let us use settable reactive values
  
  rv$last.takesample.value<-0
  rv$samples.z<-list()

  rv$lastAction <- 'none'# To start out, lastAction == NULL, meaning nothing clicked yet
  
  rv$lastDist <- " "
   
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
    cv$samples.x<-list()
    cv$n.samples<-length(rv$samples.z)
    
    
    
    ## Computation of sample related values ##
    if(cv$n.samples>0){
      for(i in 1:cv$n.samples){
        if (input$dist == "DN"){cv$samples.x[[i]]<-round((rv$samples.z[[i]]*v$sx)+v$mx,2)}
        if (input$dist == "DU"){cv$samples.x[[i]]<-round(rv$samples.z[[i]]*v$b,2)}
        if (input$dist == "DE"){cv$samples.x[[i]]<-round(rv$samples.z[[i]],2)}
        if (input$dist == "DC"){cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}
        if (input$dist == "DF"){cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}        
        if (input$dist == "DB"){cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}
       }
  
      if (rv$lastDist!=v$dist) {
        rv$lastAction <- 'changeDist'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      ##Pour passer d'une liste à une matrice
      cv$samples.x.mat <- matrix(nrow = cv$n.samples, ncol = v$n) 
      for(i in 1:cv$n.samples){
        cv$samples.x.mat[i,] <- cv$samples.x[[i]]
      }
      
      ## Computation of descriptives
      cv$samples.x.m.vec<-c() # vector of mean values, each line a sample
      cv$samples.x.sd.vec<-c()
      cv$samples.x.m.vec<-round(apply(cv$samples.x.mat,1,mean),2)#means of samples
      cv$samples.x.sd.vec<-round(apply(cv$samples.x.mat,1,sd),2)#sds of samples
      
      ## Define subset to plot
      cv$samples.x.n.toshow<-0
      cv$samples.x.from<-1
      if(cv$n.samples>5){
          cv$samples.x.from<-cv$n.samples-5+1
        }    
      cv$samples.x.to<-cv$n.samples
        
      cv$samples.x.mat.toshow<-matrix(cv$samples.x.mat[cv$samples.x.from:cv$samples.x.to,],ncol=v$n)
      cv$samples.x.m.vec.toshow<-cv$samples.x.m.vec[cv$samples.x.from:cv$samples.x.to]
      cv$samples.x.sd.vec.toshow<-cv$samples.x.sd.vec[cv$samples.x.from:cv$samples.x.to]
      cv$samples.x.i.vec.toshow<-c(cv$samples.x.from:cv$samples.x.to)
      cv$samples.x.n.toshow<-length(cv$samples.x.mat.toshow[,1])
      cv$samples.y.mat.toshow<-c()
        
        if(cv$samples.x.n.toshow>0){
        cv$samples.y.mat.toshow<-matrix(rep(y.delta/(5+1)*c(1:cv$samples.x.n.toshow),nrow=length(cv$samples.x.mat.toshow[,1])))
        }
      }  
      
      ## Last takesample value
      rv$last.takesample.value<-v$takesample
      rv$lastDist<-v$dist
      return(cv)
    
  })
 
 
 output$doublePlot <- renderPlot({
   
 v <- getInputValues ()
 cv <- getComputedValues ()
   
 par(mfcol = c(2,2))
 m<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
 layout(m)
   

 ######Output 1 : THEORETICAL DISTRIBUTION#####
 
 if(input$dist=="DN"){x.lim.inf<-min(input$rangeXdn)
                      x.lim.sup<-max(input$rangeXdn)}
 if(input$dist=="DU"){x.lim.inf<-min(input$rangeXdu)
                      x.lim.sup<-max(input$rangeXdu)}
 if(input$dist=="DE"){x.lim.inf<-min(input$rangeXde)
                      x.lim.sup<-max(input$rangeXde)}
 if(input$dist=="DC"){x.lim.inf<-min(input$rangeXdc)
                      x.lim.sup<-max(input$rangeXdc)}
 if(input$dist=="DF"){x.lim.inf<-min(input$rangeXdf)
                      x.lim.sup<-max(input$rangeXdf)}
  if(input$dist=="DB"){x.lim.inf<-min(input$rangeXdb)
                      x.lim.sup<-max(input$rangeXdb)}
 
 ###Définition des X conditionnellement à la distribution 
 
 if(input$dist=="DN"){X = seq(-10,40, length = 1000)}
 if(input$dist=="DU"){X = seq(-5,25, length = 1000)}
 if(input$dist=="DE"){X = seq(-5,5, length = 1000)}
 if(input$dist=="DC"){X = seq(-5,60, length = 1000)}
 if(input$dist=="DF"){X = seq(-5,10, length = 1000)}
 
 
 getY <-reactive({
   if (input$dist == "DN")
     return(dnorm(X, mean=input$mx, sd=input$sx))
   if (input$dist == "DU")
     return(dunif (X, min=0, max=input$b))
   if (input$dist == "DE")
     return (dexp(X, rate=input$Lambda))
   if (input$dist == "DC")
     return (dchisq(X, df=input$df))
   if (input$dist == "DF")
     return(df(X,df1=input$df1, df2=input$df2))
   if (input$dist == "DB")
     return(density(c(rnorm(1000000/2,input$m1, input$sd1), rnorm(1000000/2, input$m2, input$sd2))))
 })
 
 
 if (input$dist == "DB"){
   par(mai=c(0.5,0.5,0.5,0.5))
   dens <- getY()
   plot(dens,xlab="",bty="n",main=HTML("Distribution théorique"),type="l", xlim = c(x.lim.inf, x.lim.sup))
 }
 else {
   Y<-getY()
   par(mai=c(0.5,0.5,0.5,0.5))
   plot(X,Y,xlab="",bty="n",main = HTML("Distribution théorique"),type = "l", xlim = c(x.lim.inf, x.lim.sup))  
 }
 
 

###### Output 2 : statistiques descriptives #####

if(is.null(cv$samples.x.mat)){
  par(mai=c(0.5,0.5,0.5,0.5))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  mtext(bquote(paste("Descriptives : ", N == .(0), sep="")),side=1,line=1,at=0.05,adj=0)
}

else{ 
  par(mai=c(0.5,0.5,0.5,0.5))
  plot(c(0,1),c(0,0),col="white",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,0.05*2.1),bty="n",las=1)
 
if(cv$samples.x.n.toshow>0){
    for(i in 1:cv$samples.x.n.toshow){
      text(0,cv$samples.y.mat.toshow[i,1],labels=bquote(paste(bar(x)[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.m.vec.toshow[i])),sep="")),col="blue",pos=4, cex=1.3) #,cex=cex.samples
      text(0.3,cv$samples.y.mat.toshow[i,1],labels=bquote(paste(s[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.sd.vec.toshow[i])),sep="")),pos=4, cex=1.3) #,cex=cex.samples
                                   }
                           }
mtext(bquote(paste("Descriptives : ", N == .(cv$n.samples), sep="")),side=1,line=1,at=0.05,adj=0)
}  


###### Output 3 : HIST SAMPLE OBSERVATIONS #####

if(is.null(cv$samples.x.mat)){
  Y <- c()
  X <-c()
  par(mai=c(0.5,0.5,0.5,0.5))
  plot(X, Y, main=HTML("Histogramme des données d'échantillonnage"), xlim = c(x.lim.inf, x.lim.sup), ylim = c(0,10), xlab ="", bty="n", ylab = "") 
 }
else{
  par(mai=c(0.5,0.5,0.5,0.5))
  hist(cv$samples.x.mat, freq = TRUE, xlab = "",breaks = 50, col = 'grey',main = HTML("Histogramme des données d'échantillonnage"), xlim = c(x.lim.inf, x.lim.sup))
}        


##### Output 4 : HIST SAMPLE MEANS#######

if(is.null(cv$samples.x.mat)){
  Y <- c()
  X <-c()
  par(mai=c(0.5,0.5,0.5,0.5))
  plot(X, Y, main=HTML("Histogramme des moyennes d'échantillonnage"), xlim = c(x.lim.inf, x.lim.sup), ylim = c(0,10), xlab ="", ylab = "", bty="n") 
  
}
else{
  par(mai=c(0.5,0.5,0.5,0.5))
  h<-hist(cv$samples.x.m.vec, freq = TRUE, breaks=seq(x.lim.inf, x.lim.sup,0.1), xlab ="", main=HTML("Histogramme des moyennes d'échantillonnage"), col = 'grey', xlim = c(x.lim.inf, x.lim.sup))
}   

#afficher la densité normale sur l'histogramme (option)  
if(input$showNdensity && !is.null(cv$samples.x.mat)){  
  lim_inf <- min (cv$samples.x.m.vec)-1
  lim_sup <- max(cv$samples.x.m.vec)+1
  xfit<-seq(lim_inf,lim_sup,length=100) 
  yfit<-dnorm(xfit,mean=mean(cv$samples.x.m.vec),sd=sd(cv$samples.x.m.vec))
  yfit <- yfit*diff(h$mids[1:2])*length(cv$samples.x.m.vec) 
  lines(xfit, yfit, col="blue", type = 'l',lwd=2)
}
   else{}

        
       },height = getPlotHeight, width=getPlotWidth)
  
 
    
  })
