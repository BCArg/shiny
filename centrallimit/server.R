Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots


library(shiny)
library(plotrix)



shinyServer(function(input, output){ 

  rv<-reactiveValues()  # Create a reactiveValues object, to let us use settable reactive values
  
  rv$last.takesample.value<-0
  rv$samples.z<-list()

  rv$lastAction <- 'none'# To start out, lastAction == NULL, meaning nothing clicked yet
  
  rv$lastDist <- " "
  rv$lastN<-0
  
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
          if (input$dist == "DN") {samples[[i]]<-rnorm(input$n)}
          if (input$dist == "DLN"){samples[[i]]<-rlnorm(input$n)}
          if (input$dist == "DU") {samples[[i]]<-runif (input$n)}
          if (input$dist == "DE") {samples[[i]]<-rexp(input$n)} 
          if (input$dist == "DC") {samples[[i]]<-rchisq(input$n, df = input$df)}
          if (input$dist == "DF") {samples[[i]]<-rf(input$n,df1 = input$df1,df2 = input$df2)}
          if (input$dist == "DB") {samples[[i]]<-c(rnorm(input$n/2,input$m1, input$sd1), rnorm(input$n/2, input$m2, input$sd2))}
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
        if (v$dist == "DN") {cv$samples.x[[i]]<-round((rv$samples.z[[i]]*v$sx)+v$mx,2)}
        if (v$dist == "DLN"){cv$samples.x[[i]]<-round((rv$samples.z[[i]]*v$lsx)+v$lmx,2)}
        if (v$dist == "DU") {cv$samples.x[[i]]<-round(rv$samples.z[[i]]*v$b,2)}
        if (v$dist == "DE") {cv$samples.x[[i]]<-round(rv$samples.z[[i]]*(1/v$Lambda),2)}
        if (v$dist == "DC") {cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}
        if (v$dist == "DF") {cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}        
        if (v$dist == "DB") {cv$samples.x[[i]]<-round(rv$samples.z[[i]], 2)}
       }
  
      if (rv$lastDist!=v$dist) {
        rv$lastAction <- 'changeDist'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      if (rv$lastN!=v$n) {
        rv$lastAction <- 'changeN'
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
      
      
      cv$samples.x.m.m <- round(mean(cv$samples.x.m.vec),4)
      cv$samples.x.v.m <- round(var(cv$samples.x.m.vec),4)
      
      hd<-hist(cv$samples.x.mat, freq = TRUE)
      cv$freqcl <- unlist(hd[2])
      
      densx<-density(cv$samples.x.mat)
      cv$highdens <- unlist(densx[2])
        }  
    cv$vx<-v$sx^2 
    cv$lvx<-v$lsx^2   
      ## Last takesample value
      rv$last.takesample.value<-v$takesample
      rv$lastDist<-v$dist
      rv$lastN<-v$n
      return(cv)
    
  })
 
 
 output$doublePlot <- renderPlot({
   
 v <- getInputValues ()
 cv <- getComputedValues ()
   
 par(mfcol = c(2,2))
 m<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
 layout(m, width=c(2,1))
   
 ## Set graphic parameters
 if(v$display=="default") {
   cex.main.title<-2
   cex.title<-1.5
   cex.samples<-1.5 
   cex.axis<-1.3
 }
 if(v$display=="1024") {
   cex.main.title<-1.75
   cex.title<-1.2
   cex.samples<-1.2
   cex.axis<-1.1
 }
 if(v$display=="800") {
   cex.main.title<-1.5
   cex.title<-1
   cex.samples<-1
   cex.axis<-0.9
 }
 
 #Définition des limites de l'axe des abscisses pour le plot
 
 if(v$dist=="DN") {x.lim.inf<-min(v$rangeXdn)
                       x.lim.sup<-max(v$rangeXdn)}
 if(v$dist=="DLN"){x.lim.inf<-min(v$rangeXdln)
                       x.lim.sup<-max(v$rangeXdln)}
 if(v$dist=="DU") {x.lim.inf<-min(v$rangeXdu)
                       x.lim.sup<-max(v$rangeXdu)}
 if(v$dist=="DE") {x.lim.inf<-min(v$rangeXde)
                       x.lim.sup<-max(v$rangeXde)}
 if(v$dist=="DC") {x.lim.inf<-min(v$rangeXdc)
                       x.lim.sup<-max(v$rangeXdc)}
 if(v$dist=="DF") {x.lim.inf<-min(v$rangeXdf)
                       x.lim.sup<-max(v$rangeXdf)}
 if(v$dist=="DB") {x.lim.inf<-min(v$rangeXdb)
                       x.lim.sup<-max(v$rangeXdb)}
 
 #Définition des X conditionnellement à la distribution 
 
 if(v$dist=="DN"){X=seq(-10,40, length=1000)}
 if(v$dist=="DLN"){X=seq(-10,40, length=1000)}
 if(v$dist=="DU"){X=seq(-5,25, length=1000)}
 if(v$dist=="DE"){X=seq(-5,20, length=1000)}
 if(v$dist=="DC"){X=seq(-5,60, length=1000)}
 if(v$dist=="DF"){X=seq(-5,10, length=1000)}
 
 #Définition de la densité théorique 
 
 getY <-reactive({
   if (v$dist=="DN")
     return(dnorm(X, mean=v$mx, sd=v$sx))
   if (v$dist=="DLN")
     return(dlnorm(X,meanlog=v$lmx, sdlog =v$lsx))
   if (v$dist=="DU")
     return(dunif (X, min=0, max=v$b))
   if (v$dist=="DE")
     return (dexp(X, rate=v$Lambda))
   if (v$dist=="DC")
     return (dchisq(X, df=v$df))
   if (v$dist=="DF")
     return(df(X,df1=v$df1, df2=v$df2))
   if (v$dist=="DB")
     return(density(c(rnorm(1000000/2,v$m1, v$sd1), rnorm(1000000/2, v$m2, v$sd2))))
 })
 
 #------------------- Output 1 : ------------------------------
 #Afficher les observations pour 5 échantillons prélevés 
 #Afficher la distribution théorique d'origine (optionnel
 #-------------------------------------------------------------
 
 if (v$dist == "DB"){
   dens<-getY()
   d<-unlist(dens[2])
   y.delta<-max(d)
 }
 else{
   y.delta <- max(getY())
 }
   
 
 cv$samples.y.mat.toshow<-c()
 if(cv$n.samples>0 && cv$samples.x.n.toshow>0){
   cv$samples.y.mat.toshow<-matrix(rep(y.delta/(5+1)*c(1:cv$samples.x.n.toshow),length(cv$samples.x.mat.toshow[,1])),nrow=length(cv$samples.x.mat.toshow[,1]), ncol = v$n)
 }
 
 par(mai=c(0.3,0.6,0.5,0))
 label<-""
 
 
 if(is.null(cv$samples.x.mat)){
   plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(x.lim.inf,x.lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(x.lim.inf,x.lim.sup,20),main=bquote(paste("Echantillons prélevés :")),cex.main=cex.title) 
 }
 else{ 
 plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(x.lim.inf,x.lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(x.lim.inf,x.lim.sup,20),main=bquote(paste("Echantillons prélevés :")),cex.main=cex.title)
 if(cv$samples.x.n.toshow>0){
   for(i in 1:cv$samples.x.n.toshow){
     points(cv$samples.x.mat.toshow[i,],cv$samples.y.mat.toshow[i,],cex=cex.samples*0.8)
     text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(bar(x)[.(cv$samples.x.i.vec.toshow[i])]),cex=cex.samples*1.2,col="blue")
   }
  }
 }
 
 if(v$showreality){
   axis(2,las=2,yaxp=c(0,signif(y.delta,1),5),cex.axis=cex.axis)
   if (v$dist == "DB"){
     dens <- getY()
     lines(dens)
   }
   else {
     Y<-getY()
     points(X,Y, type="l")
  }
   if(v$dist=="DN"){text(1,signif(y.delta,1)*0.75,labels=bquote(paste(X*"~"*N(mu*","*sigma^2) ," ", X*"~"*N(.(v$mx)*","*.(cv$vx)),sep='')),cex=1.4, pos=4)}
   if(v$dist=="DLN"){text(1,signif(y.delta,1)*0.75,labels=bquote(paste(log(X)*"~"*N(mu*","*sigma^2) ," ", log(X)*"~"*N(.(v$lmx)*","*.(cv$lvx)),sep='')),cex=1.4, pos=4)}
   if(v$dist=="DU"){text(1,signif(y.delta,1)*0.75,labels=bquote(paste(X*"~"*U(theta[1]*","*theta[2]) ," ", X*"~"*U(.0*","*.(v$b)),sep='')),cex=1.4, pos=4)}
   if(v$dist=="DE"){text(x.lim.sup-1,signif(y.delta,1)*0.75,labels=bquote(paste(X*"~"*E(lambda) ," ", X*"~"*E(.(v$Lambda)),sep='')),cex=1.4, pos=4)}
   if(v$dist=="DC"){text(x.lim.sup/1.5,signif(y.delta,1)*0.65,labels=bquote(paste(X*"~"*chi^2, (nu)," ", X*"~"*chi^2,(.(v$df)),sep='')),cex=1.4, pos=4)}
   if(v$dist=="DF"){text(x.lim.sup-2,signif(y.delta,1)*0.75,labels=bquote(paste(X*"~"*F[nu[1]*","*nu[2]] ," ", X*"~"*F[.(v$df1)*","*.(v$df2)],sep='')),cex=1.4, pos=4)}
   
 }
 
 
 #------------------- Output 2 : --------------------------------------
 #Afficher les stats descriptives des échantillons prélevés (optionnel)
 #---------------------------------------------------------------------
 
 #empty plot for layout
 if(is.null(cv$samples.x.mat)){
   par(mai=c(0.3,0,0.5,0))
   plot(c(0,1),c(0,0),col="white",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,y.delta),bty="n",las=1)
   #mtext(bquote(paste("Descriptives : ", N == .(0), sep="")),side=1,line=1,at=0.05,adj=0)
 }
 
 else{ 
  par(mai=c(0.3,0,0.5,0))
  plot(c(0,1),c(0,0),col="white",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,y.delta),bty="n",las=1)
 
  if(v$empPl){
    mtext(bquote(paste("Descriptives : ", N == .(cv$n.samples), sep="")),side=1,line=1,at=0.05,adj=0)
    
  if(cv$samples.x.n.toshow>0){
   for(i in 1:cv$samples.x.n.toshow){
     text(0,cv$samples.y.mat.toshow[i,1],labels=bquote(paste(bar(x)[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.m.vec.toshow[i])),sep="")),col="blue",pos=4, cex=cex.samples) 
     text(0.5,cv$samples.y.mat.toshow[i,1],labels=bquote(paste(s[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.sd.vec.toshow[i])),sep="")),pos=4,cex=cex.samples) 
   }
 #mtext(bquote(paste("E(",bar(x)[k],")" == .(cv$samples.x.m.m), "      V(",bar(x)[k],")" == .(cv$samples.x.v.m), sep="")),side=1,line=1,adj=0)
 }}
 }
 
  
 #------------------- Output 3 : --------------------------------------
 #Histogramme des données d'échantillonnage
 #Afficher leur distribution (optionnel)
 #---------------------------------------------------------------------
 
 
 range<-x.lim.sup-x.lim.inf
 breaks<-seq(x.lim.inf, x.lim.sup, 0.1)
  
 if(is.null(cv$samples.x.mat)){
   Y <- c()
   X <-c()
   par(mai=c(0.3,0.6,0.5,0), xaxs="i",yaxs="i")
   plot(X, Y, main=HTML("Histogramme des données d'échantillonnage"),yaxt="n",bty="n",cex.axis=cex.axis,xlim=c(x.lim.inf,x.lim.sup),ylim=c(0,y.delta),xlab="", ylab = "",xaxp=c(x.lim.inf,x.lim.sup,20),cex.main = cex.title) 
 }
 else{
     if(input$showNdensity && !is.null(cv$samples.x.mat)){  
     h<-hist(cv$samples.x.mat, probability=TRUE,yaxt="n",xlim=c(x.lim.inf,x.lim.sup),xlab="",ylim =c(0, max(cv$highdens)), ylab="",xaxp=c(x.lim.inf,x.lim.sup,20),cex.axis=cex.axis,col = 'grey',main = HTML("Histogramme des données d'échantillonnage"), cex.main = cex.title) #,breaks = 50
     den <- density(cv$samples.x.mat)
     lines(den, col = "red")
   }
   else{par(mai=c(0.3,0.6,0.5,0), xaxs="i",yaxs="i")
        h<-hist(cv$samples.x.mat, freq=TRUE,xlim=c(x.lim.inf,x.lim.sup),ylim=c(0,max(cv$freqcl)), xlab="",ylab="",xaxp=c(x.lim.inf,x.lim.sup,20), cex.axis=cex.axis,col = 'grey',main = HTML("Histogramme des données d'échantillonnage"), cex.main = cex.title) #,breaks = 50
   }
}        
 
 
 
 #------------------- Output 4 : --------------------------------------
 #Histogramme des moyennes d'échantillonnage
 #Afficher leur distribution (optionnel)
 #---------------------------------------------------------------------

 
 if(is.null(cv$samples.x.mat)){
   Y <- c()
   X <-c()
   par(mai=c(0.3,0.6,0.5,0), xaxs="i",yaxs="i")
   plot(X, Y, main=HTML("Histogramme des moyennes d'échantillonnage"), xlim = c(x.lim.inf, x.lim.sup), ylim = c(0,10), xlab ="", ylab = "", bty="n", , cex.main = cex.title) 
   
 }
 else{
   par(mai=c(0.3,0.6,0.5,0), xaxs="i",yaxs="i")
   h<-hist(cv$samples.x.m.vec, freq = TRUE, breaks=breaks, xlab ="", main=HTML("Histogramme des moyennes d'échantillonnage"), col = 'grey', xlim = c(x.lim.inf, x.lim.sup), cex.main = cex.title)
 }
 
 
 #afficher la densité normale sur l'histogramme (option)  
 if(input$showMdensity && !is.null(cv$samples.x.mat)){  
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
