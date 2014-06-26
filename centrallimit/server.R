Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots


library(shiny)
library(plotrix)



shinyServer(function(input, output){ 

  rv<-reactiveValues()  # Create a reactiveValues object, to let us use settable reactive values
  
  rv$last.takesample.value<-0
  rv$samples.z<-list()

  rv$lastAction<-'none'# To start out, lastAction == NULL, meaning nothing clicked yet
  
  rv$lastDist<-" "
  
  rv$lastp<-0.5
  rv$lastminrud<-1
  rv$lastmaxrud<-6
  rv$lastdf<-5
  rv$lastdf1<-5
  rv$lastdf2<-20
  rv$lastm1<-8
  rv$lastm2<-4
  rv$lastsd1<-1.5
  rv$lastsd2<-1.1
  
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
          if (input$dist == "DBin") {samples[[i]]<-rbinom(n = 1, size = input$n, prob = input$p)}
          #if (input$dist == "DLN"){samples[[i]]<-rlnorm(input$n)}
          if (input$dist == "DUD") {x <- min(input$RUD) : max(input$RUD)
                                    samples[[i]]<-sample(x, input$n, replace = TRUE)}
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
        if (v$dist == "DBin") {cv$samples.x[[i]]<-round(rv$samples.z[[i]],2)}
        #if (v$dist == "DLN"){cv$samples.x[[i]]<-round((rv$samples.z[[i]]*v$lsx)+v$lmx,2)}
        if (v$dist == "DUD") {cv$samples.x[[i]]<-rv$samples.z[[i]]}
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
     
      if (rv$lastp!=v$p) {
        rv$lastAction <- 'changep'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      if (rv$lastminrud!=min(v$RUD)) {
        rv$lastAction <- 'changeminrud'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      if (rv$lastmaxrud!=max(v$RUD)) {
        rv$lastAction <- 'changemaxrud'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
            
      if (rv$lastdf!=v$df) {
        rv$lastAction <- 'changedf'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
           
      if (rv$lastdf1!=v$df1) {
        rv$lastAction <- 'changedf1'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      if (rv$lastdf2!=v$df2) {
        rv$lastAction <- 'changedf2'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      if (rv$lastm1!=v$m1) {
        rv$lastAction <- 'changem1'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      if (rv$lastm2!=v$m2) {
        rv$lastAction <- 'changem2'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      if (rv$lastsd1!=v$sd1) {
        rv$lastAction <- 'changesd1'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
      }
      
      if (rv$lastsd2!=v$sd2) {
        rv$lastAction <- 'changesd2'
        rv$last.takesample.value<-0
        rv$samples.z <- list()
        cv$samples.x <- list()
        
      }
      ##Pour passer d'une liste à une matrice
      if(v$dist == "DBin"){
      cv$samples.x.mat <- matrix(nrow = cv$n.samples, ncol = 1) 
      cv$samples.p.mat <- matrix(nrow = cv$n.samples, ncol = 1) 
      
      for(i in 1:cv$n.samples){
        cv$samples.x.mat[i,1] <- cv$samples.x[[i]]
        cv$samples.p.mat[i,1] <-cv$samples.x[[i]]/v$n
      }}
      else{
      cv$samples.x.mat <- matrix(nrow = cv$n.samples, ncol = v$n) 
      for(i in 1:cv$n.samples){
        cv$samples.x.mat[i,] <- cv$samples.x[[i]]
      }
      }
     
      
      ## Computation of descriptives
      cv$samples.x.m.vec<-c() # vector of mean values, each line a sample
      cv$samples.x.sd.vec<-c()
      cv$samples.x.m.vec<-round(apply(cv$samples.x.mat,1,mean),2)#means of samples
      cv$samples.x.sd.vec<-round(apply(cv$samples.x.mat,1,sd),2)#sds of samples
      if(v$dist == "DBin"){
      cv$samples.p.m.vec<-round(apply(cv$samples.p.mat,1,mean),2)#means of samples
      cv$samples.p.sd.vec<-round(apply(cv$samples.p.mat,1,sd),2)#sds of samples
      }
            
      ## Define subset to plot
      cv$samples.x.n.toshow<-0
      cv$samples.x.from<-1
      if(cv$n.samples>5){
          cv$samples.x.from<-cv$n.samples-5+1
        }    
      cv$samples.x.to<-cv$n.samples
        
      
      if(v$dist =="DBin"){
        cv$samples.x.mat.toshow<-matrix(cv$samples.x.mat[cv$samples.x.from:cv$samples.x.to,],ncol=1)
      }
      else{
        cv$samples.x.mat.toshow<-matrix(cv$samples.x.mat[cv$samples.x.from:cv$samples.x.to,],ncol=v$n)
      }
      
      cv$samples.x.m.vec.toshow<-cv$samples.x.m.vec[cv$samples.x.from:cv$samples.x.to]
      cv$samples.x.sd.vec.toshow<-cv$samples.x.sd.vec[cv$samples.x.from:cv$samples.x.to]
      cv$samples.x.i.vec.toshow<-c(cv$samples.x.from:cv$samples.x.to)
      cv$samples.x.n.toshow<-length(cv$samples.x.mat.toshow[,1])
      

      cv$samples.x.m.m <- round(mean(cv$samples.x.m.vec),4)
      cv$samples.x.v.m <- round(var(cv$samples.x.m.vec),4)
      
      if(v$dist == "DBin"){
      cv$samples.p.m.m <- round(mean(cv$samples.p.m.vec),4)
      cv$samples.p.v.m <- round(var(cv$samples.p.m.vec),4)
        
      }
      
      hf<-hist(cv$samples.x.mat, freq= TRUE, breaks = 50)
      freqcl<- hf$counts
      cv$maxfreqcl<-max(freqcl)
      n.obs.tot<-length(cv$samples.x.mat)
      probcl <- freqcl/n.obs.tot        
      cv$maxprobcl<-max(probcl)
      
      
      #if(cv$n.samples <100){breaks =10}
      #else{breaks <- sqrt(cv$n.samples)}
      
      #hm<-hist(cv$samples.x.m.vec, freq = TRUE, breaks = breaks)
      #cv$freqmcl <- unlist(hm[2])
      
      #densm<-density(cv$samples.x.m.vec)
      #cv$highdensm <- unlist(densm[2])
        }  
    cv$vx<-v$sx^2 
    cv$lvx<-v$lsx^2   
      
    ## Last takesample value
      rv$last.takesample.value<-v$takesample
      rv$lastDist<-v$dist
      rv$lastp<-v$p
      rv$lastminrud<-min(v$RUD)  
      rv$lastmaxrud<-max(v$RUD)
      rv$lastdf<-v$df
      rv$lastdf1<-v$df1
      rv$lastdf2<-v$df2
      rv$lastm1<-v$m1
      rv$lastm2<-v$m2
      rv$lastsd1<-v$sd1
      rv$lastsd2<-v$sd2
      
      rv$lastN<-v$n
    
    
      return(cv)
    
  })
 
 
 output$doublePlot <- renderPlot({
   
 v <- getInputValues ()
 cv <- getComputedValues ()
   
 par(mfcol = c(2,2))
 m<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
 layout(m, width=c(4,3))  #
   
 ## Set graphic parameters
 if(v$display=="default") {
   cex.main.title<-2
   cex.title<-1.5
   cex.samples<-1.5 
   cex.axis<-1.1
   cex.label<-1.2
 }
 if(v$display=="1024") {
   cex.main.title<-1.75
   cex.title<-1.2
   cex.samples<-1.2
   cex.axis<-1
   cex.label<-1
 }
 if(v$display=="800") {
   cex.main.title<-1.5
   cex.title<-1
   cex.samples<-1
   cex.axis<-0.8
   cex.label<-0.8
 }

 
 
 
 #Définition des limites de l'axe des abscisses pour le plot
 
 if(v$dist=="DN"&& v$range =="SameRange") {x.lim.inf<-min(v$rangeXdn)
                                           x.lim.sup<-max(v$rangeXdn)}
 if(v$dist=="DBin"&& v$range =="SameRange") {x.lim.inf<-min(v$rangeXdbin)
                                             x.lim.sup<-max(v$rangeXdbin)}
 #if(v$dist=="DLN"&& v$range =="SameRange"){x.lim.inf<-min(v$rangeXdln)
 #                                           x.lim.sup<-max(v$rangeXdln)}
 if(v$dist=="DUD"&& v$range =="SameRange") {x.lim.inf<-min(v$rangeXdud)
                                            x.lim.sup<-max(v$rangeXdud)}
 if(v$dist=="DU"&& v$range =="SameRange") {x.lim.inf<-min(v$rangeXdu)
                                           x.lim.sup<-max(v$rangeXdu)}
 if(v$dist=="DE"&& v$range =="SameRange") {x.lim.inf<-min(v$rangeXde)
                                           x.lim.sup<-max(v$rangeXde)}
 if(v$dist=="DC"&& v$range =="SameRange") {x.lim.inf<-min(v$rangeXdc)
                                           x.lim.sup<-max(v$rangeXdc)}
 if(v$dist=="DF"&& v$range =="SameRange") {x.lim.inf<-min(v$rangeXdf)
                                           x.lim.sup<-max(v$rangeXdf)}
 if(v$dist=="DB"&& v$range =="SameRange") {x.lim.inf<-min(v$rangeXdb)
                                           x.lim.sup<-max(v$rangeXdb)}
 
 if(v$dist=="DN"&& v$range =="DifRange") {Obs.lim.inf<-min(v$rangeObsdn)
                                          Obs.lim.sup<-max(v$rangeObsdn)
                                          Xbar.lim.inf<-min(v$rangeXbardn)
                                          Xbar.lim.sup<-max(v$rangeXbardn)}
 if(v$dist=="DBin"&& v$range =="DifRange") {Obs.lim.inf<-min(v$rangeObsdbin)
                                            Obs.lim.sup<-max(v$rangeObsdbin)
                                            Xbar.lim.inf<-min(v$rangeXbardbin)
                                            Xbar.lim.sup<-max(v$rangeXbardbin)}
 #if(v$dist=="DLN"&& v$range =="DifRange"){Obs.lim.inf<-min(v$rangeObsdln)
  #                                        Obs.lim.sup<-max(v$rangeObsdln)
  #                                        Xbar.lim.inf<-min(v$rangeXbardln)
  #                                        Xbar.lim.sup<-max(v$rangeXbardln)}
 if(v$dist=="DUD"&& v$range =="DifRange") {Obs.lim.inf<-min(v$rangeObsdud)
                                           Obs.lim.sup<-max(v$rangeObsdud)
                                          Xbar.lim.inf<-min(v$rangeXbardud)
                                          Xbar.lim.sup<-max(v$rangeXbardud)}
 if(v$dist=="DU"&& v$range =="DifRange") {Obs.lim.inf<-min(v$rangeObsdu)
                                          Obs.lim.sup<-max(v$rangeObsdu)
                                          Xbar.lim.inf<-min(v$rangeXbardu)
                                          Xbar.lim.sup<-max(v$rangeXbardu)}
 if(v$dist=="DE"&& v$range =="DifRange") {Obs.lim.inf<-min(v$rangeObsde)
                                          Obs.lim.sup<-max(v$rangeObsde)
                                          Xbar.lim.inf<-min(v$rangeXbarde)
                                          Xbar.lim.sup<-max(v$rangeXbarde)}
 if(v$dist=="DC"&& v$range =="DifRange") {Obs.lim.inf<-min(v$rangeObsdc)
                                          Obs.lim.sup<-max(v$rangeObsdc)
                                          Xbar.lim.inf<-min(v$rangeXbardc)
                                          Xbar.lim.sup<-max(v$rangeXbardc)}
 if(v$dist=="DF"&& v$range =="DifRange") {Obs.lim.inf<-min(v$rangeObsdf)
                                          Obs.lim.sup<-max(v$rangeObsdf)
                                          Xbar.lim.inf<-min(v$rangeXbardf)
                                          Xbar.lim.sup<-max(v$rangeXbardf)}
 if(v$dist=="DB"&& v$range =="DifRange") {Obs.lim.inf<-min(v$rangeObsdb)
                                          Obs.lim.sup<-max(v$rangeObsdb)
                                          Xbar.lim.inf<-min(v$rangeXbardb)
                                          Xbar.lim.sup<-max(v$rangeXbardb)}
 
 
 #Définition des X conditionnellement à la distribution 
 
 if(v$dist=="DN"){X=seq(-10,40, length=1000)}
 if(v$dist=="DBin"){X=0:v$n}
 #if(v$dist=="DLN"){X=seq(-10,40, length=1000)}
 if(v$dist=="DUD"){X= min(v$RUD):max(v$RUD)}
 if(v$dist=="DU"){X=seq(-5,25, length=1000)}
 if(v$dist=="DE"){X=seq(-5,20, length=1000)}
 if(v$dist=="DC"){X=seq(-5,60, length=1000)}
 if(v$dist=="DF"){X=seq(-5,10, length=1000)}
 
 #Définition de la densité théorique 
 
 getY <-reactive({
   if (v$dist=="DN")
     return(dnorm(X, mean=v$mx, sd=v$sx))
   if (v$dist=="DBin")
     return(dbinom(X, size=v$n, prob=v$p))
   #if (v$dist=="DLN")
    # return(dlnorm(X,meanlog=v$lmx, sdlog =v$lsx))
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
 if (v$dist == "DUD"){
   p <- rep(1/length(X), length(X))
   y.delta<-p[1]+p[1]/length(X)
 }
 else{
   y.delta <- max(getY())
 }
 }

 cv$samples.y.mat.toshow<-c()
 if(cv$n.samples>0 && cv$samples.x.n.toshow>0){
   if(v$dist == "DBin"){
   cv$samples.y.mat.toshow<-matrix(rep(y.delta/(5+1)*c(1:cv$samples.x.n.toshow),length(cv$samples.x.mat.toshow[,1])),nrow=length(cv$samples.x.mat.toshow[,1]), ncol = 1)
   }
   else{
   cv$samples.y.mat.toshow<-matrix(rep(y.delta/(5+1)*c(1:cv$samples.x.n.toshow),length(cv$samples.x.mat.toshow[,1])),nrow=length(cv$samples.x.mat.toshow[,1]), ncol = v$n)
   }
 }
 
 par(mai=c(0.5,0.8,0.5,0.5))
 label<-""
 if(v$range =="SameRange"){
   lim.inf<-x.lim.inf
   lim.sup<-x.lim.sup
 }
 if(v$range =="DifRange"){
   lim.inf<-Obs.lim.inf
   lim.sup<-Obs.lim.sup
 }
 range<-lim.sup-lim.inf 

 
 if(range>10){nbgrad <- range}
 if(range>5 & range <=10){nbgrad <- range*2}
 if(range<=5){nbgrad <- range*4}
 
 ## Test about range of 'x'
 
 if(cv$n.samples>0){
  if(cv$samples.x.mat > lim.sup || cv$samples.x.mat < lim.inf) {error <-1}
  else{error <-0}
 }

 
##############PLOT########################
 
 ### CAS N°1 : Si aucune donnée :
  if(is.null(cv$samples.x.mat)){
    
    ### CAS N°1.1 : Si l'option "afficher la distribution théorique est cochée :
    if(v$showreality){
      #Si distribution Binomiale
      if (v$dist=="DBin"){
        Y <- getY()
        plot(Y, type = "h",bty="n", xaxs="i",yaxs="i",xlab ="x", ylab = expression(P(x)), xlim=c(lim.inf,lim.sup),xaxp=c(lim.inf,lim.sup,10), lwd=2, col = "red", main = "")
        mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
        mtext(bquote(paste(X*"~"*Bin(n*","*p)," ",X*"~"*Bin(.(v$n)*","*.(v$p)),sep='')), side=3,line=-1,adj=0.05, cex=cex.label)
      }
      else{
      #Si distribution Uniforme Discrète 
        if (v$dist=="DUD"){
          plot(X, p, col = "red", type = "h",bty="n", xaxs="i",yaxs="i", xlab =" ", ylab=" ",lwd = 2, xlim=c(lim.inf,lim.sup),ylim = c(0, y.delta), main = "") 
          mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
          mtext(bquote(paste(X*"~"*U*"{"*.(min(v$RUD))*",...,"*.(max(v$RUD))*"}",sep='')), side=3,line=1,adj=-0.1, cex=cex.label)
          points(X,p, col = "red", lwd = 2, pch = 19)
          lines(X, p, lty = 3)
        }
      #Pour les autres distributions 
      else{
      plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(lim.inf,lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(lim.inf,lim.sup,nbgrad),main="") 
      axis(2,las=2,yaxp=c(0,signif(y.delta,1),5),cex.axis=cex.axis)
      if (v$dist == "DB"){
        dens <- getY()
        lines(dens)
      }
      else {
        Y<-getY()
        points(X,Y, type="l")
      }
      mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
      
      if(v$dist=="DN"){mtext(bquote(paste(X*"~"*N(mu*","*sigma^2) ," ", X*"~"*N(.(v$mx)*","*.(cv$vx)),sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
      if(v$dist=="DU"){mtext(bquote(paste(X*"~"*U(theta[1]*","*theta[2]) ," ", X*"~"*U(.0*","*.(v$b)),sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
      if(v$dist=="DE"){mtext(bquote(paste(X*"~"*E(lambda) ," ", X*"~"*E(.(v$Lambda)),sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
      if(v$dist=="DC"){mtext(bquote(paste(X*"~"*chi^2, (nu)," ", X*"~"*chi^2,(.(v$df)),sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
      if(v$dist=="DF"){mtext(bquote(paste(X*"~"*F[nu[1]*","*nu[2]] ," ", X*"~"*F[.(v$df1)*","*.(v$df2)],sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
            
      }}
    }
    
    ###CAS N°1.2 : Si l'option "afficher la distribution théorique" n'est pas cochée : 
    else{
      #Si la distribution est Binomiale
      if (v$dist=="DBin"){
        plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(lim.inf,lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(lim.inf,lim.sup,10),main="") 
        mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
      }
      else{
      #Si la distribution est Uniforme discrète
        if (v$dist=="DUD"){
        plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(lim.inf,lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(lim.inf,lim.sup,range),main="") 
        mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
        }
      #Pour toutes les autres distributions
        else{
        plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(lim.inf,lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(lim.inf,lim.sup,nbgrad),main="") 
        mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
        }  
      }}
    }
 
 ##CAS N°2 : Si 1 ou plusieurs échantillons ont déjà été tirés : 
 else{
   ### CAS N°2.1 : Si conflit entre limites des X et observations prélevées : afficher un msg d'erreur  
   if(error==1){
     plot(1:10,1:10, col = "white", xlab="",ylab="",xaxt="n",yaxt="n",bty="n",type='l')
     text(5,8, labels = bquote("Certaines valeurs dépassent les limites défines en abscisse."), cex = cex.label, col = "red")
     text(5,7, labels = bquote("Modifiez le choix de l'étendue au moyen du slider adéquat."), cex = cex.label, col = "red")
    }
   ### CAS N°2.2 : Si pas d'erreur : 
   if(error==0){
   ### CAS N°2.2.1 : Si l'option "afficher la distribution théorique" est cochée : 
     if(v$showreality){
       ##Si la distribution est binomiale :
       if (v$dist=="DBin"){
         Y <- getY()
         plot(Y, type = "h",bty="n", xaxs="i",yaxs="i",xlab ="x", ylab = expression(P(x)), xlim=c(lim.inf,lim.sup),xaxp=c(lim.inf,lim.sup,10), lwd=2, col = "red", main = "")
         mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
         mtext(bquote(paste(X*"~"*Bin(n*","*p)," ",X*"~"*Bin(.(v$n)*","*.(v$p)),sep='')), side=3,line=-1,adj=0.05, cex=cex.label)
         for(i in 1:cv$samples.x.n.toshow){
           points(cv$samples.x.mat.toshow[i,],cv$samples.y.mat.toshow[i,],cex=cex.samples*0.8)
           text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(x[.(cv$samples.x.i.vec.toshow[i])]),cex=cex.samples*1.2,col="blue")
         }
       }
       
       else{
       ##Si la distribution est uniforme discète : 
       if (v$dist=="DUD"){
         plot(X, p, col = "red", type = "h",bty="n", xaxs="i",yaxs="i", ylab=" ",lwd = 2, xlim=c(lim.inf,lim.sup),ylim = c(0, y.delta), main = "") 
         mtext(bquote(paste("Distribution théorique")), side=3,line=1,adj=0.5, cex=cex.label)
         mtext(bquote(paste(X*"~"*U*"{"*.(min(v$RUD))*",...,"*.(max(v$RUD))*"}",sep='')), side=3,line=1,adj=-0.1, cex=cex.label)
         points(X,p, col = "red", lwd = 2, pch = 19)
         lines(X, p, lty = 3)
         for(i in 1:cv$samples.x.n.toshow){
           points(jitter(cv$samples.x.mat.toshow[i,],0.5),jitter(cv$samples.y.mat.toshow[i,],0.5),cex=cex.samples*0.8)
           text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(bar(x)[.(cv$samples.x.i.vec.toshow[i])]),cex=cex.samples*1.2,col="blue")
         }
       }
       
       ##Pour toutes les autres distributions : 
       else{
         plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(lim.inf,lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(lim.inf,lim.sup,nbgrad),main="")
         mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
         for(i in 1:cv$samples.x.n.toshow){
             points(cv$samples.x.mat.toshow[i,],cv$samples.y.mat.toshow[i,],cex=cex.samples*0.8)
             text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(bar(x)[.(cv$samples.x.i.vec.toshow[i])]),cex=cex.samples*1.2,col="blue")
           }
         axis(2,las=2,yaxp=c(0,signif(y.delta,1),5),cex.axis=cex.axis)
         
         if (v$dist == "DB"){
           dens <- getY()
           lines(dens)
         }
         else {
           Y<-getY()
           points(X,Y, type="l")
         }
               
         if(v$dist=="DN"){mtext(bquote(paste(X*"~"*N(mu*","*sigma^2) ," ", X*"~"*N(.(v$mx)*","*.(cv$vx)),sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
         if(v$dist=="DU"){mtext(bquote(paste(X*"~"*U(theta[1]*","*theta[2]) ," ", X*"~"*U(.0*","*.(v$b)),sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
         if(v$dist=="DE"){mtext(bquote(paste(X*"~"*E(lambda) ," ", X*"~"*E(.(v$Lambda)),sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
         if(v$dist=="DC"){mtext(bquote(paste(X*"~"*chi^2, (nu)," ", X*"~"*chi^2,(.(v$df)),sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
         if(v$dist=="DF"){mtext(bquote(paste(X*"~"*F[nu[1]*","*nu[2]] ," ", X*"~"*F[.(v$df1)*","*.(v$df2)],sep='')), side=3,line=1,adj=-0.1, cex=cex.label)}
       }
      }
     }
     
     ###CAS N°2.2.2 : Si la case "Afficher la distribution théorique n'est pas cochée":
     else{
       ##Si la distribution est binomiale : 
       if (v$dist=="DBin"){
         plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(lim.inf,lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(lim.inf,lim.sup,10), main="") #,
         mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
         for(i in 1:cv$samples.x.n.toshow){
           points(cv$samples.x.mat.toshow[i,],cv$samples.y.mat.toshow[i,],cex=cex.samples*0.8)
           text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(x[.(cv$samples.x.i.vec.toshow[i])]),cex=cex.samples*1.2,col="blue")
         }
        }
       ##Si la distribution est uniforme discète : 
       else{
       if(v$dist=="DUD"){
         plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(lim.inf,lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(lim.inf,lim.sup,range), main="") #,
         mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
         for(i in 1:cv$samples.x.n.toshow){
           points(jitter(cv$samples.x.mat.toshow[i,],0.5),jitter(cv$samples.y.mat.toshow[i,],0.5),cex=cex.samples*0.8)
           text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(bar(x)[.(cv$samples.x.i.vec.toshow[i])]),cex=cex.samples*1.2,col="blue")
         }
        }
       ##Pour toutes les autres distributions : 
       else{   
       plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=cex.axis,xlim=c(lim.inf,lim.sup),ylim=c(0,y.delta),xlab="",ylab=label,xaxp=c(lim.inf,lim.sup,nbgrad),main="")
       mtext(bquote(paste("Echantillons prélevés :")), side=3,line=1,adj=0.5, cex=cex.label)
       for(i in 1:cv$samples.x.n.toshow){
           points(cv$samples.x.mat.toshow[i,],cv$samples.y.mat.toshow[i,],cex=cex.samples*0.8)
           text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(bar(x)[.(cv$samples.x.i.vec.toshow[i])]),cex=cex.samples*1.2,col="blue")
         }
       }}}
   }
 }


 
 
 #------------------- Output 2 : --------------------------------------
 #Afficher les stats descriptives des échantillons prélevés (optionnel)
 #---------------------------------------------------------------------
 
### CAS n°1 : Si aucune donnée : 
 if(is.null(cv$samples.x.mat)){
   par(mai=c(0.5,0,0.5,0))
   plot(c(0,1),c(0,0),col="white",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,y.delta),bty="n",las=1)
   mtext(bquote(paste("Descriptives : ", N == .(0), sep="")),side=3,line=1,adj=0, at=0.00, cex=cex.label)
 } 
 
### CAS n°2 : Si 1 ou plusieurs échantillons ont déjà été tirés : 
 else{ 
   par(mai=c(0.5,0,0.5,0))
   plot(c(0,1),c(0,0),col="white",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,y.delta),bty="n",las=1)

### CAS N°2.1 : Si l'option "afficher les stat descr" est cochée : 
   if(v$empPl){
     mtext(bquote(paste("Descriptives : ", N == .(cv$n.samples), sep="")),side=3,line=1,adj=0, at=0.00, cex=cex.label)
     
     if(cv$samples.x.n.toshow>0){
       if(v$dist =="DBin"){
         for(i in 1:cv$samples.x.n.toshow){
           text(0,cv$samples.y.mat.toshow[i,1],labels=bquote(paste(x[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.mat.toshow[i])),sep="")),col="blue",pos=4, cex=cex.samples) 
           text(0.3,cv$samples.y.mat.toshow[i,1],labels=bquote(paste(p[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.mat.toshow[i]/v$n)),sep="")),pos=4,cex=cex.samples) 
         }
       }
       else{
         for(i in 1:cv$samples.x.n.toshow){
           text(0,cv$samples.y.mat.toshow[i,1],labels=bquote(paste(bar(x)[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.m.vec.toshow[i])),sep="")),col="blue",pos=4, cex=cex.samples) 
           text(0.3,cv$samples.y.mat.toshow[i,1],labels=bquote(paste(s[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.sd.vec.toshow[i])),sep="")),pos=4,cex=cex.samples) 
         }
       }
       if(cv$n.samples>1 && v$dist!="DBin"){
         mtext(bquote(paste("E(",bar(X),")" == .(cv$samples.x.m.m), sep="")),side=1,line=1,adj=0, at=0.01, cex=cex.label)
         mtext(bquote(paste("V(",bar(X),")" == .(cv$samples.x.v.m), sep="")),side=1,line=1,adj=0, at=0.31, cex=cex.label)
       }
       if(cv$n.samples>1 && v$dist=="DBin"){
         mtext(bquote(paste("E(",X,")" == .(cv$samples.x.m.m), sep="")),side=1,line=-1,adj=0, at=0.01, cex=cex.label)
         mtext(bquote(paste("V(",X,")" == .(cv$samples.x.v.m), sep="")),side=1,line= 1,adj=0, at=0.01, cex=cex.label)
         mtext(bquote(paste("E(",p,")" == .(cv$samples.p.m.m), sep="")),side=1,line=-1,adj=0, at=0.31, cex=cex.label)
         mtext(bquote(paste("V(",p,")" == .(cv$samples.p.v.m), sep="")),side=1,line= 1,adj=0, at=0.31, cex=cex.label)
         
       }
       
     }}
   ### CAS N°2.2 : Si l'option "afficher les stat descr" n'est pas cochée : 
   else {}
 }
 

 

 
 },height = getPlotHeight, width=getPlotWidth)
  
  
  
})
