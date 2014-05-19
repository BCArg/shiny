#to be sure that accents in text will be allowed in plots
library(shiny)
library(bstats)
library(lmtest)
library(nlme)
library(plotrix)
library(sandwich)
library(MASS)
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

debug<-1#set to 1 to debug layout (draw boxes)

shinyServer(function(input, output) {

  
  rv <- reactiveValues()  # Create a reactiveValues object, to let us use settable reactive values
  rv$last.takesample.value<-0
  rv$X <- list()
  rv$Y <- list()
  rv$lastAction <- 'none'# To start out, lastAction == 'none', meaning nothing clicked yet
  #Set a reactive value to record last value of n, alpha1, and beta1 to be able to reset samples on change. Theses reactives values will have the input corresponding values at the end of getComputedValues
  rv$lastAlpha1<-0
  rv$lastBeta1<-0
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
      rv$X <- list()
      rv$Y <- list()
    }
  })
  
#  
  
  
  getX <- reactive({
    if(input$takesample > rv$last.takesample.value && rv$lastAction == "takesample"){
        X <- list()
        for (i in 1:input$ns){
          X[[i]]<- runif(n = input$n, min = 0, max = 20)}
      return (X)
  }
    else {
      return(NULL)
    }})
  
  
  getY <- reactive({
     if(input$takesample > rv$last.takesample.value && rv$lastAction == "takesample"){
      
        X <- getX()
        var.eps <- list()
        epsilon <-list()
        Y <- list()
        
        for (i in 1:input$ns){
          if (input$beta1 == "h0") {beta1 <- 0}
          if (input$beta1 == "h1") {beta1 <- 1}          
          if (input$alpha1 == "homo") {var.eps[[i]] <- 2*(X[[i]]^0)}
          if (input$alpha1 == "hetero") {var.eps[[i]] <- 2*X[[i]]^input$V.alpha1}
          epsilon[[i]] <-rnorm(n = input$n, mean = 0, sd = sqrt(var.eps[[i]]))
                    
          Y[[i]] <-0 + beta1*X[[i]]+ epsilon[[i]]
        }      
      return (Y)} 
    else {return(NULL)}
     })
  
  getInputValues<-reactive({
    return(input)#collect all inputs
  })
  
  getPlotHeight <- function() {
      if(input$display=="default") {
	unit.height<-200 #cannot be auto because height is already "auto" in ui and double auto = conflict
      }
      if(input$display=="1024") {
	unit.height<-180
      }
      if(input$display=="800") {
	unit.height<-140 
      }
    return(2*unit.height)#because there is 2 rows in main plot
  }
  
  getPlotWidth <- function() {
      if(input$display=="default") {
	full.plot.width<-1310-310#"auto"
      }
      if(input$display=="1024") {
	full.plot.width<-900-310
      }
      if(input$display=="800") {
	full.plot.width<-700-310
      }
      if(input$visM && input$display!="default"){
	full.plot.width<-full.plot.width+310
      }
    return(full.plot.width)
  }
  
  getComputedValues<-reactive({
    X <- list()
    X <- getX()
    rv$X <- c(rv$X,X)
    Y<-list()
    Y<-getY()
    rv$Y<-c(rv$Y,Y)
   
    
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
    
    if (rv$lastN!=v$n || rv$lastBeta1!=v$beta1 || rv$lastAlpha1!=v$alpha1) {
    rv$lastAction <- 'changeN'
    rv$last.takesample.value<-0
    rv$X <- list()
    rv$Y <- list()
    }
    
    cv$X<-list()
    cv$Y<-list()
    
    #regression OLS classique
    cv$res<-list()
    cv$sumres<-list()
    cv$coef<-list()
    cv$intercept<-list()
    cv$beta1<-list()
    cv$se.int<-list()
    cv$se.b<-list()
    cv$t.int<-list()
    cv$t.b<-list()
    cv$p.int<-list()
    cv$p.b<-list()
    
    cv$test.conclusion <- list()
    
    cv$test.conclusion.pc.rh0<-0
    cv$test.conclusion.pc.nrh0<-0
           
    
    #regression OLS WHITE
    cv$resw<-list()    
    cv$interceptw<-list()
    cv$beta1w<-list()
    cv$se.intw<-list()
    cv$se.bw<-list()
    cv$t.intw<-list()
    cv$t.bw<-list()
    cv$p.intw<-list()
    cv$p.bw<-list()
    
    cv$test.w.conclusion <- list()
    
    cv$test.w.conclusion.pc.rh0<-0
    cv$test.w.conclusion.pc.nrh0<-0
     
    
    cv$n.Y<-length(rv$Y)
        
    if(cv$n.Y>0){
      for(i in 1:cv$n.Y){
          cv$X[[i]]<- rv$X[[i]]
          cv$Y[[i]]<- rv$Y[[i]]
          
          #OLS classique
          cv$res[[i]]<-lm(cv$Y[[i]] ~ cv$X[[i]])
          cv$sumres[[i]]<-summary(cv$res[[i]])
          cv$coef[[i]]<-cv$sumres[[i]]$coefficients
          cv$intercept[[i]]<-signif(cv$coef[[i]][1],4)
          cv$beta1[[i]]<-signif(cv$coef[[i]][2],4)
          cv$se.int[[i]]<-signif(cv$coef[[i]][3],4)
          cv$se.b[[i]]<-signif(cv$coef[[i]][4],4)
          cv$t.int[[i]]<-signif(cv$coef[[i]][5],4)
          cv$t.b[[i]]<-signif(cv$coef[[i]][6],4)
          cv$p.int[[i]]<-round(cv$coef[[i]][7],5)
          cv$p.b[[i]]<-round(cv$coef[[i]][8],5)
          
          if(cv$p.b[[i]] < 0.05)
          {cv$test.conclusion[[i]]<-"rh0"} 
          else {cv$test.conclusion[[i]]<-"nrh0"}
          
          if(length(cv$test.conclusion)>0){
            cv$test.conclusion.n.rh0<-length(which(cv$test.conclusion == "rh0"))
            cv$test.conclusion.n.nrh0<-cv$n.Y-cv$test.conclusion.n.rh0
            cv$test.conclusion.pc.rh0<-round(cv$test.conclusion.n.rh0/length(cv$test.conclusion),4)*100
            cv$test.conclusion.pc.nrh0<-100-cv$test.conclusion.pc.rh0
          }
          
          #OLS avec inférence robuste (White)
          f1 <- formula(cv$Y[[i]] ~ cv$X[[i]])
          cv$resw[[i]]<- coeftest(lm(f1), vcov = (vcovHC(lm(f1), "HC0")))
          cv$interceptw[[i]] <- signif(cv$resw[[i]][1],4)
          cv$beta1w[[i]] <- signif(cv$resw[[i]][2],4)
          cv$se.intw[[i]]<-signif(cv$resw[[i]][3],4)
          cv$se.bw[[i]]<-signif(cv$resw[[i]][4],4)
          cv$t.intw[[i]]<-signif(cv$resw[[i]][5],4)
          cv$t.bw[[i]]<-signif(cv$resw[[i]][6],4)
          cv$p.intw[[i]]<-round(cv$resw[[i]][7],5)
          cv$p.bw[[i]]<-round(cv$resw[[i]][8],5)
          
          if(cv$p.bw[[i]] < 0.05)
          {cv$test.w.conclusion[[i]]<-"rh0"} 
          else {cv$test.w.conclusion[[i]]<-"nrh0"}
      
          if(length(cv$test.w.conclusion)>0){
            cv$test.w.conclusion.n.rh0<-length(which(cv$test.w.conclusion == "rh0"))
            cv$test.w.conclusion.n.nrh0<-cv$n.Y-cv$test.w.conclusion.n.rh0
            cv$test.w.conclusion.pc.rh0<-round(cv$test.w.conclusion.n.rh0/length(cv$test.w.conclusion),4)*100
            cv$test.w.conclusion.pc.nrh0<-100-cv$test.w.conclusion.pc.rh0
          }
          rv$last.takesample.value<-v$takesample
      }
      #return(cv)#Etait sur mauvaise ligne !!!
    }
    #determination des limites de l'axe Y
    cv$y.lim.nint<-8
    cv$y.lim.sup<-8
    cv$y.lim.inf<--8
    if(cv$n.Y>0){
      cv$y.lim.sup <-(round((max(abs(rev(cv$Y)[[1]])) / cv$y.lim.nint)*1.1) + 1) * cv$y.lim.nint #Find the nearest integer divisible by 8 : from http://stackoverflow.com/questions/15672096/next-nearest-number-divisible-by-x : le facteur 1.1 est ajouté pour qu'il y ai toujours au moins 10% de l'axe dispo au dessus de la valeur maximale pour une meilleure visibilité
      if(cv$y.lim.sup < 8){
	cv$y.lim.sup<-8
      }
      cv$y.lim.inf <- cv$y.lim.sup*-1
    }
    
    rv$lastAlpha1<-v$alpha1
    rv$lastBeta1<-v$beta1
    rv$lastN<-v$n
    
    return(cv)
  }) 
  

#####-------------------------------------------------------------------------------------#######

# ---------------Common plot for all situations -------------------------#

output$mainPlot <- renderPlot({
  # one of the main of a common plot instead of different plots is to call getComputedValues() only once by page call, wich accelerate the script execution
  v <- getInputValues ()
  cv <- getComputedValues ()
  # one other advanteg is to be able to define a layout more precisely
  m<-matrix(c(1,2,3,1,4,5),2,3,byrow=TRUE)#first plot rendering on the two rows
  layout(m,width=c(3,2,1))#on each row, the two first plots width is double than the third (barplot)
  
  ####PLOT 1 : graphe X-Y####

    if(cv$n.Y==0){
    Y <- c()
    X <-c()
    plot(X, Y, main = "Graphique X-Y", xlim = c(0,20),ylim = c(cv$y.lim.inf,cv$y.lim.sup), xlab = "X", ylab = "Y",  xaxs="i", yaxs="i",xaxp=c(0,20,10),yaxp=c(cv$y.lim.inf,cv$y.lim.sup,cv$y.lim.nint),las=1) #nuage de points bty="n",
    mtext(bquote(k == .(0)), side = 3, adj = 0, cex = 1)#afficher le nombre d'échantillons
    lines(c(0,20),c(0,0),lty=3)
  } else { #This plot is the same in homo and hetero for v$alpha1 : so do not create it twice : is someone change one, he might not change the other : avoid this
      plot(rev(cv$X)[[1]], rev(cv$Y)[[1]], main = "Graphique X-Y", xlim = c(0,20),xaxp=c(0,20,10), xlab = "X", ylab = "Y",ylim = c(cv$y.lim.inf,cv$y.lim.sup),yaxp=c(cv$y.lim.inf,cv$y.lim.sup,cv$y.lim.nint),xaxs="i",yaxs="i",las=1)
      mtext(bquote(k == .(cv$n.Y)), side = 3, adj = 0, cex = 1)#afficher le nombre d'échantillons
      abline (rev(cv$res)[[1]], col = "blue")
      lines(c(0,20),c(0,0),lty=3)
  }
  if(debug){
    box(which="outer",lty = 'dotted', col = 'red')
    box(which="figure",lty = 'dotted', col = 'blue')
    box(which="plot",lty = 'dotted', col = 'blue')
  }
  ####PLOT 2 : Afficher les coefficients estimés####

  if(cv$n.Y==0){
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  }
  else{ 
    if(v$alpha1 == "homo"){
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
      title(main = "Méthode des moindres carrés ordinaires") 
      estim <- data.frame(c(rev(cv$intercept)[[1]], rev(cv$beta1)[[1]]), c(rev(cv$se.int)[[1]], rev(cv$se.b)[[1]]), c(rev(cv$t.int)[[1]], rev(cv$t.b)[[1]]), c(rev(cv$p.int)[[1]], rev(cv$p.b)[[1]]))
      colnames(estim)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      rownames(estim)<-c("Intercept :","Pente :")
      addtable2plot(0,0.75,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.1) 
    } else {#v$alpha1 == 'hetero'
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
      title(main = "OLS : Estimations et inférence")
      estim <- data.frame(c(rev(cv$intercept)[[1]], rev(cv$beta1)[[1]]), c(rev(cv$se.int)[[1]], rev(cv$se.b)[[1]]), c(rev(cv$t.int)[[1]], rev(cv$t.b)[[1]]), c(rev(cv$p.int)[[1]], rev(cv$p.b)[[1]]))
      colnames(estim)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      rownames(estim)<-c("Intercept :","Pente :")
      addtable2plot(-0.2,0.5,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.2) 
    }
  }
  if(debug){
    box(which="outer",lty = 'dotted', col = 'red')
    box(which="figure",lty = 'dotted', col = 'blue')
    box(which="plot",lty = 'dotted', col = 'blue')
  }
  ####PLOT 3 : barplot % RH0 et NRH0 OLS classique ####
  if(v$barplot!= 0){
    if(is.null(cv$Y)){
      includes<-c("NRHo"=0,"RHo"=0)
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
    else{
      includes<-c("NRHo"=cv$test.conclusion.pc.nrh0,"RHo"=cv$test.conclusion.pc.rh0)
      if(input$beta1 == "h0"){
	barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("green", "red"),cex.names=1.1,cex.axis=1.1)
	}
      if(input$beta1 == "h1"){
	barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("red", "green"),cex.names=1.1,cex.axis=1.1)
	}   
      testmean<-data.frame(c(cv$test.conclusion.n.nrh0,cv$test.conclusion.pc.nrh0),c(" "," "),c(cv$test.conclusion.n.rh0,cv$test.conclusion.pc.rh0))
      colnames(testmean)<-c(" NRHo "," "," RHo ")
      rownames(testmean)<-c("n ","% ")
      addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)
    }
  } else {
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  }
  if(debug){
    box(which="outer",lty = 'dotted', col = 'red')
    box(which="figure",lty = 'dotted', col = 'blue')
    box(which="plot",lty = 'dotted', col = 'blue')
  }
  ####PLOT 4 : Afficher les coefficients estimés de seconde situtaion ####

  if(cv$n.Y>0 && v$alpha1 == "hetero"){
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
    title(main = "OLS avec inférence robuste (White)")
    estim <- data.frame(c(rev(cv$interceptw)[[1]], rev(cv$beta1w)[[1]]), c(rev(cv$se.intw)[[1]], rev(cv$se.bw)[[1]]), c(rev(cv$t.intw)[[1]], rev(cv$t.bw)[[1]]), c(rev(cv$p.intw)[[1]], rev(cv$p.bw)[[1]]))
    colnames(estim)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    rownames(estim)<-c("Intercept :","Pente :")
    addtable2plot(-0.2,0.5,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.2) 
  } else {
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  }
  if(debug){
    box(which="outer",lty = 'dotted', col = 'red')
    box(which="figure",lty = 'dotted', col = 'blue')
    box(which="plot",lty = 'dotted', col = 'blue')
  }
  ####PLOT 5 : barplot % RH0 et NRH0 OLS White ####

  if(v$barplot!= 0){
    if(is.null(cv$Y)){
      includes<-c("NRHo"=0,"RHo"=0)
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    } else{
      if(v$alpha1 == "hetero"){
	includes<-c("NRHo"=cv$test.w.conclusion.pc.nrh0,"RHo"=cv$test.w.conclusion.pc.rh0)
	if(input$beta1 == "h0"){
	  barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("green", "red"),cex.names=1.1,cex.axis=1.1)
	  }
	if(input$beta1 == "h1"){
	  barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("red", "green"),cex.names=1.1,cex.axis=1.1)
	  }   
	testmean<-data.frame(c(cv$test.w.conclusion.n.nrh0,cv$test.w.conclusion.pc.nrh0),c(" "," "),c(cv$test.w.conclusion.n.rh0,cv$test.w.conclusion.pc.rh0))
	colnames(testmean)<-c(" NRHo "," "," RHo ")
	rownames(testmean)<-c("n ","% ")
	addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)
      }
    }
  } else {
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  }
  if(debug){
    box(which="outer",lty = 'dotted', col = 'red')
    box(which="figure",lty = 'dotted', col = 'blue')
    box(which="plot",lty = 'dotted', col = 'blue')
  }
}, height = getPlotHeight, width=getPlotWidth)

})  
 