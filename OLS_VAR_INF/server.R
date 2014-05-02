Sys.setlocale()#to be sure that accents in text will be allowed in plots
library(shiny)
library(bstats)
library(lmtest)
library(nlme)
library(plotrix)
library(sandwich)
library(MASS)


shinyServer(function(input, output) {

  
  rv <- reactiveValues()  # Create a reactiveValues object, to let us use settable reactive values
  rv$last.takesample.value<-0
  rv$X <- list()
  rv$Y <- list()
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
      rv$X <- list()
      rv$Y <- list()

    }
  })
 
  
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
          if (input$beta1 == "h1") {beta1 <- 0.5}          
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
  
  getComputedValues<-reactive({
    X <- list()
    X <- getX()
    rv$X <- c(rv$X,X)
    Y<-list()
    Y<-getY()
    rv$Y<-c(rv$Y,Y)
   
    
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
    
    cv$X<-list()
    cv$Y<-list()
    
    #régression OLS classique
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
           
    
    #régression OLS WHITE
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
      return(cv)
    }
    
  }) 
  

#####-------------------------------------------------------------------------------------#######

  #------------------1. En Situation d'homoscédasticité------------------#
  
output$doublePlot <- renderPlot({

  v <- getInputValues ()
  cv <- getComputedValues ()


par(mfcol= c(1,2))

  
####PLOT 1 : graphe X-Y####

  if(is.null(cv$Y)){
    Y <- c()
    X <-c()
    plot(X, Y, main = "Plot X-Y", xlim = c(0,20), ylim = c(-5,5), xlab = "X", ylab = "Y", bty="n") #nuage de points
  }
    
  else{  
    v <- getInputValues ()
    cv <- getComputedValues ()
    
    #y.lim.inf = min(rev(cv$Y)[[1]])-1
    #y.lim.sup = max(rev(cv$X)[[1]])+1
    plot(rev(cv$X)[[1]], rev(cv$Y)[[1]], main = "Plot X-Y", xlim = c(0,20), xlab = "X", ylab = "Y", bty="n") #nuage de points ylim = c(y.lim.inf, y.lim.sup),
    mtext(bquote(nsamples == .(cv$n.Y)), side = 3, adj = 0, cex = 1)#afficher le nombre d'?chantillons
    abline (rev(cv$res)[[1]], col = "blue")
  }
  
 
####PLOT 2 : Afficher les coefficients estimés####

if(is.null(cv$Y)){
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
}
else{ 
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
  title(main = "Method : Least Squares") 
  estim <- data.frame(c(rev(cv$intercept)[[1]], rev(cv$beta1)[[1]]), c(rev(cv$se.int)[[1]], rev(cv$se.b)[[1]]), c(rev(cv$t.int)[[1]], rev(cv$t.b)[[1]]), c(rev(cv$p.int)[[1]], rev(cv$p.b)[[1]]))
  colnames(estim)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(estim)<-c("Intercept :","Pente :")
  addtable2plot(0,0.5,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.2) 
}

}, height = 300, width = 750)  


Text<-reactive({
  if (input$alpha1 == "homo" &&input$beta1 == "h0"){
    text <-HTML("Si les hypothèses du modèle sont respectées, quand Ho est vraie (la pente est nulle), 
                  le % de RHo doit converger vers le risque d'erreur de type I (fixé à 5%) quand n &rarr; &infin;")
  }
  if (input$alpha1 == "homo" && input$beta1 == "h1"){
    text <-HTML("")
  }
  return(text) 
})
  
output$comments<-renderText({Text()})
  
output$barPlot<-renderPlot({
  
  v <- getInputValues ()
  cv <- getComputedValues ()
  
  ####PLOT : barplot % RH0 et NRH0 OLS classique ####

  if(is.null(cv$Y)){
    includes<-c("NRHo"=0,"RHo"=0)
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
  else{includes<-c("NRHo"=cv$test.conclusion.pc.nrh0,"RHo"=cv$test.conclusion.pc.rh0)
#       par(mai=c(0.5,0.5,0,0))
       if(input$beta1 == "h0") 
       {barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("green", "red"),cex.names=1.1,cex.axis=1.1)}
       
       if(input$beta1 == "h1") 
       {barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("red", "green"),cex.names=1.1,cex.axis=1.1)}   
       
       
       testmean<-data.frame(c(cv$test.conclusion.n.nrh0,cv$test.conclusion.pc.nrh0),c(" "," "),c(cv$test.conclusion.n.rh0,cv$test.conclusion.pc.rh0))
       colnames(testmean)<-c(" NRHo "," "," RHo ")
       rownames(testmean)<-c("n ","% ")
       addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)#,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]"))
  }
  
  }, height = 300, width = 170)

  
#------------------2. En Situation d'hétéroscédasticité------------------#
  
####Output 1 : graphe X-Y####
  
output$XY <- renderPlot({
    
    v <- getInputValues ()
    cv <- getComputedValues ()
  
    if(is.null(cv$Y)){
      Y <- c()
      X <-c()
      plot(X, Y, main = "Plot X-Y", xlim = c(0,20), ylim = c(-5,5), xlab = "X", ylab = "Y", bty="n") #nuage de points
    }
    
    else{  
      v <- getInputValues ()
      cv <- getComputedValues ()
        
      plot(rev(cv$X)[[1]], rev(cv$Y)[[1]], main = "Plot X-Y", xlim = c(0,20), xlab = "X", ylab = "Y", bty="n") #nuage de points 
      mtext(bquote(nsamples == .(cv$n.Y)), side = 3, adj = 0, cex = 1)#afficher le nombre d'?chantillons
      abline (rev(cv$res)[[1]], col = "blue")
    }
    
}, height = 300, width = 375)
 
      
####Output 2 ####
  
output$classique<-renderText({HTML("OLS : Estimations et inférence")}) 

  
####Output 3 ####
  
output$OLS <- renderPlot({

  v <- getInputValues ()
  cv <- getComputedValues ()
  
  par(mfcol = c(1,2))
  m<-matrix(c(1,2),1,2,byrow=TRUE)
  layout(m,width=c(2,1))
  
  ####PLOT : Afficher les coefficients estimés : OLS classique ####
    
    if(is.null(cv$Y)){
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
    else{ 
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
    #  title(main = "Method : Least Squares") 
      estim <- data.frame(c(rev(cv$intercept)[[1]], rev(cv$beta1)[[1]]), c(rev(cv$se.int)[[1]], rev(cv$se.b)[[1]]), c(rev(cv$t.int)[[1]], rev(cv$t.b)[[1]]), c(rev(cv$p.int)[[1]], rev(cv$p.b)[[1]]))
      colnames(estim)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      rownames(estim)<-c("Intercept :","Pente :")
      addtable2plot(-0.2,0.5,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.2) 
    }
    
  ####PLOT : barplot % RH0 et NRH0 OLS classique ####
  
  if(is.null(cv$Y)){
    includes<-c("NRHo"=0,"RHo"=0)
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
  else{includes<-c("NRHo"=cv$test.conclusion.pc.nrh0,"RHo"=cv$test.conclusion.pc.rh0)
       #       par(mai=c(0.5,0.5,0,0))
       if(input$beta1 == "h0") 
       {barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("green", "red"),cex.names=1.1,cex.axis=1.1)}
       
       if(input$beta1 == "h1") 
       {barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("red", "green"),cex.names=1.1,cex.axis=1.1)}   
       
       
       testmean<-data.frame(c(cv$test.conclusion.n.nrh0,cv$test.conclusion.pc.nrh0),c(" "," "),c(cv$test.conclusion.n.rh0,cv$test.conclusion.pc.rh0))
       colnames(testmean)<-c(" NRHo "," "," RHo ")
       rownames(testmean)<-c("n ","% ")
       addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)#,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]"))
  }
  }, height = 200, width = 550)
  

####Output 4 #### 
  
output$white<-renderText({HTML("OLS avec inférence robuste : Estimations et inférence (White)")}) 
  
  
####Output 5 #### 
  
output$OLSW <- renderPlot({
    
    v <- getInputValues ()
    cv <- getComputedValues ()
    
    par(mfcol = c(1,2))
    m<-matrix(c(1,2),1,2,byrow=TRUE)
    layout(m,width=c(2,1))
    
    ####PLOT : Afficher les coefficients estimés : OLS classique ####
    
    if(is.null(cv$Y)){
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
    else{ 
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
     # title(main = "Method : Least Squares") 
      estim <- data.frame(c(rev(cv$interceptw)[[1]], rev(cv$beta1w)[[1]]), c(rev(cv$se.intw)[[1]], rev(cv$se.bw)[[1]]), c(rev(cv$t.intw)[[1]], rev(cv$t.bw)[[1]]), c(rev(cv$p.intw)[[1]], rev(cv$p.bw)[[1]]))
      colnames(estim)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      rownames(estim)<-c("Intercept :","Pente :")
      addtable2plot(-0.2,0.5,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.2) 
    }
    
    ####PLOT : barplot % RH0 et NRH0 OLS classique ####
    
    if(is.null(cv$Y)){
      includes<-c("NRHo"=0,"RHo"=0)
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
    else{includes<-c("NRHo"=cv$test.w.conclusion.pc.nrh0,"RHo"=cv$test.w.conclusion.pc.rh0)
         #       par(mai=c(0.5,0.5,0,0))
         if(input$beta1 == "h0") 
         {barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("green", "red"),cex.names=1.1,cex.axis=1.1)}
         
         if(input$beta1 == "h1") 
         {barplot.H0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col= c("red", "green"),cex.names=1.1,cex.axis=1.1)}   
         
         
         testmean<-data.frame(c(cv$test.w.conclusion.n.nrh0,cv$test.w.conclusion.pc.nrh0),c(" "," "),c(cv$test.w.conclusion.n.rh0,cv$test.w.conclusion.pc.rh0))
         colnames(testmean)<-c(" NRHo "," "," RHo ")
         rownames(testmean)<-c("n ","% ")
         addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)#,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]"))
    }
  }, height = 200, width = 550) 

  
})   
 