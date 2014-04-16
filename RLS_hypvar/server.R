Sys.setlocale()#to be sure that accents in text will be allowed in plots
library(shiny)
library(bstats)
library(lmtest)
library(plotrix)
library(sandwich)


shinyServer(function(input, output) {

  #--------------------------------------------------------------------------------
  #                             Tabset 1  : données générées
  #--------------------------------------------------------------------------------
  
  rv <- reactiveValues()  # Create a reactiveValues object, to let us use settable reactive values
  rv$last.Y.value<-0
  rv$X<-list()
  rv$Y<-list()
  rv$lastAction <- 'none'# To start out, lastAction == NULL, meaning nothing clicked yet
 

  # An observe block for each button, to record that the action happened
  observe({
    if (input$takeY != 0) {rv$lastAction <- 'takeY'}
  })
  observe({
    if (input$reset != 0) {
      rv$lastAction <- 'reset'
      rv$Y<-list()}
  })
  
  getX <- reactive({
    if(rv$lastAction == "takeY"){return(rv$X<-runif (n = input$n, min = 0, max = 20))}
      })
  
  getY <- reactive({
    X <-getX()
    X <-unlist(X)
    if(rv$lastAction == "takeY"){
    if(input$var == "exp") {var.eps <- input$alpha0*(X^input$alpha1)}
    if (input$var == "lin") {var.eps <- input$alpha0+input$alpha1*(X^2)}
    espilon <-rnorm(n = input$n, mean = 0, sd = sqrt(var.eps))
    return (rv$Y <-input$intercept + input$beta1*X + espilon )
    }
      })
  

output$doublePlot1 <- renderPlot({

  X <-getX()
  X <-unlist(X) 
  Y<-getY()
  Y<-unlist(Y) 

par(mfrow = c(2,2))

##### PLOT XY ######
if(is.null(Y)) {
  Y <- c()
  X <-c()
  plot(X, Y, main = "Plot X-Y", xlim = c(0,20), ylim = c(-5,5), xlab = "X", ylab = "Y", bty="n") #nuage de points
} 

else {  
  Y<-getY()
  Y<-unlist(Y)
  y.lim.inf = min(Y)-1
  y.lim.sup = max(Y)+1
    plot(X, Y, main = "Plot X-Y", xlim = c(0,20), ylim = c(y.lim.inf, y.lim.sup), xlab = "X", ylab = "Y", bty="n") #nuage de points
  res <- lm (Y ~ X)
  sumres <-summary(res)
  if(input$droite=="TRUE"){abline (res, col = "blue")}

  if(input$Coef1=="TRUE"){
    ## Plot vide pour afficher les coefficients estimés
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
    coef <-res[1]
    coef <-unlist(coef)
    intercept <- round(coef[1],4)
    beta1 <- round(coef[2],4)
    p.int<-round(sumres$coefficients[7],4)
    p.b <- round(sumres$coefficients[8],4)
  estim <- data.frame(c(intercept, beta1), c(p.int, p.b))
  colnames(estim)<-c("Estimations", " Pr(>|t|)")
  rownames(estim)<-c("Intercept :","Pente :")
  addtable2plot(0,0.25,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.3) 
    } 
  else{plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
  
#####PLOT RESID VS X ##########  
  
  resid <- unlist(res[2])
  if(input$plotresid=="TRUE"){plot (X, resid, main = HTML("Plot des résidus en fonction de X"), bty="n")
  abline (h = 0)} 
  else{plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 

  if(input$Test1=="TRUE"){
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
    bp <- bptest(Y ~ X)
    res <- lm (Y ~ X)
    white <- white.test(res)
    bpstat<-round(bp$statistic, 4)
    wstat <- round(white$statistic, 4)
    bpp <-round(bp$p.value,5)
    wp <-round(white$p.value, 5)
    tests <- data.frame(c(bpstat, wstat), c(bpp,wp))
    colnames(tests)<-c("Statistique","p-valeur" )
    rownames(tests)<-c("Breusch-Pagan","White :")
    addtable2plot(0,0.25,tests,display.rownames=TRUE,title=bquote("Tests d'homoscédasticité"), cex = 1.3) 
    }
  else{plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 

}}, height = 550, width = 750)
 
        
    
            
#--------------------------------------------------------------------------------
#                           Tabset 2  : données téléchargées
#--------------------------------------------------------------------------------
  
  
output$contents <- renderTable({
    if(input$obs=="TRUE"){  
   data <- read.csv('rent.csv', header=T, sep=';')
   data<- as.data.frame(data)
   data[1:10,1:2]}
   })
  
output$InitPlot<- renderPlot({
  data <- read.csv('rent.csv', header=T, sep=';')
  data<- as.data.frame(data)
    
  X <- data$income/1000
  Y <- data$rent/1000
  
  par(mfcol = c(3,2))
  m<-matrix(c(1,1,1,2,3,4),3,2,byrow=FALSE)
  layout(m,width=c(2,3))  
  
  ####Plot1######
  plot(X, Y, main ="Plot X-Y", bty="n", pch = 19, xlim = c(0,100), ylim = c(0,30), xlab = "Income (dollars, x1000)", ylab = "Rent (dollars, x1000)", cex.axis = 1.3, cex.lab = 1.4, cex.main = 1.5)
  res2 <- lm (Y~X)
  if(input$droite2=="TRUE"){abline (res2, col = "blue")}
 
  
  ####Plot2######
  if(input$Coef2=="TRUE"){
    ## Plot vide pour afficher les coefficients estimés
  par(mai = c(0,0,1,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = "Method : Least Squares", cex.main = 1.5)  
  X <- X*1000
  Y <- Y*1000 
  res3 <-lm(Y~X)
  sumres2 <- summary(res3)
    coef2 <- sumres2$coefficients
    a <-signif(coef2[1],4)
    b <-signif(coef2[2],4)
    c <-signif(coef2[3],4)
    d <-signif(coef2[4],4)
    e <-signif(coef2[5],4)
    f <-signif(coef2[6],4)
    g <-signif(coef2[7],5)
    h <-signif(coef2[8],5)
          
    estim2 <- data.frame(c(a,b), c(c,d), c(e,f), c(g,h))
    colnames(estim2)<-c("Coefficient", "Std.Error", "t-Statistic", "Prob.")
    rownames(estim2)<-c("Intercept","Income")
    addtable2plot(0,0,estim2,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.6) 
  }
  else{plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
  
  
  ####Plot3######
  if(input$Coef2=="TRUE"){
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  r <-signif(sumres2$r.squared,4)
  ra<-signif(sumres2$adj.r.squared,4)
  fstat <- signif(unlist(sumres2[10])[1], 4)
  pfstat <- signif(1-pf(sumres2$fstatistic[1],sumres2$fstatistic[2],sumres2$fstatistic[3]),4)
  rsq <- data.frame(c(r, ra), c("F-statistic", "Prob(F-stat)"), c(fstat, pfstat))
  colnames(rsq)<-c("")
  rownames(rsq)<-c("R-squared", "Adjusted R-squared")
  addtable2plot(0,0.5,rsq,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.6) 
  }
  else{plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
  
  
  ####Plot4######
  if(input$Test2=="TRUE"){
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homoscédasticité", cex.main = 1.5, adj = 0  
  X <- data$income
  Y <- data$rent
  bp2 <- bptest(Y ~ X)
  restest<-lm(Y~X)
  w2<- white.test(restest)
  i<-signif(bp2$statistic,4)
  j<-signif(w2$statistic,4)
  k<-round(bp2$p.value,5)
  l<-round(w2$p.value,5)
  tests2 <- data.frame(c(i,j), c(k,l))
  colnames(tests2)<-c("Statistique","p-valeur" )
  rownames(tests2)<-c("Breusch-Pagan","White :")
  addtable2plot(0.15,0.35,tests2,display.rownames=TRUE, hlines=FALSE, title=bquote("Tests d'homoscédasticité"), cex = 1.6) 
  }
  else{plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
  
  
}, height = 300, width = 700)
  

############# Remèdes ###########################
  
output$caption <- renderText({
  
  if(input$sol=="logY"){
    "Solution envisagée : prendre le log(Y)"
  }
  
})  
  
  
  output$LogPlot<- renderPlot({
  
  if(input$sol=="logY"){
    
    data <- read.csv('rent.csv', header=T, sep=';')
    data<- as.data.frame(data)
    
    X <- data$income/1000
    Y <- log(data$rent/1000)
    
    par(mfcol = c(3,2))
    m<-matrix(c(1,1,1,2,3,4),3,2,byrow=FALSE)
    layout(m,width=c(2,3))  
    
    ####Plot1######
    plot(X, Y, main ="Plot X-log(Y)", bty="n", pch = 19, xlim = c(0,100), ylim = c(0,5), xlab = "Income (dollars, x1000)", ylab = "Log(Rent (dollars, x1000))", cex.axis = 1.3, cex.lab = 1.4, cex.main = 1.5)
    res <- lm (Y~X)
    if(input$droite2=="TRUE"){abline (res, col = "blue")}
    
    par(mai = c(0,0,1,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = "Method : Least Squares", cex.main = 1.5)  
    X <- X*1000
    Y <- Y*1000 
    res <-lm(Y~X)
    sumres <- summary(res)
    coef <- sumres$coefficients
    a <-signif(coef[1],4)
    b <-signif(coef[2],4)
    c <-signif(coef[3],4)
    d <-signif(coef[4],4)
    e <-signif(coef[5],4)
    f <-signif(coef[6],4)
    g <-signif(coef[7],5)
    h <-signif(coef[8],5)
      
    estim <- data.frame(c(a,b), c(c,d), c(e,f), c(g,h))
      colnames(estim)<-c("Coefficient", "Std.Error", "t-Statistic", "Prob.")
      rownames(estim)<-c("Intercept","Income")
      addtable2plot(0,0,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.6) 
        
    ####Plot3######
   par(mai = c(0,0,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
      r <-signif(sumres$r.squared,4)
      ra<-signif(sumres$adj.r.squared,4)
      fstat <- signif(unlist(sumres[10])[1], 4)
      pfstat <- signif(1-pf(sumres$fstatistic[1],sumres$fstatistic[2],sumres$fstatistic[3]),4)
      rsq <- data.frame(c(r, ra), c("F-statistic", "Prob(F-stat)"), c(fstat, pfstat))
      colnames(rsq)<-c("")
      rownames(rsq)<-c("R-squared", "Adjusted R-squared")
      addtable2plot(0,0.5,rsq,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.6) 
        
    ####Plot4######
   par(mai = c(0,0,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homoscédasticité", cex.main = 1.5, adj = 0  
      X <- data$income
      Y <- log(data$rent)
      bp <- bptest(Y ~ X)
      restest<-lm(Y~X)
      w<- white.test(restest)
      i<-signif(bp$statistic,4)
      j<-signif(w$statistic,4)
      k<-round(bp$p.value,5)
      l<-round(w$p.value,5)
      tests <- data.frame(c(i,j), c(k,l))
      colnames(tests)<-c("Statistique","p-valeur" )
      rownames(tests)<-c("Breusch-Pagan","White :")
      addtable2plot(0.15,0.35,tests,display.rownames=TRUE, hlines=FALSE, title=bquote("Tests d'homoscédasticité"), cex = 1.6) 
    }
     
    
  }, height = 300, width = 700)
  
  
})   
 