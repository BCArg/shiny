Sys.setlocale()#to be sure that accents in text will be allowed in plots
library(shiny)
library(bstats)
library(lmtest)
library(nlme)
library(plotrix)
library(sandwich)
library(MASS)


shinyServer(function(input, output) {

  #--------------------------------------------------------------------------------
  #                             Tabset 1  : donn?es g?n?r?es
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
    ## Plot vide pour afficher les coefficients estim?s
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')  
    coef <-res[1]
    coef <-unlist(coef)
    intercept <- signif(coef[1],4)
    beta1 <- signif(coef[2],4)
    p.int<-round(sumres$coefficients[7],5)
    p.b <- round(sumres$coefficients[8],5)
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
    bpstat<-signif(bp$statistic, 4)
    wstat <- signif(white$statistic, 4)
    bpp <-round(bp$p.value,5)
    wp <-round(white$p.value, 5)
    tests <- data.frame(c(bpstat, wstat), c(bpp,wp))
    colnames(tests)<-c("Statistique","p-valeur" )
    rownames(tests)<-c("Breusch-Pagan","White :")
    addtable2plot(0,0.25,tests,display.rownames=TRUE,title=HTML("Tests d'homosc?dasticit?"), cex = 1.3) 
    }
  else{plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 

}}, height = 550, width = 750)
 
        
    
            
#--------------------------------------------------------------------------------
#                           Tabset 2  : donn?es t?l?charg?es
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
    ## Plot vide pour afficher les coefficients estim?s
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
    g <-round(coef2[7],5)
    h <-round(coef2[8],5)
          
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
  pfstat <- round(1-pf(sumres2$fstatistic[1],sumres2$fstatistic[2],sumres2$fstatistic[3]),5)
  rsq <- data.frame(c(r, ra), c("F-statistic", "Prob(F-stat)"), c(fstat, pfstat))
  colnames(rsq)<-c("")
  rownames(rsq)<-c("R-squared", "Adjusted R-squared")
  addtable2plot(0,0.5,rsq,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(""), cex = 1.6) 
  }
  else{plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')} 
  
  
  ####Plot4######
  if(input$Test2=="TRUE"){
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homosc?dasticit?", cex.main = 1.5, adj = 0  
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
  

############# Rem?des ###########################

Text <- reactive({
  
  if (input$sol=="aucune"){} 
  else{
  if(input$sol=="Trans" && input$tt == "logY"){option <- HTML("prendre le log(Y)")}  
  if(input$sol=="Trans" && input$tt == "BC"){option <- HTML("transformation box cox de Y")}
  if(input$sol=="OLS"){option <-HTML("OLS avec inférence robuste")}  
  if(input$sol=="GLS" && input$wi == "GLS1"){option <-HTML("GLS avec poids estimés = 1/X")}
  if(input$sol=="GLS" && input$wi == "GLS2"){option <-HTML("GLS avec poids estimés = 1/sqrt(X)")}
  if(input$sol=="GLS" && input$wi == "GLS3"){option <-HTML("GLS avec poids estimés = 1/X^2")}
  if(input$sol=="FGLS"){option <-HTML("FGLS")}
  
  paste(HTML("Solution envisagée :"), option)}
  })
  
  
output$Caption <- renderText({Text()})  
  
  
output$SolutPlot<- renderPlot({


#----------------------Option n°1.1. LOGY------------------------------

 
if(input$sol=="Trans" && input$tt=="logY"){
    
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
    
    #####Plot2#####
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
    g <-round(coef[7],5)
    h <-round(coef[8],5)
      
    estim <- data.frame(c(a,b), c(c,d), c(e,f), c(g,h))
      colnames(estim)<-c("Coefficient", "Std.Error", "t-Statistic", "Prob.")
      rownames(estim)<-c("Intercept","Income")
      addtable2plot(0,0,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
        
    ####Plot3######
   par(mai = c(0,0,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
      r <-signif(sumres$r.squared,4)
      ra<-signif(sumres$adj.r.squared,4)
      fstat <- signif(unlist(sumres[10])[1], 4)
      pfstat <- round(1-pf(sumres$fstatistic[1],sumres$fstatistic[2],sumres$fstatistic[3]),5)
      rsq <- data.frame(c(r, ra), c("F-statistic", "Prob(F-stat)"), c(fstat, pfstat))
      colnames(rsq)<-c("")
      rownames(rsq)<-c("R-squared", "Adjusted R-squared")
      addtable2plot(0,0.5,rsq,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
        
    ####Plot4######
   par(mai = c(0,0,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homosc?dasticit?", cex.main = 1.5, adj = 0  
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
      addtable2plot(0.15,0.35,tests,display.rownames=TRUE, hlines=FALSE, title=HTML("Tests d'homoscédasticité"), cex = 1.6) 
    }
  

#--------------------Option n°1.2. BOX COX------------------------------

if(input$sol=="Trans" && input$tt=="BC"){
  data <- read.csv('rent.csv', header=T, sep=';')
  data<- as.data.frame(data)
  X <- data$income
  Y<-data$rent
  
if (input$boxcox =="lambda"){  

par(mfcol = c(1,2))

####Plot1######
  b <- boxcox(Y ~ X, lambda = seq(-2, 2, length = 10))
  title(main = "Index qui maximise le log de vraisemblance", cex.main = 1)
  
####Plot2######
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homosc?dasticit?", cex.main = 1.5, adj = 0  
  lambda <- b$x
  lik <- b$y
  bc <- cbind(lambda, lik)
  maxlambda <- bc[order(-lik), ][1]
  m <- round(maxlambda, 4)
  lam <- data.frame(m)
  colnames(lam)<-c("Meilleure valeur de lambda :" )
  rownames(lam)<-c("")
  addtable2plot(0,0.5,lam,display.rownames=TRUE, hlines=FALSE, title=(""), cex = 1.2)
}

if (input$boxcox =="OLSYt"){  
  par(mfcol = c(3,2))
  m<-matrix(c(1,1,1,2,3,4),3,2,byrow=FALSE)
  layout(m,width=c(2,3)) 
  
####Plot1#####
  b <- boxcox(Y ~ X, lambda = seq(-2, 2, length = 10), plotit = FALSE)
  lambda <- b$x
  lik <- b$y
  bc <- cbind(lambda, lik)
  maxlambda <- bc[order(-lik), ][1]
  Yt <- Y^maxlambda
  plot(Yt ~X, main = "Plot X-Y^lambda", bty="n", pch = 19, cex.axis = 1.3, cex.lab = 1.4, cex.main = 1.5) 
  bc.lm <- lm (Yt ~X) 
  abline(bc.lm, col = "blue")

####Plot2####

par(mai = c(0,0,1,0))
plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = "Method : Least Squares", cex.main = 1.5)  
sumbc.lm <- summary(bc.lm)
coef <- sumbc.lm$coefficients
a <-signif(coef[1],4)
b <-signif(coef[2],4)
c <-signif(coef[3],4)
d <-signif(coef[4],4)
e <-signif(coef[5],4)
f <-signif(coef[6],4)
g <-round(coef[7],5)
h <-round(coef[8],5)

estim <- data.frame(c(a,b), c(c,d), c(e,f), c(g,h))
colnames(estim)<-c("Coefficient", "Std.Error", "t-Statistic", "Prob.")
rownames(estim)<-c("Intercept","Income")
addtable2plot(0,0,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 


####Plot3######
par(mai = c(0,0,0,0))
plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
r <-signif(sumbc.lm$r.squared,4)
ra<-signif(sumbc.lm$adj.r.squared,4)
fstat <- signif(unlist(sumbc.lm[10])[1], 4)
pfstat <- round(1-pf(sumbc.lm$fstatistic[1],sumbc.lm$fstatistic[2],sumbc.lm$fstatistic[3]),5)
rsq <- data.frame(c(r, ra), c("F-statistic", "Prob(F-stat)"), c(fstat, pfstat))
colnames(rsq)<-c("")
rownames(rsq)<-c("R-squared", "Adjusted R-squared")
addtable2plot(0,0.5,rsq,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 

####Plot4######
par(mai = c(0,0,0,0))
plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homosc?dasticit?", cex.main = 1.5, adj = 0  
bp <- bptest(Yt ~ X)
restest<-lm(Yt~X)
w<- white.test(restest)
i<-signif(bp$statistic,4)
j<-signif(w$statistic,4)
k<-round(bp$p.value,5)
l<-round(w$p.value,5)
tests <- data.frame(c(i,j), c(k,l))
colnames(tests)<-c("Statistique","p-valeur" )
rownames(tests)<-c("Breusch-Pagan","White :")
addtable2plot(0.15,0.35,tests,display.rownames=TRUE, hlines=FALSE, title=HTML("Tests d'homoscédasticité"), cex = 1.6) 
}
}


#----------------------------------Option n°2----------------------------------
  
  if(input$sol=="OLS"){
    
    data <- read.csv('rent.csv', header=T, sep=';')
    data<- as.data.frame(data)
    
    X <- data$income/1000
    Y <- data$rent/1000
    
    par(mfcol = c(3,2))
    m<-matrix(c(1,1,1,2,2,2),3,2,byrow=FALSE)
    layout(m,width=c(2,3))  
    
    ####Plot1######
    plot(X, Y, main ="Plot X-Y", bty="n", pch = 19, xlim = c(0,100), ylim = c(0, 30), xlab = "Income (dollars, x1000)", ylab = "Rent (dollars, x1000)", cex.axis = 1.3, cex.lab = 1.4, cex.main = 1.5)
    res <- lm (Y~X)
    if(input$droite2=="TRUE"){abline (res, col = "blue")}
    
    #####Plot2#####
    par(mai = c(1,0,1,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = " ")  
    title(main=paste("Method : Least Squares","\n",sep=""),cex.main=1.6)
    title(main=paste("\n","White Robust Standard Errors",sep=""),cex.main=1.5) 
    X <- data$income
    Y <- data$rent
    f1 <- formula(Y ~ X)
    RolsCoef <- coeftest(lm(f1), vcov = (vcovHC(lm(f1), "HC0")))
    a <-signif(RolsCoef[1],4)
    b <-signif(RolsCoef[2],4)
    c <-signif(RolsCoef[3],3)
    d <-signif(RolsCoef[4],4)
    e <-signif(RolsCoef[5],4)
    f <-signif(RolsCoef[6],4)
    g <-round(RolsCoef[7],5)
    h <-round(RolsCoef[8],5)
    
    estim <- data.frame(c(a,b), c(c,d), c(e,f), c(g,h))
    colnames(estim)<-c("Coefficient", "Std.Error", "t-Statistic", "Prob.")
    rownames(estim)<-c("Intercept","Income")
    addtable2plot(0,0.8,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
}
  


#----------------------------------Option n°3.1----------------------------------

if(input$sol=="GLS" && input$wi=="GLS1"){

  data <- read.csv('rent.csv', header=T, sep=';')
  data<- as.data.frame(data)
  X <- data$income
  Y<-data$rent

  par(mfcol = c(3,2))
  m<-matrix(c(1,1,1,2,3,4),3,2,byrow=FALSE)
  layout(m,width=c(2,3))  
  
#####Plot 1#####  
plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = " ")  
  
#####Plot 2#####  
par(mai = c(0,0,1,0))
plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = "Method : Least Squares", cex.main = 1.5)  
  
  f1 <- gls(Y ~ X, weights = varFixed(~(X^2)))
  aa1 <- (attributes(summary(f1)$modelStruct$varStruct)$weights)^2 
  f2 <- lm(Y~X, weights = aa1) 
  sumf2 <- summary(f2) 

  coef <- sumf2$coefficients
  a <-signif(coef[1],4)
  b <-signif(coef[2],4)
  c <-signif(coef[3],4)
  d <-signif(coef[4],4)
  e <-signif(coef[5],4)
  f <-signif(coef[6],4)
  g <-round(coef[7],5)
  h <-round(coef[8],5)
  
  estim <- data.frame(c(a,b), c(c,d), c(e,f), c(g,h))
  colnames(estim)<-c("Coefficient", "Std.Error", "t-Statistic", "Prob.")
  rownames(estim)<-c("Intercept","Income")
  addtable2plot(0,0,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
  
#####Plot 3#####  
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  r <-signif(sumf2$r.squared,4)
  ra<-signif(sumf2$adj.r.squared,4)
  fstat <- signif(unlist(sumf2[11])[1], 4)
  pfstat <- round(1-pf(sumf2$fstatistic[1],sumf2$fstatistic[2],sumf2$fstatistic[3]),5)
  rsq <- data.frame(c(r, ra), c("F-statistic", "Prob(F-stat)"), c(fstat, pfstat))
  colnames(rsq)<-c("")
  rownames(rsq)<-c("R-squared", "Adjusted R-squared")
  addtable2plot(0,0.5,rsq,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
  
#####Plot 4#####  
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homosc?dasticit?", cex.main = 1.5, adj = 0  
  #bp <- bptest(Yt ~ X)
  #i<-signif(bp$statistic,4)
  #k<-round(bp$p.value,5)
  
  w<- white.test(f2)
  j<-signif(w$statistic,4)
  l<-round(w$p.value,5)
  tests <- data.frame(c(j),c(l))
  colnames(tests)<-c("Statistique","p-valeur" )
  #rownames(tests)<-c("Breusch-Pagan","White :")
  addtable2plot(0.15,0.35,tests,display.rownames=TRUE, hlines=FALSE, title=HTML("Tests d'homoscédasticité"), cex = 1.6)   

}


#----------------------------------Option n°3.2----------------------------------

if(input$sol=="GLS" && input$wi=="GLS2"){
  
  data <- read.csv('rent.csv', header=T, sep=';')
  data<- as.data.frame(data)
  X <- data$income
  Y<-data$rent
  
  par(mfcol = c(3,2))
  m<-matrix(c(1,1,1,2,3,4),3,2,byrow=FALSE)
  layout(m,width=c(2,3))  
  
  #####Plot 1#####  
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = " ")  
  
  #####Plot 2#####  
  par(mai = c(0,0,1,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = "Method : Least Squares", cex.main = 1.5)  
  
  f3 <- gls(Y ~ X, weights = varFixed(~X))
  aa2 <- (attributes(summary(f3)$modelStruct$varStruct)$weights)^2 
  f4 <- lm(Y~X, weights = aa2) 
  sumf4 <- summary(f4) 
  
  coef <- sumf4$coefficients
  a <-signif(coef[1],4)
  b <-signif(coef[2],4)
  c <-signif(coef[3],4)
  d <-signif(coef[4],4)
  e <-signif(coef[5],4)
  f <-signif(coef[6],4)
  g <-round(coef[7],5)
  h <-round(coef[8],5)
  
  estim <- data.frame(c(a,b), c(c,d), c(e,f), c(g,h))
  colnames(estim)<-c("Coefficient", "Std.Error", "t-Statistic", "Prob.")
  rownames(estim)<-c("Intercept","Income")
  addtable2plot(0,0,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
  
  #####Plot 3#####  
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  r <-signif(sumf4$r.squared,4)
  ra<-signif(sumf4$adj.r.squared,4)
  fstat <- signif(unlist(sumf4[11])[1], 4)
  pfstat <- round(1-pf(sumf4$fstatistic[1],sumf4$fstatistic[2],sumf4$fstatistic[3]),5)
  rsq <- data.frame(c(r, ra), c("F-statistic", "Prob(F-stat)"), c(fstat, pfstat))
  colnames(rsq)<-c("")
  rownames(rsq)<-c("R-squared", "Adjusted R-squared")
  addtable2plot(0,0.5,rsq,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
  
  #####Plot 4#####  
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homosc?dasticit?", cex.main = 1.5, adj = 0  
  #bp <- bptest(Yt ~ X)
  #i<-signif(bp$statistic,4)
  #k<-round(bp$p.value,5)
  
  w<- white.test(f4)
  j<-signif(w$statistic,4)
  l<-round(w$p.value,5)
  tests <- data.frame(c(j),c(l))
  colnames(tests)<-c("Statistique","p-valeur" )
  #rownames(tests)<-c("Breusch-Pagan","White :")
  addtable2plot(0.15,0.35,tests,display.rownames=TRUE, hlines=FALSE, title=HTML("Tests d'homoscédasticité"), cex = 1.6)   
  
}

#if(input$wi=="GLS3"){}




#----------------------------------Option n°3.2----------------------------------

if(input$sol=="FGLS"){
  
  data <- read.csv('rent.csv', header=T, sep=';')
  data<- as.data.frame(data)
  X <- data$income
  Y<-data$rent
  
  par(mfcol = c(3,2))
  m<-matrix(c(1,1,1,2,3,4),3,2,byrow=FALSE)
  layout(m,width=c(2,3))  
  
  #####Plot 1#####  
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = " ")  
  
  f0 <- lm(Y~X)
  reginit<-summary(f0) 
  residuals <- unlist(reginit[3])
  fgls.1 <- lm(log(residuals^2)~log(X))
  hest <- unlist(fgls.1$coefficients)[2]
  sqhest <- sqrt(hest)
  
  a <- HTML("Hypothèse sur la structure de l'hétéroscédasticité : ")
  b <- HTML("Var(&epsilon; = &sigma;<sup>2</sup> X<sup>h</sup><sub>i</sub>")
  c <- "Estimation de h"
  d <- HTML("ln(e<sup>2</sup><sub>i</sub>) = ln(&sigma;<sup>2</sup>) +h ln(X<sub>i</sub>)")
  e <- paste("h =", hest, sep = "")
  f <- paste (HTML ("poids = &radic;h :"), sqhest, sep = "") 
  
  
  
  poids <- data.frame(c(a,b), c(c,d), c(e,f))
  #addtable2plot(0,0,poids,bty="n",display.rownames=FALSE, hlines=FALSE,title="", cex = 1.6) 
  
  
  
  #####Plot 2#####  
  par(mai = c(0,0,1,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l', main = "Method : Least Squares", cex.main = 1.5)  
  
  Xt <- X^hest
  
  f7 <- gls(Y ~ X, weights = varFixed(~(Xt)))
  aa4 <- (attributes(summary(f7)$modelStruct$varStruct)$weights)^2 
  f8 <- lm(Y~X, weights = aa4) 
  
  sumf8 <- summary(f8) 
  
  coef <- sumf8$coefficients
  a <-signif(coef[1],4)
  b <-signif(coef[2],4)
  c <-signif(coef[3],4)
  d <-signif(coef[4],4)
  e <-signif(coef[5],4)
  f <-signif(coef[6],4)
  g <-round(coef[7],5)
  h <-round(coef[8],5)
  
  estim <- data.frame(c(a,b), c(c,d), c(e,f), c(g,h))
  colnames(estim)<-c("Coefficient", "Std.Error", "t-Statistic", "Prob.")
  rownames(estim)<-c("Intercept","Income")
  addtable2plot(0,0,estim,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
  
  #####Plot 3#####  
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  r <-signif(sumf8$r.squared,4)
  ra<-signif(sumf8$adj.r.squared,4)
  fstat <- signif(unlist(sumf8[11])[1], 4)
  pfstat <- round(1-pf(sumf8$fstatistic[1],sumf8$fstatistic[2],sumf8$fstatistic[3]),5)
  rsq <- data.frame(c(r, ra), c("F-statistic", "Prob(F-stat)"), c(fstat, pfstat))
  colnames(rsq)<-c("")
  rownames(rsq)<-c("R-squared", "Adjusted R-squared")
  addtable2plot(0,0.5,rsq,bty="n",display.rownames=TRUE,hlines=FALSE,title="", cex = 1.6) 
  
  #####Plot 4#####  
  par(mai = c(0,0,0,0))
  plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l') #, main = "Tests d'homosc?dasticit?", cex.main = 1.5, adj = 0  
  #bp <- bptest(Yt ~ X)
  #i<-signif(bp$statistic,4)
  #k<-round(bp$p.value,5)
  
  w<- white.test(f8)
  j<-signif(w$statistic,4)
  l<-round(w$p.value,5)
  tests <- data.frame(c(j),c(l))
  colnames(tests)<-c("Statistique","p-valeur" )
  #rownames(tests)<-c("Breusch-Pagan","White :")
  addtable2plot(0.15,0.35,tests,display.rownames=TRUE, hlines=FALSE, title=HTML("Tests d'homoscédasticité"), cex = 1.6)   
  
}
 
    
  }, height = 300, width = 700)
  
  
})   
 