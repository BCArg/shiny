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

# Pour utiliser différents jeux de données dans une lib R
  
#  datasetInput <- reactive({
#    switch(input$dataset, "Orange" = Orange, "airquality" = airquality)
#    Orange <- Orange[complete.cases(Orange), 2:3]
#    airquality <-  airquality[complete.cases(airquality), 1:4]
#  })
  
#  output$view <- renderTable({head(datasetInput(), n = 100)})
  
  #    inFile <- input$file1
  #    if (is.null(inFile))
  #      return(NULL)
 
  
  
  output$contents <- renderTable({
    if(input$obs=="TRUE"){  
   data <- read.csv('rent.csv', header=T, sep=';')
   data<- as.data.frame(data)
   data[1:10,1:2]}
   })
  
    
  output$doublePlot2<- renderPlot({
   data <- read.csv('rent.csv', header=T, sep=';')
   data<- as.data.frame(data)
    if(input$graphes=="TRUE"){ 
     X <- data$income/1000
     Y <- data$rent/1000
     par(mfrow = c(1,3))
     plot(X, Y, main ="Plot X-Y", bty="n", xlab = "Income (dollars, x1000)", ylab = "Rent(dollars, x1000)")
     res2 <- lm (Y~X)
     abline (res2, col = "blue")
     resid <- unlist(res2[2])
     plot (X, resid, main = HTML("Plot des résidus en fonction de X"), bty="n", xlab = "Income(dollars, x1000)", ylab = HTML("Résidus")) #rÃ©sidus en fonction de X)
     abline (h = 0)}
    
    if(input$logY=="TRUE"){ 
     X <- data$income/1000
     Y <- log(data$rent/1000)
     par(mfrow = c(1,3))
     plot(X, Y, main ="Plot X-log(Y)", bty="n", xlab = "Income (dollars, x1000)", ylab = "log(Rent(dollars, x1000))")
     res2 <- lm (Y~X)
     abline (res2, col = "blue")
     resid <- unlist(res2[2])
     plot (X, resid, main = HTML("Plot des résidus en fonction de X"), xlab = "Income", ylab = HTML("Résidus"), bty="n") #rÃ©sidus en fonction de X)
     abline (h = 0)}
   
   if(input$GLS1=="TRUE"){
     X <- 1/(data$income)
     Y <- data$rent/data$income
     par(mfrow = c(1,3))
     plot(X, Y, main ="Plot X-Y pondérés", bty="n", xlab = "Inverse of Income (x1000)", ylab = "Rent/Income Ratio")
     res2 <- lm (Y~X)
     abline (res2, col = "blue")
     resid <- unlist(res2[2])
     plot (X, resid, main = HTML("Plot des résidus en fonction de X"), xlab = "Income", ylab = HTML("Résidus"), bty="n") #rÃ©sidus en fonction de X)
     abline (h = 0)}
   
   if(input$GLS2=="TRUE"){
     X <- 1/sqrt(data$income)
     Y <- data$rent/sqrt(data$income)
     par(mfrow = c(1,3))
     plot(X, Y, main ="Plot X-Y pondérés", bty="n", xlab = "Inverse of Income (x1000)", ylab = "Rent/Income Ratio")
     res2 <- lm (Y~X)
     abline (res2, col = "blue")
     resid <- unlist(res2[2])
     plot (X, resid, main = HTML("Plot des résidus en fonction de X"), xlab = "Income", ylab = HTML("Résidus"), bty="n") #rÃ©sidus en fonction de X)
     abline (h = 0)}
   
   
  },height = 300)  
    

  ###Afficher les coefficients estimés
  output$Coef2<- renderTable({
   data <- read.csv('rent.csv', header=T, sep=';')
   data<- as.data.frame(data)
    if(input$Coef2=="TRUE"){
      X <- data$income
      Y <- data$rent
      res <- lm (Y~X)
      sumres <- summary(res)
      coef <- sumres$coefficients
      
      table1 <- matrix(nrow = 2, ncol = 4)
      dimnames(table1) = list(c("Intercept", "Income"),c("Coefficient", "Std.Error", "t-Statistic", "Prob."))
      table1[1,1] <- coef[1]
      table1[2,1] <- coef[2]
      table1[1,2] <- coef[3]
      table1[2,2] <- coef[4]
      table1[1,3] <- coef[5]
      table1[2,3] <- coef[6]
      table1[1,4] <- coef[7]
      table1[2,4] <- coef[8]
      table1}
  }, digits=5 ) 
   
  
  
  #table1[3,1:4] <- sumres[8] ######NB : trouver un moyen d'afficher le R² et adjR²
  #table1[4,1:4] <- sumres[9]
  
  
   output$CoeflogY<- renderTable({ 
     data <- read.csv('rent.csv', header=T, sep=';')
     data<- as.data.frame(data)
    if(input$logY=="TRUE"){ 
      X <- data$income
      Y <- log(data$rent)
      res <- lm (Y~X)
      sumres <- summary(res)
      coef <- sumres$coefficients
      
      table1 <- matrix(nrow = 2, ncol = 4)
      dimnames(table1) = list(c("Intercept", "Income"),c("Coefficient", "Std.Error", "t-Statistic", "Prob."))
      table1[1,1] <- coef[1]
      table1[2,1] <- coef[2]
      table1[1,2] <- coef[3]
      table1[2,2] <- coef[4]
      table1[1,3] <- coef[5]
      table1[2,3] <- coef[6]
      table1[1,4] <- coef[7]
      table1[2,4] <- coef[8]
     table1}
   }, digits=5 ) 

  
  
  output$CoefGLS1<- renderTable({ 
    data <- read.csv('rent.csv', header=T, sep=';')
    data<- as.data.frame(data)
    if(input$GLS1=="TRUE"){ 
      X <- 1/(data$income)
      Y <- data$rent/data$income
      res <- lm (Y~X)
      sumres <- summary(res)
      coef <- sumres$coefficients
      
      table1 <- matrix(nrow = 2, ncol = 4)
      dimnames(table1) = list(c("Intercept", "Income"),c("Coefficient", "Std.Error", "t-Statistic", "Prob."))
      table1[1,1] <- coef[1]
      table1[2,1] <- coef[2]
      table1[1,2] <- coef[3]
      table1[2,2] <- coef[4]
      table1[1,3] <- coef[5]
      table1[2,3] <- coef[6]
      table1[1,4] <- coef[7]
      table1[2,4] <- coef[8]
      table1}
  }, digits=5 )
  
  output$CoefGLS2<- renderTable({ 
    data <- read.csv('rent.csv', header=T, sep=';')
    data<- as.data.frame(data)
    if(input$GLS2=="TRUE"){ 
      X <- data$income
      Y <- data$rent
      W <-sqrt(X)
      Y2 <- Y/W
      X2 <-X/W
      
      res <- lm (Y2~X2)
      sumres <- summary(res)
      coef <- sumres$coefficients
      
      table1 <- matrix(nrow = 2, ncol = 4)
      dimnames(table1) = list(c("Intercept", "Income"),c("Coefficient", "Std.Error", "t-Statistic", "Prob."))
      table1[1,1] <- coef[1]
      table1[2,1] <- coef[2]
      table1[1,2] <- coef[3]
      table1[2,2] <- coef[4]
      table1[1,3] <- coef[5]
      table1[2,3] <- coef[6]
      table1[1,4] <- coef[7]
      table1[2,4] <- coef[8]
      table1}
  }, digits=5 )
output$Test2<- renderTable({
 data <- read.csv('rent.csv', header=T, sep=';')
 data<- as.data.frame(data)
  if(input$Test2=="TRUE"){  
    X <- data$income/1000
    Y <- data$rent/1000
    bp <- bptest(Y~X)
    res <- lm (Y~X)
    white <- white.test(res)
    table2 <- matrix(nrow = 2, ncol = 2)
      dimnames(table2) = list(c("Statistique du test", "p-valeur"), c("Breusch-Pagan", "White"))
      table2[1,1] <- bp$statistic
      table2[1,2] <- white$statistic
      table2[2,1] <- bp$p.value
      table2[2,2] <- white$p.value
      table2} 
 }, digits=5 ) 


 
output$TestlogY<- renderTable({
  data <- read.csv('rent.csv', header=T, sep=';')
  data<- as.data.frame(data)
  if(input$logY=="TRUE"){
    X <- data$income/1000
    Y <- log(data$rent/1000)
    bp <- bptest(Y~X)
    res <- lm (Y~X)
    white <- white.test(res)
    table2 <- matrix(nrow = 2, ncol = 2)
    dimnames(table2) = list(c("Statistique du test", "p-valeur"), c("Breusch-Pagan", "White"))
    table2[1,1] <- bp$statistic
    table2[1,2] <- white$statistic
    table2[2,1] <- bp$p.value
    table2[2,2] <- white$p.value
    table2}
}, digits=5 ) 
  
  
})

  