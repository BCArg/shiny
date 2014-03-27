#Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots
library(shiny)
#library(bstats)
#library(lmtest)


shinyServer(function(input, output) {

  

output$doublePlot1 <- renderPlot({
  X <- runif (n = input$n, min = 0, max = 20)
  epsilon <- rnorm(n = input$n, mean = 0, sd = sqrt(5 + input$alpha1*X^2))
  
  Y <- 2 + 2*X + epsilon  
  
  
  par(mfrow = c(1,2))

  plot(X, Y, main = "Plot X-Y") #nuage de points
  res <- lm (Y ~ X)
  abline (res, col = "blue")
  
  resid <- unlist(res[2])
  plot (X, resid, main = "Plot des résidus en fonction de X") #résidus en fonction de X)
  abline (h = 0)
      
},height = 300)
 

###Afficher la statistique et la p-valeur du test de Breusch-Pagan  et du test de White
output$Test1<- renderTable({    
  
  X <- runif (n = input$n, min = 0, max = 10)
  epsilon <- rnorm(n = input$n, mean = 0, sd = sqrt(5 + input$alpha1*X^2))
  Y <- 2 + 2*X + epsilon 
  
  bp <- bptest(Y ~ X)
  
  res <- lm (Y ~ X)
  white <- white.test(res)
  
  
  
  table <- matrix(nrow = 2, ncol = 2)
  dimnames(table) = list(c("Statistique du test", "p-valeur"), c("Breusch-Pagan", "White"))
  table[1,1] <- bp$statistic
  table[1,2] <- white$statistic
  table[2,1] <- bp$p.value
  table[2,2] <- white$p.value
  table
}, digits=5  , height = getPlotHeight, width=full.plot.width) 



output$doublePlot2 <- renderPlot({

X <- runif (n = input$n, min = 0, max = 20)
epsilon <- rnorm(n = input$n, mean = 0, sd = sqrt(5*X^input$alpha1))
Y <- 2 + 2*X + epsilon

par(mfrow = c(1,2))
plot(X, Y, main = "Plot X-Y") #nuage de points
res <- lm (Y ~ X)
abline (res, col = "blue")

resid <- unlist(res[2])
plot (X, resid, main = "Plot des résidus en fonction de X") #résidus en fonction de X)
abline (h = 0)




}, height = 300)



###Afficher la statistique et la p-valeur du test de Breusch-Pagan  et du test de White
output$Test2<- renderTable({    
  
  X <- runif (n = input$n, min = 0, max = 20)
  epsilon <- rnorm(n = input$n, mean = 0, sd = sqrt(5*X^input$alpha1))
  Y <- 2 + 2*X + epsilon
  
  bp <- bptest(Y ~ X)
  
  res <- lm (Y ~ X)
  white <- white.test(res)
  
  
  
  table <- matrix(nrow = 2, ncol = 2)
  dimnames(table) = list(c("Statistique du test", "p-valeur"), c("Breusch-Pagan", "White"))
  table[1,1] <- bp$statistic
  table[1,2] <- white$statistic
  table[2,1] <- bp$p.value
  table[2,2] <- white$p.value
  table

    
  }, digits=5  , height = getPlotHeight, width=full.plot.width) 



})