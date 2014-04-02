#Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots
library(shiny)
library(bstats)
library(lmtest)


shinyServer(function(input, output) {

  #--------------------------------------------------------------------------------
  #                             Tabset 1  : données générées
  #--------------------------------------------------------------------------------

  X <- runif (n = 100, min = 0, max = 20)
  
  rv <- reactiveValues()  # Create a reactiveValues object, to let us use settable reactive values
  
  rv$last.Y.value<-0
  rv$Y<-list()
  
  rv$lastAction <- 'none'# To start out, lastAction == NULL, meaning nothing clicked yet
  
  # An observe block for each button, to record that the action happened
  observe({
    if (input$takeY != 0) {
      rv$lastAction <- 'takeY'
    }
  })
  observe({
    if (input$reset != 0) {
      rv$lastAction <- 'reset'
      rv$Y<-list()
    }
  })
  
  
  
  getY <- reactive({
    
    if(rv$lastAction == "takeY")
    
    #epsilon <- rnorm(n = 100, mean = 0, sd = sqrt(5 + input$alpha1*X^2))
    
      
      return (
      rv$Y <-input$intercept + input$beta1*X + rnorm(n = 100, mean = 0, sd = sqrt(input$alpha0*X^input$alpha1))
      )
  })
  

  
  

output$doublePlot1 <- renderPlot({

  Y<-getY()
  Y<-unlist(Y) 

  par(mfrow = c(1,2))

if(is.null(Y)) {
  Y <- c()
  X <-c()
  
  plot(X, Y, main = "Plot X-Y", xlim = c(0,20), ylim = c(-5,5), xlab = "X", ylab = "Y", bty="n") #nuage de points
  #res <- lm (Y ~ X)
  #abline (res, col = "blue")
  
  #resid <- unlist(res[2])
  #plot (X, resid, main = "Plot des rÃ©sidus en fonction de X") #rÃ©sidus en fonction de X)
  #abline (h = 0)
  

} else {  Y<-getY()
          Y<-unlist(Y)
  
  plot(X, Y, main = "Plot X-Y", xlab = "X", ylab = "Y", bty="n") #nuage de points
  res <- lm (Y ~ X)
  abline (res, col = "blue")
  
  resid <- unlist(res[2])
  plot (X, resid, main = "Plot des rÃ©sidus en fonction de X", bty="n") #rÃ©sidus en fonction de X)
  abline (h = 0)
      
}
  } ,height = 300)
 

  ###Afficher les coefficients estimés
  output$Coef1<- renderTable({
    if(input$Coef=="TRUE")
    { 
      
      Y<-getY()
      Y<-unlist(Y) 
      
      if(is.null(Y)) {}
      else {
        res <- lm (Y ~ X)
        coef <-res[1]
        coef <-unlist(coef)
        intercept <- coef[1]
        beta1 <- coef[2]
        
        table1 <- matrix(nrow = 1, ncol = 2)
        dimnames(table1) = list(c("Estimations"),c("Coefficient.Intercept", "Coefficient.Pente"))
        table1[1,1] <- intercept
        table1[1,2] <- beta1
        table1
      }
    }
  })
  
  
  
        
  ###Afficher la statistique et la p-valeur du test de Breusch-Pagan  et du test de White
  output$Test1<- renderTable({
    if(input$Test=="TRUE")
      {  
      
        Y<-getY()
        Y<-unlist(Y) 
      
        if(is.null(Y)) {}
        else {
          bp <- bptest(Y ~ X)
          res <- lm (Y ~ X)
          white <- white.test(res)
          
          table2 <- matrix(nrow = 2, ncol = 2)
          dimnames(table2) = list(c("Statistique du test", "p-valeur"), c("Breusch-Pagan", "White"))
          table2[1,1] <- bp$statistic
          table2[1,2] <- white$statistic
          table2[2,1] <- bp$p.value
          table2[2,2] <- white$p.value
          table2
              }
      }
    }, digits=5) 
  

  
  
            
            
#--------------------------------------------------------------------------------
#                           Tabset 2  : données téléchargées
#--------------------------------------------------------------------------------
            
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=T, sep=';')
    data<- as.data.frame(data)
    data[1:10,1:2]
  })
  
  
  output$doublePlot2<- renderPlot({
  
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header=T, sep=';')
    data<- as.data.frame(data)
  
    Income <- data$income
    Rent <- data$rent
    
    
    par(mfrow = c(1,2))
    
    
    plot(Income, Rent, main ="Plot X-Y")
    res2 <- lm (Rent ~ Income)
    abline (res2, col = "blue")
    
    resid <- unlist(res2[2])
    plot (Income, resid, main = "Plot des rÃ©sidus en fonction de X", bty="n") #rÃ©sidus en fonction de X)
    abline (h = 0)
    
    
  },height = 300)  
    

  ###Afficher les coefficients estimés
  output$Coef2<- renderTable({
    if(input$Coef=="TRUE")
    { 
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      data <- read.csv(inFile$datapath, header=T, sep=';')
      data<- as.data.frame(data)
      
      Income <- data$income
      Rent <- data$rent
      
      res <- lm (Rent ~ Income)
        coef <-res[1]
        coef <-unlist(coef)
        intercept <- coef[1]
        beta1 <- coef[2]
        
        table1 <- matrix(nrow = 1, ncol = 2)
        dimnames(table1) = list(c("Estimations"),c("Coefficient.Intercept", "Coefficient.Pente"))
        table1[1,1] <- intercept
        table1[1,2] <- beta1
        table1
      }
  })



output$Test2<- renderTable({
  if(input$Test=="TRUE")
  {  
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header=T, sep=';')
    data<- as.data.frame(data)
    
    Income <- data$income
    Rent <- data$rent
    
      bp <- bptest(Rent  ~ Income)
      res <- lm (Rent  ~ Income)
      white <- white.test(res)
      
      table2 <- matrix(nrow = 2, ncol = 2)
      dimnames(table2) = list(c("Statistique du test", "p-valeur"), c("Breusch-Pagan", "White"))
      table2[1,1] <- bp$statistic
      table2[1,2] <- white$statistic
      table2[2,1] <- bp$p.value
      table2[2,2] <- white$p.value
      table2
    }
}, digits=5 ) 
  
  
})