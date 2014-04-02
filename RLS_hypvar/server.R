#Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots
library(shiny)
library(bstats)
library(lmtest)


shinyServer(function(input, output) {

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
  
 #   getInputValues<-reactive({
#      return(input)#collect all inputs
 #   })
    
  #  getComputedValues<-reactive({
  #    Y<-getY()
            
  #    v<-getInputValues() # get all values of input list
  #    cv<-list()#created empty computed values list
      
  
   # rv$last.Y.value<-v$takeY
    #  return(cv)
    
     #})
   
  
  

output$doublePlot1 <- renderPlot({

  
  Y<-getY()
  Y<-unlist(Y) 
 # v<-getInputValues()
 #  cv<-getComputedValues()
  
  par(mfrow = c(1,2))

if(is.null(Y)) {
  Y <- c()
  X <-c()
  
  plot(X, Y, main = "Plot X-Y", xlim = c(0,20), ylim = c(-5,5)) #nuage de points
  #res <- lm (Y ~ X)
  #abline (res, col = "blue")
  
  #resid <- unlist(res[2])
  #plot (X, resid, main = "Plot des résidus en fonction de X") #résidus en fonction de X)
  #abline (h = 0)
  

} else {  Y<-getY()
          Y<-unlist(Y)
  
  plot(X, Y, main = "Plot X-Y") #nuage de points
  res <- lm (Y ~ X)
  abline (res, col = "blue")
  
  resid <- unlist(res[2])
  plot (X, resid, main = "Plot des résidus en fonction de X") #résidus en fonction de X)
  abline (h = 0)
      
}
  } ,height = 300)
 

  ###Afficher la statistique et la p-valeur du test de Breusch-Pagan  et du test de White
  output$Test1<- 
    
    renderTable({   if(input$Test=="TRUE"){  
    
    Y<-getY()
    Y<-unlist(Y) 
    
    
    if(is.null(Y)) {}
    else {
    
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

    }}
    }, digits=5  , height = getPlotHeight, width=full.plot.width) 
  

  


})