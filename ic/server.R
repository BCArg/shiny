  #initiate global counters
  SP<-list()
  SP$n.ic<-0
  SP$n.ic.inc.mu<-list()
  SP$n.ic.noninc.mu<-list()
  SP$l.ic.inc.mu<-list()
  SP$l.ic.noninc.mu<-list()

shinyServer(function(input, output) {
  # Create a reactiveValues object, to let us use settable reactive values
  rv <- reactiveValues()
  # To start out, lastAction == NULL, meaning nothing clicked yet
  rv$lastAction <- 'none'
  # An observe block for each button, to record that the action happened
  observe({
    if (input$takeech != 0) {
      rv$lastAction <- 'takeech'
    }
  })
  observe({
    if (input$reset != 0) {
      rv$lastAction <- 'reset'
    }
  })
  
  getech<-reactive({#créee n valeurs aléatoires N(0;1) quand input$takeech est implémenté (quand le bouton takeech est pressé)
    #don't do anything until after the first button is pushed
    if(input$takeech == 0)
      return(NULL)
      return(isolate({
	#Now do the expensive stuff
	rnorm(input$n)#créee n valeurs aléatoires N(0;1)
      }))
  })

  getInputValues<-reactive({
    v<-list()
    v<-input #collect all inputs
    return(v)
  })
  
  getComputedValues<-reactive({
    cv<-list()#created empty computed values list
    v<-getInputValues() # get all values of input list
    
    
    cv$sx.dech<-v$sx/sqrt(v$n)#ecart-type de la distribution d'échantillonnage
    cv$vx<-v$sx^2
    cv$vx.dech<-cv$sx.dech^2
    
    #Calcul de la densité maximale entre la distribution d'origine et celle d'échantillonnage
    cv$dmx<-dnorm(v$mx,mean=v$mx,sd=v$sx)
    cv$dmx.dech<-dnorm(v$mx,mean=v$mx,sd=cv$sx.dech)
    #cv$maxdmx<-max(cv$dmx,cv$dmx.dech)
    cv$maxdmx<-cv$dmx
    cv$yaxislim<-cv$maxdmx+(cv$maxdmx*0.2)
    
    # Calcul des valeurs des X pour tracer les polygones des distributions
    z<-seq(-5,5,length=100)
    cv$xr<-(z*v$sx)+v$mx #x pour tracer la distribution "réalité"
    cv$xr.dech<-(z*cv$sx.dech)+v$mx #x pour tracer la distribution d'échantillonnage

    cv$yr<-dnorm(cv$xr,mean=v$mx,sd=v$sx)
    cv$yr.dech<-dnorm(cv$xr.dech,mean=v$mx,sd=cv$sx.dech)
    
    # Tout ce qui est relatif à l'échantillon aléatoire prélevé dans la réalité
    cv$ech.z<-getech()#créee n valeurs aléatoires N(0;1) quand input$takeech est implémenté (quand le bouton takeech est pressé)
    if (rv$lastAction=='reset') {
      cv$ech.z<-NULL
      SP$n.ic<<-0
      SP$n.ic.inc.mu<<-0
      SP$n.ic.noninc.mu<<-0
      SP$l.ic.inc.mu<<-list()
      SP$l.ic.noninc.mu<<-list()
    }
    cv$ech.exist<-length(cv$ech.z)#ne pas prendre n mais calculer le nombre de valeurs dans l'échantillon juste pour s'assurer qu'un échantillon a été créé = le bouton action a été poussé
    cv$ech.x<-(cv$ech.z*v$sx)+v$mx
    if(cv$ech.exist){
      cv$ech.m<-mean(cv$ech.x)
      cv$ech.m.z<-(cv$ech.m-v$mx)/v$sx
      cv$ech.m.z.dech<-(cv$ech.m-v$mx)/cv$sx.dech
      cv$ech.m.pvalue<-signif(1-pnorm(cv$ech.m.z),2)
      cv$ech.m.pvalue.dech<-signif(1-pnorm(cv$ech.m.z.dech),2)
      
      if(cv$ech.m.pvalue<0.001){
	cv$ech.m.pvalue.text<-" <0.001"
      } else {
	cv$ech.m.pvalue.text<-cv$ech.m.pvalue
      }
      if(cv$ech.m.pvalue.dech<0.001){
	cv$ech.m.pvalue.dech.text<-" <0.001"
      } else {
	cv$ech.m.pvalue.dech.text<-cv$ech.m.pvalue.dech
      }
      cv$ech.y<-seq(0.3,0.3,length=cv$ech.exist)#liste des coordonnées y des points de l'échantillon
          
      # Tout ce qui est relatif à la puissance, confiance, alpha, et beta
      cv$alpha<-round(1-v$confidence,3)
      cv$alpha.z<-round(qnorm(v$confidence),3)
      cv$alpha.x<-(cv$alpha.z*v$sx)+v$mx
      cv$alpha.y<-dnorm(cv$alpha.x, mean=v$mx, sd=v$sx)
      
      cv$alpha.z.polygon<-seq(cv$alpha.z,5,length=100)
      cv$alpha.x.polygon<-(cv$alpha.z.polygon*v$sx)+v$mx
      cv$alpha.y.polygon<-dnorm(cv$alpha.x.polygon,mean=v$mx,sd=v$sx)
      
      cv$confidence.z.polygon<-seq(-5,cv$alpha.z,length=100)
      cv$confidence.x.polygon<-(cv$confidence.z.polygon*v$sx)+v$mx
      cv$confidence.y.polygon<-dnorm(cv$confidence.x.polygon,mean=v$mx,sd=v$sx)
      
      #Tout ce qui est relatif à l'IC àa la moyennes    
      cv$ic.z<-qnorm(1-cv$alpha/2)
      cv$ic.t<-qt(1-cv$alpha/2,v$n-1)
      cv$ic.z.limit.inf<-mean(cv$ech.x)-cv$ic.z*cv$sx.dech
      cv$ic.z.limit.sup<-mean(cv$ech.x)+cv$ic.z*cv$sx.dech
      cv$ic.t.limit.inf<-mean(cv$ech.x)-cv$ic.t*cv$sx.dech
      cv$ic.t.limit.sup<-mean(cv$ech.x)+cv$ic.t*cv$sx.dech
    }
    
    return(cv)
    })
    
  output$plotReality <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    #par(mfrow=c(3,1))
    ##################
    ## Plot Reality ##
    ##################
    par(mai=c(0.5,1,0.2,1))
    plot(cv$xr,cv$yr,type="l",lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,xlim=c(0,100),ylim=c(0,cv$yaxislim),ylab="density",xlab="",xaxp=c(0,100,20)) #trace une courbe a partir de tous les couples x;y, et la colore en rouge. bty : A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. xaxt="n" = pas dessiner axe des x
    #polygon(c(-5,cv$xr.dech),c(0,cv$yr.dech),lty=3)
    axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.9,labels="Realite",cex=2, pos=4)
    text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(N *"~"* ( mu *","* sigma^2 )),cex=1.5,pos=4)#paste("N~(",mx1,",",round(x.var,2),")",sep="")
    text(1,signif(cv$maxdmx,1)*0.5,labels=bquote(N *"~"* (.(v$mx)*","*.(cv$vx))),cex=1.5,pos=4)
    lines(x<-c(v$mx,v$mx),y <- c(0,cv$maxdmx),lty=3,lwd=1)
    lines(x<-c(cv$ic.z.limit.inf,cv$ic.z.limit.inf),y <- c(0,cv$maxdmx),lty=3,lwd=1)
    lines(x<-c(cv$ic.z.limit.sup,cv$ic.z.limit.sup),y <- c(0,cv$maxdmx),lty=3,lwd=1)
    if(cv$ech.exist){
      #points(cv$ech.x,cv$ech.y)
      rug(cv$ech.x,lwd=2)
      lines(x<-c(cv$ech.m,cv$ech.m),y <- c(0,cv$maxdmx),lty=3,lwd=1)
     } 

    }, height = 250)


  output$plotSample <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    #par(mfrow=c(3,1))
    par(mai=c(0.2,1,0.2,1))
 
     if(cv$ech.exist){
      ########################
      ## Plot sample values ##
      ########################
      values.labels.y<-0.55
      values.lines.y<-0.625
      par(mai=c(0,1,0,1),bty="n")#,pin=c(11,0.3)
      plot(cv$ech.x,cv$ech.y,pch=23,cex=1,lty=2,lwd=1,col="black",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,xlim=c(0,100),ylim=c(0,0.50),ylab="",xlab="",xaxp=c(0,100,20), xaxt="n", yaxt="n") #trace une courbe a partir de tous les couples 
      axis(2,las=2,yaxp=c(0,0.40,4))
      text(1,cv$ech.y,labels="Echantillon",cex=2,pos=4)
      text(99,cv$ech.y,labels=bquote(bar(x) == .(round(cv$ech.m,2))),cex=1.5,pos=2)
      boxplot(cv$ech.x,horizontal = TRUE,add = TRUE,at = 0.1, boxwex = 0.3, xaxt="n", yaxt="n")#, xaxt="n", yaxt="n"
    }
    }, height = 50)
    

})

