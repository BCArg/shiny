  #initiate global counters
  SP<-list()
  SP$n.ic<-0
  SP$n.ic.z.inc.mu<-0
  SP$n.ic.z.noninc.mu<-0
  SP$pc.ic.z.inc.mu<-0
  SP$n.ic.t.inc.mu<-0
  SP$n.ic.t.noninc.mu<-0
  SP$pc.ic.t.inc.mu<-0
  SP$l.n.ic<-list()
  SP$l.pc.ic.z.inc.mu<-list()
  SP$l.pc.ic.t.inc.mu<-list()


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
    if(v$seedech){
      cv$maxdmx<-max(cv$dmx,cv$dmx.dech)
    } else {
      cv$maxdmx<-cv$dmx
    }    
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
      SP$n.ic.z.inc.mu<<-0
      SP$n.ic.z.noninc.mu<<-0
      SP$pc.ic.z.inc.mu<<-0
      SP$n.ic.t.inc.mu<<-0
      SP$n.ic.t.noninc.mu<<-0
      SP$pc.ic.t.inc.mu<<-0
      SP$l.n.ic<<-list()
      SP$l.pc.ic.z.inc.mu<<-list()
      SP$l.pc.ic.t.inc.mu<<-list()

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
      cv$ech.y.value<-0.30
      cv$ech.y<-seq(cv$ech.y.value,cv$ech.y.value,length=cv$ech.exist)#liste des coordonnées y des points de l'échantillon
          
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
      
      SP$n.ic<<-SP$n.ic+1
      if(v$mx >= cv$ic.z.limit.inf && v$mx <= cv$ic.z.limit.sup){
	SP$n.ic.z.inc.mu<<-SP$n.ic.z.inc.mu+1
	cv$ic.z.color<-'lightgreen'
	cv$ic.z.density<-10
      } else {
	SP$n.ic.z.noninc.mu<<-SP$n.ic.z.noninc.mu+1
	cv$ic.z.color<-'indianred1'
	cv$ic.z.density<-25
      }
      if(v$mx >= cv$ic.t.limit.inf && v$mx <= cv$ic.t.limit.sup){
	SP$n.ic.t.inc.mu<<-SP$n.ic.t.inc.mu+1
	cv$ic.t.color<-'lightgreen'
	cv$ic.t.density<-10
      } else {
	SP$n.ic.t.noninc.mu<<-SP$n.ic.t.noninc.mu+1
	cv$ic.t.color<-'indianred1'
	cv$ic.t.density<-25
      }
      SP$pc.ic.z.inc.mu<<-round(SP$n.ic.z.inc.mu/SP$n.ic,4)
      SP$pc.ic.t.inc.mu<<-round(SP$n.ic.t.inc.mu/SP$n.ic,4)
      SP$l.n.ic<<-c(SP$l.n.ic,list(SP$n.ic))
      SP$l.pc.ic.z.inc.mu<<-c(SP$l.pc.ic.z.inc.mu,list(SP$pc.ic.z.inc.mu))
      SP$l.pc.ic.t.inc.mu<<-c(SP$l.pc.ic.t.inc.mu,list(SP$pc.ic.t.inc.mu))

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
    if(v$seedech){
      polygon(c(-5,cv$xr.dech),c(0,cv$yr.dech),lty=3)
      text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(paste("Distrib. echan. ", N *"~"* ( mu *","* frac(sigma^2,n) ) ," ", N *"~"* (.(v$mx)*","*.(signif(cv$vx/v$n,2))) ,sep='')),cex=1, pos=4)
    }
    axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.9,labels=bquote(paste("Realite ", N *"~"* ( mu *","* sigma^2 ) ," ", N *"~"* (.(v$mx)*","*.(cv$vx)) ,sep='')),cex=1, pos=4)
    #text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(N *"~"* ( mu *","* sigma^2 )),cex=1.25,pos=4)#paste("N~(",mx1,",",round(x.var,2),")",sep="")
    #text(1,signif(cv$maxdmx,1)*0.5,labels=bquote(N *"~"* (.(v$mx)*","*.(cv$vx))),cex=1.25,pos=4)
    if(v$seemu){
      lines(x<-c(v$mx,v$mx),y <- c(0,cv$maxdmx),lty=1,lwd=1)
      text(v$mx,cv$maxdmx*1.05,labels=bquote(mu),cex=1)
    }

    if(cv$ech.exist){
      #points(cv$ech.x,cv$ech.y)
      m.ech.z.y.delta<-0.7#1.05
      m.ech.t.y.delta<-0.35#1.05
      rug(cv$ech.x,lwd=2)

      if(v$seeicvarknown){
	text(99,cv$maxdmx*m.ech.z.y.delta,labels=bquote(paste("IC",.(v$confidence*100)," pour ",sigma^2," connue : [",.(round(cv$ic.z.limit.inf,2)),";",.(round(cv$ic.z.limit.sup,2)),"]",sep="")),cex=1,pos=2)
	text(99,cv$maxdmx*m.ech.z.y.delta*0.75,labels=bquote(paste("%IC couvrant ",mu," = ",frac(.(SP$n.ic.z.inc.mu),.(SP$n.ic))," = ",.(SP$pc.ic.z.inc.mu*100),"%",sep="")),cex=1,pos=2)
	text(cv$ech.m,cv$maxdmx*m.ech.z.y.delta,labels=bquote(bar(x)),cex=1)#,pos=2
	lines(x<-c(cv$ech.m,cv$ech.m),y <- c(0,cv$maxdmx*m.ech.z.y.delta),lty=2,lwd=1)
	#lines(x<-c(cv$ic.z.limit.inf,cv$ic.z.limit.inf),y <- c(0,cv$maxdmx*m.ech.z.y.delta),lty=4,lwd=1)
	#lines(x<-c(cv$ic.z.limit.sup,cv$ic.z.limit.sup),y <- c(0,cv$maxdmx*m.ech.z.y.delta),lty=4,lwd=1)
      
	text(cv$ic.z.limit.inf,cv$maxdmx*m.ech.z.y.delta,labels="[",cex=1,col=cv$ic.z.color)#,pos=2
	text(cv$ic.z.limit.sup,cv$maxdmx*m.ech.z.y.delta,labels="]",cex=1,col=cv$ic.z.color)#,pos=2
	lines(x<-c(cv$ic.z.limit.inf,cv$ech.m-1),y <- c(cv$maxdmx*m.ech.z.y.delta,cv$maxdmx*m.ech.z.y.delta),lwd=0.5)#lty=4,
	lines(x<-c(cv$ech.m+1,cv$ic.z.limit.sup),y <- c(cv$maxdmx*m.ech.z.y.delta,cv$maxdmx*m.ech.z.y.delta),lwd=0.5)#lty=4,
	polygon(c(cv$ic.z.limit.inf,cv$ic.z.limit.inf,cv$ic.z.limit.sup,cv$ic.z.limit.sup),c(cv$maxdmx*m.ech.z.y.delta+cv$maxdmx*m.ech.z.y.delta*-0.1,cv$maxdmx*m.ech.z.y.delta+cv$maxdmx*m.ech.z.y.delta*0.1,cv$maxdmx*m.ech.z.y.delta+cv$maxdmx*m.ech.z.y.delta*0.1,cv$maxdmx*m.ech.z.y.delta+cv$maxdmx*m.ech.z.y.delta*-0.1),col=cv$ic.z.color,density=cv$ic.z.density)
      }
      if(v$seeicvarunknown){
	text(99,cv$maxdmx*m.ech.t.y.delta,labels=bquote(paste("IC",.(v$confidence*100)," pour ",sigma^2," inconnue : [",.(round(cv$ic.t.limit.inf,2)),";",.(round(cv$ic.t.limit.sup,2)),"]",sep="")),cex=1,pos=2)
	text(99,cv$maxdmx*m.ech.t.y.delta*0.5,labels=bquote(paste("%IC couvrant ",mu," = ",frac(.(SP$n.ic.t.inc.mu),.(SP$n.ic))," = ",.(SP$pc.ic.t.inc.mu*100),"%",sep="")),cex=1,pos=2)
	text(cv$ech.m,cv$maxdmx*m.ech.t.y.delta,labels=bquote(bar(x)),cex=1)#,pos=2
	lines(x<-c(cv$ech.m,cv$ech.m),y <- c(0,cv$maxdmx*m.ech.t.y.delta),lty=2,lwd=1)
	#lines(x<-c(cv$ic.t.limit.inf,cv$ic.t.limit.inf),y <- c(0,cv$maxdmx*m.ech.t.y.delta),lty=4,lwd=1)
	#lines(x<-c(cv$ic.t.limit.sup,cv$ic.t.limit.sup),y <- c(0,cv$maxdmx*m.ech.t.y.delta),lty=4,lwd=1)
      
	text(cv$ic.t.limit.inf,cv$maxdmx*m.ech.t.y.delta,labels="[",cex=1,col=cv$ic.z.color)#,pos=2
	text(cv$ic.t.limit.sup,cv$maxdmx*m.ech.t.y.delta,labels="]",cex=1,col=cv$ic.z.color)#,pos=2
	lines(x<-c(cv$ic.t.limit.inf,cv$ech.m-1),y <- c(cv$maxdmx*m.ech.t.y.delta,cv$maxdmx*m.ech.t.y.delta),lwd=0.5)#lty=4,
	lines(x<-c(cv$ech.m+1,cv$ic.t.limit.sup),y <- c(cv$maxdmx*m.ech.t.y.delta,cv$maxdmx*m.ech.t.y.delta),lwd=0.5)#lty=4,
	polygon(c(cv$ic.t.limit.inf,cv$ic.t.limit.inf,cv$ic.t.limit.sup,cv$ic.t.limit.sup),c(cv$maxdmx*m.ech.t.y.delta+cv$maxdmx*m.ech.z.y.delta*-0.1,cv$maxdmx*m.ech.t.y.delta+cv$maxdmx*m.ech.z.y.delta*0.1,cv$maxdmx*m.ech.t.y.delta+cv$maxdmx*m.ech.z.y.delta*0.1,cv$maxdmx*m.ech.t.y.delta+cv$maxdmx*m.ech.z.y.delta*-0.1),col=cv$ic.t.color,density=cv$ic.t.density)
      }
     } 

    }, height = 250)


  output$plotSample <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    #par(mfrow=c(3,1))
    #par(mai=c(0.2,1,0.2,1))
 
     if(cv$ech.exist){
      ########################
      ## Plot sample values ##
      ########################
      values.labels.y<-0.55
      values.lines.y<-0.625
      par(mai=c(0,1,0,1),bty="n")#,pin=c(11,0.3)
      plot(cv$ech.x,cv$ech.y,pch=23,cex=1,lty=2,lwd=1,col="black",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,xlim=c(0,100),ylim=c(0,0.50),ylab="",xlab="",xaxp=c(0,100,20), xaxt="n", yaxt="n") #trace une courbe a partir de tous les couples 
      #axis(2,las=2,yaxp=c(0,0.40,4))
      text(1,cv$ech.y.value,labels="Echantillon",cex=1,pos=4)
      text(99,cv$ech.y.value,labels=bquote(bar(x) == .(round(cv$ech.m,2))),cex=1.25,pos=2)
#      text(cv$ech.m,cv$ech.y.value+0.10,labels=bquote(bar(x)),cex=1)#,pos=2
#      text(cv$ic.z.limit.inf,cv$ech.y.value+0.10,labels="[",cex=1)#,pos=2
#      text(cv$ic.z.limit.sup,cv$ech.y.value+0.10,labels="]",cex=1)#,pos=2
#      lines(x<-c(cv$ic.z.limit.inf,cv$ech.m-1),y <- c(cv$ech.y.value+0.10,cv$ech.y.value+0.10),lwd=0.5)#lty=4,
#      lines(x<-c(cv$ech.m+1,cv$ic.z.limit.sup),y <- c(cv$ech.y.value+0.10,cv$ech.y.value+0.10),lwd=0.5)#lty=4,
      #lines(x<-c(cv$ic.z.limit.sup,cv$ic.z.limit.sup),y <- c(cv$ech.y+0.15,0.5),lty=4,lwd=1)
      boxplot(cv$ech.x,horizontal = TRUE,add = TRUE,at = 0.1, boxwex = 0.3, xaxt="n", yaxt="n")#, xaxt="n", yaxt="n"
    }
    }, height = 50)
    
  output$plotPercent<- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(cv$ech.exist && v$seeicpcevolution){
      ###########################
      ## Plot % IC including µ ##
      ###########################
	if(SP$n.ic<2){
	  SP$n.ic.lim<-2
	} else {
	  SP$n.ic.lim<-SP$n.ic
	}
      par(mai=c(0.5,1,0.2,1))
      plot(SP$l.n.ic,SP$l.pc.ic.z.inc.mu,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,ylim=c(0,1),ylab="%IC couvrant µ",xlab="",xaxp=c(0,SP$n.ic.lim,SP$n.ic.lim),xlim=c(0,SP$n.ic.lim))#xlim=c(0,100),xaxp=c(0,100,20),type="l",
      axis(2,las=2,yaxp=c(0,1,2))
      
      lines(x<-c(0,SP$n.ic.lim),y <- c(v$confidence,v$confidence),lty=3)
      text(SP$n.ic.lim*0.01,v$confidence*0.95,expression(1-alpha),pos=4)
      #legend(1,0.25,c(bquote(paste("%IC couvrant ",mu," quand ",sigma^2," est connue ",sep="")),bquote(paste("%IC couvrant ",mu," quand ",sigma^2," est inconnue ",sep=""))))
      text(SP$n.ic.lim*0.05,0.20,labels=bquote(paste(sigma^2," connue ",sep="")),cex=1,pos=4)#"%IC couvrant ",mu," quand ",
      lines(x<-c(SP$n.ic.lim*0.01,SP$n.ic.lim*0.04),y <- c(0.20,0.20),lty=1,type="l",col="black",lwd=1,las=1)
      if(v$seeicvarunknown){
	lines(SP$l.n.ic,SP$l.pc.ic.t.inc.mu, type="l", lwd=1,lty=2)

	text(SP$n.ic.lim*0.05,0.10,labels=bquote(paste(sigma^2," inconnue ",sep="")),cex=1,pos=4)#"%IC couvrant ",mu," quand ",
	lines(x<-c(SP$n.ic.lim*0.01,SP$n.ic.lim*0.04),y <- c(0.10,0.10),lty=2,type="l",col="black",lwd=1)
      }
    }
  }, height = 250)

})

