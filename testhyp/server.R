  #initiate global counters
  SP<-list()
  SP$rho<-0
  SP$nrho<-0
  SP$N<-0
  SP$lpcrho<-list()
  SP$lnrho<-list()

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
    
    # Tout ce qui est relatif aux moyennes, ecart-types et variances de H0, H1, et réalité
    if(v$truehyp=="h1"){
      cv$mx1<-v$mx1
     }
    if(v$truehyp=="h0"){
      cv$mx1<-v$mx0
    }
    
    cv$vx<-v$sx^2 #variance de x
    cv$vx.dech<-cv$vx/v$n #variance de la distribution d'échantillonnage de x
    cv$sx.dech<-sqrt(cv$vx.dech) #écart-type de la distribution d'échantillonnage de x
    cv$sx0<-v$sx/sqrt(v$n)#ecart-type de H0
    cv$vx0<-(cv$sx0)^2 #variance de H0
    cv$sx1<-v$sx/sqrt(v$n) #ecart-type de H1
    
    #Calcul de la densité maximale entre H0 et H1 pour le tracé des axes communs
    cv$dmx0<-dnorm(v$mx0,mean=v$mx0,sd=cv$sx0)
    cv$dmx1<-dnorm(cv$mx1,mean=cv$mx1,sd=cv$sx1)
    cv$maxdmx<-max(cv$dmx0,cv$dmx1)
    if(v$freezeyaxis){
      cv$maxdmx<-0.2
    }
    cv$yaxislim<-cv$maxdmx+(cv$maxdmx*0.2)
    
    # Calcul des valeurs des X pour tracer les polygones des distributions
    z<-seq(-5,5,length=100)
    cv$xr<-(z*v$sx)+cv$mx1 #x pour tracer la distribution "réalité"
    cv$x0<-(z*cv$sx0)+v$mx0 #x pour tracer la distribution "H0"
    cv$x1<-(z*cv$sx1)+cv$mx1#x pour tracer la distribution "H1"
    cv$y0<-dnorm(cv$x0,mean=v$mx0,sd=cv$sx0)
    cv$y1<-dnorm(cv$x1,mean=cv$mx1,sd=cv$sx1)
    cv$yr<-dnorm(cv$xr,mean=cv$mx1,sd=v$sx) #y réalité
    
    # Tout ce qui est relatif à la puissance, confiance, alpha, et beta
    cv$alpha<-round(1-v$confidence,3)
    cv$alpha.z<-round(qnorm(v$confidence),3)
    cv$alpha.x<-(cv$alpha.z*cv$sx0)+v$mx0
    cv$alpha.y<-dnorm(cv$alpha.x, mean=v$mx0, sd=cv$sx0)
    
    cv$alpha.z.polygon<-seq(cv$alpha.z,5,length=100)
    cv$alpha.x.polygon<-(cv$alpha.z.polygon*cv$sx0)+v$mx0
    cv$alpha.y.polygon<-dnorm(cv$alpha.x.polygon,mean=v$mx0,sd=cv$sx0)
    
    cv$confidence.z.polygon<-seq(-5,cv$alpha.z,length=100)
    cv$confidence.x.polygon<-(cv$confidence.z.polygon*cv$sx0)+v$mx0
    cv$confidence.y.polygon<-dnorm(cv$confidence.x.polygon,mean=v$mx0,sd=cv$sx0)
    
    cv$beta.y<-dnorm(cv$alpha.x, mean=cv$mx1, sd=cv$sx1)
    cv$beta.z<-(cv$alpha.x-cv$mx1)/cv$sx1
    cv$beta<-pnorm(cv$beta.z)
    cv$beta.z.polygon<-seq(-5,cv$beta.z,length=100)
    cv$beta.x.polygon<-(cv$beta.z.polygon*cv$sx1)+cv$mx1
    cv$beta.y.polygon<-dnorm(cv$beta.x.polygon,mean=cv$mx1,sd=cv$sx1)
    
    cv$power<-1-cv$beta
    cv$power.z.polygon<-seq(cv$beta.z,5,length=100)
    cv$power.x.polygon<-(cv$power.z.polygon*cv$sx1)+cv$mx1
    cv$power.y.polygon<-dnorm(cv$power.x.polygon,mean=cv$mx1,sd=cv$sx1)
    
    # Tout ce qui est relatif à l'échantillon aléatoire prélevé dans la réalité
    cv$ech.z<-getech()#créee n valeurs aléatoires N(0;1) quand input$takeech est implémenté (quand le bouton takeech est pressé)
    if (rv$lastAction=='reset') {
      cv$ech.z<-NULL
      SP$rho<<-0
      SP$nrho<<-0
      SP$N<<-0
      SP$lpcrho<<-list()
      SP$lnrho<<-list()
    }
    cv$ech.exist<-length(cv$ech.z)#ne pas prendre n mais calculer le nombre de valeurs dans l'échantillon juste pour s'assurer qu'un échantillon a été créé = le bouton action a été poussé
    if(v$truehyp=="h1"){
      cv$ech.x<-(cv$ech.z*v$sx)+cv$mx1
    }
    if(v$truehyp=="h0"){
      cv$ech.x<-(cv$ech.z*v$sx)+v$mx0
    }
    
    if(cv$ech.exist){
      cv$ech.m<-mean(cv$ech.x)
      cv$ech.m.z0<-(cv$ech.m-v$mx0)/cv$sx0
      cv$ech.m.pvalue<-signif(1-pnorm(cv$ech.m.z0),2)
      if(cv$ech.m.pvalue<0.001){
	cv$ech.m.pvalue.text<-" <0.001"
      } else {
	cv$ech.m.pvalue.text<-cv$ech.m.pvalue
      }
      cv$ech.y<-seq(0.45,0.45,length=cv$ech.exist)#liste des coordonnées y des points de l'échantillon
      if(cv$ech.exist && v$showpvaluearea){
	cv$ech.m.pvalue.z.polygon<-seq(cv$ech.m.z0,5,length=100)
	cv$ech.m.pvalue.x.polygon<-(cv$ech.m.pvalue.z.polygon*cv$sx0)+v$mx0
	cv$ech.m.pvalue.y.polygon<-dnorm(cv$ech.m.pvalue.x.polygon,mean=v$mx0,sd=cv$sx0)
      }
    }
    #Tout ce qui est relatif à l'IC àa la moyennes    
    cv$ic.z<-qnorm(1-cv$alpha/2)
    cv$ic.t<-qt(1-cv$alpha/2,v$n-1)
    cv$ic.z.limit.inf<-mean(cv$ech.x)-cv$ic.z*cv$sx.dech
    cv$ic.z.limit.sup<-mean(cv$ech.x)+cv$ic.z*cv$sx.dech
    cv$ic.t.limit.inf<-mean(cv$ech.x)-cv$ic.t*cv$sx.dech
    cv$ic.t.limit.sup<-mean(cv$ech.x)+cv$ic.t*cv$sx.dech
    
    return(cv)
  })

  
  output$doubleplot <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()

    # Calcul du % re RH0
    if(cv$ech.exist && rv$lastAction=='takeech'){
      if(cv$ech.m >= cv$alpha.x){
	  SP$rho<<-SP$rho+1
	  SP$N<<-SP$rho+SP$nrho
	} else {
	  SP$nrho<<-SP$nrho+1
	  SP$N<<-SP$rho+SP$nrho
	}
      SP$lnrho<<-c(SP$lnrho,list(SP$N))
      SP$rhopc<<-round(SP$rho/(SP$N),2) 
      SP$lpcrho<<-c(SP$lpcrho,list(SP$rhopc))
     }

    ###############
    ## Plot %RH0 ##
    ###############
    par(mfrow=c(4,1))
    if(v$hideh1 && !v$showrhotrend){
      par(mfrow=c(3,1))
    } 
    if(!v$hideh1 && v$showrhotrend){
      par(mfrow=c(5,1))
    }
    if(v$hideh1 && v$showrhotrend){
      par(mfrow=c(4,1))
    }
    if(v$showrhotrend){
      if(cv$ech.exist){
	if(SP$N<2){
	  nrholim<-2
	} else {
	  nrholim<-SP$N
	}
	nrho<-SP$lnrho
	pcrho<-SP$lpcrho
      } else {
	nrholim<-2
	nrho<-c(1)
	pcrho<-c(0)
      }
      par(mai=c(0,1,0.5,1))
      plot(nrho,pcrho,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,ylim=c(0,1),ylab="%RH0",xlab="",xaxp=c(0,nrholim,nrholim))#xlim=c(0,100),xaxp=c(0,100,20),type="l",
      axis(2,las=2,yaxp=c(0,1,2))
      lines(x<-c(0,nrholim),y <- c(cv$power,cv$power),lty=3)
      text(1,cv$power*1.05,expression(1-beta),pos=4)
    }

    #############
    ## Plot H0 ##
    #############
    par(mai=c(0,1,0.5,1))
    plot(cv$x0,cv$y0,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,xlim=c(0,100),ylim=c(0,cv$yaxislim),ylab="density",xlab="",xaxp=c(0,100,20)) #trace une courbe a partir de tous les couples x;y, et la colore en rouge. bty : A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. xaxt="n" = pas dessiner axe des x
    axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*1.1,labels="Modeles",cex=2,pos=4)
    text(99,signif(cv$maxdmx,1)*1.1,labels="Observations",cex=2,pos=2)
    text(1,signif(cv$maxdmx,1)*0.9,labels="H0",cex=2,pos=4)
    text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(N *"~"* ( mu[0] *","* frac(sigma^2,n) )),cex=1.5,pos=4)#paste("N~(",mx1,",",round(x.var,2),")",sep="")
    text(1,signif(cv$maxdmx,1)*0.5,labels=bquote(N *"~"* (.(v$mx0)*","*.(round(cv$vx0,2)))),cex=1.5,pos=4)#text(1,signif(cv$maxdmx,1)*0.8,labels=paste("H0 N~(",mx0,",",round(cv$vx0,2),")",sep=""),cex=2,pos=4)
    text(1,signif(cv$maxdmx,1)*0.1,labels=bquote(alpha == .(cv$alpha)),cex=1.5,pos=4)
    text(1,signif(cv$maxdmx,1)*0.3,labels=bquote(1 - alpha == .(v$confidence)),cex=1.5,pos=4)
    if(v$alphabetaareas){
      polygon(c(cv$alpha.x,cv$alpha.x.polygon),c(0,cv$alpha.y.polygon),col="indianred1")
      polygon(c(cv$alpha.x,cv$confidence.x.polygon),c(0,cv$confidence.y.polygon),col="lightgreen")
    } else {
      lines(x<-c(cv$alpha.x,cv$alpha.x),y <- c(-0.1,cv$alpha.y),lty=1)
    }
    if(cv$ech.exist && v$showpvaluearea){
      polygon(c(cv$ech.m,cv$ech.m.pvalue.x.polygon),c(0,cv$ech.m.pvalue.y.polygon),density=c(20))
    }
    if(v$alphabetalabels){
      text(cv$alpha.x-0.5,cv$yaxislim*0.05,labels=expression(1-alpha),cex=1.5,pos=2)
      text(cv$alpha.x+0.5,cv$yaxislim*0.05,labels=expression(alpha),cex=1.5,pos=4)
    }
    if(cv$ech.exist){
      if(v$showmean){
	lines(x<-c(cv$ech.m,cv$ech.m),y <- c(0,dnorm(0)+0.2),lty=5,lwd=1)
	}
      if(v$showicz){
	lines(x<-c(cv$ic.z.limit.inf,cv$ic.z.limit.inf),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
	lines(x<-c(cv$ic.z.limit.sup,cv$ic.z.limit.sup),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
      }
      if(v$showict){
	lines(x<-c(cv$ic.t.limit.inf,cv$ic.t.limit.inf),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
	lines(x<-c(cv$ic.t.limit.sup,cv$ic.t.limit.sup),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
      }
      text(99,signif(cv$maxdmx,1)*0.9,labels=bquote(bar(x) == .(round(cv$ech.m,2))),cex=1.5,pos=2)
      text(99,signif(cv$maxdmx,1)*0.7,labels=paste("p-value : ",cv$ech.m.pvalue.text,sep=""),cex=1.5,pos=2)
      if(cv$ech.m >= cv$alpha.x){
	text(99,signif(cv$maxdmx,1)*0.5,labels=paste("Conclusion : RH0",sep=""),cex=1.5,pos=2)
      } else {
	text(99,signif(cv$maxdmx,1)*0.5,labels=paste("Conclusion : NRH0",sep=""),cex=1.5,pos=2)
      }
      text(99,signif(cv$maxdmx,1)*0.2,labels=bquote(paste("%RHO = ",frac(.(SP$rho),.(SP$N))," = ",.(SP$rhopc),sep="")),cex=1.5,pos=2)# paste("%RH0 :",SP$rhopc,sep="")
    }
    
    #############
    ## Plot H1 ##
    #############
    if(!v$hideh1) {
      #par(mai=c(0.5,1,0,1))
      par(mai=c(0,1,0,1))
      plot(cv$x1,cv$y1,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,xlim=c(0,100),ylim=c(0,cv$yaxislim),ylab="density",xlab="",xaxp=c(0,100,20)) #trace une courbe a partir de tous les couples x;y, et la colore en rouge. bty : A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. xaxt="n" = pas dessiner axe des x
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
      text(1,signif(cv$maxdmx,1)*0.9,labels="H1",cex=2,pos=4)
      text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(N *"~"* ( mu[1] *","* frac(sigma^2,n) )),cex=1.5,pos=4)#paste("N~(",mx1,",",round(x.var,2),")",sep="")
      text(1,signif(cv$maxdmx,1)*0.5,labels=bquote(N *"~"* (.(cv$mx1)*","*.(round(cv$vx.dech,2)))),cex=1.5,pos=4)#text(1,signif(cv$maxdmx,1)*0.8,labels=paste("H1 N~(",mx1,",",round(cv$vx.dech,2),")",sep=""),cex=2,pos=4)
      text(1,signif(cv$maxdmx,1)*0.3,labels=bquote(beta == .(signif(cv$beta,2))),cex=1.5,pos=4)
      text(1,signif(cv$maxdmx,1)*0.1,labels=bquote(1 - beta == .(signif(cv$power,2))),cex=1.5,pos=4)
      if(v$alphabetaareas){
	polygon(c(cv$alpha.x,cv$power.x.polygon),c(0,cv$power.y.polygon),col="lightgreen")
	polygon(c(cv$beta.x.polygon,cv$alpha.x),c(cv$beta.y.polygon,0),col="indianred1")
	if(!v$alphabetaproject){
	  lines(x<-c(cv$alpha.x,cv$alpha.x),y <- c(cv$beta.y,dnorm(0)+0.2),lty=1)
	}
      } else {
	if(v$alphabetaproject){
	  lines(x<-c(cv$alpha.x,cv$alpha.x),y <- c(0,cv$beta.y),lty=1)
	  } else {
	  lines(x<-c(cv$alpha.x,cv$alpha.x),y <- c(0,cv$maxdmx+(cv$maxdmx*0.5)),lty=1)
	  }
      }
      if(v$alphabetalabels){
      text(cv$alpha.x-0.5,cv$yaxislim*0.05,labels=expression(beta),cex=1.5,pos=2)
      text(cv$alpha.x+0.5,cv$yaxislim*0.05,labels=expression(1-beta),cex=1.5,pos=4)
      }
      if(cv$ech.exist){
	if(v$showmean){
	  lines(x<-c(cv$ech.m,cv$ech.m),y <- c(0,dnorm(0)+0.2),lty=5,lwd=1)
	}
	if(v$showicz){
	  lines(x<-c(cv$ic.z.limit.inf,cv$ic.z.limit.inf),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
	  lines(x<-c(cv$ic.z.limit.sup,cv$ic.z.limit.sup),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
	}
	if(v$showict){
	  lines(x<-c(cv$ic.t.limit.inf,cv$ic.t.limit.inf),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
	  lines(x<-c(cv$ic.t.limit.sup,cv$ic.t.limit.sup),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
	}
      }
    }
    
    ##################
    ## Plot Reality ##
    ##################
    par(mai=c(0,1,0,1))
    plot(cv$xr,cv$yr,type="l",lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,xlim=c(0,100),ylim=c(0,cv$yaxislim),ylab="density",xlab="",xaxp=c(0,100,20)) #trace une courbe a partir de tous les couples x;y, et la colore en rouge. bty : A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. xaxt="n" = pas dessiner axe des x
    axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.9,labels="Realite",cex=2, pos=4)
    text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(N *"~"* ( mu[1] *","* sigma^2 )),cex=1.5,pos=4)#paste("N~(",mx1,",",round(x.var,2),")",sep="")
    text(1,signif(cv$maxdmx,1)*0.5,labels=bquote(N *"~"* (.(cv$mx1)*","*.(cv$vx))),cex=1.5,pos=4)
    if(cv$ech.exist){
      #points(cv$ech.x,cv$ech.y)
      rug(cv$ech.x,lwd=2)
      text(99,signif(cv$maxdmx,1)*0.7,labels=bquote(bar(x) == .(round(cv$ech.m,2))),cex=1.5,pos=2)
      if(v$showmean){
	lines(x<-c(cv$ech.m,cv$ech.m),y <- c(-0.01,dnorm(0)+0.2),lty=5,lwd=1)
      }
      if(v$showicz){
	text(99,signif(cv$maxdmx,1)*0.5,labels=bquote(paste("IC",.(v$confidence*100)," pour ",sigma^2," connue : [",.(round(cv$ic.z.limit.inf,2)),";",.(round(cv$ic.z.limit.sup,2)),"]",sep="")),cex=1.5,pos=2)
	lines(x<-c(cv$ic.z.limit.inf,cv$ic.z.limit.inf),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
	lines(x<-c(cv$ic.z.limit.sup,cv$ic.z.limit.sup),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
      }
      if(v$showict){
	text(99,signif(cv$maxdmx,1)*0.3,labels=bquote(paste("IC",.(v$confidence*100)," pour ",sigma^2," inconnue : [",.(round(cv$ic.t.limit.inf,2)),";",.(round(cv$ic.t.limit.sup,2)),"]",sep="")),cex=1.5,pos=2)
	lines(x<-c(cv$ic.t.limit.inf,cv$ic.t.limit.inf),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
	lines(x<-c(cv$ic.t.limit.sup,cv$ic.t.limit.sup),y <- c(-0.01,dnorm(0)+0.2),lty=3,lwd=1)
      }
      
      ########################
      ## Plot sample values ##
      ########################
      values.labels.y<-0.55
      values.lines.y<-0.625
      par(mai=c(0.5,1,0,1),bty="n")#,pin=c(11,0.3)
      plot(cv$ech.x,cv$ech.y,pch=23,cex=2,lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1,xlim=c(0,100),ylim=c(0,0.75),ylab="",xlab="",xaxp=c(0,100,20), xaxt="n", yaxt="n") #trace une courbe a partir de tous les couples 
      text(1,0.45,labels="Echantillon",cex=2,pos=4)
      if(v$showboxplot){
	boxplot(cv$ech.x,horizontal = TRUE,add = TRUE,at = 0.2, boxwex = 0.5, xaxt="n", yaxt="n")#,add = TRUE,at = 0.05, boxwex = 0.03, xaxt="n", yaxt="n"
      }
      #axis(2,las=2,yaxp=c(0,0.75,4))
      if(v$showmean){
	lines(x<-c(cv$ech.m,cv$ech.m),y <- c(values.lines.y,0.75),lty=5,lwd=1)
	text(cv$ech.m,values.labels.y,labels=expression(bar(x)),cex=2)#,pos=0
      }
      if(v$showicz){
	lines(x<-c(cv$ic.z.limit.inf,cv$ic.z.limit.inf),y <- c(values.lines.y,0.75),lty=3,lwd=1)
	lines(x<-c(cv$ic.z.limit.sup,cv$ic.z.limit.sup),y <- c(values.lines.y,0.75),lty=3,lwd=1)
      }
      if(v$showict){
	lines(x<-c(cv$ic.t.limit.inf,cv$ic.t.limit.inf),y <- c(values.lines.y,0.75),lty=3,lwd=1)
	lines(x<-c(cv$ic.t.limit.sup,cv$ic.t.limit.sup),y <- c(values.lines.y,0.75),lty=3,lwd=1)
      }
    }
  }, height = 800)#, height = 700, width = 900
})

