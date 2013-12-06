## testhypic Shiny/R app server.R                                           
##                                                                      
## Author(s) :
## -----------
## Grégoire Vincke http://www.uclouvain.be/gregoire.vincke       
## For Statistical eLearning Tools http://sites.uclouvain.be/selt/      
##                                                                      
## Licences : 
## ---------
## CC-BY for the web page http://sites.uclouvain.be/selt/shiny/testhypic
## see http://creativecommons.org/licenses/by/2.0/be/ for more informations       
##
## GPLv2 for source code on https://github.com/uclouvain-selt/shiny  
## See LICENCE.tx or http://www.gnu.org/licenses/old-licenses/gpl-2.0.html for more informations

Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots
library(shiny)
library(plotrix)

#initiate global counters
  SP<<-list()
  SP$last.takesample.value<<-0
  SP$samples.z<<-list()
  SP$samples.x<<-list()
  SP$samples.x.m<<-list()
  SP$samples.x.sd<<-list()
  SP$samples.y<<-list()
  
  SP$ic.k.limit.inf<<-list()
  SP$ic.k.limit.sup<<-list()
  SP$ic.z.limit.inf<<-list()
  SP$ic.z.limit.sup<<-list()
  SP$ic.t.limit.inf<<-list()
  SP$ic.t.limit.sup<<-list()
  
  color.true<-rgb(0,0.7,0,0.5)
  color.false<-rgb(1,0,0,0.5)
  density.true<-10
  density.false<-25
  
shinyServer(function(input, output) {
  
  rv <- reactiveValues()# Create a reactiveValues object, to let us use settable reactive values
  rv$lastAction <- 'none' # To start out, lastAction == NULL, meaning nothing clicked yet
  # An observe block for each button, to record that the action happened
  observe({
    if (input$takesample > SP$last.takesample.value) {
      rv$lastAction <- 'takesample'
    }
  })
  observe({
    if (input$reset != 0) {
      rv$lastAction <- 'reset'
    }
  })
  
  getSamples<-reactive({#créee n valeurs aléatoires N(0;1) quand input$takesample est implémenté (quand le bouton takesample est pressé)
    if(input$takesample > SP$last.takesample.value){
      return(isolate({#Now do the expensive stuff
	  samples<-list()
	  for (i in 1:input$ns){
	    samples[[i]]<-rnorm(input$n)#créee n valeurs aléatoires N(0;1)
	  }
	  return(samples)
      }))
    } else {
      return(NULL)
    }})

  getInputValues<-reactive({
    return(input)#collect all inputs
  })
  
  getComputedValues<-reactive({
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
    
    ## Define reality parameters
    cv$vx<-v$sx^2#compute variance of Reality distribution
    if(v$truehyp=="h1"){#if H1 is considered as the true model
      cv$mx<-v$mx1
    }
    if(v$truehyp=="h0"){#if H0 is considered as the true model
      cv$mx<-v$mx0
    }
    
    ## Computation of x y coordinates for Normal curve of Reality
    z<-seq(-5,5,length=100)
    cv$xr<-(z*v$sx)+cv$mx #x for Reality
    cv$yr<-dnorm(cv$xr,mean=cv$mx,sd=v$sx)#y for Reality
    
     ## Computation of alpha, beta, confidence and power related variables  ##
    cv$alpha<-round(1-v$confidence,3)#Computation of alpha probability
    
    ## Set z and t statistics for confidence intervals
    cv$ic.z<-qnorm(1-cv$alpha/2)#z positive limit of a bidirectionnal confidence interval in N(0,1) => for CI with known variance
    cv$ic.t<-qt(1-cv$alpha/2,v$n-1)#t positive limit of a bidirectionnal confidence interval in t(n-1) => for CI with unknown variance

    ## Computation of parameters of distribution of mean of samples 
    cv$sx.dech<-v$sx/sqrt(v$n)#standard deviation of mean samples distributions
    cv$vx.dech<-cv$sx.dech^2
    
    ## Computation of sample related values ##
    cv$samples.z<-list()
    cv$samples.x<-list()
    cv$samples.y<-list()
    cv$samples.x.m<-list()
    cv$samples.x.sd<-list()
    cv$ic.k.limit.inf<-list()
    cv$ic.z.limit.inf<-list()
    cv$ic.t.limit.inf<-list()
    cv$ic.k.limit.sup<-list()
    cv$ic.z.limit.sup<-list()
    cv$ic.t.limit.sup<-list()
    
    cv$samples.z<-getSamples()
    
    ## Reset values ##
    if (rv$lastAction=='reset') {
      SP$last.takesample.value<<-0
      cv$samples.z<-NULL
      SP$samples.z<<-list()
      SP$samples.x<<-list()
      SP$samples.x.m<<-list()
      SP$samples.x.sd<<-list()
      SP$samples.y<<-list()
      SP$n.samples<<-0
      SP$vect.n.samples<<-c()
      SP$ic.k.limit.inf<<-list()
      SP$ic.k.limit.sup<<-list()
      SP$ic.z.limit.inf<<-list()
      SP$ic.z.limit.sup<<-list()
      SP$ic.t.limit.inf<<-list()
      SP$ic.t.limit.sup<<-list()
    }
    
    ## Initiate values
      
      cv$n.ic.k.inc.mu0<-0
      cv$pc.ic.k.inc.mu0<-0
      cv$vect.pc.ic.k.inc.mu0<-c()
      
      cv$n.ic.k.l.ninc.mu0<-0
      cv$pc.ic.k.l.ninc.mu0<-0
      
      cv$n.ic.k.r.ninc.mu0<-0
      cv$pc.ic.k.r.ninc.mu0<-0
      
      cv$n.ic.k.inc.mu1<-0
      cv$pc.ic.k.inc.mu1<-0
      cv$vect.pc.ic.k.inc.mu1<-c()
      
      cv$n.ic.k.l.ninc.mu1<-0
      cv$pc.ic.k.l.ninc.mu1<-0
      
      cv$n.ic.k.r.ninc.mu1<-0
      cv$pc.ic.k.r.ninc.mu1<-0
      
      cv$n.ic.z.inc.mu0<-0
      cv$pc.ic.z.inc.mu0<-0
      
      cv$n.ic.z.l.ninc.mu0<-0
      cv$pc.ic.z.l.ninc.mu0<-0
      
      cv$n.ic.z.r.ninc.mu0<-0
      cv$pc.ic.z.r.ninc.mu0<-0
      
      cv$n.ic.z.inc.mu1<-0
      cv$pc.ic.z.inc.mu1<-0
      
      cv$n.ic.z.l.ninc.mu1<-0
      cv$pc.ic.z.l.ninc.mu1<-0
      
      cv$n.ic.z.r.ninc.mu1<-0
      cv$pc.ic.z.r.ninc.mu1<-0
      
      cv$n.ic.t.inc.mu0<-0
      cv$pc.ic.t.inc.mu0<-0
      
      cv$n.ic.t.l.ninc.mu0<-0
      cv$pc.ic.t.l.ninc.mu0<-0
      
      cv$n.ic.t.r.ninc.mu0<-0
      cv$pc.ic.t.r.ninc.mu0<-0
      
      cv$n.ic.t.inc.mu1<-0
      cv$pc.ic.t.inc.mu1<-0
      
      cv$n.ic.t.l.ninc.mu1<-0
      cv$pc.ic.t.l.ninc.mu1<-0
      
      cv$n.ic.t.r.ninc.mu1<-0
      cv$pc.ic.t.r.ninc.mu1<-0
    
    cv$n.samples<-length(SP$samples.z)
    cv$samples.exist<-length(cv$samples.z)#mesure length of sample values to test if a sample has been created

    if(cv$samples.exist>0){
      for(i in 1:length(cv$samples.z)){
	cv$samples.x[[i]]<-(cv$samples.z[[i]]*v$sx)+cv$mx#Then sample values are compute with H1 mean and standard deviation
	y<-c()
	for(j in 1:v$n){
	  y<-c(y,(0.05/(v$ns+1))*i)
	}
	cv$samples.y[[i]]<-y
	cv$samples.x.m[[i]]<-mean(cv$samples.x[[i]])#means of samples
	cv$samples.x.sd[[i]]<-sd(cv$samples.x[[i]])#means of samples
	
	## Computation of confidence intervals for the mean µ ##    
	cv$ic.k.limit.inf[[i]]<-cv$samples.x.m[[i]]-v$k#compute the CI lower limit with empiric k value
	cv$ic.k.limit.sup[[i]]<-cv$samples.x.m[[i]]+v$k#compute the CI higher limit with empiric k value
	cv$ic.z.limit.inf[[i]]<-cv$samples.x.m[[i]]-cv$ic.z*cv$sx.dech#compute the CI lower limit when variance known
	cv$ic.z.limit.sup[[i]]<-cv$samples.x.m[[i]]+cv$ic.z*cv$sx.dech#compute the CI higher limit when variance known
	cv$ic.t.limit.inf[[i]]<-cv$samples.x.m[[i]]-cv$ic.t*(cv$samples.x.sd[[i]]/sqrt(v$n))#compute the CI lower limit when variance unknown
	cv$ic.t.limit.sup[[i]]<-cv$samples.x.m[[i]]+cv$ic.t*(cv$samples.x.sd[[i]]/sqrt(v$n))#compute the CI higher limit when variance unknown
	## Testing if IC covers µ0 or µ1
	## K vs µ0
	cv$samples.ic.k.mu0.color[[i]]<-color.false
	cv$samples.ic.k.mu0.density[[i]]<-density.false
	if(cv$ic.k.limit.inf[[i]] <= v$mx0 && v$mx0  <= cv$ic.k.limit.sup[[i]]){
	  cv$samples.ic.k.mu0.color[[i]]<-color.true
	  cv$samples.ic.k.mu0.density[[i]]<-density.true
	}
	## K vs µ1
	cv$samples.ic.k.mu1.color[[i]]<-color.false
	cv$samples.ic.k.mu1.density[[i]]<-density.false
	if(cv$ic.k.limit.inf[[i]] <= v$mx1 && v$mx1  <= cv$ic.k.limit.sup[[i]]){
	  cv$samples.ic.k.mu1.color[[i]]<-color.true
	  cv$samples.ic.k.mu1.density[[i]]<-density.true
	}
	## Z vs µ0
	cv$samples.ic.z.mu0.color[[i]]<-color.false
	cv$samples.ic.z.mu0.density[[i]]<-density.false
	if(cv$ic.z.limit.inf[[i]] <= v$mx0 && v$mx0  <= cv$ic.z.limit.sup[[i]]){
	  cv$samples.ic.z.mu0.color[[i]]<-color.true
	  cv$samples.ic.z.mu0.density[[i]]<-density.true
	}
 	## Z vs µ1
	cv$samples.ic.z.mu1.color[[i]]<-color.false
	cv$samples.ic.z.mu1.density[[i]]<-density.false
	if(cv$ic.z.limit.inf[[i]] <= v$mx1 && v$mx1  <= cv$ic.z.limit.sup[[i]]){
	  cv$samples.ic.z.mu1.color[[i]]<-color.true
	  cv$samples.ic.z.mu1.density[[i]]<-density.true
	}
	## t vs µ0
	cv$samples.ic.t.mu0.color[[i]]<-color.false
	cv$samples.ic.t.mu0.density[[i]]<-density.false
	if(cv$ic.t.limit.inf[[i]] <= v$mx0 && v$mx0  <= cv$ic.t.limit.sup[[i]]){
	  cv$samples.ic.t.mu0.color[[i]]<-color.true
	  cv$samples.ic.t.mu0.density[[i]]<-density.true
	}
	## t vs µ1
	cv$samples.ic.t.mu1.color[[i]]<-color.false
	cv$samples.ic.t.mu1.density[[i]]<-density.false
	if(cv$ic.t.limit.inf[[i]] <= v$mx1 && v$mx1  <= cv$ic.t.limit.sup[[i]]){
	  cv$samples.ic.t.mu1.color[[i]]<-color.true
	  cv$samples.ic.t.mu1.density[[i]]<-density.true
	}

      }
      if(v$takesample > SP$last.takesample.value){
	SP$samples.z<<-c(SP$samples.z,cv$samples.z)
	SP$samples.x<<-c(SP$samples.x,cv$samples.x)
	SP$samples.x.m<<-c(SP$samples.x.m,cv$samples.x.m)
	SP$samples.x.sd<<-c(SP$samples.x.sd,cv$samples.x.sd)
	SP$samples.y<<-c(SP$samples.y,cv$samples.y)
	
	SP$ic.k.limit.inf<<-c(SP$ic.k.limit.inf,cv$ic.k.limit.inf)
	SP$ic.k.limit.sup<<-c(SP$ic.k.limit.sup,cv$ic.k.limit.sup)
	SP$ic.z.limit.inf<<-c(SP$ic.z.limit.inf,cv$ic.z.limit.inf)
	SP$ic.z.limit.sup<<-c(SP$ic.z.limit.sup,cv$ic.z.limit.sup)
	SP$ic.t.limit.inf<<-c(SP$ic.t.limit.inf,cv$ic.t.limit.inf)
	SP$ic.t.limit.sup<<-c(SP$ic.t.limit.sup,cv$ic.t.limit.sup)
      }

      
      cv$n.samples<-length(SP$samples.z)
      cv$vect.n.samples<-c(1:cv$n.samples)
      

      
      for(i in 1:cv$n.samples){
	## Testing if IC covers µ0 or µ1
	## K vs µ0
	if(SP$ic.k.limit.sup[[i]] < v$mx0){
	  cv$n.ic.k.l.ninc.mu0<-cv$n.ic.k.l.ninc.mu0+1
	}
	if(SP$ic.k.limit.inf[[i]] <= v$mx0 && v$mx0  <= SP$ic.k.limit.sup[[i]]){
	  cv$n.ic.k.inc.mu0<-cv$n.ic.k.inc.mu0+1
	}
 	if(v$mx0 < SP$ic.k.limit.inf[[i]]){
	  cv$n.ic.k.r.ninc.mu0<-cv$n.ic.k.r.ninc.mu0+1
	}
	
	## K vs µ1
	if(SP$ic.k.limit.sup[[i]] < v$mx1){
	  cv$n.ic.k.l.ninc.mu1<-cv$n.ic.k.l.ninc.mu1+1
	}
	if(SP$ic.k.limit.inf[[i]] <= v$mx1 && v$mx1  <= SP$ic.k.limit.sup[[i]]){
	  cv$n.ic.k.inc.mu1<-cv$n.ic.k.inc.mu1+1
	}
 	if(v$mx1 < SP$ic.k.limit.inf[[i]]){
	  cv$n.ic.k.r.ninc.mu1<-cv$n.ic.k.r.ninc.mu1+1
	}
	
	## Z vs µ0
	if(SP$ic.z.limit.sup[[i]] < v$mx0){
	  cv$n.ic.z.l.ninc.mu0<-cv$n.ic.z.l.ninc.mu0+1
	}
	if(SP$ic.z.limit.inf[[i]] <= v$mx0 && v$mx0  <= SP$ic.z.limit.sup[[i]]){
	  cv$n.ic.z.inc.mu0<-cv$n.ic.z.inc.mu0+1
	}
 	if(v$mx0 < SP$ic.z.limit.inf[[i]]){
	  cv$n.ic.z.r.ninc.mu0<-cv$n.ic.z.r.ninc.mu0+1
	}
	
	## Z vs µ1
	if(SP$ic.z.limit.sup[[i]] < v$mx1){
	  cv$n.ic.z.l.ninc.mu1<-cv$n.ic.z.l.ninc.mu1+1
	}
	if(SP$ic.z.limit.inf[[i]] <= v$mx1 && v$mx1  <= SP$ic.z.limit.sup[[i]]){
	  cv$n.ic.z.inc.mu1<-cv$n.ic.z.inc.mu1+1
	}
 	if(v$mx1 < SP$ic.z.limit.inf[[i]]){
	  cv$n.ic.z.r.ninc.mu1<-cv$n.ic.z.r.ninc.mu1+1
	}
	
	## t vs µ0
	if(SP$ic.t.limit.sup[[i]] < v$mx0){
	  cv$n.ic.t.l.ninc.mu0<-cv$n.ic.t.l.ninc.mu0+1
	}
	if(SP$ic.t.limit.inf[[i]] <= v$mx0 && v$mx0  <= SP$ic.t.limit.sup[[i]]){
	  cv$n.ic.t.inc.mu0<-cv$n.ic.t.inc.mu0+1
	}
 	if(v$mx0 < SP$ic.t.limit.inf[[i]]){
	  cv$n.ic.t.r.ninc.mu0<-cv$n.ic.t.r.ninc.mu0+1
	}
	
	## t vs µ1
	if(SP$ic.t.limit.sup[[i]] < v$mx1){
	  cv$n.ic.t.l.ninc.mu1<-cv$n.ic.t.l.ninc.mu1+1
	}
	if(SP$ic.t.limit.inf[[i]] <= v$mx1 && v$mx1  <= SP$ic.t.limit.sup[[i]]){
	  cv$n.ic.t.inc.mu1<-cv$n.ic.t.inc.mu1+1
	}
 	if(v$mx1 < SP$ic.t.limit.inf[[i]]){
	  cv$n.ic.t.r.ninc.mu1<-cv$n.ic.t.r.ninc.mu1+1
	}	
	
	cv$pc.ic.k.l.ninc.mu0<-round(cv$n.ic.k.l.ninc.mu0/i,3)*100
	cv$pc.ic.k.inc.mu0<-round(cv$n.ic.k.inc.mu0/i,3)*100
	cv$vect.pc.ic.k.inc.mu0<-c(cv$vect.pc.ic.k.inc.mu0,cv$pc.ic.k.inc.mu0)
	cv$pc.ic.k.r.ninc.mu0<-round(cv$n.ic.k.r.ninc.mu0/i,3)*100
	
	cv$pc.ic.z.l.ninc.mu0<-round(cv$n.ic.z.l.ninc.mu0/i,3)*100
	cv$pc.ic.z.inc.mu0<-round(cv$n.ic.z.inc.mu0/i,3)*100
	cv$vect.pc.ic.z.inc.mu0<-c(cv$vect.pc.ic.z.inc.mu0,cv$pc.ic.z.inc.mu0)
	cv$pc.ic.z.r.ninc.mu0<-round(cv$n.ic.z.r.ninc.mu0/i,3)*100
	
	cv$pc.ic.t.l.ninc.mu0<-round(cv$n.ic.t.l.ninc.mu0/i,3)*100
	cv$pc.ic.t.inc.mu0<-round(cv$n.ic.t.inc.mu0/i,3)*100
	cv$vect.pc.ic.t.inc.mu0<-c(cv$vect.pc.ic.t.inc.mu0,cv$pc.ic.t.inc.mu0)
	cv$pc.ic.t.r.ninc.mu0<-round(cv$n.ic.t.r.ninc.mu0/i,3)*100
	
	cv$pc.ic.k.l.ninc.mu1<-round(cv$n.ic.k.l.ninc.mu1/i,3)*100
	cv$pc.ic.k.inc.mu1<-round(cv$n.ic.k.inc.mu1/i,3)*100
	cv$vect.pc.ic.k.inc.mu1<-c(cv$vect.pc.ic.k.inc.mu1,cv$pc.ic.k.inc.mu1)
	cv$pc.ic.k.r.ninc.mu1<-round(cv$n.ic.k.r.ninc.mu1/i,3)*100
	
	cv$pc.ic.z.l.ninc.mu1<-round(cv$n.ic.z.l.ninc.mu1/i,3)*100
	cv$pc.ic.z.inc.mu1<-round(cv$n.ic.z.inc.mu1/i,3)*100
	cv$vect.pc.ic.z.inc.mu1<-c(cv$vect.pc.ic.z.inc.mu1,cv$pc.ic.z.inc.mu1)
	cv$pc.ic.z.r.ninc.mu1<-round(cv$n.ic.z.r.ninc.mu1/i,3)*100
	
	cv$pc.ic.t.l.ninc.mu1<-round(cv$n.ic.t.l.ninc.mu1/i,3)*100
	cv$pc.ic.t.inc.mu1<-round(cv$n.ic.t.inc.mu1/i,3)*100
	cv$vect.pc.ic.t.inc.mu1<-c(cv$vect.pc.ic.t.inc.mu1,cv$pc.ic.t.inc.mu1)
	cv$pc.ic.t.r.ninc.mu1<-round(cv$n.ic.t.r.ninc.mu1/i,3)*100

	
      }

    } 
    SP$last.takesample.value<<-v$takesample
    return(cv)
  })
    
  output$plotEmp <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    m<-matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE)
    layout(m,width=c(5,2,3))
    ##################
    ## Plot Reality ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    label<-""
    if(v$showreality){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20),main=bquote(paste("Prélèvement d'échantillons, et comparaison de l'IC pour µ avec ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	points(cv$samples.x[[i]],cv$samples.y[[i]])
      }
    }
    text(1,signif(cv$maxdmx,1)*0.95,labels="Echantillons",cex=1.4, pos=4)
    if(v$showreality){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* sigma^2 ) ," ", N *"~"* (.(cv$mx)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }
    if(v$showmur){
      lines(x<-c(cv$mx,cv$mx),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
      text(cv$mx,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    }
    ## empty plot for layout
    par(mai=c(0.5,0.4,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l',main=bquote(paste("Calcul du % de recouvrement de ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    text(0,0.9,labels=bquote(paste("Nombre total d'échantillons : ",.(cv$n.samples),sep="")),cex=1.4,pos=4)
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.k.inc.mu1,cv$pc.ic.k.inc.mu1),c(" "," "),c(cv$n.ic.k.l.ninc.mu1+cv$n.ic.k.r.ninc.mu1,cv$pc.ic.k.l.ninc.mu1+cv$pc.ic.k.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.4,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[1]," vs IC")),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.k.inc.mu0,cv$pc.ic.k.inc.mu0),c(" "," "),c(cv$n.ic.k.l.ninc.mu0+cv$n.ic.k.r.ninc.mu0,cv$pc.ic.k.l.ninc.mu0+cv$pc.ic.k.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.4,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.k.l.ninc.mu1,cv$pc.ic.k.l.ninc.mu1),c(" "," "),c(cv$n.ic.k.inc.mu1,cv$pc.ic.k.inc.mu1),c(" "," "),c(cv$n.ic.k.r.ninc.mu1,cv$pc.ic.k.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.4,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[1]," vs IC")),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.k.l.ninc.mu0,cv$pc.ic.k.l.ninc.mu0),c(" "," "),c(cv$n.ic.k.inc.mu0,cv$pc.ic.k.inc.mu0),c(" "," "),c(cv$n.ic.k.r.ninc.mu0,cv$pc.ic.k.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.4,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    }
    ## empty plot for layout
    par(mai=c(0.5,0.5,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    if(v$evolpcincmu){
      title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
    }

    ##################
    ## Plot H1     ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[1]),cex=1.4, pos=4)
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu[1]),cex=1.2)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	polygon(c(cv$ic.k.limit.inf[[i]],cv$ic.k.limit.inf[[i]],cv$ic.k.limit.sup[[i]],cv$ic.k.limit.sup[[i]]),c(cv$samples.y[[i]][1]-0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]-0.0025),col=cv$samples.ic.k.mu1.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m[[i]],cv$samples.y[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.k.limit.inf[[i]],cv$samples.y[[i]][1],labels="[",cex=1,col=cv$samples.ic.k.mu1.color[[i]])#col=cv$ic.z.color
	text(cv$ic.k.limit.sup[[i]],cv$samples.y[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.k.mu1.color[[i]]
	lines(x<-c(cv$ic.k.limit.inf[[i]],cv$samples.x.m[[i]]-1),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.k.mu1.color[[i]])
	lines(x<-c(cv$samples.x.m[[i]]+1,cv$ic.k.limit.sup[[i]]),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.k.mu1.color[[i]])
      }
    }
    
    ## Plot bar plot of includes %
    if(v$pcbp2c){
      ## Plot bar plot of includes 2 class %
      if(length(cv$samples.x)>0){
	includes<-c("µ1 ⊂ IC"=cv$pc.ic.k.inc.mu1,"µ1 ⊄ IC"=(100-cv$pc.ic.k.inc.mu1))
      } else {
	includes<-c("µ1 ⊂ IC"=0,"µ1 ⊄ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
      ## Plot bar plot of includes 3 class %
      if(length(cv$samples.x)>0){
	includes<-c("µ1 ⊄ IC G"=cv$pc.ic.k.l.ninc.mu1,"µ1 ⊂ IC"=cv$pc.ic.k.inc.mu1,"µ1 ⊄ IC D"=cv$pc.ic.k.r.ninc.mu1)
      } else {
	includes<-c("µ1 ⊄ IC G"=0,"µ1 ⊂ IC"=0,"µ1 ⊄ IC D"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    }

    if(v$evolpcincmu){
      if(length(cv$vect.n.samples)){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.7,0,0.5))
      plot(cv$vect.n.samples,cv$vect.pc.ic.k.inc.mu1,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ⊂ ",mu[1],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }

    ##################
    ## Plot H0      ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[0]),cex=1.4, pos=4)
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	polygon(c(cv$ic.k.limit.inf[[i]],cv$ic.k.limit.inf[[i]],cv$ic.k.limit.sup[[i]],cv$ic.k.limit.sup[[i]]),c(cv$samples.y[[i]][1]-0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]-0.0025),col=cv$samples.ic.k.mu0.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m[[i]],cv$samples.y[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.k.limit.inf[[i]],cv$samples.y[[i]][1],labels="[",cex=1,col=cv$samples.ic.k.mu0.color[[i]])#col=cv$ic.z.color
	text(cv$ic.k.limit.sup[[i]],cv$samples.y[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.k.mu0.color[[i]]
	lines(x<-c(cv$ic.k.limit.inf[[i]],cv$samples.x.m[[i]]-1),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.k.mu0.color[[i]])
	lines(x<-c(cv$samples.x.m[[i]]+1,cv$ic.k.limit.sup[[i]]),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.k.mu0.color[[i]])
      }
    }
    
    if(v$pcbp2c){
      ## Plot bar plot of includes %
      if(length(cv$samples.z)){
	includes<-c("µ0 ⊂ IC"=cv$pc.ic.k.inc.mu0,"µ0 ⊄ IC"=(100-cv$pc.ic.k.inc.mu0))
      } else {
	includes<-c("µ0 ⊂ IC"=0,"µ0 ⊄ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
       ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("µ0 ⊄ IC G"=cv$pc.ic.k.l.ninc.mu0,"µ0 ⊂ IC"=cv$pc.ic.k.inc.mu0,"µ0 ⊄ IC D"=cv$pc.ic.k.r.ninc.mu0)
      } else {
	includes<-c("µ0 ⊄ IC G"=0,"µ0 ⊂ IC"=0,"µ0 ⊄ IC D"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    }
    if(v$evolpcincmu){
      if(length(cv$vect.n.samples)>0){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.7,0,0.5))
      plot(cv$vect.n.samples,cv$vect.pc.ic.k.inc.mu0,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ⊂ ",mu[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }

    }, height = 600)

########################################################################################
  output$plotZ <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    m<-matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE)
    layout(m,width=c(5,2,3))
    ##################
    ## Plot Reality ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    label<-""
    if(v$showreality){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20),main=bquote(paste("Prélèvement d'échantillons, et comparaison de l'IC pour µ avec ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	points(cv$samples.x[[i]],cv$samples.y[[i]])
      }
    }
    text(1,signif(cv$maxdmx,1)*0.95,labels="Echantillons",cex=1.4, pos=4)
    if(v$showreality){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* sigma^2 ) ," ", N *"~"* (.(cv$mx)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }
    if(v$showmur){
      lines(x<-c(cv$mx,cv$mx),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
      text(cv$mx,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    }
    ## empty plot for layout
    par(mai=c(0.5,0.4,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l',main=bquote(paste("Calcul du % de recouvrement de ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    text(0,0.9,labels=bquote(paste("Nombre total d'échantillons : ",.(cv$n.samples),sep="")),cex=1.4,pos=4)
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.z.inc.mu1,cv$pc.ic.z.inc.mu1),c(" "," "),c(cv$n.ic.z.l.ninc.mu1+cv$n.ic.z.r.ninc.mu1,cv$pc.ic.z.l.ninc.mu1+cv$pc.ic.z.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.4,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[1]," vs IC")),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.z.inc.mu0,cv$pc.ic.z.inc.mu0),c(" "," "),c(cv$n.ic.z.l.ninc.mu0+cv$n.ic.z.r.ninc.mu0,cv$pc.ic.z.l.ninc.mu0+cv$pc.ic.z.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.4,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.z.l.ninc.mu1,cv$pc.ic.z.l.ninc.mu1),c(" "," "),c(cv$n.ic.z.inc.mu1,cv$pc.ic.z.inc.mu1),c(" "," "),c(cv$n.ic.z.r.ninc.mu1,cv$pc.ic.z.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.4,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[1]," vs IC")),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.z.l.ninc.mu0,cv$pc.ic.z.l.ninc.mu0),c(" "," "),c(cv$n.ic.z.inc.mu0,cv$pc.ic.z.inc.mu0),c(" "," "),c(cv$n.ic.z.r.ninc.mu0,cv$pc.ic.z.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.4,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    }
    ## empty plot for layout
    par(mai=c(0.5,0.5,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    if(v$evolpcincmu){
      title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
    }
  
    ##################
    ## Plot H1     ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[1]),cex=1.4, pos=4)
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu[1]),cex=1.2)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	polygon(c(cv$ic.z.limit.inf[[i]],cv$ic.z.limit.inf[[i]],cv$ic.z.limit.sup[[i]],cv$ic.z.limit.sup[[i]]),c(cv$samples.y[[i]][1]-0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]-0.0025),col=cv$samples.ic.z.mu1.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m[[i]],cv$samples.y[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.z.limit.inf[[i]],cv$samples.y[[i]][1],labels="[",cex=1,col=cv$samples.ic.z.mu1.color[[i]])#col=cv$ic.z.color
	text(cv$ic.z.limit.sup[[i]],cv$samples.y[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.z.mu1.color[[i]]
	lines(x<-c(cv$ic.z.limit.inf[[i]],cv$samples.x.m[[i]]-1),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.z.mu1.color[[i]])
	lines(x<-c(cv$samples.x.m[[i]]+1,cv$ic.z.limit.sup[[i]]),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.z.mu1.color[[i]])
      }
    }
    
    ## Plot bar plot of includes %
    if(v$pcbp2c){
      ## Plot bar plot of includes 2 class %
      if(length(cv$samples.x)>0){
	includes<-c("µ1 ⊂ IC"=cv$pc.ic.z.inc.mu1,"µ1 ⊄ IC"=(100-cv$pc.ic.z.inc.mu1))
      } else {
	includes<-c("µ1 ⊂ IC"=0,"µ1 ⊄ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
      ## Plot bar plot of includes 3 class %
      if(length(cv$samples.x)>0){
	includes<-c("µ1 ⊄ IC G"=cv$pc.ic.z.l.ninc.mu1,"µ1 ⊂ IC"=cv$pc.ic.z.inc.mu1,"µ1 ⊄ IC D"=cv$pc.ic.z.r.ninc.mu1)
      } else {
	includes<-c("µ1 ⊄ IC G"=0,"µ1 ⊂ IC"=0,"µ1 ⊄ IC D"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    }

    if(v$evolpcincmu){
      if(length(cv$vect.n.samples)){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.7,0,0.5))
      plot(cv$vect.n.samples,cv$vect.pc.ic.z.inc.mu1,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ⊂ ",mu[1],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      lines(x<-c(0,npclim),y <- c(v$confidence*100,v$confidence*100),lty=3)
      text(npclim*0.01,v$confidence*95,expression(1-alpha),pos=4)
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }

    ##################
    ## Plot H0      ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[0]),cex=1.4, pos=4)
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	polygon(c(cv$ic.z.limit.inf[[i]],cv$ic.z.limit.inf[[i]],cv$ic.z.limit.sup[[i]],cv$ic.z.limit.sup[[i]]),c(cv$samples.y[[i]][1]-0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]-0.0025),col=cv$samples.ic.z.mu0.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m[[i]],cv$samples.y[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.z.limit.inf[[i]],cv$samples.y[[i]][1],labels="[",cex=1,col=cv$samples.ic.z.mu0.color[[i]])#col=cv$ic.z.color
	text(cv$ic.z.limit.sup[[i]],cv$samples.y[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.z.mu0.color[[i]]
	lines(x<-c(cv$ic.z.limit.inf[[i]],cv$samples.x.m[[i]]-1),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.z.mu0.color[[i]])
	lines(x<-c(cv$samples.x.m[[i]]+1,cv$ic.z.limit.sup[[i]]),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.z.mu0.color[[i]])
      }
    }
    
    if(v$pcbp2c){
      ## Plot bar plot of includes %
      if(length(cv$samples.z)){
	includes<-c("µ0 ⊂ IC"=cv$pc.ic.z.inc.mu0,"µ0 ⊄ IC"=(100-cv$pc.ic.z.inc.mu0))
      } else {
	includes<-c("µ0 ⊂ IC"=0,"µ0 ⊄ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
       ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("µ0 ⊄ IC G"=cv$pc.ic.z.l.ninc.mu0,"µ0 ⊂ IC"=cv$pc.ic.z.inc.mu0,"µ0 ⊄ IC D"=cv$pc.ic.z.r.ninc.mu0)
      } else {
	includes<-c("µ0 ⊄ IC G"=0,"µ0 ⊂ IC"=0,"µ0 ⊄ IC D"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    }
    if(v$evolpcincmu){
      if(length(cv$vect.n.samples)>0){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.7,0,0.5))
      plot(cv$vect.n.samples,cv$vect.pc.ic.z.inc.mu0,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ⊂ ",mu[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
  
    }, height = 600)
########################################################################################
  output$plotT <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    m<-matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE)
    layout(m,width=c(5,2,3))
    ##################
    ## Plot Reality ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    label<-""
    if(v$showreality){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20),main=bquote(paste("Prélèvement d'échantillons, et comparaison de l'IC pour µ avec ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	points(cv$samples.x[[i]],cv$samples.y[[i]])
      }
    }
    text(1,signif(cv$maxdmx,1)*0.95,labels="Echantillons",cex=1.4, pos=4)
    if(v$showreality){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* sigma^2 ) ," ", N *"~"* (.(cv$mx)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }
    if(v$showmur){
      lines(x<-c(cv$mx,cv$mx),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
      text(cv$mx,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    }
    ## empty plot for layout
    par(mai=c(0.5,0.4,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l',main=bquote(paste("Calcul du % de recouvrement de ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    text(0,0.9,labels=bquote(paste("Nombre total d'échantillons : ",.(cv$n.samples),sep="")),cex=1.4,pos=4)
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.t.inc.mu1,cv$pc.ic.t.inc.mu1),c(" "," "),c(cv$n.ic.t.l.ninc.mu1+cv$n.ic.t.r.ninc.mu1,cv$pc.ic.t.l.ninc.mu1+cv$pc.ic.t.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.4,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[1]," vs IC")),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.t.inc.mu0,cv$pc.ic.t.inc.mu0),c(" "," "),c(cv$n.ic.t.l.ninc.mu0+cv$n.ic.t.r.ninc.mu0,cv$pc.ic.t.l.ninc.mu0+cv$pc.ic.t.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.4,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.t.l.ninc.mu1,cv$pc.ic.t.l.ninc.mu1),c(" "," "),c(cv$n.ic.t.inc.mu1,cv$pc.ic.t.inc.mu1),c(" "," "),c(cv$n.ic.t.r.ninc.mu1,cv$pc.ic.t.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.4,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[1]," vs IC")),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.t.l.ninc.mu0,cv$pc.ic.t.l.ninc.mu0),c(" "," "),c(cv$n.ic.t.inc.mu0,cv$pc.ic.t.inc.mu0),c(" "," "),c(cv$n.ic.t.r.ninc.mu0,cv$pc.ic.t.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.4,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    }
    ## empty plot for layout
    par(mai=c(0.5,0.5,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    if(v$evolpcincmu){
      title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
    }
   
    ##################
    ## Plot H1     ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[1]),cex=1.4, pos=4)
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu[1]),cex=1.2)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	polygon(c(cv$ic.t.limit.inf[[i]],cv$ic.t.limit.inf[[i]],cv$ic.t.limit.sup[[i]],cv$ic.t.limit.sup[[i]]),c(cv$samples.y[[i]][1]-0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]-0.0025),col=cv$samples.ic.t.mu1.color[[i]])#,density=cv$ic.t.density
	text(cv$samples.x.m[[i]],cv$samples.y[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.t.limit.inf[[i]],cv$samples.y[[i]][1],labels="[",cex=1,col=cv$samples.ic.t.mu1.color[[i]])#col=cv$ic.t.color
	text(cv$ic.t.limit.sup[[i]],cv$samples.y[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.t.mu1.color[[i]]
	lines(x<-c(cv$ic.t.limit.inf[[i]],cv$samples.x.m[[i]]-1),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.t.mu1.color[[i]])
	lines(x<-c(cv$samples.x.m[[i]]+1,cv$ic.t.limit.sup[[i]]),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.t.mu1.color[[i]])
      }
    }
    
    ## Plot bar plot of includes %
    if(v$pcbp2c){
      ## Plot bar plot of includes 2 class %
      if(length(cv$samples.x)>0){
	includes<-c("µ1 ⊂ IC"=cv$pc.ic.t.inc.mu1,"µ1 ⊄ IC"=(100-cv$pc.ic.t.inc.mu1))
      } else {
	includes<-c("µ1 ⊂ IC"=0,"µ1 ⊄ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
      ## Plot bar plot of includes 3 class %
      if(length(cv$samples.x)>0){
	includes<-c("µ1 ⊄ IC G"=cv$pc.ic.t.l.ninc.mu1,"µ1 ⊂ IC"=cv$pc.ic.t.inc.mu1,"µ1 ⊄ IC D"=cv$pc.ic.t.r.ninc.mu1)
      } else {
	includes<-c("µ1 ⊄ IC G"=0,"µ1 ⊂ IC"=0,"µ1 ⊄ IC D"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    }

    if(v$evolpcincmu){
      if(length(cv$vect.n.samples)){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.7,0,0.5))
      plot(cv$vect.n.samples,cv$vect.pc.ic.t.inc.mu1,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ⊂ ",mu[1],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      lines(x<-c(0,npclim),y <- c(v$confidence*100,v$confidence*100),lty=3)
      text(npclim*0.01,v$confidence*95,expression(1-alpha),pos=4)
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
    
    ##################
    ## Plot H0      ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[0]),cex=1.4, pos=4)
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    if(length(cv$samples.x)>0){
      for(i in 1:length(cv$samples.z)){
	polygon(c(cv$ic.t.limit.inf[[i]],cv$ic.t.limit.inf[[i]],cv$ic.t.limit.sup[[i]],cv$ic.t.limit.sup[[i]]),c(cv$samples.y[[i]][1]-0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]+0.0025,cv$samples.y[[i]][1]-0.0025),col=cv$samples.ic.t.mu0.color[[i]])#,density=cv$ic.t.density
	text(cv$samples.x.m[[i]],cv$samples.y[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.t.limit.inf[[i]],cv$samples.y[[i]][1],labels="[",cex=1,col=cv$samples.ic.t.mu0.color[[i]])#col=cv$ic.t.color
	text(cv$ic.t.limit.sup[[i]],cv$samples.y[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.t.mu0.color[[i]]
	lines(x<-c(cv$ic.t.limit.inf[[i]],cv$samples.x.m[[i]]-1),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.t.mu0.color[[i]])
	lines(x<-c(cv$samples.x.m[[i]]+1,cv$ic.t.limit.sup[[i]]),y <- c(cv$samples.y[[i]][1],cv$samples.y[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.t.mu0.color[[i]])
      }
    }
    
    if(v$pcbp2c){
      ## Plot bar plot of includes %
      if(length(cv$samples.z)){
	includes<-c("µ0 ⊂ IC"=cv$pc.ic.t.inc.mu0,"µ0 ⊄ IC"=(100-cv$pc.ic.t.inc.mu0))
      } else {
	includes<-c("µ0 ⊂ IC"=0,"µ0 ⊄ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
       ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("µ0 ⊄ IC G"=cv$pc.ic.t.l.ninc.mu0,"µ0 ⊂ IC"=cv$pc.ic.t.inc.mu0,"µ0 ⊄ IC D"=cv$pc.ic.t.r.ninc.mu0)
      } else {
	includes<-c("µ0 ⊄ IC G"=0,"µ0 ⊂ IC"=0,"µ0 ⊄ IC D"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    }
    if(v$evolpcincmu){
      if(length(cv$vect.n.samples)>0){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.7,0,0.5))
      plot(cv$vect.n.samples,cv$vect.pc.ic.t.inc.mu0,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ⊂ ",mu[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
 
    }, height = 600)
###################################################################
  output$test1 <- renderText({
    v<-getInputValues()
    cv<-getComputedValues()
    paste("Tab",input$Tabset,"n inc µ0 :",cv$n.ic.k.inc.mu0," | N :",cv$n.samples," | takesample : ",input$takesample,SP$last.takesample.value," | Last action : ",rv$lastAction," | Sample.exist :",cv$samples.exist,sep=" ")
  })
  
  output$test2 <- renderText({
    paste("Tab",input$Tabset,sep=" ")
  })
  
  output$test3 <- renderText({
    paste("Tab",input$Tabset,sep=" ")
  })
})
