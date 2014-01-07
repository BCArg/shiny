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
library(xtable)

color.true<-rgb(0,0.7,0,0.5)
color.false<-rgb(1,0,0,0.5)
density.true<-10
density.false<-25

cex.hypoth<-1.7#size of hypothesis descriptions
hypoth.text.levels<-c(1,0.7,0.4,0.1)

x.lim.min<-20
x.lim.max<-60
x.amp<-x.lim.max-x.lim.min

full.plot.width<-1000

shinyServer(function(input, output) {
  
  rv <- reactiveValues()# Create a reactiveValues object, to let us use settable reactive values
  
  rv$last.takesample.value<-0
  rv$samples.z<-list()
  
  rv$lastAction <- 'none' # To start out, lastAction == NULL, meaning nothing clicked yet
  # An observe block for each button, to record that the action happened
  observe({
    if (input$takesample != 0) {
      rv$lastAction <- 'takesample'
    }
  })
  observe({
    if (input$reset != 0) {
      rv$lastAction <- 'reset'
      rv$last.takesample.value<-0
      rv$samples.z<-list()
    }
  })
  
  getSamples<-reactive({#créee n valeurs aléatoires N(0;1) quand input$takesample est implémenté (quand le bouton takesample est pressé)
    if(input$takesample > rv$last.takesample.value && rv$lastAction == "takesample"){
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

  getPlotHeight <- function() {
    unit.height<-220
    if(input$showR && input$showh0 && input$showh1){
      return(3*unit.height)
    } 
    if((input$showR && input$showh0 && !input$showh1) || (input$showR && !input$showh0 && input$showh1) || (!input$showR && input$showh0 && input$showh1)){
      return(2*unit.height)
    }
    if((!input$showR && !input$showh0 && input$showh1) || (!input$showR && input$showh0 && !input$showh1) || (input$showR && !input$showh0 && !input$showh1)){
      return(1*unit.height)
    }
    if(!input$showR && !input$showh0 && !input$showh1){
      return(1*unit.height)
    }
  }
    
  getInputValues<-reactive({
    return(input)#collect all inputs
  })
  
  getComputedValues<-reactive({
    samples<-list()
    samples<-getSamples()
    rv$samples.z<-c(rv$samples.z,samples)
  
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
    
    ## Define reality parameters
    cv$vx<-v$sx^2#compute variance of Reality distribution
    
    ## Computation of x y coordinates for Normal curve of Reality
    z<-seq(-5,5,length=100)
    cv$xr<-(z*v$sx)+v$mx1 #x for Reality
    cv$yr<-dnorm(cv$xr,mean=v$mx1,sd=v$sx)#y for Reality
    
     ## Computation of alpha, beta, confidence and power related variables  ##
    cv$alpha<-round(1-v$confidence,3)#Computation of alpha probability
    
    ## Set z and t statistics for confidence intervals
    cv$ic.z<-qnorm(1-cv$alpha/2)#z positive limit of a bidirectionnal confidence interval in N(0,1) => for CI with known variance
    cv$ic.t<-qt(1-cv$alpha/2,v$n-1)#t positive limit of a bidirectionnal confidence interval in t(n-1) => for CI with unknown variance

    ## Computation of parameters of distribution of mean of samples 
    cv$sx.dech<-v$sx/sqrt(v$n)#standard deviation of mean samples distributions
    cv$vx.dech<-cv$sx.dech^2
    
    ## Computation of sample related values ##
#     cv$samples.z<-list()
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
    
    cv$n.samples<-length(rv$samples.z)
#     cv$samples.exist<-length(cv$samples.z)#mesure length of sample values to test if a sample has been created
    cv$vect.n.samples<-c()
    cv$samples.x.n.toshow<-0
    
    if(cv$n.samples>0){
      cv$vect.n.samples<-c(1:cv$n.samples)
      for(i in 1:cv$n.samples){
	cv$samples.x[[i]]<-round((rv$samples.z[[i]]*v$sx)+v$mx1,2)#Then sample values are compute with H1 mean and standard deviation
	y<-c()
	for(j in 1:v$n){
	  y<-c(y,(0.05/(v$ns+1))*i)
	}
	cv$samples.y[[i]]<-y
	cv$samples.x.m[[i]]<-round(mean(cv$samples.x[[i]]),2)#means of samples
	cv$samples.x.sd[[i]]<-round(sd(cv$samples.x[[i]]),2)#means of samples
	
	## Computation of confidence intervals for the mean µ ##    
	cv$ic.k.limit.inf[[i]]<-round(cv$samples.x.m[[i]]-v$k,2)#compute the CI lower limit with empiric k value
	cv$ic.k.limit.sup[[i]]<-round(cv$samples.x.m[[i]]+v$k,2)#compute the CI higher limit with empiric k value
	cv$ic.z.limit.inf[[i]]<-round(cv$samples.x.m[[i]]-cv$ic.z*cv$sx.dech,2)#compute the CI lower limit when variance known
	cv$ic.z.limit.sup[[i]]<-round(cv$samples.x.m[[i]]+cv$ic.z*cv$sx.dech,2)#compute the CI higher limit when variance known
	cv$ic.t.limit.inf[[i]]<-round(cv$samples.x.m[[i]]-cv$ic.t*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the CI lower limit when variance unknown
	cv$ic.t.limit.sup[[i]]<-round(cv$samples.x.m[[i]]+cv$ic.t*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the CI higher limit when variance unknown
      }
      
      for(i in 1:cv$n.samples){
	## Testing if IC covers µ0 or µ1
	## K vs µ0
	if(cv$ic.k.limit.sup[[i]] < v$mx0){
	  cv$n.ic.k.l.ninc.mu0<-cv$n.ic.k.l.ninc.mu0+1
	}
	if(cv$ic.k.limit.inf[[i]] <= v$mx0 && v$mx0  <= cv$ic.k.limit.sup[[i]]){
	  cv$n.ic.k.inc.mu0<-cv$n.ic.k.inc.mu0+1
	}
 	if(v$mx0 < cv$ic.k.limit.inf[[i]]){
	  cv$n.ic.k.r.ninc.mu0<-cv$n.ic.k.r.ninc.mu0+1
	}
	
	## K vs µ1
	if(cv$ic.k.limit.sup[[i]] < v$mx1){
	  cv$n.ic.k.l.ninc.mu1<-cv$n.ic.k.l.ninc.mu1+1
	}
	if(cv$ic.k.limit.inf[[i]] <= v$mx1 && v$mx1  <= cv$ic.k.limit.sup[[i]]){
	  cv$n.ic.k.inc.mu1<-cv$n.ic.k.inc.mu1+1
	}
 	if(v$mx1 < cv$ic.k.limit.inf[[i]]){
	  cv$n.ic.k.r.ninc.mu1<-cv$n.ic.k.r.ninc.mu1+1
	}
	
	## Z vs µ0
	if(cv$ic.z.limit.sup[[i]] < v$mx0){
	  cv$n.ic.z.l.ninc.mu0<-cv$n.ic.z.l.ninc.mu0+1
	}
	if(cv$ic.z.limit.inf[[i]] <= v$mx0 && v$mx0  <= cv$ic.z.limit.sup[[i]]){
	  cv$n.ic.z.inc.mu0<-cv$n.ic.z.inc.mu0+1
	}
 	if(v$mx0 < cv$ic.z.limit.inf[[i]]){
	  cv$n.ic.z.r.ninc.mu0<-cv$n.ic.z.r.ninc.mu0+1
	}
	
	## Z vs µ1
	if(cv$ic.z.limit.sup[[i]] < v$mx1){
	  cv$n.ic.z.l.ninc.mu1<-cv$n.ic.z.l.ninc.mu1+1
	}
	if(cv$ic.z.limit.inf[[i]] <= v$mx1 && v$mx1  <= cv$ic.z.limit.sup[[i]]){
	  cv$n.ic.z.inc.mu1<-cv$n.ic.z.inc.mu1+1
	}
 	if(v$mx1 < cv$ic.z.limit.inf[[i]]){
	  cv$n.ic.z.r.ninc.mu1<-cv$n.ic.z.r.ninc.mu1+1
	}
	
	## t vs µ0
	if(cv$ic.t.limit.sup[[i]] < v$mx0){
	  cv$n.ic.t.l.ninc.mu0<-cv$n.ic.t.l.ninc.mu0+1
	}
	if(cv$ic.t.limit.inf[[i]] <= v$mx0 && v$mx0  <= cv$ic.t.limit.sup[[i]]){
	  cv$n.ic.t.inc.mu0<-cv$n.ic.t.inc.mu0+1
	}
 	if(v$mx0 < cv$ic.t.limit.inf[[i]]){
	  cv$n.ic.t.r.ninc.mu0<-cv$n.ic.t.r.ninc.mu0+1
	}
	
	## t vs µ1
	if(cv$ic.t.limit.sup[[i]] < v$mx1){
	  cv$n.ic.t.l.ninc.mu1<-cv$n.ic.t.l.ninc.mu1+1
	}
	if(cv$ic.t.limit.inf[[i]] <= v$mx1 && v$mx1  <= cv$ic.t.limit.sup[[i]]){
	  cv$n.ic.t.inc.mu1<-cv$n.ic.t.inc.mu1+1
	}
 	if(v$mx1 < cv$ic.t.limit.inf[[i]]){
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
    ## Choose values to show in plots
    cv$samples.x.toshow<-list()
    if(length(cv$samples.x)>0){
      cv$samples.x.from<-1
      if(length(cv$samples.x)>v$nss){
	cv$samples.x.from<-length(cv$samples.x)-v$nss+1
      }
      cv$samples.x.to<-length(cv$samples.x)
      cv$samples.x.toshow<-cv$samples.x[cv$samples.x.from:cv$samples.x.to]
      
      cv$samples.x.m.toshow<-cv$samples.x.m[cv$samples.x.from:cv$samples.x.to]
      cv$samples.x.sd.toshow<-cv$samples.x.sd[cv$samples.x.from:cv$samples.x.to]
      cv$samples.x.i.toshow<-c(cv$samples.x.from:cv$samples.x.to)
      
      cv$ic.k.limit.inf.toshow<-cv$ic.k.limit.inf[cv$samples.x.from:cv$samples.x.to]
      cv$ic.k.limit.sup.toshow<-cv$ic.k.limit.sup[cv$samples.x.from:cv$samples.x.to]
      
      cv$ic.z.limit.inf.toshow<-cv$ic.z.limit.inf[cv$samples.x.from:cv$samples.x.to]
      cv$ic.z.limit.sup.toshow<-cv$ic.z.limit.sup[cv$samples.x.from:cv$samples.x.to]
      
      cv$ic.t.limit.inf.toshow<-cv$ic.t.limit.inf[cv$samples.x.from:cv$samples.x.to]
      cv$ic.t.limit.sup.toshow<-cv$ic.t.limit.sup[cv$samples.x.from:cv$samples.x.to]
      
      cv$samples.y.toshow<-list()
      cv$samples.x.n.toshow<-length(cv$samples.x.toshow)
      if(cv$samples.x.n.toshow>0){
	for(i in 1:cv$samples.x.n.toshow){
	  cv$samples.y.toshow[[i]]<-list()
	  for(j in 1:length(cv$samples.x.toshow[[i]])){
	    cv$samples.y.toshow[[i]]<-c(cv$samples.y.toshow[[i]],c((0.05/(v$nss+1))*i))#
	  }
	  cv$samples.y.toshow[[i]]<-as.vector(cv$samples.y.toshow[[i]],mode='numeric')
	  
	  
	  ## Testing if IC covers µ0 or µ1
	  ## K vs µ0
	  cv$samples.ic.k.mu0.color[[i]]<-color.false
	  cv$samples.ic.k.mu0.density[[i]]<-density.false
	  if(cv$ic.k.limit.inf.toshow[[i]] <= v$mx0 && v$mx0  <= cv$ic.k.limit.sup.toshow[[i]]){
	    cv$samples.ic.k.mu0.color[[i]]<-color.true
	    cv$samples.ic.k.mu0.density[[i]]<-density.true
	  }
	  ## K vs µ1
	  cv$samples.ic.k.mu1.color[[i]]<-color.false
	  cv$samples.ic.k.mu1.density[[i]]<-density.false
	  if(cv$ic.k.limit.inf.toshow[[i]] <= v$mx1 && v$mx1  <= cv$ic.k.limit.sup.toshow[[i]]){
	    cv$samples.ic.k.mu1.color[[i]]<-color.true
	    cv$samples.ic.k.mu1.density[[i]]<-density.true
	  }
	  ## Z vs µ0
	  cv$samples.ic.z.mu0.color[[i]]<-color.false
	  cv$samples.ic.z.mu0.density[[i]]<-density.false
	  if(cv$ic.z.limit.inf.toshow[[i]] <= v$mx0 && v$mx0  <= cv$ic.z.limit.sup.toshow[[i]]){
	    cv$samples.ic.z.mu0.color[[i]]<-color.true
	    cv$samples.ic.z.mu0.density[[i]]<-density.true
	  }
	  ## Z vs µ1
	  cv$samples.ic.z.mu1.color[[i]]<-color.false
	  cv$samples.ic.z.mu1.density[[i]]<-density.false
	  if(cv$ic.z.limit.inf.toshow[[i]] <= v$mx1 && v$mx1  <= cv$ic.z.limit.sup.toshow[[i]]){
	    cv$samples.ic.z.mu1.color[[i]]<-color.true
	    cv$samples.ic.z.mu1.density[[i]]<-density.true
	  }
	  ## t vs µ0
	  cv$samples.ic.t.mu0.color[[i]]<-color.false
	  cv$samples.ic.t.mu0.density[[i]]<-density.false
	  if(cv$ic.t.limit.inf.toshow[[i]] <= v$mx0 && v$mx0  <= cv$ic.t.limit.sup.toshow[[i]]){
	    cv$samples.ic.t.mu0.color[[i]]<-color.true
	    cv$samples.ic.t.mu0.density[[i]]<-density.true
	  }
	  ## t vs µ1
	  cv$samples.ic.t.mu1.color[[i]]<-color.false
	  cv$samples.ic.t.mu1.density[[i]]<-density.false
	  if(cv$ic.t.limit.inf.toshow[[i]] <= v$mx1 && v$mx1  <= cv$ic.t.limit.sup.toshow[[i]]){
	    cv$samples.ic.t.mu1.color[[i]]<-color.true
	    cv$samples.ic.t.mu1.density[[i]]<-density.true
	  }
	
	}
      }
      

      
    }
    ## Last takesample value
    rv$last.takesample.value<-v$takesample
    return(cv)
  })
    
  output$plotEmp <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5,6,6,7,8),3,4,byrow=TRUE)
    }
    if(v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3,4,4,5,6),2,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2),1,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),3,4,byrow=TRUE)
    }
    layout(m,width=c(4,2,1,3))
    
if(v$showR){
    ##################
    ## Plot Reality ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showreality){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(x.lim.min,x.lim.max,20),main=bquote(paste("Prélèvement d'échantillons, et comparaison de l'IC pour µ avec ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	mtext(bquote(paste(bar(x)[.(cv$samples.x.i.toshow[[i]])] == .(cv$samples.x.m.toshow[[i]]),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s[.(cv$samples.x.i.toshow[[i]])] == .(cv$samples.x.sd.toshow[[i]]),sep="")),side=4,line=9,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(1,signif(cv$maxdmx,1)*0.95,labels="Echantillons",cex=1.4, pos=4)
    if(v$showreality){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* sigma^2 ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }

      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)

    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
    

    if(v$thresholds == "formula"){
      text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=cex.hypoth,pos=4)
      text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",mu[0] %notin% group("[",list(bar(x)-K,bar(x)+K),"]"),sep="")),cex=cex.hypoth,pos=4)
      text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",mu[0] %in% group("[",list(bar(x)-K,bar(x)+K),"]"),sep="")),cex=cex.hypoth,pos=4)
    } else {
      text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=cex.hypoth,pos=4)

      if(length(rv$samples.z)>0){
	if(v$thresholds == "calcul"){
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])-.(v$k),.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])+.(v$k)),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])-.(v$k),.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])+.(v$k)),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[4]],bquote(paste(bar(x) == .(cv$samples.x.m[[length(rv$samples.z)]])," est la moyenne du dernier échantillon obtenu.",sep="")),cex=cex.hypoth,pos=4)
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(.(cv$ic.k.limit.inf.toshow[[cv$samples.x.n.toshow]]),.(cv$ic.k.limit.sup.toshow[[cv$samples.x.n.toshow]])),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(.(cv$ic.k.limit.inf.toshow[[cv$samples.x.n.toshow]]),.(cv$ic.k.limit.sup.toshow[[cv$samples.x.n.toshow]])),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[4]],bquote(paste("Seuils critiques calculés avec ",bar(x) == .(cv$samples.x.m[[length(rv$samples.z)]]),sep="")),cex=cex.hypoth,pos=4)
	}
      } else {
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(bar(x)-.(v$k),bar(x)+.(v$k)),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(bar(x)-.(v$k),bar(x)+.(v$k)),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[4]],bquote(paste(bar(x)," sera remplacé par la moyenne du prochain échantillon.",sep="")),cex=cex.hypoth,pos=4)
      }
    }
}



if(v$showh0){
    ##################
    ## Plot H0      ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[0]),cex=1.4, pos=4)
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	polygon(c(cv$ic.k.limit.inf.toshow[[i]],cv$ic.k.limit.inf.toshow[[i]],cv$ic.k.limit.sup.toshow[[i]],cv$ic.k.limit.sup.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=cv$samples.ic.k.mu0.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.k.limit.inf.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="[",cex=1,col=cv$samples.ic.k.mu0.color[[i]])#col=cv$ic.z.color
	text(cv$ic.k.limit.sup.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.k.mu0.color[[i]]
	lines(x<-c(cv$ic.k.limit.inf.toshow[[i]],cv$samples.x.m.toshow[[i]]-1),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.k.mu0.color[[i]])
	lines(x<-c(cv$samples.x.m.toshow[[i]]+1,cv$ic.k.limit.sup.toshow[[i]]),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.k.mu0.color[[i]])
      }
    }
    mtext(bquote(paste(mu[0]," vs IC => conclusion",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
	  for(i in 1:cv$samples.x.n.toshow){
		if(v$mx0 < cv$ic.k.limit.inf.toshow[[i]] || v$mx0 > cv$ic.k.limit.sup.toshow[[i]]){
		    mtext(bquote(paste(.(v$mx0) %notin% group("[",list(.(cv$ic.k.limit.inf.toshow[[i]]),.(cv$ic.k.limit.sup.toshow[[i]])),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  	} else {
	    mtext(bquote(paste(.(v$mx0) %in% group("[",list(.(cv$ic.k.limit.inf.toshow[[i]]),.(cv$ic.k.limit.sup.toshow[[i]])),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
		}
	  }
    }
 
    if(v$pcbp2c){
      ## Plot bar plot of includes %
      if(length(rv$samples.z)){
	includes<-c("∈"=cv$pc.ic.k.inc.mu0,"µo ∉ IC"=(100-cv$pc.ic.k.inc.mu0))
      } else {
	includes<-c("∈"=0,"µo ∉ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
#       text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
       ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("∉"=cv$pc.ic.k.l.ninc.mu0,"∈"=cv$pc.ic.k.inc.mu0,"∉"=cv$pc.ic.k.r.ninc.mu0)
      } else {
	includes<-c("∉"=0,"∈"=0,"∉"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
#       text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    }
    
    if(v$pcbp2c){
      ICvsmu0<-data.frame(c(cv$n.ic.k.inc.mu0,cv$pc.ic.k.inc.mu0),c(" "," "),c(cv$n.ic.k.l.ninc.mu0+cv$n.ic.k.r.ninc.mu0,cv$pc.ic.k.l.ninc.mu0+cv$pc.ic.k.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ∈ ",""," ∉ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0,110,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu0<-data.frame(c(cv$n.ic.k.l.ninc.mu0,cv$pc.ic.k.l.ninc.mu0),c(" "," "),c(cv$n.ic.k.inc.mu0,cv$pc.ic.k.inc.mu0),c(" "," "),c(cv$n.ic.k.r.ninc.mu0,cv$pc.ic.k.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ∉ ",""," ∈ ",""," ∉ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(-0.75,110,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    }
    
    
    
    par(mai=c(0.5,0.7,0,0.5))
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
      plot(cv$vect.n.samples,cv$vect.pc.ic.k.inc.mu0,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ∈ ",mu[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
    } else {
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
}

if(v$showh1){
    ##################
    ## Plot H1     ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20),main=bquote(paste("IC pour µ calculés selon [ ",bar(x)-K,",",bar(x)+K,"]",sep="")),cex.main=1.5)
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[1]),cex=1.4, pos=4)
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	polygon(c(cv$ic.k.limit.inf.toshow[[i]],cv$ic.k.limit.inf.toshow[[i]],cv$ic.k.limit.sup.toshow[[i]],cv$ic.k.limit.sup.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=cv$samples.ic.k.mu1.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.k.limit.inf.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="[",cex=1,col=cv$samples.ic.k.mu1.color[[i]])#col=cv$ic.z.color
	text(cv$ic.k.limit.sup.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.k.mu1.color[[i]]
	lines(x<-c(cv$ic.k.limit.inf.toshow[[i]],cv$samples.x.m.toshow[[i]]-1),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.k.mu1.color[[i]])
	lines(x<-c(cv$samples.x.m.toshow[[i]]+1,cv$ic.k.limit.sup.toshow[[i]]),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.k.mu1.color[[i]])
      }
    } 
	
    mtext(bquote(paste(mu," vs IC",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
		if(v$mx1 < cv$ic.k.limit.inf.toshow[[i]] || v$mx1 > cv$ic.k.limit.sup.toshow[[i]]){
		  mtext(bquote(paste(.(v$mx1) %notin% group("[",list(.(cv$ic.k.limit.inf.toshow[[i]]),.(cv$ic.k.limit.sup.toshow[[i]])),"]"),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
		} else {
		  mtext(bquote(paste(.(v$mx1) %in% group("[",list(.(cv$ic.k.limit.inf.toshow[[i]]),.(cv$ic.k.limit.sup.toshow[[i]])),"]"),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
		}
      }
    }
    ## Plot bar plot of includes %
    if(v$pcbp2c){
      ## Plot bar plot of includes 2 class %
      if(length(cv$samples.x)>0){
	includes<-c("∈"=cv$pc.ic.k.inc.mu1,"∉"=(100-cv$pc.ic.k.inc.mu1))
      } else {
	includes<-c("∈"=0,"∉"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
#       text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
      ## Plot bar plot of includes 3 class %
      if(length(cv$samples.x)>0){
		 includes<-c("∉"=cv$pc.ic.k.l.ninc.mu1,"∈"=cv$pc.ic.k.inc.mu1,"∉"=cv$pc.ic.k.r.ninc.mu1)
      } else {
		includes<-c("∉"=0,"∈"=0,"∉"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
#       text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    }
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.k.inc.mu1,cv$pc.ic.k.inc.mu1),c(" "," "),c(cv$n.ic.k.l.ninc.mu1+cv$n.ic.k.r.ninc.mu1,cv$pc.ic.k.l.ninc.mu1+cv$pc.ic.k.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.k.l.ninc.mu1,cv$pc.ic.k.l.ninc.mu1),c(" "," "),c(cv$n.ic.k.inc.mu1,cv$pc.ic.k.inc.mu1),c(" "," "),c(cv$n.ic.k.r.ninc.mu1,cv$pc.ic.k.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ∉ ",""," ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(-0.75,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu," vs IC")),cex=1.4,xjust=0,yjust=1)
    }   
    
    par(mai=c(0.5,0.7,0,0.5))
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
      plot(cv$vect.n.samples,cv$vect.pc.ic.k.inc.mu1,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ∈ ",mu,sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
    } else {
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
  }

    }, height = getPlotHeight, width=full.plot.width)

########################################################################################
  output$plotZ <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5,6,6,7,8),3,4,byrow=TRUE)
    }
    if(v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3,4,4,5,6),2,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2),1,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),3,4,byrow=TRUE)
    }
    layout(m,width=c(4,2,1,3))
    
if(v$showR){
    ##################
    ## Plot Reality ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showreality){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(x.lim.min,x.lim.max,20),main=bquote(paste("Prélèvement d'échantillons, et comparaison de l'IC pour µ avec ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	mtext(bquote(paste(bar(x)[.(cv$samples.x.i.toshow[[i]])] == .(cv$samples.x.m.toshow[[i]]),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s[.(cv$samples.x.i.toshow[[i]])] == .(cv$samples.x.sd.toshow[[i]]),sep="")),side=4,line=9,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(1,signif(cv$maxdmx,1)*0.95,labels="Echantillons",cex=1.4, pos=4)
    if(v$showreality){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* sigma^2 ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }

      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)

    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
    

    if(v$thresholds == "formula"){
      text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=cex.hypoth,pos=4)
      text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",mu[0] %notin% group("[",list(bar(x)-Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n)),bar(x)+Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex=cex.hypoth,pos=4)
      text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",mu[0] %in% group("[",list(bar(x)-Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n)),bar(x)+Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex=cex.hypoth,pos=4)
    } else {
      text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
      if(length(rv$samples.z)>0){
	if(v$thresholds == "calcul"){
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[4]],bquote(paste(bar(x) == .(cv$samples.x.m[[length(rv$samples.z)]])," est la moyenne du dernier échantillon obtenu.",sep="")),cex=cex.hypoth,pos=4)
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(.(cv$ic.z.limit.inf.toshow[[cv$samples.x.n.toshow]]),.(cv$ic.z.limit.sup.toshow[[cv$samples.x.n.toshow]])),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(.(cv$ic.z.limit.inf.toshow[[cv$samples.x.n.toshow]]),.(cv$ic.z.limit.sup.toshow[[cv$samples.x.n.toshow]])),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[4]],bquote(paste("Seuils critiques calculés avec ",bar(x) == .(cv$samples.x.m[[length(rv$samples.z)]]),sep="")),cex=cex.hypoth,pos=4)
	}
      } else {
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(bar(x)-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),bar(x)+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(bar(x)-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),bar(x)+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[4]],bquote(paste(bar(x)," sera remplacé par la moyenne du prochain échantillon.",sep="")),cex=cex.hypoth,pos=4)
      }
    } 

#     ## empty plot for layout
#     par(mai=c(0.5,0.5,0.5,0))
#     plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
#     if(v$evolpcincmu){
#       title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
#     }
}


if(v$showh0){
    ##################
    ## Plot H0      ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[0]),cex=1.4, pos=4)
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	polygon(c(cv$ic.z.limit.inf.toshow[[i]],cv$ic.z.limit.inf.toshow[[i]],cv$ic.z.limit.sup.toshow[[i]],cv$ic.z.limit.sup.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=cv$samples.ic.z.mu0.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.z.limit.inf.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="[",cex=1,col=cv$samples.ic.z.mu0.color[[i]])#col=cv$ic.z.color
	text(cv$ic.z.limit.sup.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.z.mu0.color[[i]]
	lines(x<-c(cv$ic.z.limit.inf.toshow[[i]],cv$samples.x.m.toshow[[i]]-1),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.z.mu0.color[[i]])
	lines(x<-c(cv$samples.x.m.toshow[[i]]+1,cv$ic.z.limit.sup.toshow[[i]]),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.z.mu0.color[[i]])
      }
    }
	
	mtext(bquote(paste(mu[0]," vs IC => conclusion",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
	if(cv$samples.x.n.toshow>0){
		for(i in 1:cv$samples.x.n.toshow){
			if(v$mx0 < cv$ic.z.limit.inf.toshow[[i]] || v$mx0 > cv$ic.z.limit.sup.toshow[[i]]){
				mtext(bquote(paste(.(v$mx0) %notin% group("[",list(.(cv$ic.z.limit.inf.toshow[[i]]),.(cv$ic.z.limit.sup.toshow[[i]])),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
			} else {
				mtext(bquote(paste(.(v$mx0) %in% group("[",list(.(cv$ic.z.limit.inf.toshow[[i]]),.(cv$ic.z.limit.sup.toshow[[i]])),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
			}
		}
	}
	
    if(v$pcbp2c){
      ## Plot bar plot of includes %
      if(length(rv$samples.z)){
	includes<-c("∈"=cv$pc.ic.z.inc.mu0,"µo ∉ IC"=(100-cv$pc.ic.z.inc.mu0))
      } else {
	includes<-c("∈"=0,"µo ∉ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
       ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("∉"=cv$pc.ic.z.l.ninc.mu0,"∈"=cv$pc.ic.z.inc.mu0,"∉"=cv$pc.ic.z.r.ninc.mu0)
      } else {
	includes<-c("∉"=0,"∈"=0,"∉"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
    }
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.z.inc.mu0,cv$pc.ic.z.inc.mu0),c(" "," "),c(cv$n.ic.z.l.ninc.mu0+cv$n.ic.z.r.ninc.mu0,cv$pc.ic.z.l.ninc.mu0+cv$pc.ic.z.r.ninc.mu0))
      colnames(ICvsmu1)<-c(" ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.z.l.ninc.mu0,cv$pc.ic.z.l.ninc.mu0),c(" "," "),c(cv$n.ic.z.inc.mu0,cv$pc.ic.z.inc.mu0),c(" "," "),c(cv$n.ic.z.r.ninc.mu0,cv$pc.ic.z.r.ninc.mu0))
      colnames(ICvsmu1)<-c(" ∉ ",""," ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(-0.75,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    }
    
    par(mai=c(0.5,0.7,0,0.5))
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
      plot(cv$vect.n.samples,cv$vect.pc.ic.z.inc.mu0,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ∈ ",mu[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
    } else {
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
}  



if(v$showh1){
    ##################
    ## Plot H1     ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20),main=bquote(paste("IC pour µ calculés selon [ ",bar(x)-Z[1-alpha/2]*frac(sigma,sqrt(n)),",",bar(x)+Z[1-alpha/2]*frac(sigma,sqrt(n)),"]",sep="")),cex.main=1.5)
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[1]),cex=1.4, pos=4)
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	polygon(c(cv$ic.z.limit.inf.toshow[[i]],cv$ic.z.limit.inf.toshow[[i]],cv$ic.z.limit.sup.toshow[[i]],cv$ic.z.limit.sup.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=cv$samples.ic.z.mu1.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.z.limit.inf.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="[",cex=1,col=cv$samples.ic.z.mu1.color[[i]])#col=cv$ic.z.color
	text(cv$ic.z.limit.sup.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.z.mu1.color[[i]]
	lines(x<-c(cv$ic.z.limit.inf.toshow[[i]],cv$samples.x.m.toshow[[i]]-1),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.z.mu1.color[[i]])
	lines(x<-c(cv$samples.x.m.toshow[[i]]+1,cv$ic.z.limit.sup.toshow[[i]]),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.z.mu1.color[[i]])
      }
    }
	mtext(bquote(paste(mu," vs IC",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
	if(cv$samples.x.n.toshow>0){
		for(i in 1:cv$samples.x.n.toshow){
			if(v$mx1 < cv$ic.z.limit.inf.toshow[[i]] || v$mx1 > cv$ic.z.limit.sup.toshow[[i]]){
				mtext(bquote(paste(.(v$mx1) %notin% group("[",list(.(cv$ic.z.limit.inf.toshow[[i]]),.(cv$ic.z.limit.sup.toshow[[i]])),"]"),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
			} else {
				mtext(bquote(paste(.(v$mx1) %in% group("[",list(.(cv$ic.z.limit.inf.toshow[[i]]),.(cv$ic.z.limit.sup.toshow[[i]])),"]"),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
			}
		}
	}
    ## Plot bar plot of includes %
    if(v$pcbp2c){
      ## Plot bar plot of includes 2 class %
      if(length(cv$samples.x)>0){
	includes<-c("µ1 ∈ IC"=cv$pc.ic.z.inc.mu1,"µ1 ∉ IC"=(100-cv$pc.ic.z.inc.mu1))
      } else {
	includes<-c("µ1 ∈ IC"=0,"µ1 ∉ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
      ## Plot bar plot of includes 3 class %
      if(length(cv$samples.x)>0){
	includes<-c("∉"=cv$pc.ic.z.l.ninc.mu1,"∈"=cv$pc.ic.z.inc.mu1,"∉"=cv$pc.ic.z.r.ninc.mu1)
      } else {
	includes<-c("∉"=0,"∈"=0,"∉"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
    }
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.z.inc.mu1,cv$pc.ic.z.inc.mu1),c(" "," "),c(cv$n.ic.z.l.ninc.mu1+cv$n.ic.z.r.ninc.mu1,cv$pc.ic.z.l.ninc.mu1+cv$pc.ic.z.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.z.l.ninc.mu1,cv$pc.ic.z.l.ninc.mu1),c(" "," "),c(cv$n.ic.z.inc.mu1,cv$pc.ic.z.inc.mu1),c(" "," "),c(cv$n.ic.z.r.ninc.mu1,cv$pc.ic.z.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ∉ ",""," ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(-0.75,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu," vs IC")),cex=1.4,xjust=0,yjust=1)
    }

    par(mai=c(0.5,0.7,0,0.5))
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
      plot(cv$vect.n.samples,cv$vect.pc.ic.z.inc.mu1,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ∈ ",mu,sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      lines(x<-c(0,npclim),y <- c(v$confidence*100,v$confidence*100),lty=3)
      text(npclim*0.01,v$confidence*95,expression(1-alpha),pos=4)
    } else {
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
}

    }, height = getPlotHeight, width=full.plot.width)
########################################################################################
  output$plotT <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5,6,6,7,8),3,4,byrow=TRUE)
    }
    if(v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3,4,4,5,6),2,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2),1,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),3,4,byrow=TRUE)
    }
    layout(m,width=c(4,2,1,3))
    
if(v$showR){
    ##################
    ## Plot Reality ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showreality){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(x.lim.min,x.lim.max,20),main=bquote(paste("Prélèvement d'échantillons, et comparaison de l'IC pour µ avec ",mu[0]," et ",mu[1],sep="")),cex.main=1.5)
    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	mtext(bquote(paste(bar(x)[.(cv$samples.x.i.toshow[[i]])] == .(cv$samples.x.m.toshow[[i]]),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s[.(cv$samples.x.i.toshow[[i]])] == .(cv$samples.x.sd.toshow[[i]]),sep="")),side=4,line=9,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(1,signif(cv$maxdmx,1)*0.95,labels="Echantillons",cex=1.4, pos=4)
    if(v$showreality){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* sigma^2 ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }

      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)

    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')


    if(v$thresholds == "formula"){
      text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=cex.hypoth,pos=4)
      text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",mu[0] %notin% group("[",list(bar(x)-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n)),bar(x)+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n))),"]"),sep="")),cex=cex.hypoth,pos=4)
      text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",mu[0] %in% group("[",list(bar(x)-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n)),bar(x)+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n))),"]"),sep="")),cex=cex.hypoth,pos=4)
    } else {
      text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
      if(length(rv$samples.z)>0){
	if(v$thresholds == "calcul"){
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(cv$samples.x.sd[[length(rv$samples.z)]]),sqrt(.(v$n))),.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(cv$samples.x.sd[[length(rv$samples.z)]]),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(cv$samples.x.sd[[length(rv$samples.z)]]),sqrt(.(v$n))),.(cv$samples.x.m.toshow[[cv$samples.x.n.toshow]])+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(cv$samples.x.sd[[length(rv$samples.z)]]),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[4]]+0.1,bquote(paste(bar(x) == .(cv$samples.x.m[[length(rv$samples.z)]])," est la moyenne du dernier échantillon obtenu.",sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[4]]-0.1,bquote(paste(S == .(cv$samples.x.sd[[length(rv$samples.z)]])," est l'écart-type du dernier échantillon obtenu.",sep="")),cex=cex.hypoth,pos=4)
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(.(cv$ic.t.limit.inf.toshow[[cv$samples.x.n.toshow]]),.(cv$ic.t.limit.sup.toshow[[cv$samples.x.n.toshow]])),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(.(cv$ic.t.limit.inf.toshow[[cv$samples.x.n.toshow]]),.(cv$ic.t.limit.sup.toshow[[cv$samples.x.n.toshow]])),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[4]],bquote(paste("Seuils critiques calculés avec ",bar(x) == .(cv$samples.x.m[[length(rv$samples.z)]])," et ", S == .(cv$samples.x.sd[[length(rv$samples.z)]]),sep="")),cex=cex.hypoth,pos=4)
	}
      } else {
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",.(v$mx0) %notin% group("[",list(bar(x)-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),bar(x)+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",.(v$mx0) %in% group("[",list(bar(x)-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),bar(x)+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[4]]+0.1,bquote(paste(bar(x)," sera remplacé par la moyenne du prochain échantillon.",sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[4]]-0.1,bquote(paste("S sera remplacé par l'écart-type du prochain échantillon.",sep="")),cex=cex.hypoth,pos=4)
      }
    } 
    
#     ## empty plot for layout
#     par(mai=c(0.5,0.5,0.5,0))
#     plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
#     if(v$evolpcincmu){
#       title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
#     }
}


if(v$showh0){
    ##################
    ## Plot H0      ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[0]),cex=1.4, pos=4)
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	polygon(c(cv$ic.t.limit.inf.toshow[[i]],cv$ic.t.limit.inf.toshow[[i]],cv$ic.t.limit.sup.toshow[[i]],cv$ic.t.limit.sup.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=cv$samples.ic.t.mu0.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.t.limit.inf.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="[",cex=1,col=cv$samples.ic.t.mu0.color[[i]])#col=cv$ic.z.color
	text(cv$ic.t.limit.sup.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.t.mu0.color[[i]]
	lines(x<-c(cv$ic.t.limit.inf.toshow[[i]],cv$samples.x.m.toshow[[i]]-1),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.t.mu0.color[[i]])
	lines(x<-c(cv$samples.x.m.toshow[[i]]+1,cv$ic.t.limit.sup.toshow[[i]]),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.t.mu0.color[[i]])
      }
    }
	
	mtext(bquote(paste(mu[0]," vs IC => conclusion",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
	if(cv$samples.x.n.toshow>0){
		for(i in 1:cv$samples.x.n.toshow){
			if(v$mx0 < cv$ic.t.limit.inf.toshow[[i]] || v$mx0 > cv$ic.t.limit.sup.toshow[[i]]){
				mtext(bquote(paste(.(v$mx0) %notin% group("[",list(.(cv$ic.t.limit.inf.toshow[[i]]),.(cv$ic.t.limit.sup.toshow[[i]])),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
			} else {
				mtext(bquote(paste(.(v$mx0) %in% group("[",list(.(cv$ic.t.limit.inf.toshow[[i]]),.(cv$ic.t.limit.sup.toshow[[i]])),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
			}
		}
	}
	
    if(v$pcbp2c){
      ## Plot bar plot of includes %
      if(length(rv$samples.z)){
	includes<-c("∈"=cv$pc.ic.t.inc.mu0,"µo ∉ IC"=(100-cv$pc.ic.t.inc.mu0))
      } else {
	includes<-c("∈"=0,"µo ∉ IC"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
       ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("∉"=cv$pc.ic.t.l.ninc.mu0,"∈"=cv$pc.ic.t.inc.mu0,"∉"=cv$pc.ic.t.r.ninc.mu0)
      } else {
	includes<-c("∉"=0,"∈"=0,"∉"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
    }
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.t.inc.mu0,cv$pc.ic.t.inc.mu0),c(" "," "),c(cv$n.ic.t.l.ninc.mu0+cv$n.ic.t.r.ninc.mu0,cv$pc.ic.t.l.ninc.mu0+cv$pc.ic.t.r.ninc.mu0))
      colnames(ICvsmu1)<-c(" ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.t.l.ninc.mu0,cv$pc.ic.t.l.ninc.mu0),c(" "," "),c(cv$n.ic.t.inc.mu0,cv$pc.ic.t.inc.mu0),c(" "," "),c(cv$n.ic.t.r.ninc.mu0,cv$pc.ic.t.r.ninc.mu0))
      colnames(ICvsmu1)<-c(" ∉ ",""," ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(-0.75,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu[0]," vs IC")),cex=1.4,xjust=0,yjust=1)
    }
    
    par(mai=c(0.5,0.7,0,0.5))
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
      plot(cv$vect.n.samples,cv$vect.pc.ic.t.inc.mu0,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ∈ ",mu[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
    } else {
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
}



if(v$showh1){
    ##################
    ## Plot H1     ##
    ##################
    cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20),main=bquote(paste("IC pour µ calculés selon [ ",bar(x)-t[n-1.1-alpha/2]*frac(s,sqrt(n)),",",bar(x)+t[n-1.1-alpha/2]*frac(s,sqrt(n)),"]",sep="")),cex.main=1.5)
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*0.95,labels=bquote(H[1]),cex=1.4, pos=4)
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	polygon(c(cv$ic.t.limit.inf.toshow[[i]],cv$ic.t.limit.inf.toshow[[i]],cv$ic.t.limit.sup.toshow[[i]],cv$ic.t.limit.sup.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=cv$samples.ic.t.mu1.color[[i]])#,density=cv$ic.z.density
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1)
	text(cv$ic.t.limit.inf.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="[",cex=1,col=cv$samples.ic.t.mu1.color[[i]])#col=cv$ic.z.color
	text(cv$ic.t.limit.sup.toshow[[i]],cv$samples.y.toshow[[i]][1],labels="]",cex=1)#,col=cv$samples.ic.t.mu1.color[[i]]
	lines(x<-c(cv$ic.t.limit.inf.toshow[[i]],cv$samples.x.m.toshow[[i]]-1),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.t.mu1.color[[i]])
	lines(x<-c(cv$samples.x.m.toshow[[i]]+1,cv$ic.t.limit.sup.toshow[[i]]),y <- c(cv$samples.y.toshow[[i]][1],cv$samples.y.toshow[[i]][1]),lwd=1,lty=2,col=cv$samples.ic.t.mu1.color[[i]])
      }
    }
	mtext(bquote(paste(mu," vs IC",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
	if(cv$samples.x.n.toshow>0){
		for(i in 1:cv$samples.x.n.toshow){
			if(v$mx1 < cv$ic.t.limit.inf.toshow[[i]] || v$mx1 > cv$ic.t.limit.sup.toshow[[i]]){
				mtext(bquote(paste(.(v$mx1) %notin% group("[",list(.(cv$ic.t.limit.inf.toshow[[i]]),.(cv$ic.t.limit.sup.toshow[[i]])),"]"),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
			} else {
				mtext(bquote(paste(.(v$mx1) %in% group("[",list(.(cv$ic.t.limit.inf.toshow[[i]]),.(cv$ic.t.limit.sup.toshow[[i]])),"]"),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
			}
		}
	}
    ## Plot bar plot of includes %
    if(v$pcbp2c){
      ## Plot bar plot of includes 2 class %
      if(length(cv$samples.x)>0){
	includes<-c("∈"=cv$pc.ic.t.inc.mu1,"∉"=(100-cv$pc.ic.t.inc.mu1))
      } else {
	includes<-c("∈"=0,"∉"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH1,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
    } else {
      ## Plot bar plot of includes 3 class %
      if(length(cv$samples.x)>0){
	includes<-c("∉"=cv$pc.ic.t.l.ninc.mu1,"∈"=cv$pc.ic.t.inc.mu1,"∉"=cv$pc.ic.t.r.ninc.mu1)
      } else {
	includes<-c("∉"=0,"∈"=0,"∉"=0)
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH1<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true,color.false),cex.names=1.25,cex.axis=1.2)
    }
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.t.inc.mu1,cv$pc.ic.t.inc.mu1),c(" "," "),c(cv$n.ic.t.l.ninc.mu1+cv$n.ic.t.r.ninc.mu1,cv$pc.ic.t.l.ninc.mu1+cv$pc.ic.t.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu," vs IC")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.t.l.ninc.mu1,cv$pc.ic.t.l.ninc.mu1),c(" "," "),c(cv$n.ic.t.inc.mu1,cv$pc.ic.t.inc.mu1),c(" "," "),c(cv$n.ic.t.r.ninc.mu1,cv$pc.ic.t.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ∉ ",""," ∈ ",""," ∉ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(-0.75,110,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(mu," vs IC")),cex=1.4,xjust=0,yjust=1)
    }

    par(mai=c(0.5,0.7,0,0.5))
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
      plot(cv$vect.n.samples,cv$vect.pc.ic.t.inc.mu1,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%IC ∈ ",mu,sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      lines(x<-c(0,npclim),y <- c(v$confidence*100,v$confidence*100),lty=3)
      text(npclim*0.01,v$confidence*95,expression(1-alpha),pos=4)
    } else {
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
}


    }, height = getPlotHeight, width=full.plot.width)
    
  output$DataTable <- renderTable({
    v<-getInputValues()
    cv<-getComputedValues()
    ## Transpose the sample list
    if(length(cv$samples.x)>0){
      samples.as.list<-list()
      for(i in 1:length(cv$samples.x)){
	samples.as.list[[i]]<-c(round(cv$samples.x[[i]],2),c(""),round(cv$samples.x.m[[i]],2),round(cv$samples.x.sd[[i]],2),c(""),round(cv$ic.k.limit.inf[[i]],2),round(cv$ic.k.limit.sup[[i]],2),c(""),round(cv$ic.z.limit.inf[[i]],2),round(cv$ic.z.limit.sup[[i]],2),c(""),round(cv$ic.t.limit.inf[[i]],2),round(cv$ic.t.limit.sup[[i]],2))
      }
      samples.as.matrix<- do.call(rbind,samples.as.list) 
      transposed.samples<-lapply(seq_len(ncol(samples.as.matrix)),function(i) samples.as.matrix[,i]) 
      d<-data.frame(transposed.samples)
      colnames(d)<-c(paste("X",1:v$n,sep="")," ","Moy","SD"," ","LiICk","LsICk"," ","LiICz","LsICz"," ","LiICt","LsICt")
      d
    }
  })
  
###################################################################
  output$test1 <- renderText({
    v<-getInputValues()
    cv<-getComputedValues()
    t<-cv$samples.y.toshow[[1]][1]-0.002
    paste("Tab",input$Tabset,"n inc µo :",cv$n.ic.k.inc.mu0," | N :",cv$n.samples," | takesample : ",input$takesample,rv$last.takesample.value," | Last action : ",rv$lastAction," | Sample.exist :",cv$samples.exist," | sample to show : ",length(cv$samples.x.toshow[[1]])," ",length(cv$samples.y.toshow[[1]])," ",cv$samples.x.from," ",cv$samples.x.to," ",t,sep=" ")
  })
  
  output$test2 <- renderText({
    paste("Tab",input$Tabset,sep=" ")
  })
  
  output$test3 <- renderText({
    paste("Tab",input$Tabset,sep=" ")
  })
  
})
