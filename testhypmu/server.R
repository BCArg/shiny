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
  
  SP$confidence.t.limit.inf<<-list()
  SP$confidence.t.limit.sup<<-list()
  
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
    
    ## Computation of alpha, beta, confidence and power related variables  ##
    cv$alpha<-round(1-v$confidence,3)#Computation of alpha probability
    
    ## Set z and t statistics for confidence intervals
    cv$ic.z<-qnorm(1-cv$alpha/2)#z positive limit of a bidirectionnal confidence interval in N(0,1) => for CI with known variance
    cv$ic.t<-qt(1-cv$alpha/2,v$n-1)#t positive limit of a bidirectionnal confidence interval in t(n-1) => for CI with unknown variance
    
    ## Quantiles for plot curves
    z<-seq(-5,5,length=100)
    z.a<-seq(-5,cv$ic.z*-1,length=100)
    z.b<-seq(cv$ic.z*-1,cv$ic.z,length=100)
    z.c<-seq(cv$ic.z,5,length=100)
    ## Computation of x y coordinates for Normal curve of Reality
    cv$xr<-(z*v$sx)+cv$mx #x for Reality
    cv$yr<-dnorm(cv$xr,mean=cv$mx,sd=v$sx)#y for Reality
    
    ## Computation of x y coordinates for Normal curve of H1
    cv$xh1<-(z*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$yh1<-dnorm(cv$xh1,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
    
    #Quantile of values in H1
    cv$x.lim.inf.h0<-qnorm(cv$alpha/2,mean=v$mx0, sd=v$sx/sqrt(v$n))#Quantile of inferior limit in H0
    cv$p.lim.inf.h1<-pnorm(cv$x.lim.inf.h0,mean=v$mx1, sd=v$sx/sqrt(v$n))
    cv$z.lim.inf.h1<-qnorm(cv$p.lim.inf.h1,mean=0,sd=1)
    if(cv$z.lim.inf.h1 < -5){
      cv$z.lim.inf.h1<- -5
    }
    
    cv$x.lim.sup.h0<-qnorm(v$confidence+(cv$alpha/2),mean=v$mx0, sd=v$sx/sqrt(v$n))
    cv$p.lim.sup.h1<-pnorm(cv$x.lim.sup.h0,mean=v$mx1, sd=v$sx/sqrt(v$n))
    cv$z.lim.sup.h1<-qnorm(cv$p.lim.sup.h1,mean=0,sd=1)
    if(cv$z.lim.sup.h1 > 5){
      cv$z.lim.sup.h1<- 5
    }
    cv$power=cv$p.lim.inf.h1+(1-cv$p.lim.sup.h1)
    cv$beta=1-cv$power
    
    cv$xh1<-(z*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$yh1<-dnorm(cv$xh1,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
    
    cv$zh1.a<-seq(-5,cv$z.lim.inf.h1,length=100)
    cv$xh1.a<-(cv$zh1.a*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$yh1.a<-dnorm(cv$xh1.a,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
    
    cv$zh1.b<-seq(cv$z.lim.inf.h1,cv$z.lim.sup.h1,length=100)
    cv$xh1.b<-(cv$zh1.b*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$yh1.b<-dnorm(cv$xh1.b,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
    
    cv$zh1.c<-seq(cv$z.lim.sup.h1,5,length=100)
    cv$xh1.c<-(cv$zh1.c*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$yh1.c<-dnorm(cv$xh1.c,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
    
    ## Computation of x y coordinates for Normal curve of H0
    cv$xh0<-(z*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
    cv$yh0<-dnorm(cv$xh0,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0

    cv$xh0.a<-(z.a*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
    cv$yh0.a<-dnorm(cv$xh0.a,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
    
    cv$xh0.b<-(z.b*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
    cv$yh0.b<-dnorm(cv$xh0.b,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
    
    cv$xh0.c<-(z.c*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
    cv$yh0.c<-dnorm(cv$xh0.c,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
    
    ## Computation of maximal density for plots
    cv$maxdmx<-0.05
    if(max(cv$yh1,cv$yh0) > 0.05){
      cv$maxdmx<-max(cv$yh1,cv$yh0)
    }
    
    


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
    cv$confidence.t.limit.inf<-list()
    cv$confidence.t.limit.sup<-list()
    
    cv$xh0.t<-list()
    cv$yh0.t<-list()
    cv$xh1.t<-list()
    cv$yh1.t<-list()
    
    ## Computation of confidence lilmits
    cv$confidence.k.limit.inf=v$mx0-v$k#compute the confidence lower limit with empiric k value
    cv$confidence.k.limit.sup=v$mx0+v$k#compute the confidence higher limit with empiric k value
    cv$confidence.z.limit.inf<-v$mx0-cv$ic.z*cv$sx.dech#compute the confidence lower limit when variance known
    cv$confidence.z.limit.sup<-v$mx0+cv$ic.z*cv$sx.dech#compute the confidence higher limit when variance known
    # Compute of confidence interval for unknown variance : see samples variable scomputations as it depends of mean of each sample
    
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
      
      SP$confidence.t.limit.inf<<-list()
      SP$confidence.t.limit.sup<<-list()
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
	cv$confidence.t.limit.inf[[i]]<-v$mx0-cv$ic.t*(cv$samples.x.sd[[i]]/sqrt(v$n))#compute the confidence lower limit when variance unknown
	cv$confidence.t.limit.sup[[i]]<-v$mx0+cv$ic.t*(cv$samples.x.sd[[i]]/sqrt(v$n))#compute the confidence higher limit when variance unknown
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
	
	SP$confidence.t.limit.inf<<-c(SP$confidence.t.limit.inf,cv$confidence.t.limit.inf)
	SP$confidence.t.limit.sup<<-c(SP$confidence.t.limit.sup,cv$confidence.t.limit.sup)
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
    ## Choose values to show in plots
    cv$samples.x.toshow<-list()
    if(length(SP$samples.x)>0){
      cv$samples.x.from<-1
      if(length(SP$samples.x)>v$nss){
	cv$samples.x.from<-length(SP$samples.x)-v$nss+1
      }
      cv$samples.x.to<-length(SP$samples.x)
      cv$samples.x.toshow<-SP$samples.x[cv$samples.x.from:cv$samples.x.to]
      
      cv$samples.x.m.toshow<-SP$samples.x.m[cv$samples.x.from:cv$samples.x.to]
      cv$samples.x.sd.toshow<-SP$samples.x.sd[cv$samples.x.from:cv$samples.x.to]
      
      cv$ic.k.limit.inf.toshow<-SP$ic.k.limit.inf[cv$samples.x.from:cv$samples.x.to]
      cv$ic.k.limit.sup.toshow<-SP$ic.k.limit.sup[cv$samples.x.from:cv$samples.x.to]
      
      cv$ic.z.limit.inf.toshow<-SP$ic.z.limit.inf[cv$samples.x.from:cv$samples.x.to]
      cv$ic.z.limit.sup.toshow<-SP$ic.z.limit.sup[cv$samples.x.from:cv$samples.x.to]
      
      cv$ic.t.limit.inf.toshow<-SP$ic.t.limit.inf[cv$samples.x.from:cv$samples.x.to]
      cv$ic.t.limit.sup.toshow<-SP$ic.t.limit.sup[cv$samples.x.from:cv$samples.x.to]
      
      cv$confidence.t.limit.inf.toshow<-SP$confidence.t.limit.inf[cv$samples.x.from:cv$samples.x.to]
      cv$confidence.t.limit.sup.toshow<-SP$confidence.t.limit.sup[cv$samples.x.from:cv$samples.x.to]
      
      
      cv$samples.y.toshow<-list()
      if(length(cv$samples.x.toshow)>0){
	for(i in 1:length(cv$samples.x.toshow)){
	  cv$samples.y.toshow[[i]]<-list()
	  for(j in 1:length(cv$samples.x.toshow[[i]])){
	    cv$samples.y.toshow[[i]]<-c(cv$samples.y.toshow[[i]],c((cv$maxdmx/(v$nss+1))*i))#
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
	
	cv$confidence.t.limit.inf[[i]]<-v$mx0-cv$ic.t*(cv$samples.x.sd.toshow[[i]]/sqrt(v$n))#compute the confidence lower limit when variance unknown
	cv$confidence.t.limit.sup[[i]]<-v$mx0+cv$ic.t*(cv$samples.x.sd.toshow[[i]]/sqrt(v$n))#compute the confidence higher limit when variance unknown
	t<-seq(-8,8,length=100)
	cv$xh0.t[[i]]<-(t*(cv$samples.x.sd.toshow[[i]]/sqrt(v$n)))+v$mx0
	cv$xh1.t[[i]]<-(t*(cv$samples.x.sd.toshow[[i]]/sqrt(v$n)))+v$mx1
	cv$yh0.t[[i]]<-dt(t,v$n-1)*(sqrt(v$n)/v$sx)
	cv$yh1.t[[i]]<-dt(t,v$n-1)*(sqrt(v$n)/v$sx)
	#if(max(cv$maxdmx,max(cv$yh0.t[[i]])) > 0.05){
	#  cv$maxdmx<-max(cv$maxdmx,max(cv$yh0.t[[i]]))
	#}
	}
      }
    }
    ## Last takesample value
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
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    label<-""
    if(v$showrh1h0){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20))

    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	#text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }
    text(1,signif(cv$maxdmx,1)*1.1,labels="Echantillons",cex=1.4, pos=4)
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(paste(X*"~"* N(.(cv$mx)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }

    lines(x<-c(cv$mx,cv$mx),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(cv$mx,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)

    ## empty plot for layout
    par(mai=c(0.5,0.4,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')#,main=bquote(paste("Calcul du % de ",RH[0]," et de ",NRH[0],sep="")),cex.main=1.5
    title(bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex.main=1.4)
    #text(0,1.05,labels=bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=1.4,pos=4)
    text(0,0.8,labels=bquote(paste("Calcul du % de ",RH[0]," et de ",NRH[0]," :",sep="")),cex=1.4,pos=4)
    text(0,0.6,labels=bquote(paste("Nombre total d'échantillons : ",.(cv$n.samples),sep="")),cex=1.4,pos=4)
    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.k.inc.mu1,cv$pc.ic.k.inc.mu1),c(" "," "),c(cv$n.ic.k.l.ninc.mu1+cv$n.ic.k.r.ninc.mu1,cv$pc.ic.k.l.ninc.mu1+cv$pc.ic.k.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]")),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.k.inc.mu0,cv$pc.ic.k.inc.mu0),c(" "," "),c(cv$n.ic.k.l.ninc.mu0+cv$n.ic.k.r.ninc.mu0,cv$pc.ic.k.l.ninc.mu0+cv$pc.ic.k.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]")),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.k.l.ninc.mu1,cv$pc.ic.k.l.ninc.mu1),c(" "," "),c(cv$n.ic.k.inc.mu1,cv$pc.ic.k.inc.mu1),c(" "," "),c(cv$n.ic.k.r.ninc.mu1,cv$pc.ic.k.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]")),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.k.l.ninc.mu0,cv$pc.ic.k.l.ninc.mu0),c(" "," "),c(cv$n.ic.k.inc.mu0,cv$pc.ic.k.inc.mu0),c(" "," "),c(cv$n.ic.k.r.ninc.mu0,cv$pc.ic.k.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]")),cex=1.4,xjust=0,yjust=1)
    }

    ## empty plot for layout
    par(mai=c(0.5,0.5,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    title(bquote(paste("Règle de rejet de ",H[0]," : ",sep="")),cex.main=1.4)
    text(0,0.8,labels=bquote(paste(RH[0]," : Rejet de ",H[0]," si ",bar(x) %notin% group("[",list(mu[0]-K,mu[0]+K),"]"),sep="")),cex=1.4,pos=4)
    text(0,0.6,labels=bquote(paste(NRH[0]," : Non rejet de ",H[0]," si ",bar(x) %in% group("[",list(mu[0]-K,mu[0]+K),"]"),sep="")),cex=1.4,pos=4)
    if(v$evolpcincmu){
      #title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
    }

    ##################
    ## Plot H1     ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #title(main=bquote(paste("Confiance sous ",H[0]," calculée selon [ ",mu[0]-K," , ",mu[0]+K,"]",sep="")),cex.main=1.5)

    text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(H[1]),cex=1.4, pos=4)
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xh1,cv$yh1,type="l")
      text(1,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[1] *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.6,labels=bquote(paste(bar(X) *"~"* N (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
    }

    ## Confidence interval compute under H0
    lines(x<-c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),y<-c(0,cv$maxdmx*1))
    lines(x<-c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),y<-c(0,cv$maxdmx*1))
    ## Confidence interval compute under H0 : polygones
    polygon(c(0,0,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
    polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
    polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)

    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    } 
    
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu[1]),cex=1.2)
    
    
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
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    title(main=bquote(paste("Confiance sous ",H[0]," calculée selon [ ",mu[0]-K," , ",mu[0]+K,"]",sep="")),cex.main=1.5)

    text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(H[0]),cex=1.4, pos=4)
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xh0,cv$yh0,type="l")
      text(1,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[0] *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.6,labels=bquote(paste(bar(X) *"~"* N (.(v$mx0)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
    }
    
    
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    

    ## Confidence interval compute under H0
    lines(x<-c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),y<-c(0,cv$maxdmx*1))
    lines(x<-c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),y<-c(0,cv$maxdmx*1))
    ## Confidence interval compute under H0 : polygones
    polygon(c(0,0,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
    polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
    polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)

    
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
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
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    label<-""
    if(v$showrh1h0){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20))

    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
      }
    }
    text(1,signif(cv$maxdmx,1)*1.1,labels="Echantillons",cex=1.4, pos=4)
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(paste(X*"~"* N(.(cv$mx)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }

    lines(x<-c(cv$mx,cv$mx),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(cv$mx,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)

    ## empty plot for layout
    par(mai=c(0.5,0.4,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l',main=bquote(paste("Calcul du % de ",RH[0]," et de ",NRH[0],sep="")),cex.main=1.5)
    text(0,1.05,labels=bquote(paste("Nombre total d'échantillons : ",.(cv$n.samples),sep="")),cex=1.4,pos=4)


    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.z.inc.mu1,cv$pc.ic.z.inc.mu1),c(" "," "),c(cv$n.ic.z.l.ninc.mu1+cv$n.ic.z.r.ninc.mu1,cv$pc.ic.z.l.ninc.mu1+cv$pc.ic.z.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.5,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs ",1 - alpha)),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.z.inc.mu0,cv$pc.ic.z.inc.mu0),c(" "," "),c(cv$n.ic.z.l.ninc.mu0+cv$n.ic.z.r.ninc.mu0,cv$pc.ic.z.l.ninc.mu0+cv$pc.ic.z.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.5,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs ",1 - alpha)),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.z.l.ninc.mu1,cv$pc.ic.z.l.ninc.mu1),c(" "," "),c(cv$n.ic.z.inc.mu1,cv$pc.ic.z.inc.mu1),c(" "," "),c(cv$n.ic.z.r.ninc.mu1,cv$pc.ic.z.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.5,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs ",1 - alpha)),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.z.l.ninc.mu0,cv$pc.ic.z.l.ninc.mu0),c(" "," "),c(cv$n.ic.z.inc.mu0,cv$pc.ic.z.inc.mu0),c(" "," "),c(cv$n.ic.z.r.ninc.mu0,cv$pc.ic.z.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.5,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs ",1 - alpha)),cex=1.4,xjust=0,yjust=1)
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
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))

    title(main=bquote(paste("Confiance sous ",H[0]," : [ ",mu[0]-Z[1-alpha/2]*frac(sigma,sqrt(n))," , ",mu[0]+Z[1-alpha/2]*frac(sigma,sqrt(n)),"]",sep="")),cex.main=1.5)

    
    text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(H[1]),cex=1.4, pos=4)
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xh1,cv$yh1,type="l")
      text(1,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[1] *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.6,labels=bquote(paste(bar(X) *"~"* N (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
    }


    if(v$showrh1h0){
      polygon(c(cv$xh1.a,max(cv$xh1.a)),c(cv$yh1.a,0),col=color.true)
      polygon(c(min(cv$xh1.b),cv$xh1.b,max(cv$xh1.b)),c(0,cv$yh1.b,0),col=color.false)
      polygon(c(min(cv$xh1.c),cv$xh1.c),c(0,cv$yh1.c),col=color.true)
      ## Confidence interval compute under H0
      lines(x<-c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),y<-c(0,cv$maxdmx*1))
      lines(x<-c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),y<-c(0,cv$maxdmx*1))
    } else {
      ## Confidence interval compute under H0
      lines(x<-c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),y<-c(0,cv$maxdmx*1))
      lines(x<-c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),y<-c(0,cv$maxdmx*1))
      ## Confidence interval compute under H0 : polygones
      polygon(c(0,0,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
    }


    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }
    
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu[1]),cex=1.2)
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
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(H[0]),cex=1.4, pos=4)
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xh0,cv$yh0,type="l")
      text(1,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[0] *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.6,labels=bquote(paste(bar(X) *"~"* N (.(v$mx0)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
    }
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    
    if(v$showrh1h0){
    polygon(c(cv$xh0.a,max(cv$xh0.a)),c(cv$yh0.a,0),col=color.false)
    polygon(c(min(cv$xh0.b),cv$xh0.b,max(cv$xh0.b)),c(0,cv$yh0.b,0),col=color.true)
    polygon(c(min(cv$xh0.c),cv$xh0.c),c(0,cv$yh0.c),col=color.false)
    ## Confidence interval compute under H0
    lines(x<-c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),y<-c(0,cv$maxdmx*1))
    lines(x<-c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),y<-c(0,cv$maxdmx*1))
    } else {
      ## Confidence interval compute under H0
      lines(x<-c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),y<-c(0,cv$maxdmx*1))
      lines(x<-c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),y<-c(0,cv$maxdmx*1))
      ## Confidence interval compute under H0 : polygones
      polygon(c(0,0,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
    }

    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
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
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    label<-""
    if(v$showrh1h0){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20))

    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
      }
    }
    text(1,signif(cv$maxdmx,1)*1.1,labels="Echantillons",cex=1.4, pos=4)
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(paste(X*"~"* N(.(cv$mx)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }

    lines(x<-c(cv$mx,cv$mx),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(cv$mx,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)

    ## empty plot for layout
    par(mai=c(0.5,0.4,0.5,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l',main=bquote(paste("Calcul du % de ",RH[0]," et de ",NRH[0],sep="")),cex.main=1.5)
    text(0,1.05,labels=bquote(paste("Nombre total d'échantillons : ",.(cv$n.samples),sep="")),cex=1.4,pos=4)


    if(v$pcbp2c){
      ICvsmu1<-data.frame(c(cv$n.ic.t.inc.mu1,cv$pc.ic.t.inc.mu1),c(" "," "),c(cv$n.ic.t.l.ninc.mu1+cv$n.ic.t.r.ninc.mu1,cv$pc.ic.t.l.ninc.mu1+cv$pc.ic.t.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.5,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs ",1 - alpha)),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.t.inc.mu0,cv$pc.ic.t.inc.mu0),c(" "," "),c(cv$n.ic.t.l.ninc.mu0+cv$n.ic.t.r.ninc.mu0,cv$pc.ic.t.l.ninc.mu0+cv$pc.ic.t.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.5,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs ",1 - alpha)),cex=1.4,xjust=0,yjust=1)
    } else {
      ICvsmu1<-data.frame(c(cv$n.ic.t.l.ninc.mu1,cv$pc.ic.t.l.ninc.mu1),c(" "," "),c(cv$n.ic.t.inc.mu1,cv$pc.ic.t.inc.mu1),c(" "," "),c(cv$n.ic.t.r.ninc.mu1,cv$pc.ic.t.r.ninc.mu1))
      colnames(ICvsmu1)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu1)<-c("n ","% ")
      addtable2plot(0,0.5,ICvsmu1,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs ",1 - alpha)),cex=1.4,xjust=0,yjust=1)

      ICvsmu0<-data.frame(c(cv$n.ic.t.l.ninc.mu0,cv$pc.ic.t.l.ninc.mu0),c(" "," "),c(cv$n.ic.t.inc.mu0,cv$pc.ic.t.inc.mu0),c(" "," "),c(cv$n.ic.t.r.ninc.mu0,cv$pc.ic.t.r.ninc.mu0))
      colnames(ICvsmu0)<-c(" ⊄ ",""," ⊂ ",""," ⊄ ")
      rownames(ICvsmu0)<-c("n ","% ")
      addtable2plot(0.5,0.5,ICvsmu0,bty="n",display.rownames=TRUE,hlines=FALSE,title=bquote(paste(bar(x)," vs ",1 - alpha)),cex=1.4,xjust=0,yjust=1)
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
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))

    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))

    title(main=bquote(paste("Confiance sous ",H[0]," : [ ",mu[0]-t[n-1.1-alpha/2]*frac(s,sqrt(n)),",",mu[0]+t[n-1.1-alpha/2]*frac(s,sqrt(n)),"]",sep="")),cex.main=1.5)

    
    text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(H[1]),cex=1.4, pos=4)

    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu[1]),cex=1.2)
    
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	if(i == length(cv$samples.x.toshow)){
	  if(v$showrh1h0){
	    axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
	    points(cv$xh1.t[[i]],cv$yh1.t[[i]],type="l")
	    text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu[1] *","* frac(sigma^2,sqrt(n)) ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
	  }
	}
	## Confidence interval compute under H0
	#lines(x<-c(cv$confidence.t.limit.inf[[i]],cv$confidence.t.limit.inf[[i]]),y<-c(0,cv$maxdmx*1))
	#lines(x<-c(cv$confidence.t.limit.sup[[i]],cv$confidence.t.limit.sup[[i]]),y<-c(0,cv$maxdmx*1))
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.t.limit.inf.toshow[[i]],cv$confidence.t.limit.inf.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=color.false)
	polygon(c(cv$confidence.t.limit.sup.toshow[[i]],cv$confidence.t.limit.sup.toshow[[i]],100,100),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=color.false)
	polygon(c(cv$confidence.t.limit.inf.toshow[[i]],cv$confidence.t.limit.inf.toshow[[i]],cv$confidence.t.limit.sup.toshow[[i]],cv$confidence.t.limit.sup.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=color.true)
	  
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
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
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.5))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),4))
    text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(H[0]),cex=1.4, pos=4)

    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)


    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
      	if(i == length(cv$samples.x.toshow)){
	  if(v$showrh1h0){
	    axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
	    points(cv$xh0.t[[i]],cv$yh0.t[[i]],type="l")
	    text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu[1] *","* frac(sigma^2,sqrt(n)) ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
	  }
	}
	

	## Confidence interval compute under H0
	#lines(x<-c(cv$confidence.t.limit.inf[[i]],cv$confidence.t.limit.inf[[i]]),y<-c(0,cv$maxdmx*1))
	#lines(x<-c(cv$confidence.t.limit.sup[[i]],cv$confidence.t.limit.sup[[i]]),y<-c(0,cv$maxdmx*1))
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.t.limit.inf.toshow[[i]],cv$confidence.t.limit.inf.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=color.false)
	polygon(c(cv$confidence.t.limit.sup.toshow[[i]],cv$confidence.t.limit.sup.toshow[[i]],100,100),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=color.false)
	polygon(c(cv$confidence.t.limit.inf.toshow[[i]],cv$confidence.t.limit.inf.toshow[[i]],cv$confidence.t.limit.sup.toshow[[i]],cv$confidence.t.limit.sup.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]+0.0025,cv$samples.y.toshow[[i]][1]-0.0025),col=color.true)

	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
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
    
  output$DataTable <- renderTable({
    v<-getInputValues()
    cv<-getComputedValues()
    ## Transpose the sample list
    if(length(SP$samples.x)>0){
      samples.as.list<-list()
      for(i in 1:length(SP$samples.x)){
	samples.as.list[[i]]<-c(round(SP$samples.x[[i]],2),c(""),round(SP$samples.x.m[[i]],2),round(SP$samples.x.sd[[i]],2),c(""),round(SP$ic.k.limit.inf[[i]],2),round(SP$ic.k.limit.sup[[i]],2),c(""),round(SP$ic.z.limit.inf[[i]],2),round(SP$ic.z.limit.sup[[i]],2),c(""),round(SP$ic.t.limit.inf[[i]],2),round(SP$ic.t.limit.sup[[i]],2))
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
    paste("Tab",input$Tabset,"n inc µ0 :",cv$n.ic.k.inc.mu0," | N :",cv$n.samples," | takesample : ",input$takesample,SP$last.takesample.value," | Last action : ",rv$lastAction," | Sample.exist :",cv$samples.exist," | sample to show : ",length(cv$samples.x.toshow[[1]])," ",length(cv$samples.y.toshow[[1]])," ",cv$samples.x.from," ",cv$samples.x.to," ",t,sep=" ")
  })
  
  output$test2 <- renderText({
      v<-getInputValues()
    cv<-getComputedValues()
    paste("Tab",input$Tabset," | ",cv$power,cv$beta,cv$p.lim.inf.h1,cv$p.lim.sup.h1,sep=" ")
  })
  
  output$test3 <- renderText({
    v<-getInputValues()
    cv<-getComputedValues()
    paste("Tab",input$Tabset, " | ",cv$p.lim.inf.h1, sep=" ")
  })
  
})
