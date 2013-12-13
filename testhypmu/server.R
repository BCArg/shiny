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
 
  SP$confidence.t.limit.inf.bilat<<-list()
  SP$confidence.t.limit.sup.bilat<<-list()
  SP$confidence.t.limit.inf.unilat<<-list()
  SP$confidence.t.limit.sup.unilat<<-list()
  
  SP$test.k.conclusion<<-list()
  SP$test.z.conclusion<<-list()
  SP$test.t.conclusion<<-list()

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

  getPlotHeight <- function() {
    if(input$showh1){
      return(600)
    } else {
      return(400)
    }
  }
  
  getInputValues<-reactive({
    return(input)#collect all inputs
  })
  
  getComputedValues<-reactive({
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list

    ## Define reality parameters
    cv$vx<-v$sx^2#compute variance of Reality distribution

    ## Quantiles for plot curves
    cv$z<-seq(-5,5,length=100)
    cv$z.d<-dnorm(cv$z)
    
    cv$t<-seq(-5,5,length=100)
    cv$t.d<-dt(cv$t,v$n-1)
    
    cv$z.diff.mu<-seq(-50,50,length=500)# x values for computation of power vector -> see lower cv$z.power in each dirtest

    ## Computation of x y coordinates for Normal curve of Reality
    cv$xr<-(cv$z*v$sx)+v$mx1 #x for Reality
    cv$yr<-dnorm(cv$xr,mean=v$mx1,sd=v$sx)#y for Reality
    
    ### Empiric model ###
    ## Computation of x y coordinates for Normal curve of H1 in first tab
    # x1=mu0-k
    cv$emp.x.lim.inf.h0<-v$mx0-v$k
    cv$emp.x.lim.sup.h0<-v$mx0+v$k
    
    cv$emp.p.lim.inf.h0<-pnorm(cv$emp.x.lim.inf.h0,mean=v$mx0, sd=v$sx/sqrt(v$n))
    cv$emp.p.lim.sup.h0<-1-pnorm(cv$emp.x.lim.sup.h0,mean=v$mx0, sd=v$sx/sqrt(v$n))
    
    cv$emp.p.lim.inf.h1<-pnorm(cv$emp.x.lim.inf.h0,mean=v$mx1, sd=v$sx/sqrt(v$n))
    cv$emp.p.lim.sup.h1<-1-pnorm(cv$emp.x.lim.sup.h0,mean=v$mx1, sd=v$sx/sqrt(v$n))
        
    cv$emp.z.lim.inf.h0<-(cv$emp.x.lim.inf.h0-v$mx0)/(v$sx/sqrt(v$n))
    cv$emp.z.lim.sup.h0<-(cv$emp.x.lim.sup.h0-v$mx0)/(v$sx/sqrt(v$n))
    
    cv$emp.z.lim.inf.h1<-(cv$emp.x.lim.inf.h0-v$mx1)/(v$sx/sqrt(v$n))
    cv$emp.z.lim.sup.h1<-(cv$emp.x.lim.sup.h0-v$mx1)/(v$sx/sqrt(v$n))
    
    if(v$dirtest == "bilat"){
      cv$emp.alpha<-signif(cv$emp.p.lim.inf.h0+cv$emp.p.lim.sup.h0,2)
      cv$emp.confidence<-signif(1-cv$emp.alpha,2)
      cv$emp.power<-signif(cv$emp.p.lim.inf.h1+cv$emp.p.lim.sup.h1,2)
      cv$emp.beta<-signif(1-cv$emp.power)
      
      cv$emp.zh0.a<-seq(-5,cv$emp.z.lim.inf.h0,length=100)
      cv$emp.xh0.a<-(cv$emp.zh0.a*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.a<-dnorm(cv$emp.xh0.a,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh0.b<-seq(cv$emp.z.lim.inf.h0,cv$emp.z.lim.sup.h0,length=100)
      cv$emp.xh0.b<-(cv$emp.zh0.b*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.b<-dnorm(cv$emp.xh0.b,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh0.c<-seq(cv$emp.z.lim.sup.h0,5,length=100)
      cv$emp.xh0.c<-(cv$emp.zh0.c*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.c<-dnorm(cv$emp.xh0.c,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.a<-seq(-5,cv$emp.z.lim.inf.h1,length=100)
      cv$emp.xh1.a<-(cv$emp.zh1.a*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.a<-dnorm(cv$emp.xh1.a,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.b<-seq(cv$emp.z.lim.inf.h1,cv$emp.z.lim.sup.h1,length=100)
      cv$emp.xh1.b<-(cv$emp.zh1.b*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.b<-dnorm(cv$emp.xh1.b,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.c<-seq(cv$emp.z.lim.sup.h1,5,length=100)
      cv$emp.xh1.c<-(cv$emp.zh1.c*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.c<-dnorm(cv$emp.xh1.c,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
    }
    
    if(v$dirtest == "unilatg"){
      cv$emp.alpha<-signif(cv$emp.p.lim.inf.h0,2)
      cv$emp.confidence<-signif(1-cv$emp.alpha,2)
      cv$emp.power<-signif(cv$emp.p.lim.inf.h1,2)
      cv$emp.beta<-signif(1-cv$emp.power)
      
      cv$emp.zh0.a<-seq(-5,cv$emp.z.lim.inf.h0,length=100)
      cv$emp.xh0.a<-(cv$emp.zh0.a*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.a<-dnorm(cv$emp.xh0.a,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh0.b<-seq(cv$emp.z.lim.inf.h0,5,length=100)
      cv$emp.xh0.b<-(cv$emp.zh0.b*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.b<-dnorm(cv$emp.xh0.b,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh0.c<-seq(5,5,length=1)
      cv$emp.xh0.c<-(cv$emp.zh0.c*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.c<-dnorm(cv$emp.xh0.c,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.a<-seq(-5,cv$emp.z.lim.inf.h1,length=100)
      cv$emp.xh1.a<-(cv$emp.zh1.a*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.a<-dnorm(cv$emp.xh1.a,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.b<-seq(cv$emp.z.lim.inf.h1,5,length=100)
      cv$emp.xh1.b<-(cv$emp.zh1.b*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.b<-dnorm(cv$emp.xh1.b,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.c<-seq(5,5,length=1)
      cv$emp.xh1.c<-(cv$emp.zh1.c*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.c<-dnorm(cv$emp.xh1.c,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
    }  
    
    if(v$dirtest == "unilatd"){
      cv$emp.alpha<-signif(cv$emp.p.lim.sup.h0,2)
      cv$emp.confidence<-signif(1-cv$emp.alpha,2)
      cv$emp.power<-signif(cv$emp.p.lim.sup.h1,2)
      cv$emp.beta<-signif(1-cv$emp.power)
      
      cv$emp.zh0.a<-seq(-5,-5,length=1)
      cv$emp.xh0.a<-(cv$emp.zh0.a*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.a<-dnorm(cv$emp.xh0.a,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh0.b<-seq(-5,cv$emp.z.lim.sup.h0,length=100)
      cv$emp.xh0.b<-(cv$emp.zh0.b*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.b<-dnorm(cv$emp.xh0.b,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh0.c<-seq(cv$emp.z.lim.sup.h0,5,length=100)
      cv$emp.xh0.c<-(cv$emp.zh0.c*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$emp.yh0.c<-dnorm(cv$emp.xh0.c,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.a<-seq(-5,-5,length=1)
      cv$emp.xh1.a<-(cv$emp.zh1.a*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.a<-dnorm(cv$emp.xh1.a,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.b<-seq(-5,cv$emp.z.lim.sup.h1,length=100)
      cv$emp.xh1.b<-(cv$emp.zh1.b*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.b<-dnorm(cv$emp.xh1.b,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
      
      cv$emp.zh1.c<-seq(cv$emp.z.lim.sup.h1,5,length=100)
      cv$emp.xh1.c<-(cv$emp.zh1.c*(v$sx/sqrt(v$n)))+v$mx1 #x for H0
      cv$emp.yh1.c<-dnorm(cv$emp.xh1.c,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H0
    }
    
    ## Computation of confidence lilmits
    cv$confidence.k.limit.inf=round(v$mx0-v$k,2)#compute the confidence lower limit with empiric k value
    cv$confidence.k.limit.sup=round(v$mx0+v$k,2)#compute the confidence higher limit with empiric k values
    
    ### Normal var known model ###
    
    ## Computation of alpha, beta, confidence and power related variables  ##
    cv$confidence<-signif(v$confidence,2)
    cv$alpha<-signif(1-cv$confidence,2)#Computation of alpha probability
    
    if(v$dirtest == "bilat"){
      cv$z.z.lim.inf.h0<-qnorm(cv$alpha/2)
      cv$z.z.lim.sup.h0<-qnorm(1-cv$alpha/2)
      
      cv$z.x.lim.inf.h0<-qnorm(cv$alpha/2,mean=v$mx0, sd=v$sx/sqrt(v$n))
      cv$z.x.lim.sup.h0<-qnorm(1-cv$alpha/2,mean=v$mx0, sd=v$sx/sqrt(v$n))
      
      ## Computation of confidence limits
      cv$confidence.z.limit.inf<-round(v$mx0-qnorm(1-cv$alpha/2)*(v$sx/sqrt(v$n)),2)#compute the confidence lower limit when variance known
      cv$confidence.z.limit.sup<-round(v$mx0+qnorm(1-cv$alpha/2)*(v$sx/sqrt(v$n)),2)#compute the confidence higher limit when variance known
      # Compute of confidence interval for unknown variance : see samples variable scomputations as it depends of mean of each samples
      
      ## Computation of power vector for evolution of power in function of µ - µ0
      cv$z.power<-(1-pnorm(qnorm(1-cv$alpha/2,mean=0,sd=1)-((cv$z.diff.mu*-1)/(v$sx/sqrt(v$n))),mean=0,sd=1)) + (1-pnorm(qnorm(1-cv$alpha/2,mean=0,sd=1)-(cv$z.diff.mu/(v$sx/sqrt(v$n))),mean=0,sd=1))
    }
    
    if(v$dirtest == "unilatg"){
      cv$z.z.lim.inf.h0<-qnorm(cv$alpha)
      cv$z.z.lim.sup.h0<-5
      
      cv$z.x.lim.inf.h0<-qnorm(cv$alpha,mean=v$mx0, sd=v$sx/sqrt(v$n))
      cv$z.x.lim.sup.h0<-qnorm(1,mean=v$mx0, sd=v$sx/sqrt(v$n))
      
      ## Computation of confidence lilmits
      cv$confidence.z.limit.inf<-round(v$mx0-qnorm(1-cv$alpha)*(v$sx/sqrt(v$n)),2)#compute the confidence lower limit when variance known
      # Compute of confidence interval for unknown variance : see samples variable scomputations as it depends of mean of each samples
      
      ## Computation of power vector for evolution of power in function of µ - µ0
      cv$z.power<-1-pnorm(qnorm(1-cv$alpha,mean=0,sd=1)-((cv$z.diff.mu)/(v$sx/sqrt(v$n))),mean=0,sd=1)
    }
    
    if(v$dirtest == "unilatd"){
      cv$z.z.lim.inf.h0<--5
      cv$z.z.lim.sup.h0<-qnorm(1-cv$alpha)
      
      cv$z.x.lim.inf.h0<-qnorm(0,mean=v$mx0, sd=v$sx/sqrt(v$n))
      cv$z.x.lim.sup.h0<-qnorm(1-cv$alpha,mean=v$mx0, sd=v$sx/sqrt(v$n))
      
      ## Computation of confidence lilmits
      cv$confidence.z.limit.sup<-round(v$mx0+qnorm(1-cv$alpha)*(v$sx/sqrt(v$n)),2)#compute the confidence higher limit when variance known
      # Compute of confidence interval for unknown variance : see samples variable scomputations as it depends of mean of each sample
      
      ## Computation of power vector for evolution of power in function of µ - µ0
      cv$z.power<-1-pnorm(qnorm(1-cv$alpha,mean=0,sd=1)-((cv$z.diff.mu*-1)/(v$sx/sqrt(v$n))),mean=0,sd=1)
    }
    
    cv$z.p.lim.inf.h1<-pnorm(cv$z.x.lim.inf.h0,mean=v$mx1, sd=v$sx/sqrt(v$n))
    cv$z.p.lim.sup.h1<-pnorm(cv$z.x.lim.sup.h0,mean=v$mx1, sd=v$sx/sqrt(v$n))
  
    cv$z.z.lim.inf.h1<-qnorm(cv$z.p.lim.inf.h1,mean=0,sd=1)
    cv$z.z.lim.sup.h1<-qnorm(cv$z.p.lim.sup.h1,mean=0,sd=1)
    
    if(cv$z.z.lim.inf.h1 < -5){
      cv$z.z.lim.inf.h1<- -5
    }
    if(cv$z.z.lim.sup.h1 > 5){
      cv$z.z.lim.sup.h1<- 5
    }
    cv$power=signif(cv$z.p.lim.inf.h1+(1-cv$z.p.lim.sup.h1),2)
    cv$beta=signif(1-cv$power,2)
  
    cv$z.xh0<-(cv$z*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
    cv$z.yh0<-dnorm(cv$z.xh0,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
    
    cv$z.xh1<-(cv$z*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$z.yh1<-dnorm(cv$z.xh1,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
    
    cv$z.zh1.a<-seq(-5,cv$z.z.lim.inf.h1,length=100)
    cv$z.xh1.a<-(cv$z.zh1.a*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$z.yh1.a<-dnorm(cv$z.xh1.a,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
    
    cv$z.zh1.b<-seq(cv$z.z.lim.inf.h1,cv$z.z.lim.sup.h1,length=100)
    cv$z.xh1.b<-(cv$z.zh1.b*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$z.yh1.b<-dnorm(cv$z.xh1.b,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
    
    cv$z.zh1.c<-seq(cv$z.z.lim.sup.h1,5,length=100)
    cv$z.xh1.c<-(cv$z.zh1.c*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
    cv$z.yh1.c<-dnorm(cv$z.xh1.c,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
  
    cv$z.zh0.a<-seq(-5,cv$z.z.lim.inf.h0,length=100)
    cv$z.xh0.a<-(cv$z.zh0.a*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
    cv$z.yh0.a<-dnorm(cv$z.xh0.a,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
    
    cv$z.zh0.b<-seq(cv$z.z.lim.inf.h0,cv$z.z.lim.sup.h0,length=100)
    cv$z.xh0.b<-(cv$z.zh0.b*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
    cv$z.yh0.b<-dnorm(cv$z.xh0.b,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
    
    cv$z.zh0.c<-seq(cv$z.z.lim.sup.h0,5,length=100)
    cv$z.xh0.c<-(cv$z.zh0.c*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
    cv$z.yh0.c<-dnorm(cv$z.xh0.c,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0
      

    ## Set z and t statistics for confidence intervals
    cv$ic.z<-qnorm(1-cv$alpha/2)#z positive limit of a bidirectionnal confidence interval in N(0,1) => for CI with known variance
    cv$ic.t<-qt(1-cv$alpha/2,v$n-1)#t positive limit of a bidirectionnal confidence interval in t(n-1) => for CI with unknown variance  
       
    ## Computation of maximal density for plots
    cv$maxdmx<-0.05
    if(max(cv$z.yh1,cv$z.yh0) > 0.05){
      cv$maxdmx<-max(cv$z.yh1,cv$z.yh0)
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

    cv$confidence.t.limit.inf<-list()
    cv$confidence.t.limit.sup<-list()
    
    cv$xh0.t<-list()
    cv$yh0.t<-list()
    cv$xh1.t<-list()
    cv$yh1.t<-list()

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
      
    cv$test.k.conclusion.pc.rh0<-0
    cv$test.k.conclusion.pc.nrh0<-0
    cv$test.k.conclusion.pcrh0.vect<-c()
    
    cv$test.z.conclusion.pc.rh0<-0
    cv$test.z.conclusion.pc.nrh0<-0
    cv$test.z.conclusion.pcrh0.vect<-c()
    
    cv$test.t.conclusion.pc.rh0<-0
    cv$test.t.conclusion.pc.nrh0<-0
    cv$test.t.conclusion.pcrh0.vect<-c()
    
    cv$test.k.conclusion.toshow<-list()
    cv$test.z.conclusion.toshow<-list()
    cv$test.t.conclusion.toshow<-list()
    
    cv$n.samples<-length(SP$samples.z)
    cv$samples.exist<-length(cv$samples.z)#mesure length of sample values to test if a sample has been created

    if(cv$samples.exist>0){
      for(i in 1:length(cv$samples.z)){
	cv$samples.x[[i]]<-round((cv$samples.z[[i]]*v$sx)+v$mx1,2)#Then sample values are compute with H1 mean and standard deviation
	y<-c()
	for(j in 1:v$n){
	  y<-c(y,(0.05/(v$ns+1))*i)
	}
	cv$samples.y[[i]]<-y
	cv$samples.x.m[[i]]<-round(mean(cv$samples.x[[i]]),2)#means of samples
	cv$samples.x.sd[[i]]<-round(sd(cv$samples.x[[i]]),2)#means of samples
	
	## Computation of confidence intervals for the mean µ ##    
	cv$confidence.t.limit.inf.bilat[[i]]<-round(v$mx0-qt(1-cv$alpha/2,v$n-1)*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the confidence lower limit when variance unknown
	cv$confidence.t.limit.sup.bilat[[i]]<-round(v$mx0+qt(1-cv$alpha/2,v$n-1)*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the confidence higher limit when variance unknown
	cv$confidence.t.limit.inf.unilat[[i]]<-round(v$mx0-qt(1-cv$alpha,v$n-1)*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the confidence lower limit when variance unknown
	cv$confidence.t.limit.sup.unilat[[i]]<-round(v$mx0+qt(1-cv$alpha,v$n-1)*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the confidence higher limit when variance unknown

      }
      if(v$takesample > SP$last.takesample.value){
	SP$samples.z<<-c(SP$samples.z,cv$samples.z)
	SP$samples.x<<-c(SP$samples.x,cv$samples.x)
	SP$samples.x.m<<-c(SP$samples.x.m,cv$samples.x.m)
	SP$samples.x.sd<<-c(SP$samples.x.sd,cv$samples.x.sd)
	SP$samples.y<<-c(SP$samples.y,cv$samples.y)
	
	SP$confidence.t.limit.inf.bilat<<-c(SP$confidence.t.limit.inf.bilat,cv$confidence.t.limit.inf.bilat)
	SP$confidence.t.limit.sup.bilat<<-c(SP$confidence.t.limit.sup.bilat,cv$confidence.t.limit.sup.bilat)
	SP$confidence.t.limit.inf.unilat<<-c(SP$confidence.t.limit.inf.unilat,cv$confidence.t.limit.inf.unilat)
	SP$confidence.t.limit.sup.unilat<<-c(SP$confidence.t.limit.sup.unilat,cv$confidence.t.limit.sup.unilat)
      }

      
      cv$n.samples<-length(SP$samples.z)
      cv$vect.n.samples<-c(1:cv$n.samples)
      
      for(i in 1:cv$n.samples){
	## Testing if mean of sample is or not in the confidence area
	
	if(v$dirtest == "bilat"){
	  ## Model K
	  if(SP$samples.x.m[[i]] < cv$confidence.k.limit.inf || SP$samples.x.m[[i]] > cv$confidence.k.limit.sup){
	    SP$test.k.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.k.conclusion[[i]]<-"nrh0"
	  }
	  ## Model Z
	  if(SP$samples.x.m[[i]] < cv$confidence.z.limit.inf || SP$samples.x.m[[i]] > cv$confidence.z.limit.sup){
	    SP$test.z.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.z.conclusion[[i]]<-"nrh0"
	  }
	  ## Model t
	  if(SP$samples.x.m[[i]] < SP$confidence.t.limit.inf.bilat[[i]] || SP$samples.x.m[[i]] > SP$confidence.t.limit.sup.bilat[[i]]){
	    SP$test.t.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.t.conclusion[[i]]<-"nrh0"
	  }
	}
	if(v$dirtest == "unilatg"){
	  ## Model K
	  if(SP$samples.x.m[[i]] < cv$confidence.k.limit.inf){
	    SP$test.k.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.k.conclusion[[i]]<-"nrh0"
	  }
	  ## Model Z
	  if(SP$samples.x.m[[i]] < cv$confidence.z.limit.inf){
	    SP$test.z.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.z.conclusion[[i]]<-"nrh0"
	  }
	  ## Model t
	  if(SP$samples.x.m[[i]] < SP$confidence.t.limit.inf.unilat[[i]]){
	    SP$test.t.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.t.conclusion[[i]]<-"nrh0"
	  }
	}
	if(v$dirtest == "unilatd"){
	   ## Model K
	  if(SP$samples.x.m[[i]] > cv$confidence.k.limit.sup){
	    SP$test.k.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.k.conclusion[[i]]<-"nrh0"
	  }
	  ## Model Z
	  if(SP$samples.x.m[[i]] > cv$confidence.z.limit.sup){
	    SP$test.z.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.z.conclusion[[i]]<-"nrh0"
	  }
	  ## Model t
	  if(SP$samples.x.m[[i]] > SP$confidence.t.limit.sup.unilat[[i]]){
	    SP$test.t.conclusion[[i]]<-"rh0"
	  } else {
	    SP$test.t.conclusion[[i]]<-"nrh0"
	  }
	}
	
	cv$test.k.conclusion.pcrh0.vect<-c(cv$test.k.conclusion.pcrh0.vect,round(length(which(SP$test.k.conclusion == "rh0"))/length(SP$test.k.conclusion),4)*100)
	cv$test.z.conclusion.pcrh0.vect<-c(cv$test.z.conclusion.pcrh0.vect,round(length(which(SP$test.z.conclusion == "rh0"))/length(SP$test.z.conclusion),4)*100)
	cv$test.t.conclusion.pcrh0.vect<-c(cv$test.t.conclusion.pcrh0.vect,round(length(which(SP$test.t.conclusion == "rh0"))/length(SP$test.t.conclusion),4)*100)

      }
    }
    if(length(SP$test.k.conclusion)>0){
    cv$test.k.conclusion.n.rh0<-length(which(SP$test.k.conclusion == "rh0"))
    cv$test.k.conclusion.n.nrh0<-cv$n.samples-cv$test.k.conclusion.n.rh0
    cv$test.k.conclusion.pc.rh0<-round(cv$test.k.conclusion.n.rh0/length(SP$test.k.conclusion),4)*100
    cv$test.k.conclusion.pc.nrh0<-100-cv$test.k.conclusion.pc.rh0
    
    cv$test.z.conclusion.n.rh0<-length(which(SP$test.z.conclusion == "rh0"))
    cv$test.z.conclusion.n.nrh0<-cv$n.samples-cv$test.z.conclusion.n.rh0
    cv$test.z.conclusion.pc.rh0<-round(cv$test.z.conclusion.n.rh0/length(SP$test.z.conclusion),4)*100
    cv$test.z.conclusion.pc.nrh0<-100-cv$test.z.conclusion.pc.rh0
    
    cv$test.t.conclusion.n.rh0<-length(which(SP$test.t.conclusion == "rh0"))
    cv$test.t.conclusion.n.nrh0<-cv$n.samples-cv$test.t.conclusion.n.rh0
    cv$test.t.conclusion.pc.rh0<-round(cv$test.t.conclusion.n.rh0/length(SP$test.t.conclusion),4)*100
    cv$test.t.conclusion.pc.nrh0<-100-cv$test.t.conclusion.pc.rh0
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
      
      cv$confidence.t.limit.inf.bilat.toshow<-SP$confidence.t.limit.inf.bilat[cv$samples.x.from:cv$samples.x.to]
      cv$confidence.t.limit.sup.bilat.toshow<-SP$confidence.t.limit.sup.bilat[cv$samples.x.from:cv$samples.x.to]
      
      cv$confidence.t.limit.inf.unilat.toshow<-SP$confidence.t.limit.inf.unilat[cv$samples.x.from:cv$samples.x.to]
      cv$confidence.t.limit.sup.unilat.toshow<-SP$confidence.t.limit.sup.unilat[cv$samples.x.from:cv$samples.x.to]
      
      cv$test.k.conclusion.toshow<-SP$test.k.conclusion[cv$samples.x.from:cv$samples.x.to]
      cv$test.z.conclusion.toshow<-SP$test.z.conclusion[cv$samples.x.from:cv$samples.x.to]
      cv$test.t.conclusion.toshow<-SP$test.t.conclusion[cv$samples.x.from:cv$samples.x.to]
      
      cv$samples.y.toshow<-list()
      if(length(cv$samples.x.toshow)>0){
	for(i in 1:length(cv$samples.x.toshow)){
	  cv$samples.y.toshow[[i]]<-list()
	  for(j in 1:length(cv$samples.x.toshow[[i]])){
	    cv$samples.y.toshow[[i]]<-c(cv$samples.y.toshow[[i]],c((cv$maxdmx/(v$nss+1))*i))#
	  }
	  cv$samples.y.toshow[[i]]<-as.vector(cv$samples.y.toshow[[i]],mode='numeric')
	  	  
	  ## Testing if IC covers µ0 or µ1
	cv$confidence.t.limit.inf[[i]]<-v$mx0-cv$ic.t*(cv$samples.x.sd.toshow[[i]]/sqrt(v$n))#compute the confidence lower limit when variance unknown
	cv$confidence.t.limit.sup[[i]]<-v$mx0+cv$ic.t*(cv$samples.x.sd.toshow[[i]]/sqrt(v$n))#compute the confidence higher limit when variance unknown
	t<-seq(-8,8,length=100)
	cv$xh0.t[[i]]<-(t*(cv$samples.x.sd.toshow[[i]]/sqrt(v$n)))+v$mx0
	cv$xh1.t[[i]]<-(t*(cv$samples.x.sd.toshow[[i]]/sqrt(v$n)))+v$mx1
	cv$yh0.t[[i]]<-dt(t,v$n-1)*(sqrt(v$n)/v$sx)
	cv$yh1.t[[i]]<-dt(t,v$n-1)*(sqrt(v$n)/v$sx)
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
    if(v$showh1){
      m<-matrix(c(1,2,2,3,4,5,6,7,8),3,3,byrow=TRUE)
      layout(m,width=c(6,1,3))
    } else {
      m<-matrix(c(1,2,2,3,4,5),2,3,byrow=TRUE)
      layout(m,width=c(6,1,3))
    }
    ##################
    ## Plot Reality ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showrh1h0){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20))
    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	#text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	mtext(bquote(paste(bar(x) == .(cv$samples.x.m.toshow[[i]]),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s == .(cv$samples.x.sd.toshow[[i]]),sep="")),side=4,line=7,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(1,signif(cv$maxdmx,1)*1.1,labels="Réalité",cex=1.4, pos=4)

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(paste(X*"~"* N(.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
      
      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    }



    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')#,main=bquote(paste("Calcul du % de ",RH[0]," et de ",NRH[0],sep="")),cex.main=1.5
    
    if(v$dirtest == "bilat"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(mu[0]-K,mu[0]+K),"]"),sep="")),cex=1.4,pos=4)
      text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]"),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(mu[0]-K,mu[0]+K),"]"),sep="")),cex=1.4,pos=4)
      text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]"),sep="")),cex=1.4,pos=4)
    }
    if(v$dirtest == "unilatg"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu >= mu[0]," , ",H[1]," : ",mu < mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu >= .(v$mx0)," , ",H[1]," : ",mu < .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) < mu[0]-K,sep="")),cex=1.4,pos=4)
      text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) < .(cv$confidence.k.limit.inf),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) >= mu[0]-K,sep="")),cex=1.4,pos=4)
      text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) >= .(cv$confidence.k.limit.inf),sep="")),cex=1.4,pos=4)
    }
    if(v$dirtest == "unilatd"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu <= mu[0]," , ",H[1]," : ",mu > mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu <= .(v$mx0)," , ",H[1]," : ",mu > .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) > mu[0]+K,sep="")),cex=1.4,pos=4)
      text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) > .(cv$confidence.k.limit.sup),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) <= mu[0]+K,sep="")),cex=1.4,pos=4)
      text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) <= .(cv$confidence.k.limit.sup),sep="")),cex=1.4,pos=4)
      
    }

    testmean<-data.frame(c(cv$test.k.conclusion.n.nrh0,cv$test.k.conclusion.pc.nrh0),c(" "," "),c(cv$test.k.conclusion.n.rh0,cv$test.k.conclusion.pc.rh0))
    colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
    rownames(testmean)<-c("n ","% ")
    addtable2plot(0,0,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.4,xjust=0,yjust=1)#,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]"))

    if(v$evolpcincmu){
      #title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
    }


    ##################
    ## Plot H0      ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    if(v$dirtest == "bilat"){
      title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(mu[0] - K , mu[0] + K),"]") == group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]") , sep="")) ,cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu == mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatg"){
      title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(mu[0] - K , infinity),"]") == group("[",list(.(cv$confidence.k.limit.inf),infinity),"]") , sep="")) ,cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu >= mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatd"){
      title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(- infinity , mu[0] + K),"]") == group("[",list(- infinity , .(cv$confidence.k.limit.sup)),"]") , sep="")),cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu <= mu[0],sep="")),cex=1.4, pos=4)
    }

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      #points(cv$xh0,cv$yh0,type="l")
      text(1,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[0] *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.6,labels=bquote(paste(bar(X) *"~"* N (.(v$mx0)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.4,labels=bquote(paste(alpha == .(cv$emp.alpha),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.2,labels=bquote(paste(1 - alpha == .(cv$emp.confidence),sep='')),cex=1.4, pos=4)
    }
    
    
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    
    if(v$showrh1h0){
      polygon(c(cv$emp.xh0.a,max(cv$emp.xh0.a)),c(cv$emp.yh0.a,0),col=color.false)
      polygon(c(min(cv$emp.xh0.b),cv$emp.xh0.b,max(cv$emp.xh0.b)),c(0,cv$emp.yh0.b,0),col=color.true)
      polygon(c(min(cv$emp.xh0.c),cv$emp.xh0.c),c(0,cv$emp.yh0.c),col=color.false)
    } else {
      if(v$dirtest == "bilat"){
      ## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
      if(v$dirtest == "unilatg"){
	polygon(c(0,0,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
      if(v$dirtest == "unilatd"){
	polygon(c(0,0,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
    }
    ## Confidence interval compute under H0 : limits
    if(v$dirtest == "bilat"){
      lines(x<-c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),y<-c(0,cv$maxdmx*1))
      lines(x<-c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),y<-c(0,cv$maxdmx*1))
    }
    if(v$dirtest == "unilatg"){
      lines(x<-c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),y<-c(0,cv$maxdmx*1))
    }
    if(v$dirtest == "unilatd"){
      lines(x<-c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),y<-c(0,cv$maxdmx*1))
    }

    mtext(bquote(paste(bar(x)," vs confiance => conclusion",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	if(v$dirtest == "bilat"){
	  if(cv$test.k.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) %notin% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.k.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) %in% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatg"){
	  if(cv$test.k.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) < .(cv$confidence.k.limit.inf) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.k.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) >= .(cv$confidence.k.limit.inf) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatd"){
	  if(cv$test.k.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) > .(cv$confidence.k.limit.sup) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.k.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) <= .(cv$confidence.k.limit.sup) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
      }
    }
    

      ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("NRHo"=cv$test.k.conclusion.pc.nrh0,"RHo"=cv$test.k.conclusion.pc.rh0)
      } else {
	includes<-c("NRHo"=0,"RHo"=0)#"µ1 ⊄ IC"=0
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)

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
      plot(cv$vect.n.samples,cv$test.k.conclusion.pcrh0.vect,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%",RH[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      lines(x<-c(0,npclim),y <- c(cv$emp.power*100,cv$emp.power*100),lty=3)
      text(npclim*0.01,(cv$emp.power*100)-5,expression(1-beta),pos=4)
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }

    
  if(v$showh1){
    ##################
    ## Plot H1     ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    
    if(v$dirtest == "bilat"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu != mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatg"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu < mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatd"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu > mu[0],sep="")),cex=1.4, pos=4)
    }

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      #points(cv$xh1,cv$yh1,type="l")
      text(1,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.6,labels=bquote(paste(bar(X) *"~"* N (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.4,labels=bquote(paste(beta == .(cv$emp.beta),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.2,labels=bquote(paste(1 - beta == .(cv$emp.power),sep='')),cex=1.4, pos=4)
    }
    
    if(v$showrh1h0){
      polygon(c(cv$emp.xh1.a,max(cv$emp.xh1.a)),c(cv$emp.yh1.a,0),col=color.true)
      polygon(c(min(cv$emp.xh1.b),cv$emp.xh1.b,max(cv$emp.xh1.b)),c(0,cv$emp.yh1.b,0),col=color.false)
      polygon(c(min(cv$emp.xh1.c),cv$emp.xh1.c),c(0,cv$emp.yh1.c),col=color.true)
    } else {
      if(v$dirtest == "bilat"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
      if(v$dirtest == "unilatg"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
      if(v$dirtest == "unilatd"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
    }
    ## Confidence interval compute under H0 : limits
    if(v$dirtest == "bilat"){
      lines(x<-c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),y<-c(0,cv$maxdmx*1))
      lines(x<-c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),y<-c(0,cv$maxdmx*1))
    }
    if(v$dirtest == "unilatg"){
      lines(x<-c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),y<-c(0,cv$maxdmx*1))
    }
    if(v$dirtest == "unilatd"){
      lines(x<-c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),y<-c(0,cv$maxdmx*1))
    }

    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    } 
    
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    
    
    ## Plot bar plot of includes %
    par(mai=c(0.5,0.5,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
    
    par(mai=c(0.5,0.5,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  }
    }, height = getPlotHeight)

########################################################################################
  output$plotZ <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5,6,7,7,8),3,4,byrow=TRUE)#1,2,2,3,4,5,6,7,8
      layout(m,width=c(4,2,1,3))#6,1,3
    } else {
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
      layout(m,width=c(4,2,1,3))
    }
    ##################
    ## Plot Reality ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showrh1h0){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20))

    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	mtext(bquote(paste(bar(x) == .(cv$samples.x.m.toshow[[i]]),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s == .(cv$samples.x.sd.toshow[[i]]),sep="")),side=4,line=7,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(1,signif(cv$maxdmx,1)*1.1,labels="Réalité",cex=1.4, pos=4)

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(paste(X*"~"* N(.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
      
      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    }



    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
    
    if(v$dirtest == "bilat"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(mu[0]-Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n)),mu[0]+Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex=1.4,pos=4)
      text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]"),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(mu[0]-Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n)),mu[0]+Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex=1.4,pos=4)
      text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]"),sep="")),cex=1.4,pos=4)
    }
    if(v$dirtest == "unilatg"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu >= mu[0]," , ",H[1]," : ",mu < mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu >= .(v$mx0)," , ",H[1]," : ",mu < .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) < mu[0]-Z[1-alpha] %.% frac(sigma,sqrt(n)),sep="")),cex=1.4,pos=4)
      text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) < .(cv$confidence.z.limit.inf),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) >= mu[0]-Z[1-alpha] %.% frac(sigma,sqrt(n)),sep="")),cex=1.4,pos=4)
      text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) >= .(cv$confidence.z.limit.inf),sep="")),cex=1.4,pos=4)
    }
    if(v$dirtest == "unilatd"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu <= mu[0]," , ",H[1]," : ",mu > mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu <= .(v$mx0)," , ",H[1]," : ",mu > .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) > mu[0]+Z[1-alpha] %.% frac(sigma,sqrt(n)),sep="")),cex=1.4,pos=4)
      text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) > .(cv$confidence.z.limit.sup),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) <= mu[0]+Z[1-alpha] %.% frac(sigma,sqrt(n)),sep="")),cex=1.4,pos=4)
      text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) <= .(cv$confidence.z.limit.sup),sep="")),cex=1.4,pos=4)
      
    }

    testmean<-data.frame(c(cv$test.z.conclusion.n.nrh0,cv$test.z.conclusion.pc.nrh0),c(" "," "),c(cv$test.z.conclusion.n.rh0,cv$test.z.conclusion.pc.rh0))
    colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
    rownames(testmean)<-c("n ","% ")
    addtable2plot(0,0,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.4,xjust=0,yjust=1)

    if(v$evolpcincmu){
      #title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
    }


    ##################
    ## Plot H0      ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    if(v$dirtest == "bilat"){
      title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(mu[0]-Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n)),mu[0]+Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n))),"]") == group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]"),sep="")),cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu == mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatg"){
      title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(mu[0]-Z[1-alpha] %.% frac(sigma,sqrt(n)),infinity),"]") == group("[",list(.(cv$confidence.z.limit.inf),infinity),"]"),sep="")),cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu >= mu[0],sep="")),cex=1.4, pos=4)
    } 
    if(v$dirtest == "unilatd"){
      title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(-infinity,mu[0]+Z[1-alpha] %.% frac(sigma,sqrt(n))),"]") == group("[",list(-infinity,.(cv$confidence.z.limit.sup)),"]"),sep="")),cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu <= mu[0],sep="")),cex=1.4, pos=4)
    } 

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      #points(cv$xh0,cv$yh0,type="l")
      text(1,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[0] *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.6,labels=bquote(paste(bar(X) *"~"* N (.(v$mx0)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.4,labels=bquote(paste(alpha == .(cv$alpha),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.2,labels=bquote(paste(1 - alpha == .(cv$confidence),sep='')),cex=1.4, pos=4)
    }
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)
    
    if(v$showrh1h0){
      polygon(c(cv$z.xh0.a,max(cv$z.xh0.a)),c(cv$z.yh0.a,0),col=color.false)
      polygon(c(min(cv$z.xh0.b),cv$z.xh0.b,max(cv$z.xh0.b)),c(0,cv$z.yh0.b,0),col=color.true)
      polygon(c(min(cv$z.xh0.c),cv$z.xh0.c),c(0,cv$z.yh0.c),col=color.false)
    } else {
      if(v$dirtest == "bilat"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
      if(v$dirtest == "unilatg"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
      if(v$dirtest == "unilatd"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
    }
    ## Confidence interval compute under H0
    if(v$dirtest == "bilat"){
      lines(x<-c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),y<-c(0,cv$maxdmx*1))
      lines(x<-c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),y<-c(0,cv$maxdmx*1))
     }
    if(v$dirtest == "unilatg"){
      lines(x<-c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),y<-c(0,cv$maxdmx*1))
    }
    if(v$dirtest == "unilatd"){
      lines(x<-c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),y<-c(0,cv$maxdmx*1))
    }
    
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }

    mtext(bquote(paste(bar(x)," vs confiance => conclusion",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	if(v$dirtest == "bilat"){
	  if(cv$test.z.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) %notin% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.z.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) %in% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatg"){
	  if(cv$test.z.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) < .(cv$confidence.z.limit.inf) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.z.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) >= .(cv$confidence.z.limit.inf) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatd"){
	  if(cv$test.z.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) > .(cv$confidence.z.limit.sup) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.z.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) <= .(cv$confidence.z.limit.sup) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
      }
    }
    
      ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("NRHo"=cv$test.z.conclusion.pc.nrh0,"RHo"=cv$test.z.conclusion.pc.rh0)
      } else {
	includes<-c("NRHo"=0,"RHo"=0)#"µ1 ⊄ IC"=0
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)

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
      plot(cv$vect.n.samples,cv$test.z.conclusion.pcrh0.vect,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%",RH[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      lines(x<-c(0,npclim),y <- c(cv$power*100,cv$power*100),lty=3)
      text(npclim*0.01,(cv$power*100)-5,expression(1-beta),pos=4)
    } else {
      par(mai=c(0.5,0.75,0,0.1))#,mfrow=c(2,1)
      plot(cv$z,cv$z.d,xlab=bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep="")),ylab="Densité",cex.lab=1.2,bty="n",xlim=c(-5,5),ylim=c(0,0.5),xaxp=c(-5,5,10),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2)
      axis(2,las=2,yaxp=c(0,0.4,4),cex.axis=1.2)
      #text(-4.9,0.35,bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep="")),pos=4,cex=1.4)
      text(-4.9,0.25,labels=bquote(paste(alpha == .(cv$alpha),sep='')),cex=1.4, pos=4)
      text(-4.9,0.20,labels=bquote(paste(1 - alpha == .(cv$confidence),sep='')),cex=1.4, pos=4)
      if(v$dirtest == "bilat"){
	lines(c(qnorm(cv$alpha/2),qnorm(cv$alpha/2)),c(0,dnorm(qnorm(cv$alpha/2))))
	text(qnorm(cv$alpha/2),dnorm(qnorm(cv$alpha/2))+0.05,bquote(paste(-Z[1-frac(alpha,2)] == .(round(qnorm(cv$alpha/2),2)),sep="")),cex=1.4)
	
	lines(c(qnorm(1-cv$alpha/2),qnorm(1-cv$alpha/2)),c(0,dnorm(qnorm(1-cv$alpha/2))))
	text(qnorm(1-cv$alpha/2),dnorm(qnorm(1-cv$alpha/2))+0.05,bquote(paste(Z[1-frac(alpha,2)] == .(round(qnorm(cv$alpha/2),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatg"){
	lines(c(qnorm(cv$alpha),qnorm(cv$alpha)),c(0,dnorm(qnorm(cv$alpha))))
	text(qnorm(cv$alpha),dnorm(qnorm(cv$alpha))+0.05,bquote(paste(-Z[1-alpha] == .(round(qnorm(cv$alpha),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatd"){
	lines(c(qnorm(1-cv$alpha),qnorm(1-cv$alpha)),c(0,dnorm(qnorm(1-cv$alpha))))
	text(qnorm(1-cv$alpha),dnorm(qnorm(1-cv$alpha))+0.05,bquote(paste(Z[1-alpha] == .(round(qnorm(1-cv$alpha),2)),sep="")),cex=1.4)
      }
    }

    
  if(v$showh1){  
    ##################
    ## Plot H1     ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    
    if(v$dirtest == "bilat"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu != mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatg"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu < mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatd"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu > mu[0],sep="")),cex=1.4, pos=4)
    }
    
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      #points(cv$z.xh1,cv$z.yh1,type="l")
      text(1,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.6,labels=bquote(paste(bar(X) *"~"* N (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.4,labels=bquote(paste(beta == .(cv$beta),sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.2,labels=bquote(paste(1 - beta == .(cv$power),sep='')),cex=1.4, pos=4)
    }


    if(v$showrh1h0){
      polygon(c(cv$z.xh1.a,max(cv$z.xh1.a)),c(cv$z.yh1.a,0),col=color.true)
      polygon(c(min(cv$z.xh1.b),cv$z.xh1.b,max(cv$z.xh1.b)),c(0,cv$z.yh1.b,0),col=color.false)
      polygon(c(min(cv$z.xh1.c),cv$z.xh1.c),c(0,cv$z.yh1.c),col=color.true)

    } else {
      if(v$dirtest == "bilat"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
      if(v$dirtest == "unilatg"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
      if(v$dirtest == "unilatd"){
	## Confidence interval compute under H0 : polygones
	polygon(c(0,0,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,100,100),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
    }
    ## Confidence interval compute under H0
    if(v$dirtest == "bilat"){
      lines(x<-c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),y<-c(0,cv$maxdmx*1))
      lines(x<-c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),y<-c(0,cv$maxdmx*1))
     }
    if(v$dirtest == "unilatg"){
      lines(x<-c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),y<-c(0,cv$maxdmx*1))
    }
    if(v$dirtest == "unilatd"){
      lines(x<-c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),y<-c(0,cv$maxdmx*1))
    }
    
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }
    
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    ## Plot bar plot of includes %
    #par(mai=c(0.5,0.5,0,0))
    #plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
    
      par(mai=c(0.5,0.6,0.5,0.1))#,mfrow=c(2,1)
      plot(cv$z.diff.mu,cv$z.power,xlab=bquote(paste("| ",mu-mu[0]," |",sep="")),ylab="Puissance",bty="n",xlim=c(-50,50),xaxp=c(-50,50,10),ylim=c(0,1),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2,cex.lab=1.2)#
      axis(2,las=2,yaxp=c(0,1,4),ylim=c(0,1),cex.axis=1.2)
      lines(c(v$mx0-v$mx1,v$mx0-v$mx1),c(0,cv$power),lty=3)
      lines(c(-50,v$mx0-v$mx1),c(cv$power,cv$power),lty=3)
      
      par(mai=c(0.5,0.75,0,0.1))#,mfrow=c(2,1)
      plot(cv$z,cv$z.d,xlab=bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep="")),ylab="Densité",cex.lab=1.2,bty="n",xlim=c(-5,5),ylim=c(0,0.5),xaxp=c(-5,5,10),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2)
      axis(2,las=2,yaxp=c(0,0.4,4),cex.axis=1.2)
      #text(-4.9,0.35,bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep="")),pos=4,cex=1.4)
      text(-4.9,0.25,labels=bquote(paste(beta == .(cv$beta),sep='')),cex=1.4, pos=4)
      text(-4.9,0.20,labels=bquote(paste(1 - beta == .(cv$power),sep='')),cex=1.4, pos=4)
      if(v$dirtest == "bilat"){
	lines(c(cv$z.z.lim.inf.h1,cv$z.z.lim.inf.h1),c(0,dnorm(cv$z.z.lim.inf.h1)))
	text(cv$z.z.lim.inf.h1,dnorm(cv$z.z.lim.inf.h1)+0.05,bquote(paste(Z[1] == .(round(cv$z.z.lim.inf.h1,2)),sep="")),cex=1.4)
	
	lines(c(cv$z.z.lim.sup.h1,cv$z.z.lim.sup.h1),c(0,dnorm(cv$z.z.lim.sup.h1)))
	text(cv$z.z.lim.sup.h1,dnorm(cv$z.z.lim.sup.h1)+0.05,bquote(paste(Z[2] == .(round(cv$z.z.lim.sup.h1,2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatg"){
	lines(c(qnorm(cv$alpha),qnorm(cv$alpha)),c(0,dnorm(qnorm(cv$alpha))))
	text(qnorm(cv$alpha),dnorm(qnorm(cv$alpha))+0.05,bquote(paste(-Z[1-alpha] == .(round(qnorm(cv$alpha),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatd"){
	lines(c(qnorm(1-cv$alpha),qnorm(1-cv$alpha)),c(0,dnorm(qnorm(1-cv$alpha))))
	text(qnorm(1-cv$alpha),dnorm(qnorm(1-cv$alpha))+0.05,bquote(paste(Z[1-alpha] == .(round(qnorm(1-cv$alpha),2)),sep="")),cex=1.4)
      }
 
      
      #   cv$z.diff.mu   cv$z.power

  }
    }, height = getPlotHeight)
########################################################################################
  output$plotT <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$showh1){
      m<-matrix(c(1,2,2,3,4,5,6,7,8),3,3,byrow=TRUE)
      layout(m,width=c(6,1,3))
    } else {
      m<-matrix(c(1,2,2,3,4,5),2,3,byrow=TRUE)
      layout(m,width=c(6,1,3))
    }
    ##################
    ## Plot Reality ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showrh1h0){
      label<-"Density"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(0,100,20))

    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	mtext(bquote(paste(bar(x) == .(cv$samples.x.m.toshow[[i]]),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s == .(cv$samples.x.sd.toshow[[i]]),sep="")),side=4,line=7,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(1,signif(cv$maxdmx,1)*1.1,labels="Réalité",cex=1.4, pos=4)

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(1,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      text(1,signif(cv$maxdmx,1)*0.7,labels=bquote(paste(X*"~"* N(.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
      
      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    }



    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')

    if(v$dirtest == "bilat"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(mu[0]-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(sigma,sqrt(n)),mu[0]+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex=1.4,pos=4)
      #text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]"),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(mu[0]-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(sigma,sqrt(n)),mu[0]+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex=1.4,pos=4)
      #text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]"),sep="")),cex=1.4,pos=4)
    }
    if(v$dirtest == "unilatg"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu >= mu[0]," , ",H[1]," : ",mu < mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu >= .(v$mx0)," , ",H[1]," : ",mu < .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) < mu[0]-t[group("(",list(n-1,1-alpha),")")] %.% frac(sigma,sqrt(n)),sep="")),cex=1.4,pos=4)
      #text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) < .(cv$confidence.z.limit.inf),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) >= mu[0]-t[group("(",list(n-1,1-alpha),")")] %.% frac(sigma,sqrt(n)),sep="")),cex=1.4,pos=4)
      #text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) >= .(cv$confidence.z.limit.inf),sep="")),cex=1.4,pos=4)
    }
    if(v$dirtest == "unilatd"){
      text(0,1,bquote(paste("Hypothèses : ",H[0]," : ",mu <= mu[0]," , ",H[1]," : ",mu > mu[0],sep="")),cex=1.4,pos=4)
      text(0.6,1,bquote(paste(H[0]," : ",mu <= .(v$mx0)," , ",H[1]," : ",mu > .(v$mx0),sep="")),cex=1.4,pos=4)
      
      text(0,0.8,labels=bquote(paste(RH[0]," si ",bar(x) > mu[0]+t[group("(",list(n-1,1-alpha),")")] %.% frac(sigma,sqrt(n)),sep="")),cex=1.4,pos=4)
      #text(0.6,0.8,labels=bquote(paste(RH[0]," si ",bar(x) > .(cv$confidence.z.limit.sup),sep="")),cex=1.4,pos=4)
      
      text(0,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) <= mu[0]+t[group("(",list(n-1,1-alpha),")")] %.% frac(sigma,sqrt(n)),sep="")),cex=1.4,pos=4)
      #text(0.6,0.6,labels=bquote(paste(NRH[0]," si ",bar(x) <= .(cv$confidence.z.limit.sup),sep="")),cex=1.4,pos=4)
      
    }

    testmean<-data.frame(c(cv$test.t.conclusion.n.nrh0,cv$test.t.conclusion.pc.nrh0),c(" "," "),c(cv$test.t.conclusion.n.rh0,cv$test.t.conclusion.pc.rh0))
    colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
    rownames(testmean)<-c("n ","% ")
    addtable2plot(0,0,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.4,xjust=0,yjust=1)

    if(v$evolpcincmu){
      #title(main=bquote(paste("Evolution des % de recouvrement",sep="")),cex.main=1.5)
    }

    ##################
    ## Plot H0      ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    if(v$dirtest == "bilat"){
      title(main=bquote(paste("Confiance sous ",H[0]," : [ ",mu[0]-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n))," , ",mu[0]+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n)),"]",sep="")),cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu == mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatg"){
      title(main=bquote(paste("Confiance sous ",H[0]," : [ ",mu[0]-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n))," , ",infinity,"]",sep="")),cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu >= mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatd"){
      title(main=bquote(paste("Confiance sous ",H[0]," : [ ",- infinity," , ",mu[0]+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n)),"]",sep="")),cex.main=1.5)
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu <= mu[0],sep="")),cex=1.4, pos=4)
    }

    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)


    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
      	if(i == length(cv$samples.x.toshow)){
	  if(v$showrh1h0){
	    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
	    #points(cv$xh0.t[[i]],cv$yh0.t[[i]],type="l")
	    #text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* frac(sigma^2,sqrt(n)) ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
	  }
	}
	ic.halfheight=(cv$maxdmx/(v$nss+1))/2
	## Confidence interval compute under H0 : polygones
	if(v$dirtest == "bilat"){
	  polygon(c(0,0,cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.inf.bilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	  polygon(c(cv$confidence.t.limit.sup.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]],100,100),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	  polygon(c(cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	}
	if(v$dirtest == "unilatg"){
	  polygon(c(0,0,cv$confidence.t.limit.inf.unilat.toshow[[i]],cv$confidence.t.limit.inf.unilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	  polygon(c(cv$confidence.t.limit.inf.unilat.toshow[[i]],cv$confidence.t.limit.inf.unilat.toshow[[i]],100,100),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	}
	if(v$dirtest == "unilatd"){
	  polygon(c(0,0,cv$confidence.t.limit.sup.unilat.toshow[[i]],cv$confidence.t.limit.sup.unilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	  polygon(c(cv$confidence.t.limit.sup.unilat.toshow[[i]],cv$confidence.t.limit.sup.unilat.toshow[[i]],100,100),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	}

	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }

    mtext(bquote(paste(bar(x)," vs confiance => conclusion",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	if(v$dirtest == "bilat"){
	  if(cv$test.t.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) %notin% group("[",list(.(cv$confidence.t.limit.inf.bilat.toshow[[i]]),.(cv$confidence.t.limit.sup.bilat.toshow[[i]])),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.t.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) %in% group("[",list(.(cv$confidence.t.limit.inf.bilat.toshow[[i]]),.(cv$confidence.t.limit.sup.bilat.toshow[[i]])),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatg"){
	  if(cv$test.t.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) < .(cv$confidence.t.limit.inf.unilat.toshow[[i]]) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.t.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) >= .(cv$confidence.t.limit.inf.unilat.toshow[[i]]) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatd"){
	  if(cv$test.t.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) > .(cv$confidence.t.limit.sup.unilat.toshow[[i]]) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.t.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(cv$samples.x.m.toshow[[i]]) <= .(cv$confidence.t.limit.sup.unilat.toshow[[i]]) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
      }
    }

      ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("NRHo"=cv$test.t.conclusion.pc.nrh0,"RHo"=cv$test.t.conclusion.pc.rh0)
      } else {
	includes<-c("NRHo"=0,"RHo"=0)#"µ1 ⊄ IC"=0
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,120),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)

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
      plot(cv$vect.n.samples,cv$test.t.conclusion.pcrh0.vect,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%",RH[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      #lines(x<-c(0,npclim),y <- c(cv$power*100,cv$power*100),lty=3)
      #text(npclim*0.01,(cv$power*100)-5,expression(1-beta),pos=4)
    } else {
      par(mai=c(0.5,0.5,0,0.1))#,mfrow=c(2,1)
      plot(cv$t,cv$t.d,xlab="",ylab="",bty="n",xlim=c(-5,5),ylim=c(0,0.5),xaxp=c(-5,5,10),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2)
      axis(2,las=2,yaxp=c(0,0.4,4),cex.axis=1.2)
      text(-4.9,0.35,bquote(paste(t *"~"* t[(n-1)] ,sep="")),pos=4,cex=1.4)
      text(-4.9,0.25,labels=bquote(paste(alpha == .(cv$alpha),sep='')),cex=1.4, pos=4)
      text(-4.9,0.20,labels=bquote(paste(1 - alpha == .(cv$confidence),sep='')),cex=1.4, pos=4)
      if(v$dirtest == "bilat"){
	lines(c(qt(cv$alpha/2,v$n-1),qt(cv$alpha/2,v$n-1)),c(0,dt(qt(cv$alpha/2,v$n-1),v$n-1)))
	text(qt(cv$alpha/2,v$n-1),dt(qt(cv$alpha/2,v$n-1),v$n-1)+0.05,bquote(paste(-t[group("(",list(n-1,1-frac(alpha,2)),")")] == .(round(qt(cv$alpha/2,v$n-1),2)),sep="")),cex=1.4)
	
	lines(c(qt(1-cv$alpha/2,v$n-1),qt(1-cv$alpha/2,v$n-1)),c(0,dt(qt(1-cv$alpha/2,v$n-1),v$n-1)))
	text(qt(1-cv$alpha/2,v$n-1),dt(qt(1-cv$alpha/2,v$n-1),v$n-1)+0.05,bquote(paste(t[group("(",list(n-1,1-frac(alpha,2)),")")] == .(round(qt(cv$alpha/2,v$n-1),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatg"){
	lines(c(qt(cv$alpha,v$n-1),qt(cv$alpha,v$n-1)),c(0,dt(qt(cv$alpha,v$n-1),v$n-1)))
	text(qt(cv$alpha,v$n-1),dt(qt(cv$alpha,v$n-1),v$n-1)+0.05,bquote(paste(-t[group("(",list(n-1,1-alpha),")")] == .(round(qt(cv$alpha,v$n-1),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatd"){
	lines(c(qt(1-cv$alpha,v$n-1),qt(1-cv$alpha,v$n-1)),c(0,dt(qt(1-cv$alpha,v$n-1),v$n-1)))
	text(qt(1-cv$alpha,v$n-1),dt(qt(1-cv$alpha,v$n-1),v$n-1)+0.05,bquote(paste(t[group("(",list(n-1,1-alpha),")")] == .(round(qt(1-cv$alpha,v$n-1),2)),sep="")),cex=1.4)
      }
    }
 
 
  if(v$showh1){
    ##################
    ## Plot H1     ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))

    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(0,100),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(0,100,20))
    
    if(v$dirtest == "bilat"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu != mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatg"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu < mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatd"){
      text(1,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu > mu[0],sep="")),cex=1.4, pos=4)
    }
    
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    
    if(length(cv$samples.x.toshow)>0){
      for(i in 1:length(cv$samples.x.toshow)){
	if(i == length(cv$samples.x.toshow)){
	  if(v$showrh1h0){
	    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
	    #points(cv$xh1.t[[i]],cv$yh1.t[[i]],type="l")
	    #text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* frac(sigma^2,sqrt(n)) ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
	  }
	}

	## Confidence interval compute under H0 : polygones
	if(v$dirtest == "bilat"){
	  polygon(c(0,0,cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.inf.bilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	  polygon(c(cv$confidence.t.limit.sup.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]],100,100),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	  polygon(c(cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	}
	if(v$dirtest == "unilatg"){
	  polygon(c(0,0,cv$confidence.t.limit.inf.unilat.toshow[[i]],cv$confidence.t.limit.inf.unilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	  polygon(c(cv$confidence.t.limit.inf.unilat.toshow[[i]],cv$confidence.t.limit.inf.unilat.toshow[[i]],100,100),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	}
	if(v$dirtest == "unilatd"){
	  polygon(c(0,0,cv$confidence.t.limit.sup.unilat.toshow[[i]],cv$confidence.t.limit.sup.unilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	  polygon(c(cv$confidence.t.limit.sup.unilat.toshow[[i]],cv$confidence.t.limit.sup.unilat.toshow[[i]],100,100),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	}
	  
	text(cv$samples.x.m.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }
    
    ## Plot bar plot of includes %
    par(mai=c(0.5,0.5,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')

    par(mai=c(0.5,0.5,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  } 
    }, height = getPlotHeight)
    
  output$DataTable <- renderTable({
    v<-getInputValues()
    cv<-getComputedValues()
    ## Transpose the sample list
    if(length(SP$samples.x)>0){
      samples.as.list<-list()
      for(i in 1:length(SP$samples.x)){
	samples.as.list[[i]]<-c(round(SP$samples.x[[i]],2),c(""),round(SP$samples.x.m[[i]],2),round(SP$samples.x.sd[[i]],2))
      }
      samples.as.matrix<- do.call(rbind,samples.as.list) 
      transposed.samples<-lapply(seq_len(ncol(samples.as.matrix)),function(i) samples.as.matrix[,i]) 
      d<-data.frame(transposed.samples)
      colnames(d)<-c(paste("X",1:v$n,sep="")," ","Moy","SD")
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
    paste("Tab",input$Tabset," | ",min(cv$z.power),sep=" ")
  })
  
  output$test3 <- renderText({
    v<-getInputValues()
    cv<-getComputedValues()
    paste("Tab",input$Tabset, " | ", sep=" ")
  })
  
})
