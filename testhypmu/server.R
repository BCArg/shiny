## testhypmu Shiny/R app server.R                                           
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
cex.hypoth<-1.8#size of hypothesis descriptions
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
    if (input$takesample != 0) {#input$takesample > rv$last.takesample.value
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

    ## Quantiles for plot curves
    cv$z<-seq(-5,5,length=100)
    cv$z.d<-dnorm(cv$z)
    
    cv$t<-seq(-5,5,length=100)
    cv$t.d<-dt(cv$t,v$n-1)
    
    cv$z.diff.mu<-seq(-50,50,length=500)# x values for computation of power vector -> see lower cv$z.power in each dirtest

    ## Computation of x y coordinates for Normal curve of Reality
    cv$xr<-(cv$z*v$sx)+v$mx1 #x for Reality
    cv$yr<-dnorm(cv$xr,mean=v$mx1,sd=v$sx)#y for Reality
    
    ## Computation of alpha, beta, confidence and power related variables  ##
    cv$confidence<-signif(v$confidence,2)
    cv$alpha<-signif(1-cv$confidence,2)#Computation of alpha probability
    
    ## Set z and t statistics for confidence intervals
    cv$ic.z<-qnorm(1-cv$alpha/2)#z positive limit of a bidirectionnal confidence interval in N(0,1) => for CI with known variance
    cv$ic.t<-qt(1-cv$alpha/2,v$n-1)#t positive limit of a bidirectionnal confidence interval in t(n-1) => for CI with unknown variance
    
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
    
    ## Computation of confidence limits
    cv$confidence.k.limit.inf=round(v$mx0-v$k,2)#compute the confidence lower limit with empiric k value
    cv$confidence.k.limit.sup=round(v$mx0+v$k,2)#compute the confidence higher limit with empiric k values

    
    ### Normal var known model ###
     
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
      
      ## Computation of confidence limits
      cv$confidence.z.limit.inf<-round(v$mx0-qnorm(1-cv$alpha)*(v$sx/sqrt(v$n)),2)#compute the confidence lower limit when variance known
      # Compute of confidence interval for unknown variance : see samples variable scomputations as it depends of mean of each samples
      
      ## Computation of power vector for evolution of power in function of µ - µ0
      cv$z.power<-1-pnorm(qnorm(1-cv$alpha,mean=0,sd=1)-((cv$z.diff.mu*-1)/(v$sx/sqrt(v$n))),mean=0,sd=1)
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
      cv$z.power<-1-pnorm(qnorm(1-cv$alpha,mean=0,sd=1)-((cv$z.diff.mu)/(v$sx/sqrt(v$n))),mean=0,sd=1)
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
    cv$power<-signif(cv$z.p.lim.inf.h1+(1-cv$z.p.lim.sup.h1),2)
    cv$beta<-signif(1-cv$power,2)
  
#     Quantiles plots for Z

      cv$z.zh0.a<-seq(-5,cv$z.z.lim.inf.h0,length=100)
      cv$z.xh0.a<-(cv$z.zh0.a*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
      cv$z.yh0.a<-dnorm(cv$z.xh0.a,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0

      cv$z.zh0.b<-seq(cv$z.z.lim.inf.h0,cv$z.z.lim.sup.h0,length=100)
      cv$z.xh0.b<-(cv$z.zh0.b*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
      cv$z.yh0.b<-dnorm(cv$z.xh0.b,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0

      cv$z.zh0.c<-seq(cv$z.z.lim.sup.h0,5,length=100)
      cv$z.xh0.c<-(cv$z.zh0.c*(v$sx/sqrt(v$n)))+v$mx0 #x for H1
      cv$z.yh0.c<-dnorm(cv$z.xh0.c,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0

      cv$z.xh0<-(cv$z*(v$sx/sqrt(v$n)))+v$mx0 #x for H0
      cv$z.yh0<-dnorm(cv$z.xh0,mean=v$mx0,sd=v$sx/sqrt(v$n))#y for H0

      cv$z.zh1.a<-seq(-5,cv$z.z.lim.inf.h1,length=100)
      cv$z.xh1.a<-(cv$z.zh1.a*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
      cv$z.yh1.a<-dnorm(cv$z.xh1.a,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1

      cv$z.zh1.b<-seq(cv$z.z.lim.inf.h1,cv$z.z.lim.sup.h1,length=100)
      cv$z.xh1.b<-(cv$z.zh1.b*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
      cv$z.yh1.b<-dnorm(cv$z.xh1.b,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1

      cv$z.zh1.c<-seq(cv$z.z.lim.sup.h1,5,length=100)
      cv$z.xh1.c<-(cv$z.zh1.c*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
      cv$z.yh1.c<-dnorm(cv$z.xh1.c,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1

      cv$z.xh1<-(cv$z*(v$sx/sqrt(v$n)))+v$mx1 #x for H1
      cv$z.yh1<-dnorm(cv$z.xh1,mean=v$mx1,sd=v$sx/sqrt(v$n))#y for H1
	
	### Normal var unknown model ###
	
	if(v$dirtest == "bilat"){
	  cv$t.t.lim.inf.h0<-qt(cv$alpha/2,v$n-1)
	  cv$t.t.lim.sup.h0<-qt(1-cv$alpha/2,v$n-1)
	}
	
	if(v$dirtest == "unilatg"){
	  cv$t.t.lim.inf.h0<-qt(cv$alpha,v$n-1)
	  cv$t.t.lim.sup.h0<-10
	}
	
	if(v$dirtest == "unilatd"){
	  cv$t.t.lim.inf.h0<--10
	  cv$t.t.lim.sup.h0<-qt(1-cv$alpha,v$n-1)
	}
	
	#Quantiles plots for t
	cv$t.th0.a<-seq(-10,cv$t.t.lim.inf.h0,length=100)
	cv$t.yh0.a<-dt(cv$t.th0.a,v$n-1)
	
	cv$t.th0.b<-seq(cv$t.t.lim.inf.h0,cv$t.t.lim.sup.h0,length=100)
	cv$t.yh0.b<-dt(cv$t.th0.b,v$n-1)
	
	cv$t.th0.c<-seq(cv$t.t.lim.sup.h0,10,length=100)
	cv$t.yh0.c<-dt(cv$t.th0.c,v$n-1)
	
	#H1 values will be compute for each samples as it depend upon S
	  
    ## Computation of maximal density for plots
    cv$maxdmx<-0.05
    if(max(cv$z.yh1,cv$z.yh0) > 0.05){
      cv$maxdmx<-max(cv$z.yh1,cv$z.yh0)
    }

    ## Computation of parameters of distribution of mean of samples 
#    cv$sx.dech<-v$sx/sqrt(v$n)#standard deviation of mean samples distributions
#    cv$vx.dech<-cv$sx.dech^2
    
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
    
    cv$n.samples<-length(rv$samples.z)
    cv$vect.n.samples<-c()
    cv$samples.x.n.toshow<-0
    
    cv$t.x.lim.inf.h1<-list()
    cv$t.x.lim.sup.h1<-list()
    
    cv$t.t.lim.inf.h1<-list()
    cv$t.t.lim.sup.h1<-list()
    
    cv$t.th1.a<-list()
    cv$t.th1.b<-list()
    cv$t.th1.c<-list()
    
    cv$t.yh1.a<-list()
    cv$t.yh1.b<-list()
    cv$t.yh1.c<-list()
    
    cv$power.t<-list()
    cv$beta.t<-list()
    cv$t.power<-list()
        
    if(cv$n.samples>0){ #rv$lastAction=='takesample'
      cv$vect.n.samples<-c(1:cv$n.samples)
      for(i in 1:cv$n.samples){
	cv$samples.x[[i]]<-round((rv$samples.z[[i]]*v$sx)+v$mx1,2)#Then sample values are compute with H1 mean and standard deviation
	if(v$forceh0){cv$samples.x.h0[[i]]<-round((rv$samples.z[[i]]*v$sx)+v$mx0,2)}
	y<-c()
	for(j in 1:v$n){
	  y<-c(y,(0.05/(v$ns+1))*i)
	}
	cv$samples.y[[i]]<-y
	cv$samples.x.m[[i]]<-round(mean(cv$samples.x[[i]]),2)#means of samples
	cv$samples.x.sd[[i]]<-round(sd(cv$samples.x[[i]]),2)#standard deviation of samples
	
	if(v$forceh0){
	  cv$samples.x.m.h0[[i]]<-round(mean(cv$samples.x.h0[[i]]),2)#means of samples
	}
	
	## Computation of confidence intervals for the mean µ ##    
	cv$confidence.t.limit.inf.bilat[[i]]<-round(v$mx0-qt(1-cv$alpha/2,v$n-1)*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the confidence lower limit when variance unknown
	cv$confidence.t.limit.sup.bilat[[i]]<-round(v$mx0+qt(1-cv$alpha/2,v$n-1)*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the confidence higher limit when variance unknown
	cv$confidence.t.limit.inf.unilat[[i]]<-round(v$mx0-qt(1-cv$alpha,v$n-1)*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the confidence lower limit when variance unknown
	cv$confidence.t.limit.sup.unilat[[i]]<-round(v$mx0+qt(1-cv$alpha,v$n-1)*(cv$samples.x.sd[[i]]/sqrt(v$n)),2)#compute the confidence higher limit when variance unknown

	## Testing if mean of sample is or not in the confidence area
	
	if(v$dirtest == "bilat"){
	  ## Model K
	  if(cv$samples.x.m[[i]] < cv$confidence.k.limit.inf || cv$samples.x.m[[i]] > cv$confidence.k.limit.sup){
	    cv$test.k.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.k.conclusion[[i]]<-"nrh0"
	  }

	  ## Model Z
	  if(cv$samples.x.m[[i]] < cv$confidence.z.limit.inf || cv$samples.x.m[[i]] > cv$confidence.z.limit.sup){
	    cv$test.z.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.z.conclusion[[i]]<-"nrh0"
	  }

	  ## Model t
	  if(cv$samples.x.m[[i]] < cv$confidence.t.limit.inf.bilat[[i]] || cv$samples.x.m[[i]] > cv$confidence.t.limit.sup.bilat[[i]]){
	    cv$test.t.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.t.conclusion[[i]]<-"nrh0"
	  }
	  
	  if(v$forceh0){
	    ## Model K
	    if(cv$samples.x.m.h0[[i]] < cv$confidence.k.limit.inf || cv$samples.x.m.h0[[i]] > cv$confidence.k.limit.sup){
	      cv$test.k.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.k.conclusion.h0[[i]]<-"nrh0"
	    }

	    ## Model Z
	    if(cv$samples.x.m.h0[[i]] < cv$confidence.z.limit.inf || cv$samples.x.m.h0[[i]] > cv$confidence.z.limit.sup){
	      cv$test.z.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.z.conclusion.h0[[i]]<-"nrh0"
	    }

	    ## Model t
	    if(cv$samples.x.m.h0[[i]] < cv$confidence.t.limit.inf.bilat[[i]] || cv$samples.x.m.h0[[i]] > cv$confidence.t.limit.sup.bilat[[i]]){
	      cv$test.t.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.t.conclusion.h0[[i]]<-"nrh0"
	    }
	  }

	}
	if(v$dirtest == "unilatg"){
	  ## Model K
	  if(cv$samples.x.m[[i]] < cv$confidence.k.limit.inf){
	    cv$test.k.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.k.conclusion[[i]]<-"nrh0"
	  }

	  ## Model Z
	  if(cv$samples.x.m[[i]] < cv$confidence.z.limit.inf){
	    cv$test.z.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.z.conclusion[[i]]<-"nrh0"
	  }

	  ## Model t
	  if(cv$samples.x.m[[i]] < cv$confidence.t.limit.inf.unilat[[i]]){
	    cv$test.t.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.t.conclusion[[i]]<-"nrh0"
	  }
	  
	  if(v$forceh0){
	    ## Model K
	    if(cv$samples.x.m.h0[[i]] < cv$confidence.k.limit.inf){
	      cv$test.k.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.k.conclusion.h0[[i]]<-"nrh0"
	    }

	    ## Model Z
	    if(cv$samples.x.m.h0[[i]] < cv$confidence.z.limit.inf){
	      cv$test.z.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.z.conclusion.h0[[i]]<-"nrh0"
	    }

	    ## Model t
	    if(cv$samples.x.m.h0[[i]] < cv$confidence.t.limit.inf.unilat[[i]]){
	      cv$test.t.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.t.conclusion.h0[[i]]<-"nrh0"
	    }
	  }
	}
	if(v$dirtest == "unilatd"){
	   ## Model K
	  if(cv$samples.x.m[[i]] > cv$confidence.k.limit.sup){
	    cv$test.k.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.k.conclusion[[i]]<-"nrh0"
	  }

	  ## Model Z
	  if(cv$samples.x.m[[i]] > cv$confidence.z.limit.sup){
	    cv$test.z.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.z.conclusion[[i]]<-"nrh0"
	  }

	  ## Model t
	  if(cv$samples.x.m[[i]] > cv$confidence.t.limit.sup.unilat[[i]]){
	    cv$test.t.conclusion[[i]]<-"rh0"
	  } else {
	    cv$test.t.conclusion[[i]]<-"nrh0"
	  }

	  if(v$forceh0){
	    ## Model K
	    if(cv$samples.x.m.h0[[i]] > cv$confidence.k.limit.sup){
	      cv$test.k.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.k.conclusion.h0[[i]]<-"nrh0"
	    }

	    ## Model Z
	    if(cv$samples.x.m.h0[[i]] > cv$confidence.z.limit.sup){
	      cv$test.z.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.z.conclusion.h0[[i]]<-"nrh0"
	    }

	    ## Model t
	    if(cv$samples.x.m.h0[[i]] > cv$confidence.t.limit.sup.unilat[[i]]){
	      cv$test.t.conclusion.h0[[i]]<-"rh0"
	    } else {
	      cv$test.t.conclusion.h0[[i]]<-"nrh0"
	    }
	  }
	}

      cv$test.k.conclusion.pcrh0.vect<-c(cv$test.k.conclusion.pcrh0.vect,round(length(which(cv$test.k.conclusion == "rh0"))/i,4)*100)
      cv$test.z.conclusion.pcrh0.vect<-c(cv$test.z.conclusion.pcrh0.vect,round(length(which(cv$test.z.conclusion == "rh0"))/i,4)*100)
      cv$test.t.conclusion.pcrh0.vect<-c(cv$test.t.conclusion.pcrh0.vect,round(length(which(cv$test.t.conclusion == "rh0"))/i,4)*100)

      ## Computation of power quantiles limits for each sample
      ## For each sample compute corresponding t values in H1 then compute power
      #cv$t.t.lim.inf.h0 is already set by v$dirtest
      cv$t.x.lim.inf.h1[[i]]<-(cv$t.t.lim.inf.h0*(cv$samples.x.sd[[i]]/sqrt(v$n)))+v$mx0
      cv$t.x.lim.sup.h1[[i]]<-(cv$t.t.lim.sup.h0*(cv$samples.x.sd[[i]]/sqrt(v$n)))+v$mx0

      #Compute now equivalent as t in H1
      cv$t.t.lim.inf.h1[[i]]<-(cv$t.x.lim.inf.h1[[i]]-v$mx1)/(cv$samples.x.sd[[i]]/sqrt(v$n))
      cv$t.t.lim.sup.h1[[i]]<-(cv$t.x.lim.sup.h1[[i]]-v$mx1)/(cv$samples.x.sd[[i]]/sqrt(v$n))
      
      cv$t.th1.a[[i]]<-seq(-5,cv$t.t.lim.inf.h1[[i]],length=100)
      cv$t.yh1.a[[i]]<-dt(cv$t.th1.a[[i]],v$n-1)
      
      cv$t.th1.b[[i]]<-seq(cv$t.t.lim.inf.h1[[i]],cv$t.t.lim.sup.h1[[i]],length=100)
      cv$t.yh1.b[[i]]<-dt(cv$t.th1.b[[i]],v$n-1)
      
      cv$t.th1.c[[i]]<-seq(cv$t.t.lim.sup.h1[[i]],5,length=100)
      cv$t.yh1.c[[i]]<-dt(cv$t.th1.c[[i]],v$n-1)
      
      cv$beta.t[[i]]<-round(pt(cv$t.t.lim.sup.h1[[i]],v$n-1)-pt(cv$t.t.lim.inf.h1[[i]],v$n-1),2)
      cv$power.t[[i]]<-round(1-cv$beta.t[[i]],2)

      ## Computation of power vector for evolution of power in function of µ - µ0
      if(v$dirtest == "bilat"){
	cv$t.power<-(1-pt(qt(1-cv$alpha/2,v$n-1)-((cv$z.diff.mu*-1)/(cv$samples.x.sd[[i]]/sqrt(v$n))),v$n-1)) + (1-pt(qt(1-cv$alpha/2,v$n-1)-(cv$z.diff.mu/(cv$samples.x.sd[[i]]/sqrt(v$n))),v$n-1))
      }
      if(v$dirtest == "unilatg"){
	cv$t.power<-(1-pt(qt(1-cv$alpha,v$n-1)-((cv$z.diff.mu*-1)/(cv$samples.x.sd[[i]]/sqrt(v$n))),v$n-1))
      }
      if(v$dirtest == "unilatd"){
	cv$t.power<-(1-pt(qt(1-cv$alpha,v$n-1)-((cv$z.diff.mu)/(cv$samples.x.sd[[i]]/sqrt(v$n))),v$n-1))
      }
    
      }
    }

    if(length(cv$test.k.conclusion)>0){
      cv$test.k.conclusion.n.rh0<-length(which(cv$test.k.conclusion == "rh0"))
      cv$test.k.conclusion.n.nrh0<-cv$n.samples-cv$test.k.conclusion.n.rh0
      cv$test.k.conclusion.pc.rh0<-round(cv$test.k.conclusion.n.rh0/length(cv$test.k.conclusion),4)*100
      cv$test.k.conclusion.pc.nrh0<-100-cv$test.k.conclusion.pc.rh0
    }  

    if(length(cv$test.z.conclusion)>0){
      cv$test.z.conclusion.n.rh0<-length(which(cv$test.z.conclusion == "rh0"))
      cv$test.z.conclusion.n.nrh0<-cv$n.samples-cv$test.z.conclusion.n.rh0
      cv$test.z.conclusion.pc.rh0<-round(cv$test.z.conclusion.n.rh0/length(cv$test.z.conclusion),4)*100
      cv$test.z.conclusion.pc.nrh0<-100-cv$test.z.conclusion.pc.rh0
    }

    if(length(cv$test.t.conclusion)>0){
      cv$test.t.conclusion.n.rh0<-length(which(cv$test.t.conclusion == "rh0"))
      cv$test.t.conclusion.n.nrh0<-cv$n.samples-cv$test.t.conclusion.n.rh0
      cv$test.t.conclusion.pc.rh0<-round(cv$test.t.conclusion.n.rh0/length(cv$test.t.conclusion),4)*100
      cv$test.t.conclusion.pc.nrh0<-100-cv$test.t.conclusion.pc.rh0
    }
      
     if(v$forceh0){
      if(length(cv$test.k.conclusion.h0)>0){
	cv$test.k.conclusion.n.rh0.h0<-length(which(cv$test.k.conclusion.h0 == "rh0"))
	cv$test.k.conclusion.n.nrh0.h0<-cv$n.samples-cv$test.k.conclusion.n.rh0.h0
	cv$test.k.conclusion.pc.rh0.h0<-round(cv$test.k.conclusion.n.rh0.h0/length(cv$test.k.conclusion.h0),4)*100
	cv$test.k.conclusion.pc.nrh0.h0<-100-cv$test.k.conclusion.pc.rh0.h0
      }  

      if(length(cv$test.z.conclusion.h0)>0){
	cv$test.z.conclusion.n.rh0.h0<-length(which(cv$test.z.conclusion.h0 == "rh0"))
	cv$test.z.conclusion.n.nrh0.h0<-cv$n.samples-cv$test.z.conclusion.n.rh0.h0
	cv$test.z.conclusion.pc.rh0.h0<-round(cv$test.z.conclusion.n.rh0.h0/length(cv$test.z.conclusion.h0),4)*100
	cv$test.z.conclusion.pc.nrh0.h0<-100-cv$test.z.conclusion.pc.rh0.h0
      }

      if(length(cv$test.t.conclusion.h0)>0){
	cv$test.t.conclusion.n.rh0.h0<-length(which(cv$test.t.conclusion.h0 == "rh0"))
	cv$test.t.conclusion.n.nrh0.h0<-cv$n.samples-cv$test.t.conclusion.n.rh0.h0
	cv$test.t.conclusion.pc.rh0.h0<-round(cv$test.t.conclusion.n.rh0.h0/length(cv$test.t.conclusion.h0),4)*100
	cv$test.t.conclusion.pc.nrh0.h0<-100-cv$test.t.conclusion.pc.rh0.h0
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
      
      
      cv$test.k.conclusion.toshow<-cv$test.k.conclusion[cv$samples.x.from:cv$samples.x.to]

      cv$test.z.conclusion.toshow<-cv$test.z.conclusion[cv$samples.x.from:cv$samples.x.to]

      cv$confidence.t.limit.inf.bilat.toshow<-cv$confidence.t.limit.inf.bilat[cv$samples.x.from:cv$samples.x.to]
      cv$confidence.t.limit.sup.bilat.toshow<-cv$confidence.t.limit.sup.bilat[cv$samples.x.from:cv$samples.x.to]
      
      cv$confidence.t.limit.inf.unilat.toshow<-cv$confidence.t.limit.inf.unilat[cv$samples.x.from:cv$samples.x.to]
      cv$confidence.t.limit.sup.unilat.toshow<-cv$confidence.t.limit.sup.unilat[cv$samples.x.from:cv$samples.x.to]
      
      cv$test.t.conclusion.toshow<-cv$test.t.conclusion[cv$samples.x.from:cv$samples.x.to]
      
      if(v$forceh0){
	cv$samples.x.m.h0.toshow<-cv$samples.x.m.h0[cv$samples.x.from:cv$samples.x.to]
	cv$test.k.conclusion.h0.toshow<-cv$test.k.conclusion.h0[cv$samples.x.from:cv$samples.x.to]
      }
      cv$samples.y.toshow<-list()
      cv$samples.x.n.toshow<-length(cv$samples.x.toshow)
      
      if(cv$samples.x.n.toshow>0){
	for(i in 1:cv$samples.x.n.toshow){
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
    rv$last.takesample.value<-v$takesample
    return(cv)
  })
    
  output$plotEmp <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()

    if(v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5,6,7,7,8),3,4,byrow=TRUE)
    }
    if(v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,4,4,5),2,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3,4,5,5,6),2,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,2,2,3),1,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2),1,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,2,2,3),3,4,byrow=TRUE)
    }
    layout(m,width=c(4,2,1,3))

if(v$showR){
    ##################
    ## Plot Reality ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showrh1h0){
      label<-"Densité"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(x.lim.min,x.lim.max,20))
    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	#text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	mtext(bquote(paste(bar(x)[.(cv$samples.x.i.toshow[[i]])] == .(sprintf("%.2f",cv$samples.x.m.toshow[[i]])),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s[.(cv$samples.x.i.toshow[[i]])] == .(sprintf("%.2f",cv$samples.x.sd.toshow[[i]])),sep="")),side=4,line=9,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels="Réalité",cex=1.4, pos=4)

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N(.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
      }
    }
      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    
    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')#,main=bquote(paste("Calcul du % de ",RH[0]," et de ",NRH[0],sep="")),cex.main=1.5
    
    if(v$dirtest == "bilat"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(mu[0]-K,mu[0]+K),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(mu[0]-K,mu[0]+K),"]"),sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(v$thresholds == "calcul"){
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(v$mx0)-.(v$k),.(v$mx0)+.(v$k)),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(v$mx0)-.(v$k),.(v$mx0)+.(v$k)),"]"),sep="")),cex=cex.hypoth,pos=4)
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]"),sep="")),cex=cex.hypoth,pos=4)
	}
      }
    }
    if(v$dirtest == "unilatg"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu >= mu[0]," , ",H[1]," : ",mu < mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < mu[0]-K,sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= mu[0]-K,sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu >= .(v$mx0)," , ",H[1]," : ",mu < .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(v$thresholds == "calcul"){
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < .(v$mx0)-.(v$k),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= .(v$mx0)-.(v$k),sep="")),cex=cex.hypoth,pos=4)
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < .(cv$confidence.k.limit.inf),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= .(cv$confidence.k.limit.inf),sep="")),cex=cex.hypoth,pos=4)
	}
      }
    }
    if(v$dirtest == "unilatd"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu <= mu[0]," , ",H[1]," : ",mu > mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > mu[0]+K,sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= mu[0]+K,sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu <= .(v$mx0)," , ",H[1]," : ",mu > .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(v$thresholds == "calcul"){
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > .(v$mx0)+.(v$k),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= .(v$mx0)+.(v$k),sep="")),cex=cex.hypoth,pos=4)
	} else {
      text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > .(cv$confidence.k.limit.sup),sep="")),cex=cex.hypoth,pos=4)
      text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= .(cv$confidence.k.limit.sup),sep="")),cex=cex.hypoth,pos=4)
	}
      }
    }
}
if(v$showh0){
    ##################
    ## Plot H0      ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))
    if(v$dirtest == "bilat"){
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(mu[0] - K , mu[0] + K),"]"), sep="")) ,cex.main=1.5)  
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu == mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu == .(v$mx0),sep="")),cex=1.4, pos=4)
	if(v$thresholds == "calcul"){
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(.(v$mx0)-.(v$k) , .(v$mx0)+.(v$k)),"]"), sep="")) ,cex.main=1.5)  
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]"), sep="")) ,cex.main=1.5)
	}
      }
    }
    if(v$dirtest == "unilatg"){
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(mu[0] - K , infinity),"]"), sep="")) ,cex.main=1.5)
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu >= mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu >= .(v$mx0),sep="")),cex=1.4, pos=4)
	if(v$thresholds == "calcul"){
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(.(v$mx0)-.(v$k) , infinity),"]"), sep="")) ,cex.main=1.5)
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(.(cv$confidence.k.limit.inf),infinity),"]") , sep="")) ,cex.main=1.5)
	}
      }
    }
    if(v$dirtest == "unilatd"){
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(- infinity , mu[0] + K),"]") , sep="")),cex.main=1.5)
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu <= mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu <= .(v$mx0),sep="")),cex=1.4, pos=4)
	if(v$thresholds == "calcul"){
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(- infinity , .(v$mx0)+.(v$k) ),"]") , sep="")),cex.main=1.5)
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(- infinity , .(cv$confidence.k.limit.sup)),"]") , sep="")),cex.main=1.5)
	}
      }
    }

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      #points(cv$xh0,cv$yh0,type="l")
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[0] *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      } else {
	if(v$thresholds == "calcul"){
	  text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( .(v$mx0) *","* frac(.(v$sx^2),sqrt(.(v$n))) ),sep='')),cex=1.4, pos=4)
	} else {
	  text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N (.(v$mx0)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
	}
      }
      polygon(c(x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.04,x.lim.min+x.amp*0.04),c(signif(cv$maxdmx,1)*0.375,signif(cv$maxdmx,1)*0.475,signif(cv$maxdmx,1)*0.475,signif(cv$maxdmx,1)*0.375),col=color.false)
      text(x.lim.min+x.amp*0.04,signif(cv$maxdmx,1)*0.4,labels=bquote(paste(alpha == .(cv$emp.alpha),sep='')),cex=1.4, pos=4)
      polygon(c(x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.04,x.lim.min+x.amp*0.04),c(signif(cv$maxdmx,1)*0.175,signif(cv$maxdmx,1)*0.275,signif(cv$maxdmx,1)*0.275,signif(cv$maxdmx,1)*0.175),col=color.true)
      text(x.lim.min+x.amp*0.04,signif(cv$maxdmx,1)*0.2,labels=bquote(paste(1 - alpha == .(cv$emp.confidence),sep='')),cex=1.4, pos=4)
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
	polygon(c(x.lim.min,x.lim.min,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
      if(v$dirtest == "unilatg"){
	polygon(c(x.lim.min,x.lim.min,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
      if(v$dirtest == "unilatd"){
	polygon(c(x.lim.min,x.lim.min,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
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
    
    if(v$forceh0){
      if(cv$samples.x.n.toshow>0){
	for(i in 1:cv$samples.x.n.toshow){
	  text(cv$samples.x.m.h0.toshow[[i]],cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	  if(v$dirtest == "bilat"){
	    if(cv$test.k.conclusion.h0.toshow[[i]] == "rh0"){
	      mtext(bquote(paste(.(cv$samples.x.m.h0.toshow[[i]]) %notin% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	    if(cv$test.k.conclusion.h0.toshow[[i]] == "nrh0"){
	      mtext(bquote(paste(.(cv$samples.x.m.h0.toshow[[i]]) %in% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	  }
	  if(v$dirtest == "unilatg"){
	    if(cv$test.k.conclusion.h0.toshow[[i]] == "rh0"){
	      mtext(bquote(paste(.(cv$samples.x.m.h0.toshow[[i]]) < .(cv$confidence.k.limit.inf) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	    if(cv$test.k.conclusion.h0.toshow[[i]] == "nrh0"){
	      mtext(bquote(paste(.(cv$samples.x.m.h0.toshow[[i]]) >= .(cv$confidence.k.limit.inf) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	  }
	  if(v$dirtest == "unilatd"){
	    if(cv$test.k.conclusion.h0.toshow[[i]] == "rh0"){
	      mtext(bquote(paste(.(cv$samples.x.m.h0.toshow[[i]]) > .(cv$confidence.k.limit.sup) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	    if(cv$test.k.conclusion.h0.toshow[[i]] == "nrh0"){
	      mtext(bquote(paste(.(cv$samples.x.m.h0.toshow[[i]]) <= .(cv$confidence.k.limit.sup) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	  }
	}
      }
    } else {
      if(cv$samples.x.n.toshow>0){
	for(i in 1:cv$samples.x.n.toshow){
	  text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	  if(v$dirtest == "bilat"){
	    if(cv$test.k.conclusion.toshow[[i]] == "rh0"){
	      mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) %notin% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	    if(cv$test.k.conclusion.toshow[[i]] == "nrh0"){
	      mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) %in% group("[",list(.(cv$confidence.k.limit.inf),.(cv$confidence.k.limit.sup)),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	  }
	  if(v$dirtest == "unilatg"){
	    if(cv$test.k.conclusion.toshow[[i]] == "rh0"){
	      mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) < .(cv$confidence.k.limit.inf) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	    if(cv$test.k.conclusion.toshow[[i]] == "nrh0"){
	      mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) >= .(cv$confidence.k.limit.inf) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	  }
	  if(v$dirtest == "unilatd"){
	    if(cv$test.k.conclusion.toshow[[i]] == "rh0"){
	      mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) > .(cv$confidence.k.limit.sup) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	    if(cv$test.k.conclusion.toshow[[i]] == "nrh0"){
	      mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) <= .(cv$confidence.k.limit.sup) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	    }
	  }
	}
      }
    }
    
    if(v$forceh0){
      ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("NRHo"=cv$test.k.conclusion.pc.nrh0.h0,"RHo"=cv$test.k.conclusion.pc.rh0.h0)
      } else {
	includes<-c("NRHo"=0,"RHo"=0)#"µ1 ⊄ IC"=0
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      #text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)

      testmean<-data.frame(c(cv$test.k.conclusion.n.nrh0.h0,cv$test.k.conclusion.pc.nrh0.h0),c(" "," "),c(cv$test.k.conclusion.n.rh0.h0,cv$test.k.conclusion.pc.rh0.h0))
      colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
      rownames(testmean)<-c("n ","% ")
      addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)#,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]"))
    } else {
      ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("NRHo"=cv$test.k.conclusion.pc.nrh0,"RHo"=cv$test.k.conclusion.pc.rh0)
      } else {
	includes<-c("NRHo"=0,"RHo"=0)#"µ1 ⊄ IC"=0
      }
      par(mai=c(0.5,0.5,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      #text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)

      testmean<-data.frame(c(cv$test.k.conclusion.n.nrh0,cv$test.k.conclusion.pc.nrh0),c(" "," "),c(cv$test.k.conclusion.n.rh0,cv$test.k.conclusion.pc.rh0))
      colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
      rownames(testmean)<-c("n ","% ")
      addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)#,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]"))
    }


      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')

}   
  if(v$showh1){
    ##################
    ## Plot H1     ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.25))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))
    
    if(v$dirtest == "bilat"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu != mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatg"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu < mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatd"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu > mu[0],sep="")),cex=1.4, pos=4)
    }

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      #points(cv$xh1,cv$yh1,type="l")
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu *","* frac(sigma^2,sqrt(n)) ),sep='')),cex=1.4, pos=4)
      } else {
	if(v$thresholds == "calcul"){
	  text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( .(v$mx1) *","* frac(.(v$sx^2),sqrt(.(v$n))) ),sep='')),cex=1.4, pos=4)
	} else {
	  text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
	}
      }

      polygon(c(x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.04,x.lim.min+x.amp*0.04),c(signif(cv$maxdmx,1)*0.375,signif(cv$maxdmx,1)*0.475,signif(cv$maxdmx,1)*0.475,signif(cv$maxdmx,1)*0.375),col=color.false)
      text(x.lim.min+x.amp*0.04,signif(cv$maxdmx,1)*0.4,labels=bquote(paste(beta == .(cv$emp.beta),sep='')),cex=1.4, pos=4)
      polygon(c(x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.04,x.lim.min+x.amp*0.04),c(signif(cv$maxdmx,1)*0.175,signif(cv$maxdmx,1)*0.275,signif(cv$maxdmx,1)*0.275,signif(cv$maxdmx,1)*0.175),col=color.true)
      text(x.lim.min+x.amp*0.04,signif(cv$maxdmx,1)*0.2,labels=bquote(paste(1 - beta == .(cv$emp.power),sep='')),cex=1.4, pos=4)
    }
    
    if(v$showrh1h0){
      polygon(c(cv$emp.xh1.a,max(cv$emp.xh1.a)),c(cv$emp.yh1.a,0),col=color.true)
      polygon(c(min(cv$emp.xh1.b),cv$emp.xh1.b,max(cv$emp.xh1.b)),c(0,cv$emp.yh1.b,0),col=color.false)
      polygon(c(min(cv$emp.xh1.c),cv$emp.xh1.c),c(0,cv$emp.yh1.c),col=color.true)
    } else {
      if(v$dirtest == "bilat"){
	## Confidence interval compute under H0 : polygones
	polygon(c(x.lim.min,x.lim.min,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
      if(v$dirtest == "unilatg"){
	## Confidence interval compute under H0 : polygones
	polygon(c(x.lim.min,x.lim.min,cv$confidence.k.limit.inf,cv$confidence.k.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.k.limit.inf,cv$confidence.k.limit.inf,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
      if(v$dirtest == "unilatd"){
	## Confidence interval compute under H0 : polygones
	polygon(c(x.lim.min,x.lim.min,cv$confidence.k.limit.sup,cv$confidence.k.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.k.limit.sup,cv$confidence.k.limit.sup,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
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

    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    } 
    
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    
    
    ## Plot of power
    if(v$showEvolPower == "emp"){
      if(length(cv$vect.n.samples)>0){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.6,0.5,0.1))
      plot(cv$vect.n.samples,cv$test.k.conclusion.pcrh0.vect,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%",RH[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      lines(x<-c(0,npclim),y <- c(cv$emp.power*100,cv$emp.power*100),lty=3)
      text(npclim*0.01,(cv$emp.power*100)-5,expression(1-beta),pos=4)
    } 
    if(v$showEvolPower == "none"){
      ## Plot bar plot of includes %
      if(length(cv$samples.x)>0){
	includes<-c("NRHo"=cv$test.k.conclusion.pc.nrh0,"RHo"=cv$test.k.conclusion.pc.rh0)
      } else {
	includes<-c("NRHo"=0,"RHo"=0)#"µ1 ⊄ IC"=0
      }
      par(mai=c(0.5,3.3,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true),cex.names=1.25,cex.axis=1.2)
      #text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)

      testmean<-data.frame(c(cv$test.k.conclusion.n.nrh0,cv$test.k.conclusion.pc.nrh0),c(" "," "),c(cv$test.k.conclusion.n.rh0,cv$test.k.conclusion.pc.rh0))
      colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
      rownames(testmean)<-c("n ","% ")
      addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)#,title=bquote(paste(bar(x)," vs [",mu[0] %+-% K,"]"))
    }
    if(v$showEvolPower == "theor"){
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
    }

    par(mai=c(0.5,0.5,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
  }
    }, height = getPlotHeight, width=full.plot.width)

########################################################################################
  output$plotZ <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5,6,7,7,8),3,4,byrow=TRUE)
    }
    if(v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,4,4,5),2,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3,4,5,5,6),2,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,2,2,3),1,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2),1,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,2,2,3),3,4,byrow=TRUE)
    }
    layout(m,width=c(4,2,1,3))
if(v$showR){
    ##################
    ## Plot Reality ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showrh1h0){
      label<-"Densité"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(x.lim.min,x.lim.max,20))

    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	mtext(bquote(paste(bar(x)[.(cv$samples.x.i.toshow[[i]])] == .(sprintf("%.2f",cv$samples.x.m.toshow[[i]])),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s[.(cv$samples.x.i.toshow[[i]])] == .(sprintf("%.2f",cv$samples.x.sd.toshow[[i]])),sep="")),side=4,line=9,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels="Réalité",cex=1.4, pos=4)

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N(.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
      }
    }
      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)

    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
    
    if(v$dirtest == "bilat"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(mu[0]-Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n)),mu[0]+Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(mu[0]-Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n)),mu[0]+Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(v$thresholds == "calcul"){
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(v$mx0)-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),.(v$mx0)+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(v$mx0)-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),.(v$mx0)+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]"),sep="")),cex=cex.hypoth,pos=4)
	}
      } 
    }
    if(v$dirtest == "unilatg"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu >= mu[0]," , ",H[1]," : ",mu < mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < mu[0]-Z[1-alpha] %.% frac(sigma,sqrt(n)),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= mu[0]-Z[1-alpha] %.% frac(sigma,sqrt(n)),sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu >= .(v$mx0)," , ",H[1]," : ",mu < .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(v$thresholds == "calcul"){
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < .(v$mx0)-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= .(v$mx0)-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < .(cv$confidence.z.limit.inf),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= .(cv$confidence.z.limit.inf),sep="")),cex=cex.hypoth,pos=4)
	}
      } 
    }
    if(v$dirtest == "unilatd"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu <= mu[0]," , ",H[1]," : ",mu > mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > mu[0]+Z[1-alpha] %.% frac(sigma,sqrt(n)),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= mu[0]+Z[1-alpha] %.% frac(sigma,sqrt(n)),sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu <= .(v$mx0)," , ",H[1]," : ",mu > .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(v$thresholds == "calcul"){
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > .(v$mx0)+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= .(v$mx0)+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > .(cv$confidence.z.limit.sup),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= .(cv$confidence.z.limit.sup),sep="")),cex=cex.hypoth,pos=4)
	}
      }
    }
}
if(v$showh0){
    ##################
    ## Plot H0      ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))
    if(v$dirtest == "bilat"){
    
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(mu[0]-Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n)),mu[0]+Z[1-frac(alpha,2)] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex.main=1.5)
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu == mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu == .(v$mx0),sep="")),cex=1.4, pos=4)
	if(v$thresholds == "calcul"){
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(.(v$mx0)-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),.(v$mx0)+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n)))),"]"))),cex.main=1.5)
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]"),sep="")),cex.main=1.5)
	}
      }
    }
    if(v$dirtest == "unilatg"){
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(mu[0]-Z[1-alpha] %.% frac(sigma,sqrt(n)),infinity),"]"),sep="")),cex.main=1.5)
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu >= mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu >= .(v$mx0),sep="")),cex=1.4, pos=4)
	if(v$thresholds == "calcul"){
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(.(v$mx0)-.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n))),infinity),"]"),sep="")),cex.main=1.5)
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(.(cv$confidence.z.limit.inf),infinity),"]"),sep="")),cex.main=1.5)
	}
      }
    } 
    if(v$dirtest == "unilatd"){
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(-infinity,mu[0]+Z[1-alpha] %.% frac(sigma,sqrt(n))),"]"),sep="")),cex.main=1.5)
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu <= mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu <= .(v$mx0),sep="")),cex=1.4, pos=4)
	if(v$thresholds == "calcul"){
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(-infinity,.(v$mx0)+.(round(qnorm(1-cv$alpha/2),2)) %.% frac(.(v$sx),sqrt(.(v$n)))),"]"),sep="")),cex.main=1.5)
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : ",group("[",list(-infinity,.(cv$confidence.z.limit.sup)),"]"),sep="")),cex.main=1.5)
	}
      }
    } 

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      #points(cv$xh0,cv$yh0,type="l")
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[0] *","* frac(sigma^2,n) ),sep='')),cex=1.4, pos=4)
      } else {
	if(v$thresholds == "calcul"){
	  text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( .(v$mx0) *","* frac(.(cv$vx),.(v$n)) ),sep='')),cex=1.4, pos=4)
	} else {
	  text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N (.(v$mx0)*","*.(cv$vx/v$n)) ,sep='')),cex=1.4, pos=4)
	}
      }

      polygon(c(x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.04,x.lim.min+x.amp*0.04),c(signif(cv$maxdmx,1)*0.375,signif(cv$maxdmx,1)*0.475,signif(cv$maxdmx,1)*0.475,signif(cv$maxdmx,1)*0.375),col=color.false)
      text(x.lim.min+x.amp*0.04,signif(cv$maxdmx,1)*0.4,labels=bquote(paste(alpha == .(cv$alpha),sep='')),cex=1.4, pos=4)
      polygon(c(x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.04,x.lim.min+x.amp*0.04),c(signif(cv$maxdmx,1)*0.175,signif(cv$maxdmx,1)*0.275,signif(cv$maxdmx,1)*0.275,signif(cv$maxdmx,1)*0.175),col=color.true)
      text(x.lim.min+x.amp*0.04,signif(cv$maxdmx,1)*0.2,labels=bquote(paste(1 - alpha == .(cv$confidence),sep='')),cex=1.4, pos=4)
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
	polygon(c(x.lim.min,x.lim.min,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
      if(v$dirtest == "unilatg"){
	## Confidence interval compute under H0 : polygones
	polygon(c(x.lim.min,x.lim.min,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
      }
      if(v$dirtest == "unilatd"){
	## Confidence interval compute under H0 : polygones
	polygon(c(x.lim.min,x.lim.min,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
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
    
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }

    mtext(bquote(paste(bar(x)," vs confiance => conclusion",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	if(v$dirtest == "bilat"){
	  if(cv$test.z.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) %notin% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.z.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) %in% group("[",list(.(cv$confidence.z.limit.inf),.(cv$confidence.z.limit.sup)),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatg"){
	  if(cv$test.z.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) < .(cv$confidence.z.limit.inf) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.z.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) >= .(cv$confidence.z.limit.inf) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatd"){
	  if(cv$test.z.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) > .(cv$confidence.z.limit.sup) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.z.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) <= .(cv$confidence.z.limit.sup) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
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
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)

      testmean<-data.frame(c(cv$test.z.conclusion.n.nrh0,cv$test.z.conclusion.pc.nrh0),c(" "," "),c(cv$test.z.conclusion.n.rh0,cv$test.z.conclusion.pc.rh0))
      colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
      rownames(testmean)<-c("n ","% ")
      addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)
    
    ## Plot of quantile determination
    if(v$showquant){
      par(mai=c(0.5,0.75,0,0.1))#,mfrow=c(2,1)
      plot(cv$z,cv$z.d,xlab="",ylab="Densité",cex.lab=1.2,bty="n",xlim=c(-5,5),ylim=c(0,0.6),xaxp=c(-5,5,10),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2)#xlab=bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep=""))
      axis(2,las=2,yaxp=c(0,0.4,4),cex.axis=1.2)
      text(-4.9,0.35,bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep="")),pos=4,cex=1.4)
      
      polygon(c(-4.8,-4.8,-4.4,-4.4),c(0.4*0.675,0.4*0.775,0.4*0.775,0.4*0.675),col=color.false)
      text(-4.4,0.4*0.7,labels=bquote(paste(alpha == .(cv$alpha),sep='')),cex=1.4, pos=4)
      polygon(c(-4.8,-4.8,-4.4,-4.4),c(0.4*0.475,0.4*0.575,0.4*0.575,0.4*0.475),col=color.true)
      text(-4.4,0.4*0.5,labels=bquote(paste(1 - alpha == .(cv$confidence),sep='')),cex=1.4, pos=4)

      if(v$dirtest == "bilat"){
		polygon(c(cv$z.zh0.a,max(cv$z.zh0.a)),c(dnorm(cv$z.zh0.a),0),col=color.false)
		polygon(c(min(cv$z.zh0.b),cv$z.zh0.b,max(cv$z.zh0.b)),c(0,dnorm(cv$z.zh0.b),0),col=color.true)
		polygon(c(min(cv$z.zh0.c),cv$z.zh0.c),c(0,dnorm(cv$z.zh0.c)),col=color.false)
	      
		lines(c(qnorm(cv$alpha/2),qnorm(cv$alpha/2)),c(0,dnorm(qnorm(cv$alpha/2))))
		text(qnorm(cv$alpha/2),dnorm(qnorm(cv$alpha/2))+0.05,bquote(paste(-Z[1-frac(alpha,2)] == .(round(qnorm(cv$alpha/2),2)),sep="")),cex=1.4)
		#mtext(side=1,line=3,bquote(paste(-Z[1-frac(alpha,2)] == .(round(qnorm(cv$alpha/2),2)),sep="")),at=qnorm(cv$alpha/2))
		
		lines(c(qnorm(1-cv$alpha/2),qnorm(1-cv$alpha/2)),c(0,dnorm(qnorm(1-cv$alpha/2))))
		text(qnorm(1-cv$alpha/2),dnorm(qnorm(1-cv$alpha/2))+0.05,bquote(paste(Z[1-frac(alpha,2)] == .(round(qnorm(cv$alpha/2),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatg"){
		polygon(c(cv$z.zh0.a,max(cv$z.zh0.a)),c(dnorm(cv$z.zh0.a),0),col=color.false)
		polygon(c(min(cv$z.zh0.b),cv$z.zh0.b),c(0,dnorm(cv$z.zh0.b)),col=color.true)
	      
		lines(c(qnorm(cv$alpha),qnorm(cv$alpha)),c(0,dnorm(qnorm(cv$alpha))))
		text(qnorm(cv$alpha),dnorm(qnorm(cv$alpha))+0.05,bquote(paste(-Z[1-alpha] == .(round(qnorm(cv$alpha),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatd"){
		polygon(c(cv$z.zh0.b,max(cv$z.zh0.b)),c(dnorm(cv$z.zh0.b),0),col=color.true)
		polygon(c(min(cv$z.zh0.c),cv$z.zh0.c),c(0,dnorm(cv$z.zh0.c)),col=color.false)
	      
		lines(c(qnorm(1-cv$alpha),qnorm(1-cv$alpha)),c(0,dnorm(qnorm(1-cv$alpha))))
		text(qnorm(1-cv$alpha),dnorm(qnorm(1-cv$alpha))+0.05,bquote(paste(Z[1-alpha] == .(round(qnorm(1-cv$alpha),2)),sep="")),cex=1.4)
      }
      #text(3,0.35,bquote(paste(Z == frac(bar(x) - mu,frac(sigma,sqrt(n))),sep="")),cex=1.4)
    } else {
      par(mai=c(0.5,0.75,0,0.1))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
}
    
  if(v$showh1){  
    ##################
    ## Plot H1     ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))
    
    if(v$dirtest == "bilat"){
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu != mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu != .(v$mx0),sep="")),cex=1.4, pos=4)
      }
    }
    if(v$dirtest == "unilatg"){
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu < mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu < .(v$mx0),sep="")),cex=1.4, pos=4)
      }
    }
    if(v$dirtest == "unilatd"){
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu > mu[0],sep="")),cex=1.4, pos=4)
      } else {
	text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu > .(v$mx0),sep="")),cex=1.4, pos=4)
      }
    }
    
    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      #points(cv$z.xh1,cv$z.yh1,type="l")
      if(v$thresholds == "formula"){
	text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( mu[0] *","* frac(sigma^2,n) ),sep='')),cex=1.4, pos=4)
      } else {
	if(v$thresholds == "calcul"){
	  text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N ( .(v$mx1) *","* frac(.(cv$vx),.(v$n)) ),sep='')),cex=1.4, pos=4)
	} else {
	  text(x.lim.min,signif(cv$maxdmx,1)*0.8,labels=bquote(paste(bar(X) *"~"* N (.(v$mx1)*","*.(cv$vx/v$n)) ,sep='')),cex=1.4, pos=4)
	}
      }
      polygon(c(x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.04,x.lim.min+x.amp*0.04),c(signif(cv$maxdmx,1)*0.375,signif(cv$maxdmx,1)*0.475,signif(cv$maxdmx,1)*0.475,signif(cv$maxdmx,1)*0.375),col=color.false)
      text(x.lim.min+x.amp*0.04,signif(cv$maxdmx,1)*0.4,labels=bquote(paste(beta == .(cv$beta),sep='')),cex=1.4, pos=4)
      polygon(c(x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.01,x.lim.min+x.amp*0.04,x.lim.min+x.amp*0.04),c(signif(cv$maxdmx,1)*0.175,signif(cv$maxdmx,1)*0.275,signif(cv$maxdmx,1)*0.275,signif(cv$maxdmx,1)*0.175),col=color.true)
      text(x.lim.min+x.amp*0.04,signif(cv$maxdmx,1)*0.2,labels=bquote(paste(1 - beta == .(cv$power),sep='')),cex=1.4, pos=4)
    }


    if(v$showrh1h0){
      polygon(c(cv$z.xh1.a,max(cv$z.xh1.a)),c(cv$z.yh1.a,0),col=color.true)
      polygon(c(min(cv$z.xh1.b),cv$z.xh1.b,max(cv$z.xh1.b)),c(0,cv$z.yh1.b,0),col=color.false)
      polygon(c(min(cv$z.xh1.c),cv$z.xh1.c),c(0,cv$z.yh1.c),col=color.true)

    } else {
      if(v$dirtest == "bilat"){
	## Confidence interval compute under H0 : polygones
	polygon(c(x.lim.min,x.lim.min,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
      if(v$dirtest == "unilatg"){
	## Confidence interval compute under H0 : polygones
	polygon(c(x.lim.min,x.lim.min,cv$confidence.z.limit.inf,cv$confidence.z.limit.inf),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
	polygon(c(cv$confidence.z.limit.inf,cv$confidence.z.limit.inf,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
      }
      if(v$dirtest == "unilatd"){
	## Confidence interval compute under H0 : polygones
	polygon(c(x.lim.min,x.lim.min,cv$confidence.z.limit.sup,cv$confidence.z.limit.sup),c(0,cv$maxdmx,cv$maxdmx,0),col=color.false)
	polygon(c(cv$confidence.z.limit.sup,cv$confidence.z.limit.sup,x.lim.max,x.lim.max),c(0,cv$maxdmx,cv$maxdmx,0),col=color.true)
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
    
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }
    
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=1,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    
    ## Plot of power
    if(v$showEvolPower == "none"){
      if(length(cv$samples.x)>0){
	includes<-c("NRHo"=cv$test.z.conclusion.pc.nrh0,"RHo"=cv$test.z.conclusion.pc.rh0)
      } else {
	includes<-c("NRHo"=0,"RHo"=0)#"µ1 ⊄ IC"=0
      }
      par(mai=c(0.5,3.3,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true),cex.names=1.25,cex.axis=1.2)

      testmean<-data.frame(c(cv$test.z.conclusion.n.nrh0,cv$test.z.conclusion.pc.nrh0),c(" "," "),c(cv$test.z.conclusion.n.rh0,cv$test.z.conclusion.pc.rh0))
      colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
      rownames(testmean)<-c("n ","% ")
      addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)
    }
    if(v$showEvolPower == "emp"){
      if(length(cv$vect.n.samples)>0){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.6,0.5,0.1))
      plot(cv$vect.n.samples,cv$test.z.conclusion.pcrh0.vect,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%",RH[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      lines(x<-c(0,npclim),y <- c(cv$power*100,cv$power*100),lty=3)
      text(npclim*0.01,(cv$power*100)-5,expression(1-beta),pos=4)
    }
    if(v$showEvolPower == "theor"){
      par(mai=c(0.5,0.6,0.5,0.1))#,mfrow=c(2,1)
      plot(cv$z.diff.mu,cv$z.power,xlab=bquote(paste(mu-mu[0],sep="")),ylab=bquote(paste("Puissance ",1 - beta,sep="")),bty="n",xlim=c(-50,50),xaxp=c(-50,50,10),ylim=c(0,1.2),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2,cex.lab=1.2)#
      polygon(c(-50,cv$z.diff.mu,50),c(0,cv$z.power,0),col=color.true)
      polygon(c(-50,cv$z.diff.mu,50),c(1,1-(1-cv$z.power),1),col=color.false)
      axis(2,las=2,yaxp=c(0,1,4),ylim=c(0,1),cex.axis=1.2)
      lines(c(v$mx1-v$mx0,v$mx1-v$mx0),c(0,cv$power),lty=3)
      lines(c(-50,v$mx1-v$mx0),c(cv$power,cv$power),lty=3)
    } 
    if(v$showquant){
      par(mai=c(0.5,0.75,0,0.1))#,mfrow=c(2,1)
      plot(cv$z,cv$z.d,xlab="",ylab="Densité",cex.lab=1.2,bty="n",xlim=c(-5,5),ylim=c(0,0.6),xaxp=c(-5,5,10),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2)#xlab=bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep=""))
      
      axis(2,las=2,yaxp=c(0,0.4,4),cex.axis=1.2)
      text(-4.9,0.35,bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep="")),pos=4,cex=1.4)
      
      polygon(c(-4.8,-4.8,-4.4,-4.4),c(0.4*0.675,0.4*0.775,0.4*0.775,0.4*0.675),col=color.false)
      text(-4.4,0.4*0.7,labels=bquote(paste(beta == .(cv$beta),sep='')),cex=1.4, pos=4)
      polygon(c(-4.8,-4.8,-4.4,-4.4),c(0.4*0.475,0.4*0.575,0.4*0.575,0.4*0.475),col=color.true)
      text(-4.4,0.4*0.5,labels=bquote(paste(1 - beta == .(cv$power),sep='')),cex=1.4, pos=4)
      
      if(v$dirtest == "bilat"){
		polygon(c(cv$z.zh1.a,max(cv$z.zh1.a)),c(dnorm(cv$z.zh1.a),0),col=color.true)
		polygon(c(min(cv$z.zh1.b),cv$z.zh1.b,max(cv$z.zh1.b)),c(0,dnorm(cv$z.zh1.b),0),col=color.false)
		polygon(c(min(cv$z.zh1.c),cv$z.zh1.c),c(0,dnorm(cv$z.zh1.c)),col=color.true)
	      
		lines(c(cv$z.z.lim.inf.h1,cv$z.z.lim.inf.h1),c(0,dnorm(cv$z.z.lim.inf.h1)))
		text(cv$z.z.lim.inf.h1,dnorm(cv$z.z.lim.inf.h1)+0.05,bquote(paste(Z[1] == .(round(cv$z.z.lim.inf.h1,2)),sep="")),cex=1.4)
		
		lines(c(cv$z.z.lim.sup.h1,cv$z.z.lim.sup.h1),c(0,dnorm(cv$z.z.lim.sup.h1)))
		text(cv$z.z.lim.sup.h1,dnorm(cv$z.z.lim.sup.h1)+0.05,bquote(paste(Z[2] == .(round(cv$z.z.lim.sup.h1,2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatg"){
		polygon(c(cv$z.zh1.a,max(cv$z.zh1.a)),c(dnorm(cv$z.zh1.a),0),col=color.true)
		polygon(c(min(cv$z.zh1.b),cv$z.zh1.b),c(0,dnorm(cv$z.zh1.b)),col=color.false)
	      
		lines(c(cv$z.z.lim.inf.h1,cv$z.z.lim.inf.h1),c(0,dnorm(cv$z.z.lim.inf.h1)))
		text(cv$z.z.lim.inf.h1,dnorm(cv$z.z.lim.inf.h1)+0.05,bquote(paste(Z[1] == .(round(cv$z.z.lim.inf.h1,2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatd"){
		polygon(c(cv$z.zh1.b,max(cv$z.zh1.b)),c(dnorm(cv$z.zh1.b),0),col=color.false)
		polygon(c(min(cv$z.zh1.c),cv$z.zh1.c),c(0,dnorm(cv$z.zh1.c)),col=color.true)
	      
		lines(c(cv$z.z.lim.sup.h1,cv$z.z.lim.sup.h1),c(0,dnorm(cv$z.z.lim.sup.h1)))
		text(cv$z.z.lim.sup.h1,dnorm(cv$z.z.lim.sup.h1)+0.05,bquote(paste(Z[2] == .(round(cv$z.z.lim.sup.h1,2)),sep="")),cex=1.4)
      }
    } else {
      par(mai=c(0.5,0.75,0,0.1))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1),type='l')
    }
    }
    }, height = getPlotHeight, width=full.plot.width)
########################################################################################
  output$plotT <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5,6,7,7,8),3,4,byrow=TRUE)
    }
    if(v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2,3,3,4,5),2,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,2,3,4,4,5),2,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && v$showh1){
      m<-matrix(c(1,1,2,3,4,5,5,6),2,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && v$showh1){
      m<-matrix(c(1,2,2,3),1,4,byrow=TRUE)
    }
    if(!v$showR && v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,3),1,4,byrow=TRUE)
    }
    if(v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,1,2,2),1,4,byrow=TRUE)
    }
    if(!v$showR && !v$showh0 && !v$showh1){
      m<-matrix(c(1,2,2,3),3,4,byrow=TRUE)
    }
    layout(m,width=c(4,2,1,3))
    
if(v$showR){
    ##################
    ## Plot Reality ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    label<-""
    if(v$showrh1h0){
      label<-"Densité"
    }
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),xlab="",ylab=label,xaxp=c(x.lim.min,x.lim.max,20))

    title(main=bquote(paste("Comparaison de ",bar(x)," avec ","la zone de confiance sous ",H[0],sep="")),cex.main=1.5)

    mtext(bquote(paste("Echantillons : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	points(cv$samples.x.toshow[[i]],cv$samples.y.toshow[[i]])
	mtext(bquote(paste(bar(x)[.(cv$samples.x.i.toshow[[i]])] == .(sprintf("%.2f",cv$samples.x.m.toshow[[i]])),sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	mtext(bquote(paste(s[.(cv$samples.x.i.toshow[[i]])] == .(sprintf("%.2f",cv$samples.x.sd.toshow[[i]])),sep="")),side=4,line=9,at=cv$samples.y.toshow[[i]][1],las=2)
      }
    }
    text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels="Réalité",cex=1.4, pos=4)

    if(v$showrh1h0){
      axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
      points(cv$xr,cv$yr,type="l")
      text(x.lim.min,signif(cv$maxdmx,1)*0.9,labels=bquote(paste(X*"~"* N ( mu *","* sigma^2 ) ,sep='')),cex=1.4, pos=4)
      text(x.lim.min,signif(cv$maxdmx,1)*0.7,labels=bquote(paste(X*"~"* N(.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
    }
      lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
      text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
      
    ## empty plot for layout
    par(mai=c(0.5,0.4,0,0))
    plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')

    if(v$dirtest == "bilat"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == mu[0]," , ",H[1]," : ",mu != mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(mu[0]-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n)),mu[0]+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n))),"]"),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(mu[0]-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n)),mu[0]+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(s,sqrt(n))),"]"),sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu == .(v$mx0)," , ",H[1]," : ",mu != .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(length(rv$samples.z)>0){
	  if(v$thresholds == "calcul"){
	    text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(v$mx0)-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n))),.(v$mx0)+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(v$mx0)-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n))),.(v$mx0)+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[4]],bquote(paste(S == .(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]]))," est l'écart-type du dernier échantillon obtenu.",sep="")),cex=cex.hypoth,pos=4)
	  } else {
	    text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(cv$confidence.t.limit.inf.bilat[[length(rv$samples.z)]]),.(cv$confidence.t.limit.sup.bilat[[length(rv$samples.z)]])),"]"),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(cv$confidence.t.limit.inf.bilat[[length(rv$samples.z)]]),.(cv$confidence.t.limit.sup.bilat[[length(rv$samples.z)]])),"]"),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[4]],bquote(paste("Seuils critiques calculés avec ",S == .(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sep="")),cex=cex.hypoth,pos=4)
	  }
	} else {
	  text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) %notin% group("[",list(.(v$mx0)-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),.(v$mx0)+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) %in% group("[",list(.(v$mx0)-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),.(v$mx0)+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n)))),"]"),sep="")),cex=cex.hypoth,pos=4)
	  text(0,hypoth.text.levels[[4]],bquote(paste("S sera remplacé par l'écart-type du prochain échantillon.",sep="")),cex=cex.hypoth,pos=4)
	}
      } 
    }
    if(v$dirtest == "unilatg"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu >= mu[0]," , ",H[1]," : ",mu < mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < mu[0]-t[group("(",list(n-1,1-alpha),")")] %.% frac(S,sqrt(n)),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= mu[0]-t[group("(",list(n-1,1-alpha),")")] %.% frac(S,sqrt(n)),sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu >= .(v$mx0)," , ",H[1]," : ",mu < .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(length(rv$samples.z)>0){
	  if(v$thresholds == "calcul"){
	    text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < .(v$mx0)-.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= .(v$mx0)-.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[4]],bquote(paste(S == .(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]]))," est l'écart-type du dernier échantillon obtenu.",sep="")),cex=cex.hypoth,pos=4)
	  } else {
	    text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < .(cv$confidence.t.limit.inf.unilat[[length(rv$samples.z)]]),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= .(cv$confidence.t.limit.inf.unilat[[length(rv$samples.z)]]),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[4]],bquote(paste("Seuils critiques calculés avec ",S == .(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sep="")),cex=cex.hypoth,pos=4)
	  }
	} else {
	    text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) < .(v$mx0)-.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= .(v$mx0)-.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[4]],bquote(paste("S sera remplacé par l'écart-type du prochain échantillon.",sep="")),cex=cex.hypoth,pos=4)
	}
      } 
    }
    if(v$dirtest == "unilatd"){
      if(v$thresholds == "formula"){
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu <= mu[0]," , ",H[1]," : ",mu > mu[0],sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > mu[0]+t[group("(",list(n-1,1-alpha),")")] %.% frac(S,sqrt(n)),sep="")),cex=cex.hypoth,pos=4)
	text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= mu[0]+t[group("(",list(n-1,1-alpha),")")] %.% frac(S,sqrt(n)),sep="")),cex=cex.hypoth,pos=4)
      } else {
	text(0,hypoth.text.levels[[1]],bquote(paste("Hypothèses : ",H[0]," : ",mu <= .(v$mx0)," , ",H[1]," : ",mu < .(v$mx0),sep="")),cex=cex.hypoth,pos=4)
	if(length(rv$samples.z)>0){
	  if(v$thresholds == "calcul"){
	    text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > .(v$mx0)+.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) >= .(v$mx0)+.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[4]],bquote(paste(S == .(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]]))," est l'écart-type du dernier échantillon obtenu.",sep="")),cex=cex.hypoth,pos=4)
	  } else {
	    text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > .(cv$confidence.t.limit.sup.unilat[[length(rv$samples.z)]]),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= .(cv$confidence.t.limit.sup.unilat[[length(rv$samples.z)]]),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[4]],bquote(paste("Seuils critiques calculés avec ",S == .(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sep="")),cex=cex.hypoth,pos=4)
	  }
	} else {
	    text(0,hypoth.text.levels[[2]],labels=bquote(paste(RH[0]," si ",bar(x) > .(v$mx0)+.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[3]],labels=bquote(paste(NRH[0]," si ",bar(x) <= .(v$mx0)+.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),sep="")),cex=cex.hypoth,pos=4)
	    text(0,hypoth.text.levels[[4]],bquote(paste("S sera remplacé par l'écart-type du prochain échantillon.",sep="")),cex=cex.hypoth,pos=4)
	}
      } 
    }
}
if(v$showh0){
    ##################
    ## Plot H0      ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,3.1))
    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))

    if(v$dirtest == "bilat"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu == mu[0],sep="")),cex=1.4, pos=4)
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : [ ",mu[0]-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(S,sqrt(n))," , ",mu[0]+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(S,sqrt(n)),"]",sep="")),cex.main=1.5)
      } else {
	if(length(rv$samples.z)>0){
	  if(v$thresholds == "calcul"){
	    title(main=bquote(paste("Confiance sous ",H[0]," pour le dernier échantillon : [ ",.(v$mx0)-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n)))," , ",.(v$mx0)+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n))),"]",sep="")),cex.main=1.5)
	  } else {
	    title(main=bquote(paste("Confiance sous ",H[0]," pour le dernier échantillon : [ ",.(cv$confidence.t.limit.inf.bilat[[length(rv$samples.z)]])," , ",.(cv$confidence.t.limit.sup.bilat[[length(rv$samples.z)]]),"]",sep="")),cex.main=1.5)
	  }
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : [ ",.(v$mx0)-.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n)))," , ",.(v$mx0)+.(round(qt(1-cv$alpha/2,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),"]",sep="")),cex.main=1.5)
	}
      }
    }
    if(v$dirtest == "unilatg"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu >= mu[0],sep="")),cex=1.4, pos=4)
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : [ ",mu[0]-t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(S,sqrt(n))," , ",infinity,"]",sep="")),cex.main=1.5)
      } else {
	if(length(rv$samples.z)>0){
	  if(v$thresholds == "calcul"){
	    title(main=bquote(paste("Confiance sous ",H[0]," pour le dernier échantillon : [ ",.(v$mx0)-.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n)))," , ",infinity,"]",sep="")),cex.main=1.5)
	  } else {
	    title(main=bquote(paste("Confiance sous ",H[0]," pour le dernier échantillon : [ ",.(cv$confidence.t.limit.inf.unilat[[length(rv$samples.z)]])," , ",infinity,"]",sep="")),cex.main=1.5)
	  }
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : [ ",.(v$mx0)-.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(S,sqrt(.(v$n)))," , ",infinity,"]",sep="")),cex.main=1.5)
	}

      }
    }
    if(v$dirtest == "unilatd"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[0]," : ", mu <= mu[0],sep="")),cex=1.4, pos=4)
      if(v$thresholds == "formula"){
	title(main=bquote(paste("Confiance sous ",H[0]," : [ ",- infinity," , ",mu[0]+t[group("(",list(n-1,1-frac(alpha,2)),")")] %.% frac(S,sqrt(n)),"]",sep="")),cex.main=1.5)
      } else {
	if(length(rv$samples.z)>0){
	  if(v$thresholds == "calcul"){
	    title(main=bquote(paste("Confiance sous ",H[0]," pour le dernier échantillon : [ ",infinity," , ",.(v$mx0)+.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(.(sprintf("%.2f",cv$samples.x.sd[[length(rv$samples.z)]])),sqrt(.(v$n))),"]",sep="")),cex.main=1.5)
	  } else {
	    title(main=bquote(paste("Confiance sous ",H[0]," pour le dernier échantillon : [ ",infinity," , ",.(cv$confidence.t.limit.sup.unilat[[length(rv$samples.z)]]),"]",sep="")),cex.main=1.5)
	  }
	} else {
	  title(main=bquote(paste("Confiance sous ",H[0]," : [ ",- infinity," , ",.(v$mx0)+.(round(qt(1-cv$alpha,v$n-1),2)) %.% frac(S,sqrt(.(v$n))),"]",sep="")),cex.main=1.5)
	}
      }
    }
    
    lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx0,cv$maxdmx*1.1,labels=bquote(mu[0]),cex=1.2)


    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
      	if(i == cv$samples.x.n.toshow){
	  if(v$showrh1h0){
	    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
	    #points(cv$xh0.t[[i]],cv$yh0.t[[i]],type="l")
	    #text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* frac(sigma^2,sqrt(n)) ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
	  }
	}
	ic.halfheight=(cv$maxdmx/(v$nss+1))/2
	## Confidence interval compute under H0 : polygones
	if(v$dirtest == "bilat"){
	  polygon(c(x.lim.min,x.lim.min,cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.inf.bilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	  polygon(c(cv$confidence.t.limit.sup.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]],x.lim.max,x.lim.max),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	  polygon(c(cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	}
	if(v$dirtest == "unilatg"){
	  polygon(c(x.lim.min,x.lim.min,cv$confidence.t.limit.inf.unilat.toshow[[i]],cv$confidence.t.limit.inf.unilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	  polygon(c(cv$confidence.t.limit.inf.unilat.toshow[[i]],cv$confidence.t.limit.inf.unilat.toshow[[i]],x.lim.max,x.lim.max),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	}
	if(v$dirtest == "unilatd"){
	  polygon(c(x.lim.min,x.lim.min,cv$confidence.t.limit.sup.unilat.toshow[[i]],cv$confidence.t.limit.sup.unilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	  polygon(c(cv$confidence.t.limit.sup.unilat.toshow[[i]],cv$confidence.t.limit.sup.unilat.toshow[[i]],x.lim.max,x.lim.max),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	}

	text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }

    mtext(bquote(paste(bar(x)," vs confiance => conclusion",sep="")),side=4,line=1,at=cv$maxdmx*1.1,las=2)
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
	if(v$dirtest == "bilat"){
	  if(cv$test.t.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) %notin% group("[",list(.(cv$confidence.t.limit.inf.bilat.toshow[[i]]),.(cv$confidence.t.limit.sup.bilat.toshow[[i]])),"]") %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.t.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) %in% group("[",list(.(cv$confidence.t.limit.inf.bilat.toshow[[i]]),.(cv$confidence.t.limit.sup.bilat.toshow[[i]])),"]") %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatg"){
	  if(cv$test.t.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) < .(cv$confidence.t.limit.inf.unilat.toshow[[i]]) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.t.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) >= .(cv$confidence.t.limit.inf.unilat.toshow[[i]]) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	}
	if(v$dirtest == "unilatd"){
	  if(cv$test.t.conclusion.toshow[[i]] == "rh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) > .(cv$confidence.t.limit.sup.unilat.toshow[[i]]) %=>% RH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
	  }
	  if(cv$test.t.conclusion.toshow[[i]] == "nrh0"){
	    mtext(bquote(paste(.(sprintf("%.2f",cv$samples.x.m.toshow[[i]])) <= .(cv$confidence.t.limit.sup.unilat.toshow[[i]]) %=>% NRH[0],sep="")),side=4,line=1,at=cv$samples.y.toshow[[i]][1],las=2)
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
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.true,color.false),cex.names=1.25,cex.axis=1.2)
      #text(barplot.kH0,includes,label=paste(includes,"%",sep=""),pos=3,cex=1.2)
      
      testmean<-data.frame(c(cv$test.t.conclusion.n.nrh0,cv$test.t.conclusion.pc.nrh0),c(" "," "),c(cv$test.t.conclusion.n.rh0,cv$test.t.conclusion.pc.rh0))
      colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
      rownames(testmean)<-c("n ","% ")
      addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)
    
    if(v$showquant){
      par(mai=c(0.5,0.75,0,0.1))#,mfrow=c(2,1)
      plot(cv$t,cv$t.d,xlab="",ylab="Densité",cex.lab=1.2,bty="n",xlim=c(-5,5),ylim=c(0,0.6),xaxp=c(-5,5,10),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2)#xlab=bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep=""))
      
      axis(2,las=2,yaxp=c(0,0.4,4),cex.axis=1.2)
      text(-4.9,0.35,bquote(paste(t *"~"* t[(n-1)] ,sep="")),pos=4,cex=1.4)
      
      polygon(c(-4.8,-4.8,-4.4,-4.4),c(0.4*0.675,0.4*0.775,0.4*0.775,0.4*0.675),col=color.false)
      text(-4.4,0.4*0.7,labels=bquote(paste(alpha == .(cv$alpha),sep='')),cex=1.4, pos=4)
      polygon(c(-4.8,-4.8,-4.4,-4.4),c(0.4*0.475,0.4*0.575,0.4*0.575,0.4*0.475),col=color.true)
      text(-4.4,0.4*0.5,labels=bquote(paste(1 - alpha == .(cv$confidence),sep='')),cex=1.4, pos=4)

      if(v$dirtest == "bilat"){
		polygon(c(cv$t.th0.a,max(cv$t.th0.a)),c(cv$t.yh0.a,0),col=color.false)
 		polygon(c(min(cv$t.th0.b),cv$t.th0.b,max(cv$t.th0.b)),c(0,cv$t.yh0.b,0),col=color.true)
 		polygon(c(min(cv$t.th0.c),cv$t.th0.c),c(0,cv$t.yh0.c),col=color.false)
# 		  
		lines(c(qt(cv$alpha/2,v$n-1),qt(cv$alpha/2,v$n-1)),c(0,dt(qt(cv$alpha/2,v$n-1),v$n-1)))
		text(qt(cv$alpha/2,v$n-1),dt(qt(cv$alpha/2,v$n-1),v$n-1)+0.05,bquote(paste(-t[group("(",list(n-1,1-frac(alpha,2)),")")] == .(round(qt(cv$alpha/2,v$n-1),2)),sep="")),cex=1.4)
# 		
 		lines(c(qt(1-cv$alpha/2,v$n-1),qt(1-cv$alpha/2,v$n-1)),c(0,dt(qt(1-cv$alpha/2,v$n-1),v$n-1)))
 		text(qt(1-cv$alpha/2,v$n-1),dt(qt(1-cv$alpha/2,v$n-1),v$n-1)+0.05,bquote(paste(t[group("(",list(n-1,1-frac(alpha,2)),")")] == .(round(qt(1-cv$alpha/2,v$n-1),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatg"){
		polygon(c(cv$t.th0.a,max(cv$t.th0.a)),c(cv$t.yh0.a,0),col=color.false)
		polygon(c(min(cv$t.th0.b),cv$t.th0.b),c(0,cv$t.yh0.b),col=color.true)
      
		lines(c(qt(cv$alpha,v$n-1),qt(cv$alpha,v$n-1)),c(0,dt(qt(cv$alpha,v$n-1),v$n-1)))
		text(qt(cv$alpha,v$n-1),dt(qt(cv$alpha,v$n-1),v$n-1)+0.05,bquote(paste(-t[group("(",list(n-1,1-alpha),")")] == .(round(qt(cv$alpha,v$n-1),2)),sep="")),cex=1.4)
      }
      if(v$dirtest == "unilatd"){
		polygon(c(cv$t.th0.b,max(cv$t.th0.b)),c(cv$t.yh0.b,0),col=color.true)
		polygon(c(min(cv$t.th0.c),cv$t.th0.c),c(0,cv$t.yh0.c),col=color.false)
      
		lines(c(qt(1-cv$alpha,v$n-1),qt(1-cv$alpha,v$n-1)),c(0,dt(qt(1-cv$alpha,v$n-1),v$n-1)))
		text(qt(1-cv$alpha,v$n-1),dt(qt(1-cv$alpha,v$n-1),v$n-1)+0.05,bquote(paste(t[group("(",list(n-1,1-alpha),")")] == .(round(qt(1-cv$alpha,v$n-1),2)),sep="")),cex=1.4)
      }
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
    }
}
 
  if(v$showh1){
    ##################
    ## Plot H1     ##
    ##################
    #cv$maxdmx=0.05
    par(mai=c(0.5,1,0.5,0.1))

    plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.2,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*1.2),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20))
    
    if(v$dirtest == "bilat"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu != mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatg"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu < mu[0],sep="")),cex=1.4, pos=4)
    }
    if(v$dirtest == "unilatd"){
      text(x.lim.min,signif(cv$maxdmx,1)*1.1,labels=bquote(paste(H[1]," : ", mu > mu[0],sep="")),cex=1.4, pos=4)
    }
    
    lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1),lty=2,lwd=1)
    text(v$mx1,cv$maxdmx*1.1,labels=bquote(mu),cex=1.2)
    
    if(cv$samples.x.n.toshow>0){
      for(i in 1:cv$samples.x.n.toshow){
	if(i == cv$samples.x.n.toshow){
	  if(v$showrh1h0){
	    #axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
	    #points(cv$xh1.t[[i]],cv$yh1.t[[i]],type="l")
	    #text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* frac(sigma^2,sqrt(n)) ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx/sqrt(v$n))) ,sep='')),cex=1.4, pos=4)
	  }
	}

	## Confidence interval compute under H0 : polygones
	if(v$dirtest == "bilat"){
	  polygon(c(x.lim.min,x.lim.min,cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.inf.bilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	  polygon(c(cv$confidence.t.limit.sup.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]],x.lim.max,x.lim.max),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	  polygon(c(cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.inf.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]],cv$confidence.t.limit.sup.bilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	}
	if(v$dirtest == "unilatg"){
	  polygon(c(x.lim.min,x.lim.min,cv$confidence.t.limit.inf.unilat.toshow[[i]],cv$confidence.t.limit.inf.unilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	  polygon(c(cv$confidence.t.limit.inf.unilat.toshow[[i]],cv$confidence.t.limit.inf.unilat.toshow[[i]],x.lim.max,x.lim.max),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	}
	if(v$dirtest == "unilatd"){
	  polygon(c(x.lim.min,x.lim.min,cv$confidence.t.limit.sup.unilat.toshow[[i]],cv$confidence.t.limit.sup.unilat.toshow[[i]]),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.false)
	  polygon(c(cv$confidence.t.limit.sup.unilat.toshow[[i]],cv$confidence.t.limit.sup.unilat.toshow[[i]],x.lim.max,x.lim.max),c(cv$samples.y.toshow[[i]][1]-ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]+ic.halfheight,cv$samples.y.toshow[[i]][1]-ic.halfheight),col=color.true)
	} 
	text(sprintf("%.2f",cv$samples.x.m.toshow[[i]]),cv$samples.y.toshow[[i]][1],labels=bquote(bar(x)),cex=1.5)
      }
    }

    ## Plot of power
    if(v$showEvolPower == "none"){
      if(length(cv$samples.x)>0){
	includes<-c("NRHo"=cv$test.t.conclusion.pc.nrh0,"RHo"=cv$test.t.conclusion.pc.rh0)
      } else {
	includes<-c("NRHo"=0,"RHo"=0)#"µ1 ⊄ IC"=0
      }
      par(mai=c(0.5,3.3,0,0))
      barplot.kH0<-barplot(includes,ylim=c(0,150),yaxp=c(0,100,2),col = c(color.false,color.true),cex.names=1.25,cex.axis=1.2)

      testmean<-data.frame(c(cv$test.t.conclusion.n.nrh0,cv$test.t.conclusion.pc.nrh0),c(" "," "),c(cv$test.t.conclusion.n.rh0,cv$test.t.conclusion.pc.rh0))
      colnames(testmean)<-c(" NRHo "," "," RHo ")#"∈",""," ∉ "
      rownames(testmean)<-c("n ","% ")
      addtable2plot(-0.5,115,testmean,bty="n",display.rownames=TRUE,hlines=FALSE,cex=1.2,xjust=0,yjust=1)
    }
    if(v$showEvolPower == "emp"){
      if(length(cv$vect.n.samples)>0){
	if(cv$n.samples<20){#IF there is less than 20 samples, set the x axis limit to 20. Else set it to number of samples
	  npclim<-20
	} else {
	  npclim<-cv$n.samples
	}
      } else {
	npclim<-20
      }
      par(mai=c(0.5,0.6,0.5,0.1))
      plot(cv$vect.n.samples,cv$test.t.conclusion.pcrh0.vect,type="l",lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1.2,cex.axis=1.2,ylim=c(0,120),yaxp=c(0,100,2),ylab=bquote(paste("%",RH[0],sep="")),xlab="",xaxp=c(0,npclim,2),xlim=c(0,npclim))#See plot of reality for parameters explanataions
      axis(2,las=2,yaxp=c(0,100,2),cex.axis=1.2)
      if(length(rv$samples.z)>0){
      lines(x<-c(0,npclim),y <- c(cv$power.t[[length(rv$samples.z)]]*100,cv$power.t[[length(rv$samples.z)]]*100),lty=3)
      text(npclim*0.01,(cv$power.t[[length(rv$samples.z)]]*100)-5,expression(1-beta),pos=4)
      }
    }
    if(v$showEvolPower == "theor"){
      if(length(rv$samples.z)>0){
	par(mai=c(0.5,0.6,0.5,0.1))#,mfrow=c(2,1)
	plot(cv$z.diff.mu,cv$t.power,xlab=bquote(paste(mu-mu[0],sep="")),ylab=bquote(paste("Puissance ",1 - beta,sep="")),bty="n",xlim=c(-50,50),xaxp=c(-50,50,10),ylim=c(0,1.2),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2,cex.lab=1.2)#
	polygon(c(-50,cv$z.diff.mu,50),c(0,cv$t.power,0),col=color.true)
	polygon(c(-50,cv$z.diff.mu,50),c(1,1-(1-cv$t.power),1),col=color.false)
	axis(2,las=2,yaxp=c(0,1,4),ylim=c(0,1),cex.axis=1.2)
	lines(c(v$mx1-v$mx0,v$mx1-v$mx0),c(0,cv$power.t[[length(rv$samples.z)]]),lty=3)
	lines(c(-50,v$mx1-v$mx0),c(cv$power.t[[length(rv$samples.z)]],cv$power.t[[length(rv$samples.z)]]),lty=3)
      } else {
        par(mai=c(0.5,0.6,0.5,0.1))
	plot(c(-1),c(-1),xlab=bquote(paste(mu-mu[0],sep="")),ylab=bquote(paste("Puissance ",1 - beta,sep="")),bty="n",xlim=c(-50,50),xaxp=c(-50,50,10),ylim=c(0,1.2),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2,cex.lab=1.2)#
	axis(2,las=2,yaxp=c(0,1,4),ylim=c(0,1),cex.axis=1.2)
      }
    } 
    
    if(v$showquant){
      par(mai=c(0.5,0.75,0,0.1))#,mfrow=c(2,1)
      plot(cv$t,cv$t.d,xlab="",ylab="Densité",cex.lab=1.2,bty="n",xlim=c(-5,5),ylim=c(0,0.6),xaxp=c(-5,5,10),type='l',xaxs="i",yaxs="i",yaxt="n",cex.axis=1.2)#xlab=bquote(paste(Z *"~"* N ( 0 *","* 1 ) ,sep=""))
      
      axis(2,las=2,yaxp=c(0,0.4,4),cex.axis=1.2)
      text(-4.9,0.35,bquote(paste(t *"~"* t[(n-1)] ,sep="")),pos=4,cex=1.4)
      if(length(rv$samples.z)>0){
	polygon(c(-4.8,-4.8,-4.4,-4.4),c(0.4*0.675,0.4*0.775,0.4*0.775,0.4*0.675),col=color.false)
	text(-4.4,0.4*0.7,labels=bquote(paste(beta == .(cv$beta.t[[length(rv$samples.z)]]),sep='')),cex=1.4, pos=4)
	polygon(c(-4.8,-4.8,-4.4,-4.4),c(0.4*0.475,0.4*0.575,0.4*0.575,0.4*0.475),col=color.true)
	text(-4.4,0.4*0.5,labels=bquote(paste(1 - beta == .(cv$power.t[[length(rv$samples.z)]]),sep='')),cex=1.4, pos=4)
      
	if(v$dirtest == "bilat"){
	  polygon(c(cv$t.th1.a[[length(rv$samples.z)]],max(cv$t.th1.a[[length(rv$samples.z)]])),c(cv$t.yh1.a[[length(rv$samples.z)]],0),col=color.true)
	  polygon(c(min(cv$t.th1.b[[length(rv$samples.z)]]),cv$t.th1.b[[length(rv$samples.z)]],max(cv$t.th1.b[[length(rv$samples.z)]])),c(0,cv$t.yh1.b[[length(rv$samples.z)]],0),col=color.false)
	  polygon(c(min(cv$t.th1.c[[length(rv$samples.z)]]),cv$t.th1.c[[length(rv$samples.z)]]),c(0,cv$t.yh1.c[[length(rv$samples.z)]]),col=color.true)
	      
	  lines(c(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],cv$t.t.lim.inf.h1[[length(rv$samples.z)]]),c(0,dt(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],v$n-1)))
	  text(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],dt(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],v$n-1)+0.05,bquote(paste(t[1] == .(round(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],2)),sep="")),cex=1.4)
	  
	  lines(c(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],cv$t.t.lim.sup.h1[[length(rv$samples.z)]]),c(0,dt(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],v$n-1)))
	  text(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],dt(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],v$n-1)+0.05,bquote(paste(t[2] == .(round(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],2)),sep="")),cex=1.4)
	}
	if(v$dirtest == "unilatg"){
	  polygon(c(cv$t.th1.a[[length(rv$samples.z)]],max(cv$t.th1.a[[length(rv$samples.z)]])),c(cv$t.yh1.a[[length(rv$samples.z)]],0),col=color.true)
	  polygon(c(min(cv$t.th1.b[[length(rv$samples.z)]]),cv$t.th1.b[[length(rv$samples.z)]]),c(0,cv$t.yh1.b[[length(rv$samples.z)]]),col=color.false)
  
	  lines(c(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],cv$t.t.lim.inf.h1[[length(rv$samples.z)]]),c(0,dt(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],v$n-1)))
	  text(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],dt(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],v$n-1)+0.05,bquote(paste(t == .(round(cv$t.t.lim.inf.h1[[length(rv$samples.z)]],2)),sep="")),cex=1.4)
	}
	if(v$dirtest == "unilatd"){
	  polygon(c(cv$t.th1.b[[length(rv$samples.z)]],max(cv$t.th1.b[[length(rv$samples.z)]])),c(cv$t.yh1.b[[length(rv$samples.z)]],0),col=color.false)
	  polygon(c(min(cv$t.th1.c[[length(rv$samples.z)]]),cv$t.th1.c[[length(rv$samples.z)]]),c(0,cv$t.yh1.c[[length(rv$samples.z)]]),col=color.true)

	  lines(c(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],cv$t.t.lim.sup.h1[[length(rv$samples.z)]]),c(0,dt(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],v$n-1)))
	  text(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],dt(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],v$n-1)+0.05,bquote(paste(t == .(round(cv$t.t.lim.sup.h1[[length(rv$samples.z)]],2)),sep="")),cex=1.4)
	}
      }
    } else {
      par(mai=c(0.5,0.5,0,0))
      plot(c(0),c(0),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1.1),type='l')
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
	samples.as.list[[i]]<-c(round(cv$samples.x[[i]],2),c(""),round(cv$samples.x.m[[i]],2),round(cv$samples.x.sd[[i]],2))
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
    if(input$Tabset == "1"){
        paste("Tab",input$Tabset," ",cv$samples.x.n.toshow,sep=" ")
    }
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
