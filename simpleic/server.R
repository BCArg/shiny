## simpleIC Shiny/R app server.R                                           
##                                                                      
## Author(s) :
## -----------
## J.J.
## Orginal version by Grégoire Vincke http://www.uclouvain.be/gregoire.vincke       
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
library(ggplot2)

color.blue<-rgb(0,0,0.9)
color.true<-rgb(0,0.7,0)
color.false<-rgb(1,0,0,0.9)
oui.color.true<-rgb(0.3,0.3,0.3)
oui.color.false<-rgb(.6,.6,.6)
text.color.true<-rgb(0.2,0.2,0.2)
text.color.false<-rgb(.5,.5,.5)
non.color.true<-rgb(0.8,0.8,0.8)
non.color.false<-rgb(0.93,0.93,0.93)


density.true<-10
density.false<-25

cex.hypoth<-1.7#size of hypothesis descriptions
hypoth.text.levels<-c(1,0.7,0.4,0.1)

x.lim.min<-0
x.lim.max<-60
x.amp<-x.lim.max-x.lim.min

# possible values for the mean, cf ui.R mx1, mx0, mx
mu.vec<-c(x.lim.min:x.lim.max)#c(25:75)

full.plot.width<-1000

shinyServer(function(input, output){
  
  rv <- reactiveValues()# Create a reactiveValues object, to let us use settable reactive values
  
  rv$last.takesample.value<-0
  #rv$samples.z<-list() # all observations
  rv$samples.mat<-c() # matrix of all observations, each line one sample
  rv$new.sample<-c() # new matrix of observations, each line one sample
  rv$cv.ls<-list() # calculated values
  
  rv$lastAction <- 'none' # To start out, lastAction == NULL, meaning nothing clicked yet
  # An observe block for each button, to record that the action happened

  # Calculations only needed if one of these values are changed, so observe them
  rv$mx.c<-0 # population mean
  rv$sx.c<-0 # population deviation
  rv$IC.k.c<-0 # IC length
  rv$typIC.c<-'' # Type 'eCVk' empiric, 'vCVk' variance connue, 'sCVk' variance inconnue
  # Create all samples new if a change is made in sample size
  rv$n.c<-0 # sample size as in ui
  rv$tn.c<-0 # total numer of samples

  # if take sample
  observe({
    if (input$takesample != 0) {
      rv$lastAction <- 'takesample'
    }
  })

  # if reset all new
  observe({
    if (input$reset != 0) {
      rv$lastAction <- 'reset'
      rv$last.takesample.value<-0
  #    rv$samples.z<-list()
      rv$samples.mat<-c()
      rv$cv.ls<-list()
      rv$mx.c<-0 # population mean
      rv$sx.c<-0 # population deviation
      rv$IC.k.c<-0 # IC length
      rv$typIC.c<-'' # Type 'eCVk' empiric, 'vCVk' variance connue, 'sCVk' variance inconnue
      rv$n.c<-0 # sample size as in ui
      rv$tn.c<-0 # total numer of samples
    }
  })

  ## observe({
  ##   if (input$n != rv$n.c) {
  ##       isolate({#Now do the expensive stuff
  ##         rv$n.c<-input$n
  ##         rv$samples.mat<-matrix(rnorm((rv$tn.c+input$ns)*rv$n.c),ncol=rv$n.c)
  ##         rv$tn.c<-length(rv$samples.mat[,1]) # new total number of samples
  ##     })
  ##   }
  ## })

  getSamples<-reactive({#créee n valeurs aléatoires N(0;1) quand input$takesample est implémenté (quand le bouton takesample est pressé)
    if(input$takesample > rv$last.takesample.value && rv$lastAction == "takesample"){
      return(isolate({#Now do the expensive stuff
          rv$new.sample<-matrix(rnorm(input$ns*input$n),ncol=input$n)
 	  return(TRUE)
      }))
    } else {
      return(FALSE)
    }})

  getPlotHeight <- function() {
    unit.height<-300
    return(3*unit.height)
  }
    
  getInputValues<-reactive({
      return(input)#collect all inputs
  })
  
  getComputedValues<-reactive({
      # returns TRUE if new calculations other wise FALSE
      # results hold in rv$cv.ls
      # gives TRUE back if new values calculated otherwise FALSE
      calc.new<-FALSE # we do not want  to calculate all values again
      # did we create new samples?
      sample.new<-getSamples() # if TRUE then append new observations from rv$new.sample
      v<-getInputValues() # get all values of input list
      # check if sample size was changed
      if (v$n != rv$n.c){# if changed create a new observation matrix of correct size
          rv$n.c<-v$n
          if(rv$tn.c>0){
              rv$samples.mat<-matrix(rnorm(rv$tn.c*rv$n.c),ncol=rv$n.c)
          }
          calc.new<-TRUE # we have to calculate all values again
      }
      if(sample.new){#if new observations created, append them
          sample.mat<-mat.or.vec(rv$tn.c + v$ns,v$n)
          if(rv$tn.c>0){
              sample.mat[1:rv$tn.c,]<-rv$samples.mat
          }
          sample.mat[(rv$tn.c+1):(rv$tn.c+v$ns),]<-rv$new.sample
          rv$samples.mat<-sample.mat  # new observations
          rv$tn.c<-length(rv$samples.mat[,1]) # new total number of samples
          calc.new<-TRUE # we have to calculate all values again
      }
         
      # check if caluations are needed due to parameter changes
      # could be still optimized since a change in rv$IC.k.c does not needs a whole new calcul 
      if (v$mx1 != rv$mx.c){# population mean changed
          rv$mx.c<-v$mx1 # update
          calc.new<-TRUE
      }

      if (v$sx != rv$sx.c){# population sd changed
          rv$sx.c<-v$sx # update
          calc.new<-TRUE
      }

      if (v$k != rv$IC.k.c){# IC length changed
          rv$IC.k.c<-v$k # update
          calc.new<-TRUE
      }

      if (v$CVk != rv$typIC.c){# type IC  changed
          rv$typIC.c<-v$CVk # update
          calc.new<-TRUE
      }

      # new calulations if new obsarvations or
      if(calc.new){
          cv<-list()#created empty computed values list
          ## Define reality parameters
          cv$vx<-v$sx^2#compute variance of Reality distribution
    
          ## Computation of x y coordinates for Normal curve of Reality
          z<-seq(-5,5,length=100)
          cv$xr<-(z*v$sx)+v$mx1 #x for Reality
          cv$yr<-dnorm(cv$xr,mean=v$mx1,sd=v$sx)#y for Reality
	
        
          ## Computation of sample related values ##
          cv$samples.x.mat<-c() # matrix of observations, each line a sample
          cv$samples.x.m.vec<-c() # vector of mean values, each line a sample
          cv$samples.x.sd.vec<-c() # vector of sd values, each line a sample
          cv$ic.k.limit.mat<-c() # matrix of limits, columns lower and upper bound , lines by sample
          cv$ic.k.inc.allmu.mat<-c() # matrix of TRUE/FALSE if mu in IC columns all mu.vec=c(20:60) and lines by sample 
          cv$pc.ic.k.inc.allmu.vec<-c() # for all mu increment percentage covered by IC
          cv$n.ic.k.inc.allmu.vec<-c() # for all mu increment number covered by IC
        
          cv$n.samples<-rv$tn.c # number of samples
          cv$samples.x.n.toshow<-0
      
          if(cv$n.samples>0){
              cv$samples.x.mat<-mat.or.vec(cv$n.samples,v$n)
              cv$ic.k.limit.mat<-mat.or.vec(cv$n.samples,2)
              cv$vect.n.samples<-c(1:cv$n.samples)
              cv$samples.x.mat<-round((rv$samples.mat*v$sx)+v$mx1,2)#Then sample values are compute with mx1 mean and standard deviation
              ## Computation of descriptives
              cv$samples.x.m.vec<-round(apply(cv$samples.x.mat,1,mean),2)#means of samples
              cv$samples.x.sd.vec<-round(apply(cv$samples.x.mat,1,sd),2)#sds of samples
              ## Computation of confidence intervals for the mean µ ##            
              if(v$CVk == 'vCVk'){#compute the CI limits with k value and known variance
                  cv$ic.k.limit.mat[,1]<-round(cv$samples.x.m.vec-v$k*v$sx*(v$n)^(-.5),2)
                  cv$ic.k.limit.mat[,2]<-round(cv$samples.x.m.vec+v$k*v$sx*(v$n)^(-.5),2)
              }
              if(v$CVk == 'sCVk'){#compute the CI limits with k value and unknown variance
                  cv$ic.k.limit.mat[,1]<-round(cv$samples.x.m.vec-v$k*cv$samples.x.sd.vec*(v$n)^(-.5),2)
                  cv$ic.k.limit.mat[,2]<-round(cv$samples.x.m.vec+v$k*cv$samples.x.sd.vec*(v$n)^(-.5),2)
              }
              if(v$CVk == 'eCVk'){#compute the CI limits with empiric k value 
                  cv$ic.k.limit.mat[,1]<-round(cv$samples.x.m.vec-v$k,2)
                  cv$ic.k.limit.mat[,2]<-round(cv$samples.x.m.vec+v$k,2)
              }                 
              ## Check for all values in mu.vec if in IC
              cv$ic.k.inc.allmu.mat<-sapply(mu.vec,function(x){return (cv$ic.k.limit.mat[,1] <=x & x<=cv$ic.k.limit.mat[,2])})
              ## Calculate for all values in mu.vec frequencies absolute and relative
              cv$n.ic.k.inc.allmu.vec<-apply(matrix(cv$ic.k.inc.allmu.mat,ncol=length(mu.vec)),2,sum)
              cv$pc.ic.k.inc.allmu.vec<-round(cv$n.ic.k.inc.allmu.vec/cv$n.samples,3)*100
              ## Define colors
              cv$ic.k.inc.mu.color.mat<-matrix(ifelse(cv$ic.k.inc.allmu.mat,oui.color.true,oui.color.false),ncol=length(mu.vec))
              cv$ic.k.inc.mu0.color.mat<-matrix(ifelse(cv$ic.k.inc.allmu.mat,color.false,color.true),ncol=length(mu.vec))
              cv$ic.k.inc.mu1.color.mat<-matrix(ifelse(cv$ic.k.inc.allmu.mat,color.true,color.false),ncol=length(mu.vec))
              ## Define subset to plot
              cv$samples.x.from<-1
              if(cv$n.samples>v$nss){
                  cv$samples.x.from<-cv$n.samples-v$nss+1
              }
              cv$samples.x.to<-cv$n.samples
              cv$samples.x.mat.toshow<-matrix(cv$samples.x.mat[cv$samples.x.from:cv$samples.x.to,],ncol=v$n)
              cv$samples.x.m.vec.toshow<-cv$samples.x.m.vec[cv$samples.x.from:cv$samples.x.to]
              cv$samples.x.sd.vec.toshow<-cv$samples.x.sd.vec[cv$samples.x.from:cv$samples.x.to]
              cv$samples.x.i.vec.toshow<-c(cv$samples.x.from:cv$samples.x.to)
              cv$ic.k.limit.mat.toshow<-matrix(cv$ic.k.limit.mat[cv$samples.x.from:cv$samples.x.to,],ncol=2)
              cv$samples.y.mat.toshow<-c() # plot line by line the values, here corresponding y-values 
              cv$samples.x.n.toshow<-length(cv$samples.x.mat.toshow[,1])
              cv$ic.k.inc.mu.color.vec.toshow<-c() # color the IC for mu
              cv$ic.k.inc.mu0.color.vec.toshow<-c() # color the IC for mu0
              cv$ic.k.inc.mu1.color.vec.toshow<-c() # color the IC for mu1

              if(cv$samples.x.n.toshow>0){
                  cv$samples.y.mat.toshow<-matrix(rep(0.09/(v$nss+1)*c(1:cv$samples.x.n.toshow),length(cv$samples.x.mat.toshow[1,])),nrow=length(cv$samples.x.mat.toshow[,1]))
                  ##     ## Define colors if IC covers µ or µ0 or µ1
                  cv$ic.k.inc.mu.color.vec.toshow<-cv$ic.k.inc.mu.color.mat[cv$samples.x.from:cv$samples.x.to,v$mx-mu.vec[1]+1]
                  cv$ic.k.inc.mu0.color.vec.toshow<-cv$ic.k.inc.mu0.color.mat[cv$samples.x.from:cv$samples.x.to,v$mx0-mu.vec[1]+1]
                  cv$ic.k.inc.mu1.color.vec.toshow<-cv$ic.k.inc.mu1.color.mat[cv$samples.x.from:cv$samples.x.to,v$mx1-mu.vec[1]+1]
              }
          }
          rv$cv.ls<-cv # set new values
      }
      
    ## Last takesample value
    rv$last.takesample.value<-v$takesample
    return(calc.new)
  })
    
  output$plotEmp <- renderPlot({
      v<-getInputValues()
      calc.new<-getComputedValues() # TRUE if new values have been calculated
      cv<-rv$cv.ls # holds calculated values
      # if graphic values of parameter are changed without new calculus, so here calculate new values
      ## Define subset to plot
      if(cv$n.samples>0){
          cv$samples.x.from<-1
          if(cv$n.samples>v$nss){
              cv$samples.x.from<-cv$n.samples-v$nss+1
          }
          cv$samples.x.to<-cv$n.samples
          cv$samples.x.mat.toshow<-matrix(cv$samples.x.mat[cv$samples.x.from:cv$samples.x.to,],ncol=v$n)
          cv$samples.x.m.vec.toshow<-cv$samples.x.m.vec[cv$samples.x.from:cv$samples.x.to]
          cv$samples.x.sd.vec.toshow<-cv$samples.x.sd.vec[cv$samples.x.from:cv$samples.x.to]
          cv$samples.x.i.vec.toshow<-c(cv$samples.x.from:cv$samples.x.to)
          cv$ic.k.limit.mat.toshow<-matrix(cv$ic.k.limit.mat[cv$samples.x.from:cv$samples.x.to,],ncol=2)
          cv$samples.y.mat.toshow<-c() # plot line by line the values, here corresponding y-values 
          cv$samples.x.n.toshow<-length(cv$samples.x.mat.toshow[,1])
          cv$ic.k.inc.mu.color.vec.toshow<-c() # color the IC for mu
          cv$ic.k.inc.mu0.color.vec.toshow<-c() # color the IC for mu0
          cv$ic.k.inc.mu1.color.vec.toshow<-c() # color the IC for mu1

          if(cv$samples.x.n.toshow>0){
              cv$samples.y.mat.toshow<-matrix(rep(0.09/(v$nss+1)*c(1:cv$samples.x.n.toshow),length(cv$samples.x.mat.toshow[1,])),nrow=length(cv$samples.x.mat.toshow[,1]))
              ##     ## Define colors if IC covers µ or µ0 or µ1
              cv$ic.k.inc.mu.color.vec.toshow<-cv$ic.k.inc.mu.color.mat[cv$samples.x.from:cv$samples.x.to,v$mx-mu.vec[1]+1]
              cv$ic.k.inc.mu0.color.vec.toshow<-cv$ic.k.inc.mu0.color.mat[cv$samples.x.from:cv$samples.x.to,v$mx0-mu.vec[1]+1]
              cv$ic.k.inc.mu1.color.vec.toshow<-cv$ic.k.inc.mu1.color.mat[cv$samples.x.from:cv$samples.x.to,v$mx1-mu.vec[1]+1]
          }
      }
      m<-matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
      layout(m,width=c(4,2))
      ##-------------------------------------------    
      ## Plot always Reality ##
      ##-------------------------------------------    
      cv$maxdmx=0.05
      par(mai=c(0.5,1,0.5,0))
      label<-""
      if(v$showreality){
          label<-"Density"
      }
      plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.5,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*2.1),xlab="",ylab=label,xaxp=c(x.lim.min,x.lim.max,20),main=bquote(paste("Echantillons prélevés :")),cex.main=2.3)
      #box(lty = '1373', col = 'red')
      if(cv$samples.x.n.toshow>0){
          for(i in 1:cv$samples.x.n.toshow){
              points(cv$samples.x.mat.toshow[i,],cv$samples.y.mat.toshow[i,],,cex=2.2)
              text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(bar(x)[.(cv$samples.x.i.vec.toshow[i])]),cex=2.2,col=color.blue)
          }
      }
      
      if(v$showreality){
          axis(2,las=2,yaxp=c(0,signif(cv$maxdmx,1),5),cex.axis=1.2)
          points(cv$xr,cv$yr,type="l")
          text(1,signif(cv$maxdmx,1)*0.75,labels=bquote(paste(N *"~"* ( mu *","* sigma^2 ) ," ", N *"~"* (.(v$mx1)*","*.(cv$vx)) ,sep='')),cex=1.4, pos=4)
      }

      if(v$muKn){
          ## Plot true mean only if known
          lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1.8),lty=1,lwd=3)
          text(v$mx1,cv$maxdmx*1.95,labels=bquote(mu),cex=3.5,col=color.blue)
      }
      
      ## empty plot for layout
      par(mai=c(0.5,0,0.5,4))
      plot(c(0),c(-5),col="white",xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,cv$maxdmx*2.1),bty="n",las=1)

      if(v$empPl){
          mtext(bquote(paste("Descriptives : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*(-0.1),las=2)
          if(cv$samples.x.n.toshow>0){
              for(i in 1:cv$samples.x.n.toshow){
                  mtext(bquote(paste(bar(x)[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.m.vec.toshow[i])),sep="")),side=4,line=1,at=cv$samples.y.mat.toshow[i,1],las=2,col=color.blue,cex=1.3)
                  mtext(bquote(paste(s[.(cv$samples.x.i.vec.toshow[i])] == .(sprintf("%.2f",cv$samples.x.sd.vec.toshow[i])),sep="")),side=4,line=10,at=cv$samples.y.mat.toshow[i,1],las=2,cex=1.3)
              }
          }
      }
      
     if(v$icPl){
      ##-------------------------------------------    
      ## Plot IC      ##
      ##-------------------------------------------              
         cv$maxdmx=0.05
         par(mai=c(0.5,1,0.5,0))
         plot(c(0),c(-5),lty=1,lwd=1,col="black",yaxt="n",bty="n",las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.5,xlim=c(x.lim.min,x.lim.max),ylim=c(0,cv$maxdmx*2.1),ylab="",xlab="",xaxp=c(x.lim.min,x.lim.max,20),main=bquote(paste("Intervalles de confiance:")),cex.main=2)
	 #box(lty = '1373', col = 'red')
         
         if(v$cvPl != "non"){
             if(v$cvPl == "oui"){
                 ## Plot mean mx 
                 lines(x<-c(v$mx,v$mx),y <- c(0,cv$maxdmx*1.8),lty=1,lwd=3,col=oui.color.true)
                 text(v$mx,cv$maxdmx*1.95,labels=v$mx,cex=2.5,col=oui.color.true)
                 help.color.vec<-cv$ic.k.inc.mu.color.vec.toshow
             }
             if(v$cvPl == "parOri"){
                 ## Plot mean mx1 
                 lines(x<-c(v$mx1,v$mx1),y <- c(0,cv$maxdmx*1.8),lty=1,lwd=3,col=color.true)
                 text(v$mx1,cv$maxdmx*1.95,labels=bquote(mu),cex=3.5,col=color.true)
                 help.color.vec<-cv$ic.k.inc.mu1.color.vec.toshow
             }
             if(v$cvPl == "parAlt"){
                 lines(x<-c(v$mx0,v$mx0),y <- c(0,cv$maxdmx*1.8),lty=1,lwd=3,col=color.false)
                 text(v$mx0,cv$maxdmx*1.95,labels=bquote(paste(mu,"''",sep="")),cex=3.5,col=color.false)
                 help.color.vec<-cv$ic.k.inc.mu0.color.vec.toshow
             }
         } else {
             help.color.vec<-ifelse(cv$ic.k.inc.mu0.color.vec.toshow,"black","black")
         }
         if(cv$samples.x.n.toshow>0){
             for(i in 1:cv$samples.x.n.toshow){
                 polygon(c(cv$ic.k.limit.mat.toshow[i,1],cv$ic.k.limit.mat.toshow[i,1],cv$ic.k.limit.mat.toshow[i,2],cv$ic.k.limit.mat.toshow[i,2]),c(cv$samples.y.mat.toshow[i,1]-0.0025,cv$samples.y.mat.toshow[i,1]+0.0025,cv$samples.y.mat.toshow[i,1]+0.0025,cv$samples.y.mat.toshow[i,1]-0.0025),col=help.color.vec[i])
                 text(cv$samples.x.m.vec.toshow[i],cv$samples.y.mat.toshow[i,1],labels=bquote(bar(x)[.(cv$samples.x.i.vec.toshow[i])]),cex=1.6,col="blue")
                 lines(x<-c(cv$ic.k.limit.mat.toshow[i,1],cv$samples.x.m.vec.toshow[i]-1),y <- c(cv$samples.y.mat.toshow[i,1],cv$samples.y.mat.toshow[i,1]),lwd=1,lty=2,col="black")
                 lines(x<-c(cv$samples.x.m.vec.toshow[i]+1,cv$ic.k.limit.mat.toshow[i,2]),y <- c(cv$samples.y.mat.toshow[i,1],cv$samples.y.mat.toshow[i,1]),lwd=1,lty=2,col="black")
             }
         }   

          ## empty plot for layout
          par(mai=c(0.5,0,0,4))
          plot(c(0),c(-5),col="white",xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,cv$maxdmx*2.1),bty="n",las=1)

          if(v$empPl){
              mtext(bquote(paste("Intervalles : ", N == .(cv$n.samples), sep="")),side=4,line=1,at=signif(cv$maxdmx,1)*(-0.1),las=2)
              if(cv$samples.x.n.toshow>0){
                  for(i in 1:cv$samples.x.n.toshow){
                      if(v$cvPl != "non"){
                          if(v$cvPl == "oui"){
                              help.color.vec<-cv$ic.k.inc.mu.color.vec.toshow
                          }
                          if(v$cvPl == "parOri"){
                              ## Plot mean mx1 
                              help.color.vec<-cv$ic.k.inc.mu1.color.vec.toshow
                          }
                          if(v$cvPl == "parAlt"){
                              help.color.vec<-cv$ic.k.inc.mu0.color.vec.toshow
                          }
                      } else {
                          help.color.vec<-ifelse(cv$ic.k.inc.mu0.color.vec.toshow,"black","black")
                      }
                      if(v$thresholds == "formula"){
                          if(v$CVk == 'eCVk'){
                              mtext(bquote(paste(group("[",list(bar(x)[.(cv$samples.x.i.vec.toshow[i])]-c,bar(x)[.(cv$samples.x.i.vec.toshow[i])]+c),"]"),sep="")),side=4,line=1,at=cv$samples.y.mat.toshow[i,1],las=2,col=help.color.vec[i],cex=1.3)
                          }
                          if(v$CVk == 'vCVk'){
                              mtext(bquote(paste(group("[",list(bar(x)[.(cv$samples.x.i.vec.toshow[i])]-c*'*'*sigma/sqrt(n),bar(x)[.(cv$samples.x.i.vec.toshow[i])]+c*'*'*sigma/sqrt(n)),"]"),sep="")),side=4,line=1,at=cv$samples.y.mat.toshow[i,1],las=2,col=help.color.vec[i],cex=1.3)
                          }
                          if(v$CVk == 'sCVk'){
                              mtext(bquote(paste(group("[",list(bar(x)[.(cv$samples.x.i.vec.toshow[i])]-c*'*'*s[.(cv$samples.x.i.vec.toshow[i])]/sqrt(n),bar(x)[.(cv$samples.x.i.vec.toshow[i])]+c*'*'*s[.(cv$samples.x.i.vec.toshow[i])]/sqrt(n)),"]"),sep="")),side=4,line=1,at=cv$samples.y.mat.toshow[i,1],las=2,col=help.color.vec[i],cex=1.3)
                          }
                      }
                      if(v$thresholds == "calcul"){
                          if(v$CVk == 'eCVk'){
                              mtext(bquote(paste(group("[",list(.(sprintf("%.2f",cv$samples.x.m.vec.toshow[i]))-.(v$k),.(sprintf("%.2f",cv$samples.x.m.vec.toshow[i]))+.(v$k)),"]"),sep="")),side=4,line=1,at=cv$samples.y.mat.toshow[i,1],las=2,col=help.color.vec[i],cex=1.3)
                          }
                          if(v$CVk == 'vCVk'){
                              mtext(bquote(paste(group("[",list(.(sprintf("%.1f",cv$samples.x.m.vec.toshow[i]))-.(v$k)*'*'*.(v$sx)/.(sprintf("%.1f",v$n^.5)),.(sprintf("%.1f",cv$samples.x.m.vec.toshow[i]))+.(v$k)*'*'*.(v$sx)/.(sprintf("%.1f",v$n^.5))),"]"),sep="")),side=4,line=1,at=cv$samples.y.mat.toshow[i,1],las=2,col=help.color.vec[i],cex=1.3)
                          }
                          if(v$CVk == 'sCVk'){
                              mtext(bquote(paste(group("[",list(.(sprintf("%.1f",cv$samples.x.m.vec.toshow[i]))-.(v$k)*'*'*.(sprintf("%.1f",cv$samples.x.sd.vec.toshow[i]))/.(sprintf("%.1f",v$n^.5)),.(sprintf("%.1f",cv$samples.x.m.vec.toshow[i]))+.(v$k)*'*'*.(sprintf("%.1f",cv$samples.x.sd.vec.toshow[i]))/.(sprintf("%.1f",v$n^.5))),"]"),sep="")),side=4,line=1,at=cv$samples.y.mat.toshow[i,1],las=2,col=help.color.vec[i],cex=1.3)
                          }
                      }
                      if(v$thresholds == "result"){
                          mtext(bquote(paste(group("[",list(.(sprintf("%.2f",cv$ic.k.limit.mat.toshow[i,1])),.(sprintf("%.2f",cv$ic.k.limit.mat.toshow[i,2]))),"]"),sep="")),side=4,line=1,at=cv$samples.y.mat.toshow[i,1],las=2,col=help.color.vec[i],cex=1.3)
                      }
                  }
              }
          }
     


          if(v$cvPl != "non" && v$freqPl == "freqPloui"){
              par(mai=c(0.5,1,0.5,0))
              ## Plot bar plot of includes 2 class %
              if(cv$n.samples>0){
                  includes<-t(matrix(c(100-cv$pc.ic.k.inc.allmu.vec,cv$pc.ic.k.inc.allmu.vec),ncol=2))
              } else {
                  includes<-t(matrix(c(rep(0,length(mu.vec)),100-rep(0,length(mu.vec))),ncol=2))
              }
              barplot.kH1<-barplot(includes,names.arg=mu.vec,ylab="%",ylim=c(0,160),yaxp=c(0,100,2),col=c(non.color.false,non.color.true),cex.names=1.25,cex.axis=1.5,beside=FALSE,xaxs="i")
              #box(lty = '1373', col = 'red')
                                        #  barplot.kH1 is the vector of positions of th bars which we use next
              if(v$cvPl == "oui"  && cv$n.samples>0){
                  barplot.spp<-barplot(matrix(c(100-cv$pc.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)],cv$pc.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)]),ncol=1),col=c(oui.color.false,oui.color.true), add=TRUE,beside=FALSE,space=(barplot.kH1[(v$mx-mu.vec[1]+1)]-0.5),axes=FALSE)
                  ## ICvsmu<-data.frame(c(cv$n.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)],cv$n.samples-cv$n.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)]),c(" "," "),c(cv$pc.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)],100-cv$pc.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)]))
                  ## rownames(ICvsmu)<-c("in","out")
                  ## colnames(ICvsmu)<-c("n ","","% ")
                  ## addtable2plot(0,110,ICvsmu,bty="n",display.rownames=TRUE,hlines=FALSE,title=paste(v$mx," vs IC"),cex=1.4,xjust=0,yjust=1)

                  ICvsmu0.mat<-matrix(c(cv$n.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)],cv$n.samples-cv$n.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)],cv$pc.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)],100-cv$pc.ic.k.inc.allmu.vec[(v$mx-mu.vec[1]+1)]),ncol=2)
                  ICvsmu0.mat<-round(ICvsmu0.mat,0)
                  mtext(bquote(paste("% de couverture par les intervalles de confiance pour la valeur ",.(v$mx),"",sep=" ")),side=3,line=-2,adj = 0)
                  mtext(bquote(paste("n",sep=" ")),side=3,line=-4,adj = 1,at=20)
                  mtext(bquote(paste("%",sep=" ")),side=3,line=-4,adj = 1,at=25)
                  mtext(bquote(paste("IC contient ",.(v$mx)," : ",sep=" ")),side=3,line=-6,adj = 1,at=17,col=text.color.true)
                  mtext(bquote(paste(.(ICvsmu0.mat[1,1]),,sep=" ")),side=3,line=-6,adj = 1,at=20,col=text.color.true)
                  mtext(bquote(paste(.(ICvsmu0.mat[1,2]),,sep=" ")),side=3,line=-6,adj = 1,at=25,col=text.color.true)
                  mtext(bquote(paste("IC ne contient pas ",.(v$mx)," : ",sep=" ")),side=3,line=-8,adj = 1,at=17,col=text.color.false)
                  mtext(bquote(paste(.(ICvsmu0.mat[2,1]),,sep=" ")),side=3,line=-8,adj = 1,at=20,col=text.color.false)
                  mtext(bquote(paste(.(ICvsmu0.mat[2,2]),,sep=" ")),side=3,line=-8,adj = 1,at=25,col=text.color.false)

              }
              if(v$cvPl == "parOri" && cv$n.samples>0){
                  barplot.spp<-barplot(matrix(c(100-cv$pc.ic.k.inc.allmu.vec[(v$mx1-mu.vec[1]+1)],cv$pc.ic.k.inc.allmu.vec[(v$mx1-mu.vec[1]+1)]),ncol=1),col=c(color.false,color.true), add=TRUE,beside=FALSE,space=(barplot.kH1[(v$mx1-mu.vec[1]+1)]-0.5),axes=FALSE)


                  ICvsmu0.mat<-matrix(c(cv$n.ic.k.inc.allmu.vec[(v$mx1-mu.vec[1]+1)],cv$n.samples-cv$n.ic.k.inc.allmu.vec[(v$mx1-mu.vec[1]+1)],cv$pc.ic.k.inc.allmu.vec[(v$mx1-mu.vec[1]+1)],100-cv$pc.ic.k.inc.allmu.vec[(v$mx1-mu.vec[1]+1)]),ncol=2)
                  ICvsmu0.mat<-round(ICvsmu0.mat,0)
                  mtext(bquote(paste("% de couverture par les intervalles de confiance pour",sep=" ")),side=3,line=-2,adj = 0)
                  mtext(bquote(paste("la moyenne de la population origine ",mu," = ",.(v$mx1),"",sep=" ")),side=3,line=-2,adj = 0,col=color.true,at=36)
                  mtext(bquote(paste("n",sep=" ")),side=3,line=-4,adj = 1,at=20)
                  mtext(bquote(paste("%",sep=" ")),side=3,line=-4,adj = 1,at=25)
                  mtext(bquote(paste("IC contient μ"," : ",sep=" ")),side=3,line=-6,adj = 1,at=17,col=color.true)
                  mtext(bquote(paste(.(ICvsmu0.mat[1,1]),,sep=" ")),side=3,line=-6,adj = 1,at=20,col=color.true)
                  mtext(bquote(paste(.(ICvsmu0.mat[1,2]),,sep=" ")),side=3,line=-6,adj = 1,at=25,col=color.true)
                  mtext(bquote(paste("IC ne contient pas μ"," : ",sep=" ")),side=3,line=-8,adj = 1,at=17,col=color.false)
                  mtext(bquote(paste(.(ICvsmu0.mat[2,1]),,sep=" ")),side=3,line=-8,adj = 1,at=20,col=color.false)
                  mtext(bquote(paste(.(ICvsmu0.mat[2,2]),,sep=" ")),side=3,line=-8,adj = 1,at=25,col=color.false)

              }
              if(v$cvPl == "parAlt" && cv$n.samples>0){
                  barplot.spp<-barplot(matrix(c(100-cv$pc.ic.k.inc.allmu.vec[(v$mx0-mu.vec[1]+1)],cv$pc.ic.k.inc.allmu.vec[(v$mx0-mu.vec[1]+1)]),ncol=1),col=c(color.true,color.false), add=TRUE,beside=FALSE,space=(barplot.kH1[(v$mx0-mu.vec[1]+1)]-0.5),axes=FALSE)



                  ICvsmu0.mat<-matrix(c(cv$n.ic.k.inc.allmu.vec[(v$mx0-mu.vec[1]+1)],cv$n.samples-cv$n.ic.k.inc.allmu.vec[(v$mx0-mu.vec[1]+1)],cv$pc.ic.k.inc.allmu.vec[(v$mx0-mu.vec[1]+1)],100-cv$pc.ic.k.inc.allmu.vec[(v$mx0-mu.vec[1]+1)]),ncol=2)
                  ICvsmu0.mat<-round(ICvsmu0.mat,0)
                  mtext(bquote(paste("% de couverture par les intervalles de confiance pour",sep=" ")),side=3,line=-2,adj = 0)
                  mtext(bquote(paste("une valeur alternative ",mu,"'' = ",.(v$mx0),"",sep="")),side=3,line=-2,adj = 0,col=color.false,at=36)
                  mtext(bquote(paste("n",sep=" ")),side=3,line=-4,adj = 1,at=20)
                  mtext(bquote(paste("%",sep=" ")),side=3,line=-4,adj = 1,at=25)
                  mtext(bquote(paste("IC contient μ''"," : ",sep=" ")),side=3,line=-6,adj = 1,at=17,col=color.false)
                  mtext(bquote(paste(.(ICvsmu0.mat[1,1]),,sep=" ")),side=3,line=-6,adj = 1,at=20,col=color.false)
                  mtext(bquote(paste(.(ICvsmu0.mat[1,2]),,sep=" ")),side=3,line=-6,adj = 1,at=25,col=color.false)
                  mtext(bquote(paste("IC ne contient pas μ''"," : ",sep=" ")),side=3,line=-8,adj = 1,at=17,col=color.true)
                  mtext(bquote(paste(.(ICvsmu0.mat[2,1]),,sep=" ")),side=3,line=-8,adj = 1,at=20,col=color.true)
                  mtext(bquote(paste(.(ICvsmu0.mat[2,2]),,sep=" ")),side=3,line=-8,adj = 1,at=25,col=color.true)
                  

              }
          }
     }
  }, height = getPlotHeight, width=full.plot.width)

###################################################################
  output$DataTable <- renderTable({
    v<-getInputValues()
    calc.new<-getComputedValues()
    cv<-rv$cv.ls
    ## Transpose the sample list
    if(cv$n.samples>0){
        samples.as.list<-list()
        for(i in 1:cv$n.samples){
            samples.as.list[[i]]<-c(round(cv$samples.x.mat[i,],2),c(""),round(cv$samples.x.m.vec[i],2),round(cv$samples.x.sd.vec[i],2),c(""),round(cv$ic.k.limit.mat[i,1],2),round(cv$ic.k.limit.mat[i,2],2))
        }
        samples.as.matrix<- do.call(rbind,samples.as.list) 
        transposed.samples<-lapply(seq_len(ncol(samples.as.matrix)),function(i) samples.as.matrix[,i]) 
        d<-data.frame(transposed.samples)
        colnames(d)<-c(paste("X",1:v$n,sep="")," ","Moy","Sd"," ","LiICk","LsICk")
        d
    }
  })
  
###################################################################
  output$test1 <- renderText({
    v<-getInputValues()
    calc.new<-getComputedValues()
    cv<-rv$cv.ls
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
