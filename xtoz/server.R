library(shiny)
# library(xtable)

plotzmaxlim=5 #limite max de l'axe des Z
plotzlab="Z~N(0;1)"
linezlab=1
linexlab=3
linemulab=2.5

yt=0.48 #y des textes
xt=-5 #x des textes
ztxt=1.5 #zoom text
txt="" #contenu des textes

z=seq(-4,4,length=200) #détermine les valeurs de z à utiliser : de -4 à 4 créer 200 valeurs et les mettre en vecteur
y=dnorm(z) #créer pour chaque z les équivalents y de la courbe de gauss

#Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
 
  getInputValues<-reactive({
    return(input)#collect all inputs
  })

  getComputedValues<-reactive({
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
    if(v$Tabset!=3){
      if(v$Tabset==1){
	if(v$sens=="inf"){
	  cv$z1=-5
	  cv$z2=round((v$x-v$mx)/v$sx,2)
	}
	if(v$sens=="sup"){
	  cv$z2=5
	  cv$z1=round((v$x-v$mx)/v$sx,2)
	}
	if(v$sens=="equal"){
	  cv$z2=round((v$x-v$mx)/v$sx,2)
	  cv$z1=round((v$x-v$mx)/v$sx,2)
	}
      }
      
      if(v$Tabset==2){
	cv$x1=min(v$x1,v$x2)
	cv$x2=max(v$x1,v$x2)
	cv$z1=round((cv$x1-v$mx)/v$sx,2)
	cv$z2=round((cv$x2-v$mx)/v$sx,2)
      }
      
      cv$z1l=ifelse(cv$z1 < -3,-3,cv$z1) # limite la valeur de z à -3 pour le calcul de la position de
      cv$z2l=ifelse(cv$z2 > 3,3,cv$z2) # limite la valeur de z à 3 pour le calcul de la position dep
    
      cv$z1lp=ifelse(cv$z1 < -4,-4,cv$z1) # limite la valeur de z à -4 pour le tracé du polugone sinon il overwrite le pointillé
      cv$z2lp=ifelse(cv$z2 > 4,4,cv$z2) # limite la valeur de z à 4 pour le tracé du polugone sinon il overwrite le pointillé
      cv$xlp=seq(cv$z1lp,cv$z2lp,length=200) #crée une seconde abscisse de -2 à 2
      cv$ylp=dnorm(cv$xlp) #crée pour chaque xles équivalent y Normaux
	  
      cv$pz1=pnorm(cv$z1,mean=0,sd=1)
      cv$pz1r=round(cv$pz1,4)
      cv$pz1i=1-cv$pz1
      cv$pz1ir=1-cv$pz1r#round(pz1i,4)
      
      cv$pz2=pnorm(cv$z2,mean=0,sd=1)
      cv$pz2r=round(cv$pz2,4)
      cv$pz2i=1-cv$pz2
      cv$pz2ir=1-cv$pz2r#round(pz2i,4)
      
      cv$p=cv$pz2-cv$pz1
      cv$pr=cv$pz2r-cv$pz1r#round(p,4)
      cv$pp=round(cv$pr*100,0)
    
      if(v$Tabset==1){
	if(v$sens=="inf"){
	  cv$zx=(max(cv$z2l,cv$z1l)-((max(cv$z2l,cv$z1l)-min(cv$z2l,cv$z1l))/3))
	}
	if(v$sens=="sup"){
	  cv$zx=(min(cv$z2l,cv$z1l)+((max(cv$z2l,cv$z1l)-min(cv$z2l,cv$z1l))/3))
	}
	if(v$sens=="equal"){
	  cv$zx=cv$z2l
	}
      }
      
      if(v$Tabset==2){
	cv$zx=(cv$z2l-(cv$z2l-cv$z1l)/2)
      }
    } 
    return(cv)
  })
  
  output$zPlot <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
        if(v$Tabset==1){
    par(mai=c(1.02,0.8,0.9,1.5)) #marges bottom left top right en inches les valeurs par défaut sont  1 1 1 1 
    plot(z,y,type="l",lwd=2,col="blue",yaxt="n",bty="n",,las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.5,xlim=c(-5,plotzmaxlim),ylim=c(0,0.4),xaxp=c(-4,4,8),ylab="density",xlab="")
    axis(2,las=2)#col="red",col.axis="blue",font.axis=2
    lines(x=c(-4.7,-4),y = c(0,0),lty=2)
    lines(x=c(4.7,4),y = c(0,0),lty=2)
    polygon(c(cv$z1lp,cv$xlp,cv$z2lp),c(0,cv$ylp,0),col="lightblue") #trace un polygone qui commence à -2;0 puis x;y puis 2;0 et comme c'est polygon il ferme entre 2;0 et -2;0
    mtext(plotzlab,side=1,line=1,at=5,cex=ztxt,adj = 0)
    for(s in -4:4){#Ecrire valeurs de x sur abscisse
      vx=v$mx+s*v$sx
      mtext(vx,side=1,line=linexlab,at=s,cex=1.5) 
    }
    mtext(paste("X~N(",v$mx,";",v$sx^2,")",sep=""),side=1,line=linexlab,at=5,cex=1.2,adj = 0)
    text(cv$zx,0.1,cv$pr,cex=2)#cex = zoom taille texte
    }
    })
 
  output$zPlotInt <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$Tabset==2){
    par(mai=c(1.02,0.8,0.9,1.5)) #marges bottom left top right en inches les valeurs par défaut sont  1 1 1 1 
    plot(z,y,type="l",lwd=2,col="blue",yaxt="n",bty="n",,las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.5,xlim=c(-5,plotzmaxlim),ylim=c(0,0.4),xaxp=c(-4,4,8),ylab="density",xlab="") 
    axis(2,las=2)#col="red",col.axis="blue",font.axis=2
    lines(x=c(-4.7,-4),y = c(0,0),lty=2)
    lines(x=c(4.7,4),y = c(0,0),lty=2)
    polygon(c(cv$z1lp,cv$xlp,cv$z2lp),c(0,cv$ylp,0),col="lightblue") #trace un polygone qui commence à -2;0 puis x;y puis 2;0 et comme c'est polygon il ferme entre 2;0 et -2;0
    mtext(plotzlab,side=1,line=1,at=5,cex=ztxt,adj = 0)
    for(s in -4:4){#Ecrire valeurs de x sur abscisse
      vx=v$mx+s*v$sx
      mtext(vx,side=1,line=linexlab,at=s,cex=1.5) 
    }
    mtext(paste("X~N(",v$mx,";",v$sx^2,")",sep=""),side=1,line=linexlab,at=5,cex=1.2,adj = 0)
    text(cv$zx,0.1,cv$pr,cex=2)#cex = zoom taille texte
    }
    })
 
 
  output$Text <- renderText({
    v<-getInputValues()
    cv<-getComputedValues()

    txt=paste("<p>Si X est une variable al&eacute;atoire Normale X~N(&mu;;&sigma;&sup2;) telle que X~N(",v$mx,";",v$sx^2,") alors Z=(X-&mu;)/&sigma; est aussi une variable al&eacute;atoire Normale de param&egrave;tres Z~N(0;1) ",sep="")
    if(v$sens=="inf" & cv$z2>-4 & cv$z2<0){
      txt=paste(txt," et P(X&le;x) = P(Z&le;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",v$x,", alors z = (x-&mu;)/&sigma; = (",v$x,"-",v$mx,")/",v$sx," = ",v$x-v$mx,"/",v$sx," = ",cv$z2,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&le;",round(cv$z2*v$sx+v$mx,2),") = P(Z&le;",cv$z2,") = 1-P(Z&le;",cv$z2*-1,") = 1-",cv$pz2ir," = ",cv$pz2r,"</p>",sep="")
    }
    if(v$sens=="inf" & cv$z2<4 & cv$z2>=0){
      txt=paste(txt," et P(X&le;x) = P(Z&le;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",v$x,", alors z = (x-&mu;)/&sigma; = (",v$x,"-",v$mx,")/",v$sx," = ",v$x-v$mx,"/",v$sx," = ",cv$z2,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&le;",round(cv$z2*v$sx+v$mx,2),") = P(Z&le;",cv$z2,") = ",cv$pz2r,"</p>",sep="")
    }
    if(cv$z1>-4 & cv$z1<0 & v$sens=="sup"){
      txt=paste(txt," et P(X&ge;x) = P(Z&ge;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",v$x,", alors z = (x-&mu;)/&sigma; = (",v$x,"-",v$mx,")/",v$sx," = ",v$x-v$mx,"/",v$sx," = ",cv$z1,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&ge;",round(cv$z1*v$sx+v$mx,2),") = P(Z&ge;",cv$z1,") = P(Z&le;",cv$z1*-1,") = ",cv$pz1ir,"</p>",sep="")
    }
    if(cv$z1<4 & cv$z1>=0 & v$sens=="sup"){
      txt=paste(txt," et P(X&ge;x) = P(Z&ge;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",v$x,", alors z = (x-&mu;)/&sigma; = (",v$x,"-",v$mx,")/",v$sx," = ",v$x-v$mx,"/",v$sx," = ",cv$z1,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&ge;",round(cv$z1*v$sx+v$mx,2),") = P(Z&ge;",cv$z1,") = 1-P(Z&le;",cv$z1,") = 1-",cv$pz1r," = ",cv$pz1ir,"</p>",sep="")
    }
    if(v$sens=="inf" & cv$z2>=4){
      txt=paste(txt," et P(X&le;x) = P(Z&le;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",v$x,", alors z = (x-&mu;)/&sigma; = (",v$x,"-",v$mx,")/",v$sx," = ",v$x-v$mx,"/",v$sx," = ",cv$z2,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&le;",round(cv$z2*v$sx+v$mx,2),") = P(Z&le;",cv$z2,") = P(Z&le;&infin;) = ",cv$pz2r,"</p>",sep="")
    }
    if(v$sens=="inf" & cv$z2<=-4){
      txt=paste(txt," et P(X&le;x) = P(Z&le;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",v$x,", alors z = (x-&mu;)/&sigma; = (",v$x,"-",v$mx,")/",v$sx," = ",v$x-v$mx,"/",v$sx," = ",cv$z2,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&le;",round(cv$z2*v$sx+v$mx,2),") = P(Z&le;",cv$z2,") = 1-P(Z&le;",cv$z2*-1,") = 1-P(Z&le;&infin;) = 1-1 = ",cv$pz2r,"</p>",sep="")
    }
    if(cv$z1<=-4 & v$sens=="sup"){
      txt=paste(txt," et P(X&ge;x) = P(Z&ge;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",v$x,", alors z = (x-&mu;)/&sigma; = (",v$x,"-",v$mx,")/",v$sx," = ",v$x-v$mx,"/",v$sx," = ",cv$z1,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&ge;",round(cv$z1*v$sx+v$mx,2),") = P(Z&ge;",cv$z1,") = 1-P(Z&le;",cv$z1*-1,") = 1-P(Z&le;&infin;) = 1-1 = ",cv$pz1r,"</p>",sep="")
    }
    if(cv$z1>=4 & v$sens=="sup"){
      txt=paste(txt," et P(X&ge;x) = P(Z&ge;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",v$x,", alors z = (x-&mu;)/&sigma; = (",v$x,"-",v$mx,")/",v$sx," = ",v$x-v$mx,"/",v$sx," = ",cv$z1,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&ge;",round(cv$z1*v$sx+v$mx,2),") = P(Z&ge;",cv$z1,") = 1-P(Z&le;",cv$z1,") = 1-P(Z&le;&infin;) = 1-1 = ",cv$pz1r,"</p>",sep="")
    }
    if(v$sens=="equal"){
      txt=paste(txt," et P(X=x) = P(Z=z) = 0.","</p>",sep="")
    }
    paste(txt)
  })
  
  output$TextInt <- renderText({
    v<-getInputValues()
    cv<-getComputedValues()

    txt=paste("<p>Si X est une variable al&eacute;atoire Normale X~N(&mu;;&sigma;&sup2;) telle que X~N(",v$mx,";",v$sx^2,") alors Z=(X-&mu;)/&sigma; est aussi une variable al&eacute;atoire Normale de param&egrave;tres Z~N(0;1) </p>",sep="")
    txt=paste(txt,"<p>P(x<sub>1</sub> &le; X &le; x<sub>2</sub>) = P(z<sub>1</sub> &le; Z &le; z<sub>2</sub>).","</p>",sep="")
    txt=paste(txt,"<p>Si x<sub>1</sub>=",v$x1,", alors z<sub>1</sub> = (x<sub>1</sub>-&mu;)/&sigma; = (",v$x1,"-",v$mx,")/",v$sx," = ",v$x1-v$mx,"/",v$sx," = ",cv$z1,"</p>",sep="")
    txt=paste(txt,"<p>Si x<sub>2</sub>=",v$x2,", alors z<sub>2</sub> = (x<sub>2</sub>-&mu;)/&sigma; = (",v$x2,"-",v$mx,")/",v$sx," = ",v$x2-v$mx,"/",v$sx," = ",cv$z2,"</p>",sep="")
    txt=paste(txt,"<p>Donc P(",cv$x1," &le; X &le; ",cv$x2,") = P(",cv$z1," &le; Z &le; ",cv$z2,") = P(Z &le; ",cv$z1,")-P(Z &le; ",cv$z2,") = ",cv$pz1ir,"-",cv$pz2ir," = ",cv$pr,"</p>",sep="")
    paste(txt)
  })
  
  output$Table <- renderText({
    v<-getInputValues()
    if(v$signeZ == "pos"){
      rows<-seq(0.0,3.4,by=0.1)
      cols<-seq(0.00,0.09,by=0.01)
    } else {
      rows<-rev(seq(-3.4,-0.0,by=0.1))
      cols<-rev(seq(-0.09,0.00,by=0.01))
    }

    p<-c()
#     définir ici table header et faire html code de Table a la main car renderTable force les colsnames en header mais ne permet pas les rownames en header
    txt<-'<table cellspacing="0" cellpadding="5" border="0" style="border:1px solid #000;">'
    txt<-paste(txt,'<tr style="border-bottom:1px solid #000;"><th style="border-right:1px solid #000;">Z</th>',sep="")
      for(j in 1:length(cols)){
	txt<-paste(txt,'<th>',format(cols[[j]], nsmall = 2),'</th>',sep="")
      }
    txt<-paste(txt,'</tr>',sep="")
    for(i in 1:length(rows)){
	if(i == 11 || i == 21 ){
	  txt<-paste(txt,'<tr style="border-bottom:1px solid #000;">',sep="")
	} else {
	  txt<-paste(txt,'<tr>',sep="")
	}
      txt<-paste(txt,'<th style="border-right:1px solid #000;">',format(rows[[i]], nsmall = 1),'</th>',sep="")
      for(j in 1:length(cols)){
	if(v$sensTable == "inf"){
	  txt<-paste(txt,'<td>',sprintf("%.4f",round(pnorm(rows[[i]]+cols[[j]]),4)),'</td>',sep="")
	} else {
	  txt<-paste(txt,'<td>',sprintf("%.4f",1-round(pnorm(rows[[i]]+cols[[j]]),4)),'</td>',sep="")
	}
	
      }
      txt<-paste(txt,'</tr>',sep="")
    }
    txt<-paste(txt,'</table>',sep="")
    
    
        paste(txt)
#     for(i in 1:length(rows)){
#       for(j in 1:length(cols)){
# 	p<-c(p,round(pnorm(rows[[i]]+cols[[j]]),4))
#       }
#     }
#     m<-matrix(p,length(rows),length(cols),byrow=TRUE)
#     d<-data.frame(m)
#     rownames(d)<-format(rows, nsmall = 2)
#     colnames(d)<-format(cols, nsmall = 2)
#     d
  })

})

