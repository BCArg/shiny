library(shiny)

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
 
output$zPlot <- renderPlot({
  vx<-input$x
  mx<-input$mx
  sx<-input$sx
  sens<-input$sens
  
  if(sens=="inf"){
    z1=-5
    z2=round((vx-mx)/sx,2)
  }
  if(sens=="sup"){
    z2=5
    z1=round((vx-mx)/sx,2)
  }
  if(sens=="equal"){
    z2=round((vx-mx)/sx,2)
    z1=round((vx-mx)/sx,2)
  }
  
  z1lp=ifelse(z1 < -4,-4,z1) # limite la valeur de z à -4 pour le tracé du polugone sinon il overwrite le pointillé
	z2lp=ifelse(z2 > 4,4,z2) # limite la valeur de z à 4 pour le tracé du polugone sinon il overwrite le pointillé
	xlp=seq(z1lp,z2lp,length=200) #crée une seconde abscisse de -2 à 2
	ylp=dnorm(xlp) #crée pour chaque xles équivalent y Normaux
	
	par(mai=c(1.02,0.8,0.9,1.5)) #marges bottom left top right en inches les valeurs par défaut sont  1 1 1 1 
	plot(z,y,type="l",lwd=2,col="blue",yaxt="n",bty="n",,las=1,xaxs="i",yaxs="i",cex.lab=1,cex.axis=1.5,xlim=c(-5,plotzmaxlim),ylim=c(0,0.4),xaxp=c(-4,4,8),ylab="density",xlab="") #trace une courbe a partir de tous les couples x;y, et la colore en rouge. bty : A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. xaxt="n" = pas dessiner axe des x
	axis(2,las=2)#col="red",col.axis="blue",font.axis=2
	lines(x=c(-4.7,-4),y = c(0,0),lty=2)
	lines(x=c(4.7,4),y = c(0,0),lty=2)
	polygon(c(z1lp,xlp,z2lp),c(0,ylp,0),col="lightblue") #trace un polygone qui commence à -2;0 puis x;y puis 2;0 et comme c'est polygon il ferme entre 2;0 et -2;0
  mtext(plotzlab,side=1,line=1,at=5,cex=ztxt,adj = 0)
  #Ecrire valeurs de x sur abscisse
  for(s in -4:4){
    vx=mx+s*sx
    mtext(vx,side=1,line=linexlab,at=s,cex=1.5) 
  }
  mtext(paste("X~N(",mx,";",sx^2,")",sep=""),side=1,line=linexlab,at=5,cex=1.2,adj = 0)
 
  pz1=pnorm(z1,mean=0,sd=1)
  pz1r=round(pz1,4)
  pz1i=1-pz1
  pz1ir=1-pz1r#round(pz1i,4)
  pz2=pnorm(z2,mean=0,sd=1)
  pz2r=round(pz2,4)
  pz2i=1-pz2
  pz2ir=1-pz2r#round(pz2i,4)
  p=pz2-pz1
  pr=pz2r-pz1r#round(p,4)
  pp=round(pr*100,0)
  z1l=ifelse(z1 < -3,-3,z1) # limite la valeur de z à -3 pour le calcul de la position de
  z2l=ifelse(z2 > 3,3,z2) # limite la valeur de z à 3 pour le calcul de la position dep
  if(sens=="inf"){
    zx=(max(z2l,z1l)-((max(z2l,z1l)-min(z2l,z1l))/3))
  }
  if(sens=="sup"){
    zx=(min(z2l,z1l)+((max(z2l,z1l)-min(z2l,z1l))/3))
  }
  if(sens=="equal"){
    zx=z2l
  }
  text(zx,0.1,pr,cex=2)#cex = zoom taille texte
  })
 
  output$Text <- renderText({
    vx<-input$x
    mx<-input$mx
    sx<-input$sx
    sens<-input$sens
    
    if(sens=="inf"){
      z1=-5
      z2=round((vx-mx)/sx,2)
    }
    if(sens=="sup"){
      z2=5
      z1=round((vx-mx)/sx,2)
    }
    if(sens=="equal"){
      z2=round((vx-mx)/sx,2)
      z1=round((vx-mx)/sx,2)
    }
    pz1=pnorm(z1,mean=0,sd=1)
    pz1r=round(pz1,4)
    pz1i=1-pz1
    pz1ir=1-pz1r#round(pz1i,4)
    pz2=pnorm(z2,mean=0,sd=1)
    pz2r=round(pz2,4)
    pz2i=1-pz2
    pz2ir=1-pz2r#round(pz2i,4)
    p=pz2-pz1
    pr=pz2r-pz1r#round(p,4)
    pp=round(pr*100,0)
    #txt=paste("<p>z1=",z1,";z2=",z2,"</p>",sep="")
    txt=paste("<p>Si X est une variable al&eacute;atoire Normale X~N(&mu;;&sigma;&sup2;) telle que X~N(",mx,";",sx^2,") alors Z=(X-&mu;)/&sigma; est aussi une variable al&eacute;atoire Normale de param&egrave;tres Z~N(0;1) ",sep="")
    if(sens=="inf" & z2>-4 & z2<0){
      txt=paste(txt," et P(X&le;x) = P(Z&le;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",vx,", alors z = (x-&mu;)/&sigma; = (",vx,"-",mx,")/",sx," = ",vx-mx,"/",sx," = ",z2,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&le;",round(z2*sx+mx,2),") = P(Z&le;",z2,") = 1-P(Z&le;",z2*-1,") = 1-",pz2ir," = ",pz2r,"</p>",sep="")
    }
    if(sens=="inf" & z2<4 & z2>=0){
      txt=paste(txt," et P(X&le;x) = P(Z&le;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",vx,", alors z = (x-&mu;)/&sigma; = (",vx,"-",mx,")/",sx," = ",vx-mx,"/",sx," = ",z2,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&le;",round(z2*sx+mx,2),") = P(Z&le;",z2,") = ",pz2r,"</p>",sep="")
    }
    if(z1>-4 & z1<0 & sens=="sup"){
      txt=paste(txt," et P(X&ge;x) = P(Z&ge;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",vx,", alors z = (x-&mu;)/&sigma; = (",vx,"-",mx,")/",sx," = ",vx-mx,"/",sx," = ",z1,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&ge;",round(z1*sx+mx,2),") = P(Z&ge;",z1,") = P(Z&le;",z1*-1,") = ",pz1ir,"</p>",sep="")
    }
    if(z1<4 & z1>=0 & sens=="sup"){
      txt=paste(txt," et P(X&ge;x) = P(Z&ge;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",vx,", alors z = (x-&mu;)/&sigma; = (",vx,"-",mx,")/",sx," = ",vx-mx,"/",sx," = ",z1,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&ge;",round(z1*sx+mx,2),") = P(Z&ge;",z1,") = 1-P(Z&le;",z1,") = 1-",pz1r," = ",pz1ir,"</p>",sep="")
    }
    if(sens=="inf" & z2>=4){
      txt=paste(txt," et P(X&le;x) = P(Z&le;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",vx,", alors z = (x-&mu;)/&sigma; = (",vx,"-",mx,")/",sx," = ",vx-mx,"/",sx," = ",z2,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&le;",round(z2*sx+mx,2),") = P(Z&le;",z2,") = P(Z&le;&infin;) = ",pz2r,"</p>",sep="")
    }
    if(sens=="inf" & z2<=-4){
      txt=paste(txt," et P(X&le;x) = P(Z&le;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",vx,", alors z = (x-&mu;)/&sigma; = (",vx,"-",mx,")/",sx," = ",vx-mx,"/",sx," = ",z2,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&le;",round(z2*sx+mx,2),") = P(Z&le;",z2,") = 1-P(Z&le;",z2*-1,") = 1-P(Z&le;&infin;) = 1-1 = ",pz2r,"</p>",sep="")
    }
    if(z1<=-4 & sens=="sup"){
      txt=paste(txt," et P(X&ge;x) = P(Z&ge;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",vx,", alors z = (x-&mu;)/&sigma; = (",vx,"-",mx,")/",sx," = ",vx-mx,"/",sx," = ",z1,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&ge;",round(z1*sx+mx,2),") = P(Z&ge;",z1,") = 1-P(Z&le;",z1*-1,") = 1-P(Z&le;&infin;) = 1-1 = ",pz1r,"</p>",sep="")
    }
    if(z1>=4 & sens=="sup"){
      txt=paste(txt," et P(X&ge;x) = P(Z&ge;z).","</p>",sep="")
      txt=paste(txt,"<p>Si x=",vx,", alors z = (x-&mu;)/&sigma; = (",vx,"-",mx,")/",sx," = ",vx-mx,"/",sx," = ",z1,"</p>",sep="")
      txt=paste(txt,"<p>Donc P(X&ge;",round(z1*sx+mx,2),") = P(Z&ge;",z1,") = 1-P(Z&le;",z1,") = 1-P(Z&le;&infin;) = 1-1 = ",pz1r,"</p>",sep="")
    }
    if(sens=="equal"){
      txt=paste(txt," et P(X=x) = P(Z=z) = 0.","</p>",sep="")
    }
    paste(txt)
  })

})

