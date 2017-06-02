#set directory
#www.17-56.cl


####We Load the data from the polls
####Send us a request at 1756datablog@gmail.com for sharing the data
data.raw=read.csv("e2017b.csv")
data = data.raw[order(data.raw$dias1eraEnc),]##make sure data is sorted
data=subset(data,Consider==1)##some pollsters such as "Criteria Research" are recorded
                              #but not considered in the prediction

####Data from polls are stored as decimals. With this we transform it to 0 to 100
for(i in 4:16 ){
      data[,i]=100*data[,i]

}

#####Ggplot2 color convention for every candidate:
## Fuere - medio - debil - Candidato
#purple4 - purple3 - purple2 - Ossandon
#blue4 - blue3 - blue2 - Pinera
#steelblue4 - steelblue 3 - steelblue2 - FKast
#khaki4 - khaki3 - khaki2 - Goic
#darkorange3 - darkorange2 - darkorange1 - Guillier
#tomato3 - tomato2 - tomato1 - MEO
#brown4 - brown3 - brown3 - Sánchez
#lightpink4 - lightpink3 - lightpink2 -Parisi
#gray27 - gray33 - gray45 - JA Kast

library(ggplot2)

#We set the prediction date
predDate="Prediccion al X/X/2017 | www.17-56.cl"

#We define x as in how many days first round will be held (this value is negative)
x=data$diasEleccion

eval=max(x)#This is the last day when we have data from pollsters: Evaluation day
scale=max(data$dias1eraEnc.1) #this also the last day when we have data from the pollsters
                              #but counted as days from the first poll +1
sampleMax=max(data$Muestra)   #we record sample sizes from every poll. This is the max value
pesosT=sqrt(data$dias1eraEnc.1/scale) #this is a normalized (0 to 1) time weight for every poll
pesosMuestra=data$Muestra/sampleMax #this is a normalized (0 to 1) sample size weight for every poll
sumaPesos=0*pesosT+pesosMuestra #this is the sum of both normalized time and sample size weights

######################

###Since some of the candidates began the presidential race on differente dates
###and locfit.raw and geom_smooth functions present some troubles when managing
###datasets with different lengths, we create again variables for those candidates
###who require it

####Subset Sanchez
dataSanchez=subset(data,data$SanchezAdj>=0)
xSanchez=dataSanchez$diasEleccion
pesosTS=sqrt(dataSanchez$dias1eraEnc/scale)
pesosMuestraS=dataSanchez$Muestra/sampleMax
sumaPesosS=0*pesosTS+pesosMuestraS

####Subset MEO
dataMEO=subset(data,data$MEOAdj>=0)
xMEO=dataMEO$diasEleccion
pesosTM=sqrt(dataMEO$dias1eraEnc/scale)
pesosMuestraM=dataMEO$Muestra/sampleMax
sumaPesosM=0*pesosTM+pesosMuestraM

####FK
dataFK=subset(data,data$FKastAdj>=0)
xFK=dataFK$diasEleccion
pesosTF=sqrt(dataFK$dias1eraEnc/scale)
pesosMuestraF=dataFK$Muestra/sampleMax
sumaPesosF=0*pesosTF+pesosMuestraF

####GOIC
dataGoic=subset(data,data$GoicAdj>=0)
xGoic=dataGoic$diasEleccion
pesosTG=sqrt(dataGoic$dias1eraEnc/scale)
pesosMuestraG=dataGoic$Muestra/sampleMax
sumaPesosG=0*pesosTG+pesosMuestraG

####Parisi
dataParisi=subset(data,data$ParisiAdj>=0)
xParisi=dataParisi$diasEleccion
pesosTP=sqrt(dataParisi$dias1eraEnc/scale)
pesosMuestraP=dataParisi$Muestra/sampleMax
sumaPesosP=pesosTP+pesosMuestraP


#####################
#As convention we work with a span=0.7. This is arbitrary!!!
#How do we know is 0.7? We calibrated this with 2009 and 2013 elections.
#####################

####With this we create the ggplot chart 
q1=ggplot()+geom_smooth(data=data,aes(x=x,y=PineraAdj,weight=sumaPesos), fill="blue2",colour="blue3",size=1,span=0.7)+
geom_smooth(data=data,aes(x=x,y=GuillierAdj,weight=sumaPesos), fill="darkorange1",colour="darkorange2",size=1,span=0.7)+
geom_smooth(data=dataMEO,aes(x=xMEO,y=MEOAdj,weight=sumaPesosM), fill="tomato2",colour="tomato2",size=1,span=0.7)+
geom_smooth(data=dataSanchez,aes(x=xSanchez,y=SanchezAdj,weight=sumaPesosS), fill="brown2",colour="brown3",size=1,span=0.7)+
geom_smooth(data=data,aes(x=x,y=OssandonAdj,weight=sumaPesos), fill="purple2",colour="purple3",size=1,span=0.7)+
geom_smooth(data=dataGoic,aes(x=xGoic,y=GoicAdj,weight=sumaPesosG), fill="khaki2",colour="khaki3",size=1,span=0.7)+
geom_smooth(data=dataFK,aes(x=xFK,y=FKastAdj,weight=sumaPesosF), fill="steelblue2",colour="steelblue3",size=1,span=0.7)+
geom_smooth(data=dataParisi,aes(x=xParisi,y=ParisiAdj,weight=sumaPesosP), fill="lightpink2",colour="lightpink3",size=1,span=0.7)+
labs(x="Días para la Primera Vuelta",y="Porcentaje de votos en primera vuelta",title=predDate)


#####With this we render the point estimations
library(locfit)
pinera=locfit.raw(x=x,y=data$PineraAdj,kern="tri",weights=sumaPesos,alpha=0.7)
guillier=locfit.raw(x=x,y=data$GuillierAdj,kern="tri",weights=sumaPesos,alpha=0.7)
meo=locfit.raw(x=xMEO,y=dataMEO$MEOAdj,kern="tri",weights=sumaPesosM,alpha=0.7)
sanchez=locfit.raw(x=xSanchez,y=dataSanchez$SanchezAdj,kern="tri",weights=sumaPesosS,alpha=0.7)
ossandon=locfit.raw(x=x,y=data$OssandonAdj,kern="tri",weights=sumaPesos,alpha=0.7)
fkast=locfit.raw(x=xFK,y=dataFK$FKastAdj,kern="tri",weights=sumaPesosF,alpha=0.7)
goic=locfit.raw(x=xGoic,y=dataGoic$GoicAdj,kern="tri",weights=sumaPesosG,alpha=0.7)
parisi=locfit.raw(x=xParisi,y=dataParisi$ParisiAdj,kern="tri",weights=sumaPesosP,alpha=0.7)

###############
###############
####With predcandidate we determine standard errors for each point estimation
####With distcandidate we create artificial scenarios (this is just illustrative)
####TO-DO: Create a sensitivity analysis with parameters to actually create the scenarios
scenarios=1000

predpinera=predict(pinera,x,se.fit=TRUE)
distpinera=rnorm(scenarios,predict(pinera,eval),predpinera$se.fit[length(x)])

predguillier=predict(guillier,x,se.fit=TRUE)
distguillier=rnorm(scenarios,predict(guillier,eval),predguillier$se.fit[length(x)])

predsanchez=predict(sanchez,xSanchez,se.fit=TRUE)
distsanchez=rnorm(scenarios,predict(sanchez,eval),predsanchez$se.fit[length(xSanchez)])

predossandon=predict(ossandon,x,se.fit=TRUE)
distossandon=rnorm(scenarios,predict(ossandon,eval),predossandon$se.fit[length(x)])

predgoic=predict(goic,xGoic,se.fit=TRUE)
distgoic=rnorm(scenarios,predict(goic,eval),predgoic$se.fit[length(xGoic)])

predfkast=predict(fkast,xFK,se.fit=TRUE)
disfkast=rnorm(scenarios,predict(fkast,eval),predfkast$se.fit[length(xFK)])

predmeo=predict(meo,xMEO,se.fit=TRUE)
distmeo=rnorm(scenarios,predict(meo,eval),predmeo$se.fit[length(xMEO)])

predparisi=predict(parisi,xParisi,se.fit=TRUE)
distparisi=rnorm(scenarios,predict(parisi,eval),predparisi$se.fit[length(xParisi)])

#####With this we create the histograms
library(gridExtra)

sizefont=5
binw=1

q2=qplot(distpinera, geom="histogram", binwidth = binw, main = "predDate", xlab = "",ylab="Piñera",  
      fill=I("blue2"),
      xlim=c(0,50),ylim=c(0,600+0*2*400))+
      geom_segment(aes(x =predict(pinera,eval), y = 0, xend = predict(pinera,eval), yend = 400),
      linetype="dashed",  color="blue4")+
      annotate("text", x=predict(pinera,eval), y=530, label= paste(round(predict(pinera,eval),1),"%",sep=""),
            color="blue4",size=sizefont)

q3=qplot(distguillier, geom="histogram", binwidth = binw, xlab = "",ylab="Guillier",
       fill=I("darkorange1"),
     xlim=c(0,50),ylim=c(0,600+0*2*400))+
      geom_segment(aes(x =predict(guillier,eval), y = 0, xend = predict(guillier,eval), yend = 400),
      linetype="dashed",  color="darkorange3")+
      annotate("text", x=predict(guillier,eval), y=530, label= paste(round(predict(guillier,eval),1),"%",sep=""),
            color="darkorange3",size=sizefont)

q4=qplot(distsanchez, geom="histogram", binwidth = binw,  xlab = "", ylab="Sanchez", 
      fill=I("brown2"),
      xlim=c(0,50),ylim=c(0,600+0*2*400))+
      geom_segment(aes(x =predict(sanchez,eval), y = 0, xend = predict(sanchez,eval), yend = 400),
      linetype="dashed",  color="brown3")+
      annotate("text", x=predict(sanchez,eval), y=530, label= paste(round(predict(sanchez,eval),1),"%",sep=""),
            color="brown3",size=sizefont)

q5=qplot(distossandon, geom="histogram", binwidth = binw,  xlab = "", ylab="Ossandón", 
      fill=I("purple2"),
      xlim=c(0,50),ylim=c(0,600+0*400))+
      geom_segment(aes(x =predict(ossandon,eval), y = 0, xend = predict(ossandon,eval), yend = 400),
      linetype="dashed",  color="purple4")+
      annotate("text", x=predict(ossandon,eval), y=530, label= paste(round(predict(ossandon,eval),1),"%",sep=""),
            color="purple4",size=sizefont)

q6=qplot(distgoic, geom="histogram", binwidth = binw, main="predDate",  xlab = "", ylab="Goic", 
      fill=I("khaki2"),
      xlim=c(0,50),ylim=c(0,2*400))+
      geom_segment(aes(x =predict(goic,eval), y = 0, xend = predict(goic,eval), yend = 600),
      linetype="dashed",  color="khaki4")+
      annotate("text", x=predict(goic,eval), y=730, label= paste(round(predict(goic,eval),1),"%",sep=""),
            color="khaki4",size=sizefont)

q7=qplot(distparisi, geom="histogram", binwidth = binw,  xlab = "", ylab="Parisi", 
      fill=I("lightpink2"),
      xlim=c(0,50),ylim=c(0,2*400))+
      geom_segment(aes(x =predict(parisi,eval), y = 0, xend = predict(parisi,eval), yend = 600),
      linetype="dashed",  color="lightpink4")+
      annotate("text", x=predict(parisi,eval), y=730, label= paste(round(predict(parisi,eval),1),"%",sep=""),
            color="lightpink4",size=sizefont)

q8=qplot(distmeo, geom="histogram", binwidth = binw,  xlab = "", ylab="M.E.O.", 
      fill=I("tomato1"),
      xlim=c(0,50),ylim=c(0,2*400))+
      geom_segment(aes(x =predict(meo,eval), y = 0, xend = predict(meo,eval), yend = 600),
      linetype="dashed",  color="tomato3")+
      annotate("text", x=predict(meo,eval), y=730, label= paste(round(predict(meo,eval),1),"%",sep=""),
            color="tomato3",size=sizefont)

q9=qplot(disfkast, geom="histogram", binwidth = binw,  xlab = "", ylab="F. Kast", 
      fill=I("steelblue2"),
      xlim=c(0,50),ylim=c(0,2*400))+
      geom_segment(aes(x =predict(fkast,eval), y = 0, xend = predict(fkast,eval), yend = 600),
      linetype="dashed",  color="steelblue4")+
      annotate("text", x=predict(fkast,eval), y=730, label= paste(round(predict(fkast,eval),1),"%",sep=""),
            color="steelblue4",size=sizefont)

hist1to4=grid.arrange(q2,q3,q4,q5, ncol=1,nrow=4,bottom="Porcentaje de votos en Primera Vuelta",left="Escenarios generados")
hist5to8=grid.arrange(q6,q7,q8,q9, ncol=1,nrow=4,bottom="Porcentaje de votos en Primera Vuelta",left="Escenarios generados")


#########Out put from model:
predicciones=c(predict(pinera,eval),predict(guillier,eval),predict(ossandon,eval),predict(sanchez,eval),predict(meo,eval),
      predict(fkast,eval),predict(goic,eval),predict(parisi,eval))

SEs=c(predpinera$se.fit[length(x)],predguillier$se.fit[length(x)],
      predossandon$se.fit[length(x)],
      predsanchez$se.fit[length(xSanchez)],predmeo$se.fit[length(xMEO)],
      predfkast$se.fit[length(xFK)],predgoic$se.fit[length(xGoic)],
      predparisi$se.fit[length(xParisi)])

df2s=c(pinera$dp[7],guillier$dp[7],ossandon$dp[7],sanchez$dp[7],meo$dp[7],
      fkast$dp[7],goic$dp[7],parisi$dp[7])

tamano=c(length(data$PineraAdj),length(data$GuillierAdj),length(data$OssandonAdj),
      length(dataSanchez$SanchezAdj),length(dataMEO$MEOAdj),length(dataFK$FKastAdj),
      length(dataGoic$GoicAdj),length(dataParisi$ParisiAdj))

Candidato=c("Piñera","Guillier","Ossandón","Sánchez","MEO","FKast","Goic","Parisi")

upperb=predicciones+1.96*SEs##Considering 95% Confidence Interval
lowerb=predicciones-1.96*SEs

Median=round(predicciones,digits=1)
Upperb=round(upperb,digits=1)
Lowerb=round(lowerb,digits=1)
SEs=round(SEs,digits=1)

outputPred=data.frame(Candidato,Median,Lowerb,Upperb,SEs,df2s,tamano)
