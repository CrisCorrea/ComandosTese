IdadeMinimaAposentadoria1vez <- function (DadosServidores,y, rodadas){
##Estima idade m�nima de aposentadoria programada.

pop=length(DadosServidores[,1])   #declara o tamanho da popula��o inicial
r=rTempo=rIdade=vector(length=pop)
## Idade de aposentadoria por idade:rIdade    
#Idade de Aposentadoria por idade e tempo de contribui��o:rTempo   
## Menor idade em que � eleg�vel � aposentadoria:r

    
  ##Elegibilidade � aposentadoria por idade (m�nimo de 10 anos no servi�o p�blico)
  rIdade[DadosServidores$Sexo==1 & y[]<=50]=60
  rIdade[DadosServidores$Sexo==1 & y[]>50]=y[DadosServidores$Sexo==1 & y[]>50]+10     
  rIdade[DadosServidores$Sexo==2 & y[]<=55]=65
  rIdade[DadosServidores$Sexo==2 & y[]>55]=y[DadosServidores$Sexo==2 & y[]>55]+10

  # Elegibilidade � aposentadoria por tempo e idade
  ##Se entrou antes dos 25 anos de idade, aposenta por tempo de contribui��o aos 60, se homem, e aos 55, se mulher. 
  rTempo[(DadosServidores$Sexo==1 & y[]<=25)]=55
  rTempo[(DadosServidores$Sexo==1) & (y[]>25)]= 30+y[(DadosServidores$Sexo==1) & (y[]>25)]

  rTempo[(DadosServidores$Sexo==2 & y[]<=25)]=60
  rTempo[(DadosServidores$Sexo==2 & y[]>25)]= y[(DadosServidores$Sexo==2 & y[]>25)]+35
                                      
  ##Avalia idade m�nima de aposentadoria. 
  for (j in 1:pop)  {
    if (rIdade[j]<=DadosServidores$x[j]) rIdade[j]=DadosServidores$x[j]+1       ##Ajusta para que aposentadoria n�o ocorra em idade antes da idade atual. 
    if (rTempo[j]<=DadosServidores$x[j]) rTempo[j]=DadosServidores$x[j]+1 
    r[j]=min(rIdade[j],rTempo[j],70)  ##Aposentadoria compuls�ria aos 70 anos
  }



#png(paste("Idade de aposentadoria por sexo - ", pop, "Servidores.png"))
#ymin=min(r)
#ymax=max(r)
#plot(y[DadosServidores$Sexo==1],r[DadosServidores$Sexo==1],xlab="Idade de entrada", ylab="Idade Aposentadoria Programada" , col="red",pch=1, ylim=c(ymin, ymax),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#points(y[DadosServidores$Sexo==2],r[DadosServidores$Sexo==2],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#legend(x=18, y = 70, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
#dev.off()

return(r)
}


