IdadeMinimaAposentadoria1vez <- function (DadosServidores,y, rodadas){
##Estima idade mínima de aposentadoria programada.

pop=length(DadosServidores[,1])   #declara o tamanho da população inicial
r=rTempo=rIdade=vector(length=pop)
## Idade de aposentadoria por idade:rIdade    
#Idade de Aposentadoria por idade e tempo de contribuição:rTempo   
## Menor idade em que é elegível à aposentadoria:r

    
  ##Elegibilidade à aposentadoria por idade (mínimo de 10 anos no serviço público)
  rIdade[DadosServidores$Sexo==1 & y[]<=50]=60
  rIdade[DadosServidores$Sexo==1 & y[]>50]=y[DadosServidores$Sexo==1 & y[]>50]+10     
  rIdade[DadosServidores$Sexo==2 & y[]<=55]=65
  rIdade[DadosServidores$Sexo==2 & y[]>55]=y[DadosServidores$Sexo==2 & y[]>55]+10

  # Elegibilidade à aposentadoria por tempo e idade
  ##Se entrou antes dos 25 anos de idade, aposenta por tempo de contribuição aos 60, se homem, e aos 55, se mulher. 
  rTempo[(DadosServidores$Sexo==1 & y[]<=25)]=55
  rTempo[(DadosServidores$Sexo==1) & (y[]>25)]= 30+y[(DadosServidores$Sexo==1) & (y[]>25)]

  rTempo[(DadosServidores$Sexo==2 & y[]<=25)]=60
  rTempo[(DadosServidores$Sexo==2 & y[]>25)]= y[(DadosServidores$Sexo==2 & y[]>25)]+35
                                      
  ##Avalia idade mínima de aposentadoria. 
  for (j in 1:pop)  {
    if (rIdade[j]<=DadosServidores$x[j]) rIdade[j]=DadosServidores$x[j]+1       ##Ajusta para que aposentadoria não ocorra em idade antes da idade atual. 
    if (rTempo[j]<=DadosServidores$x[j]) rTempo[j]=DadosServidores$x[j]+1 
    r[j]=min(rIdade[j],rTempo[j],70)  ##Aposentadoria compulsória aos 70 anos
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


