TempoB1=function(MSA,DadosServidores,ProbFamilia,TabuaF, TabuaM, rodadas){
#Estima o tempo de dura��o do primeiro benef�cio (se houver)

MotivoSai=MSA[[1]]  ##Motivo sa�da benefici�rio 1
SB1=MSA[[2]]    ##Sexo benefici�rio 1
IB1=MSA[[3]]    ##Idade benefici�rio 1

##Tempo que benefici�rio1 recebeu benef�cio (s� sai por morte)
TB1=TempoAteSaida(IB1,SB1,TabuaF, TabuaM, rodadas)
####Se benefici�rio era filho menor, benef�cio acaba quando completa 21 anos ou quando morre, o que acontecer primeiro.
for (k in 1:rodadas) for (j in 1:dim(DadosServidores)[1]) if (MotivoSai[j,k]==4 & (21-IB1[j,k]< TB1[j,k])) TB1[j,k]=21-IB1[j,k]

png(paste("IB1 x TB1 - ", dim(DadosServidores)[1], "Servidores.png"))
plot(IB1[,1][SB1[,1]==1 & MotivoSai[,1]!=6],TB1[,1][SB1[,1]==1 & MotivoSai[,1]!=6],ylim=c(0, (max(TB1[,1])+1)),xlim=c(0,max(IB1[,1])),xlab="Idade inicial do benefici�rio 1", 
ylab="Dura��o do benef�cio em anos",col="red",pch=1,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
points(IB1[,1][SB1==2 & MotivoSai[,1]!=6],TB1[,1][SB1==2 & MotivoSai[,1]!=6] ,col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
legend("topleft", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
dev.off()

return (TB1)
}









