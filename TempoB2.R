TempoB2=function(IB2, SB2,MSB1,TabuaF, TabuaM, rodadas){
##Estima duração do benefício do segundo beneficiário (caso houver)

TB2=TempoAteSaida(IB2, SB2,TabuaF, TabuaM, rodadas)
####Se beneficiário era filho menor, benefício acaba quando completa 21 anos ou quando morre, o que acontecer primeiro.
pop=length(IB2)/rodadas
for (k in 1:rodadas){
  for (j in 1:pop){
    if (MSB1[j,k]==4 & (21-IB2[j,k]< TB2[j,k])) TB2[j,k]=21-IB2[j,k]
  }
}

png(paste("IB2 x TB2 - ", pop, "Servidores.png"))
plot(IB2[,1][SB2==1 & MSB1!=6],TB2[,1][SB2==1 & MSB1!=6], xlab="Idade inicial do beneficiário 2", ylab="Duração do benefício em anos", col="red",pch=1, 
ylim=c(min(TB2[,1]), max(TB2[,1])),cex.axis=1.5,cex.lab=1.5, cex=1.5)     
points(IB2[,1][SB2==2 & MSB1!=6],TB2[,1][SB2==2 & MSB1!=6],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5,cex=1.5)
legend("topright", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"),cex=1.5)
dev.off()


return(TB2)
}






