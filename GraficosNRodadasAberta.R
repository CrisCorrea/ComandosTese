GraficosNRodadasAberta <- function (i,DadosServidores,MatrizDeEstados,tempo){ #faz gr�ficos para v�rias rodadas
TAtivo=MatrizDeEstados[,,3]
MotivoSaiAtivo=MatrizDeEstados[,,4]
TB1=MatrizDeEstados[,,5]
TB2=MatrizDeEstados[,,6]
MotivoSaiB1=MatrizDeEstados[,,7]                                         
pop= dim(TAtivo)[1]
rodadas=dim(TAtivo)[2]
status=6
TE=DadosServidores[4:(rodadas+3)] ###Tempo de entrada de cada indiv�duo na popula��o

Estado=array(0,dim=c(pop,tempo,rodadas))
for (k in 1:rodadas) for (j in 1:pop) for (t in 1:tempo){    
      if (t>TE[j,k]+TAtivo[j,k]+TB1[j,k]+TB2[j,k]) Estado[j,t,k]=6
      if (t<=TE[j,k]+TAtivo[j,k]+TB1[j,k]+TB2[j,k]) Estado[j,t,k]=MotivoSaiB1[j,k]
      if (t<=TE[j,k]+TAtivo[j,k]+TB1[j,k]) Estado[j,t,k]=MotivoSaiAtivo[j,k]
      if (t<=TE[j,k]+TAtivo[j,k]) Estado[j,t,k]=1
      if (t<=TE[j,k]) Estado[j,t,k]=0    ##Linha necess�ria apra pop aberta
}

#### Gera matrizes auxiliares
MatrizResumo=MatrizDiferenca=array(data = NA,  dim=c(6, tempo,rodadas))  ## Resumo com a frequencia de cada status no tempo
for (j in 1:6) for (t in 1:tempo){
  for (k in 1:rodadas) MatrizResumo[j,t,k]=sum(Estado[,t,k]==j)      ##Precisa ser em for diferente
  for (k in 1:rodadas) MatrizDiferenca[j,t,k]=MatrizResumo[j,t,k]-mean(MatrizResumo[j,t,])
}
estados=c("Ativos","Inv�lidos", "Aposentados", "Filhos benefici�rios", "C�njuge benefici�rio", "Morto sem benefici�rio")
write.table(MatrizResumo[,,2], paste("MATRIZ ESTADOS", pop, "serv.txt"), sep="\t",dec=",")


MatrizMedia=Minimo=Maximo=DP=VARIA=array(data = NA,  dim=c(status, tempo))  ## Vetor de m�dia de cada status no tempo
yMin=yMax= vector(length=status)

for (t in 1:tempo){
    for (j in 1:status){
        MatrizMedia[j,t]=mean(MatrizResumo[j,t,])
        Minimo[j,t]=min(MatrizResumo[j,t,])
        Maximo[j,t]=max(MatrizResumo[j,t,])
        DP[j,t]=sd(MatrizResumo[j,t,])
        VARIA[j,t]=var(MatrizResumo[j,t,])
        yMax[j]=max(MatrizResumo[j,,])
        yMin[j]=min(MatrizResumo[j,,])        
    }
}
yMaxD=max(MatrizDiferenca)
yMinD=min(MatrizDiferenca)                       
dif=Maximo-Minimo

#for (j in 1:status){
  ###Gr�ficos dos valores observados
#    png(paste("Varia��oEstados",j," ", pop, "serv. Pop",i,".png"))
#    plot(MatrizResumo[j,,1], type="l", xlab="Tempo", ylab="Frequ�ncia", ylim=c(yMin[j],yMax[j]), main=(estados[j]),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5)
#    for (k in 2:rodadas) lines(MatrizResumo[j,,k], cex=1.5)
#    lines(MatrizMedia[j,], col="red", lwd=2, cex=1.5)
#    dev.off()
  ###Gr�ficos das diferen�as observadas 
#    png(paste("Diferen�aEstados",j," ", pop, "serv. Pop",i,".png"))
#    plot(MatrizDiferenca[j,,1], type="l", xlab="Tempo", ylab="Diferen�a em rela��o � M�dia", ylim=c(yMinD,yMaxD),main=estados[j],cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5)
#    for (k in 2:rodadas){
#        lines(MatrizDiferenca[j,,k], cex=1.5)
#    }
#    abline(h=0, col="red", lwd=2, cex=1.5)
#    dev.off()
#}
  ###Gr�ficos do m�ximo e m�nimo 
png(paste("Extremos",1," Aberta", pop, "serv. Pop",i,".png"))    
plot(Maximo[1,], type="l" , lty =2, ylab="Frequ�ncia", xlab="Tempo",main=paste("Painel A -",(estados[1])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
lines(Minimo[1,],lty =3, cex=1.5,lwd=2)
lines(MatrizMedia[1,],col="red", cex=1.5,lwd=2)
legend("topleft",legend=c("M�ximo","M�dia","M�nimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n")    
dev.off()
png(paste("Extremos",2,"Aberta ", pop, "serv. Pop",i,".png"))    
plot(Maximo[2,], type="l" , lty =2, ylab="Frequ�ncia", xlab="Tempo",main=paste("Painel C -",(estados[2])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
lines(Minimo[2,],lty =3, cex=1.5,lwd=2)
lines(MatrizMedia[2,],col="red", cex=1.5,lwd=2)
legend("topleft",legend=c("M�ximo","M�dia","M�nimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n")    
dev.off()
png(paste("Extremos",3," Aberta", pop, "serv. Pop",i,".png"))     
plot(Maximo[3,], type="l" , lty =2, ylab="Frequ�ncia", xlab="Tempo",main=paste("Painel D -",(estados[3])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
lines(Minimo[3,],lty =3, cex=1.5,lwd=2)
lines(MatrizMedia[3,],col="red", cex=1.5,lwd=2)
legend("topleft",legend=c("M�ximo","M�dia","M�nimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n")    
dev.off()
png(paste("Extremos",4,"Aberta ", pop, "serv. Pop",i,".png"))     
plot(Maximo[4,], type="l" , lty =2, ylab="Frequ�ncia", xlab="Tempo",main=paste("Painel E -",(estados[4])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
lines(Minimo[4,],lty =3, cex=1.5,lwd=2)
lines(MatrizMedia[4,],col="red", cex=1.5,lwd=2)
legend("topleft",legend=c("M�ximo","M�dia","M�nimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n")    
dev.off()
png(paste("Extremos",5,"Aberta ", pop, "serv. Pop",i,".png"))     
plot(Maximo[5,], type="l" , lty =2, ylab="Frequ�ncia", xlab="Tempo",main=paste("Painel F -",estados[5]),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
lines(Minimo[5,],lty =3, cex=1.5,lwd=2)
lines(MatrizMedia[5,],col="red", cex=1.5,lwd=2)
legend("topleft",legend=c("M�ximo","M�dia","M�nimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n")    
dev.off()
png(paste("Extremos",6," Aberta", pop, "serv. Pop",i,".png"))     
plot(Maximo[6,], type="l" , lty =2, ylab="Frequ�ncia", xlab="Tempo",main=paste("Painel B -",(estados[j])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
lines(Minimo[6,],lty =3, cex=1.5,lwd=2)
lines(MatrizMedia[6,],col="red", cex=1.5,lwd=2)
legend("topleft",legend=c("M�ximo","M�dia","M�nimo"), col=c("black","red","black"), lty = c(2,1,3),cex=1.5,bty="n")
dev.off()

png(paste("Desvio Padr�o dos estados Aberta", pop, "serv. Pop",i,".png"))
plot(DP[1,], type="l",lty = 1,lwd=2,col="red", ylim=c(min(DP), 1.3*max(DP)), ylab="Desvio Padr�o", xlab="Tempo",cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines(DP[2,], col="blue",lty =2,lwd=2 , cex=1.5)
lines(DP[3,], col="dark green",lty = 3,lwd=2, cex=1.5)
lines(DP[4,], col="dark orange",lty = 4,lwd=2, cex=1.5)
lines(DP[5,], col="brown",lty = 5,lwd=2, cex=1.5)
lines(DP[6,], col="black",lty =6 ,lwd=2, cex=1.5)
legend ("topleft",legend=estados[1:3],lty = seq(1:3),lwd=2,col = c("red", "blue","dark green"), cex=1.2, bty="n")
legend ("topright",legend=estados[4:6],lty = seq(4:6),lwd=2,col = c("dark orange","brown","black"), cex=1.2, bty="n")
dev.off()
write.table(DP, paste("DESVIO PADR�O DOS ESTADOS", pop, "serv.txt"), sep="\t",dec=",")

png(paste("Diferen�a M�xima Aberta", pop, "serv. Pop",i,".png"))
plot(dif[1,],type="l", ylim=c(min(dif), 1.3*max(dif)),lwd=2,col="red",ylab="Diferen�a m�xima", xlab="Tempo",cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines(dif[2,], col="blue",lty =2 ,lwd=2, cex=1.5)
lines(dif[3,], col="dark green",lty = 3,lwd=2, cex=1.5)
lines(dif[4,], col="dark orange",lty = 4,lwd=2, cex=1.5)
lines(dif[5,], col="brown",lty = 5,lwd=2, cex=1.5)
lines(dif[6,], col="black",lty =6 ,lwd=2, cex=1.5)
legend ("topleft",legend=estados[1:3],lty = seq(1:3),lwd=2,col = c("red", "blue","dark green"), cex=1.2, bty="n")
legend ("topright",legend=estados[4:6],lty = seq(4:6),lwd=2,col = c("dark orange","brown","black"), cex=1.2, bty="n")
dev.off()
write.table(dif,paste("Diferen�a m�xima no estado", pop, "serv.txt"), sep="\t",dec=",")

png(paste("M�dia Aberta", pop, "serv.png")) 
plot(MatrizMedia[1,],type="l", ylim=c(min(MatrizMedia), 1.3*max(MatrizMedia)),lwd=2,col="red",ylab="M�dia", xlab="Tempo",cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines(MatrizMedia[2,], col="blue",lty =2 ,lwd=2, cex=1.5)
lines(MatrizMedia[3,], col="dark green",lty = 3,lwd=2, cex=1.5)
lines(MatrizMedia[4,], col="dark orange",lty = 4,lwd=2, cex=1.5)
lines(MatrizMedia[5,], col="brown",lty = 5,lwd=2, cex=1.5)
lines(MatrizMedia[6,], col="black",lty =6 ,lwd=2, cex=1.5)
legend ("topleft",legend=estados[1:3],lty = seq(1:3),lwd=2,col = c("red", "blue","dark green"), cex=1.2, bty="n")
legend ("topright",legend=estados[4:6],lty = seq(4:6),lwd=2,col = c("dark orange","brown","black"), cex=1.2, bty="n")
dev.off()


####Tempo de contribui��o e recebimento de benef�cio
png(paste("Tempo m�dio de contribui��o e benef�cio Aberta- ", pop, "serv. Pop",i,".png"))
TCont=TBenServ=TBenDep=vector(length=rodadas)
for ( k in 1:rodadas) {
  TCont[k]= sum(MatrizResumo[1,,k])/pop 
  TBenServ[k]= (sum(MatrizResumo[2,,k])+sum(MatrizResumo[3,,k]))/pop 
  TBenDep[k]= (sum(MatrizResumo[4,,k])+sum(MatrizResumo[5,,k]))/pop 
}
#TBeneficio=  TBenServ+TBenDep
boxplot(TCont,TBenServ,TBenDep, names=c("Contribui��o", "Benef. Serv.", "Benef. Dep."), ylab="Tempo m�dio na rodada", cex.axis=1.5,cex.lab=1.5,cex=1.5)
#boxplot(TCont,TBeneficio, names=c("Contribui��o", "Beneficio"), ylab="Tempo")
dev.off()
} 





