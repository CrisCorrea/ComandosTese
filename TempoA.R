TempoA1r=function(DadosServidores,r,TMDf,TMDm, rodadas){
###Calcula tempo at� a sa�de por morte ou invalidez pela TMD.

TempoAtivo=array(data = NA,  dim=c(dim(DadosServidores)[1], rodadas))
###Calcula tempo at� a sa�da pelas t�buas definidas.
for (i in 1:dim(DadosServidores)[1]){
    TempoAtivo[i,][DadosServidores$Sexo[i]==1]=rLife(n=rodadas,object=TMDf,x=DadosServidores$x[i],type="Kx")     ##Tipe=Kx - tempo discreto (Tx=Kx+0.5)
    TempoAtivo[i,][DadosServidores$Sexo[i]==2]=rLife(n=rodadas,object=TMDm,x=DadosServidores$x[i],type="Kx")    ##N�o � poss�vel atribuir vetores de idade. 
}
##Apenas para tempo que fica como ativo � poss�vel atribuir n=rodadas, pois todas as idades iniciais s�o iguais. 

limite=c(min(TempoAtivo[,1]), max(TempoAtivo[,1]))  ##Garante que os dois gr�ficos ficar�o na mesma escala
png(paste("Tempo at� morte ou invalidez x Idade - ",dim(DadosServidores)[1], "Servidores .png"))
plot(DadosServidores$x[DadosServidores$Sexo==1],TempoAtivo[,1][DadosServidores$Sexo==1], xlab="Idade atual", ylab="Tempo at� a sa�da por invalidez ou morte", col="red",pch=1, ylim=limite,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
points(DadosServidores$x[DadosServidores$Sexo==2],TempoAtivo[,1][DadosServidores$Sexo==2], xlab="Idade atual", ylab="Tempo at� a sa�da por invalidez ou morte", col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
legend("topright", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
dev.off()

TempoAp=matrix(rep(r-DadosServidores$x, rodadas), ncol=rodadas)
TempoAtivo[TempoAtivo>TempoAp]=TempoAp[TempoAtivo>TempoAp]

#png(paste("TempoAtivo x Idade - ",dim(DadosServidores)[1], "Servidores .png"))
#plot(DadosServidores$x[DadosServidores$Sexo==1],TempoAtivo[,1][DadosServidores$Sexo==1], xlab="Idade atual", ylab="Tempo como ativo", col="red",pch=1, ylim=limite,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#points(DadosServidores$x[DadosServidores$Sexo==2],TempoAtivo[,1][DadosServidores$Sexo==2], col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#legend(x=50, y = limite[2], c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
#dev.off()

return(TempoAtivo)
}




