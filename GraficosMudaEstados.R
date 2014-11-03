###Gráficos Mudança de Estado
GraficosMudaEstado=function(DadosServidores,Estados){

y=Estados[,1,1]    ##Idade de entrada
r=Estados[,1,2]    ##Idade de aposentadoria
TempoAtivo=Estados[,,3]
MotivoSaiAtivo=Estados[,,4]
TempoBeneficiario1=Estados[,,5]
TempoBeneficiario2=Estados[,,6]
MotivoSaiBeneficiario1=Estados[,,7]
pop=dim(DadosServidores)[1]

##Idade de entrada
png(paste("Idade de entrada - ", pop,"Servidores.png"))
ymax=max(y)
plot(DadosServidores$x[DadosServidores$Sexo==1],y[DadosServidores$Sexo==1],xlab="Idade atual", ylab="Idade de entrada", col="red",pch=1, ylim=c(18,ymax),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
points(DadosServidores$x[DadosServidores$Sexo==2],y[DadosServidores$Sexo==2],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
legend("topleft", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"),cex =1.5)
dev.off()

png(paste("Pirâmide Idade de Entrada ",pop,"servidores.png"))
ParaPiramide=DadosServidores;ParaPiramide$x=y
piramide(ParaPiramide,titulo=(paste0(pop," Servidores")))
dev.off()


##Idade de Aposentadoria
png(paste("Idade de aposentadoria por sexo - ", pop, "Servidores.png"))
ymin=min(r)
ymax=max(r)
plot(y[DadosServidores$Sexo==1],r[DadosServidores$Sexo==1],xlab="Idade de entrada", ylab="Idade Aposentadoria Programada" , col="red",pch=1, ylim=c(ymin, ymax),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
points(y[DadosServidores$Sexo==2],r[DadosServidores$Sexo==2],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
legend("topleft", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
dev.off()

##Tempo Ativo
png(paste("TempoAtivo x Idade - ",pop, "Servidores .png"))
plot(DadosServidores$x[DadosServidores$Sexo==1],TempoAtivo[,1][DadosServidores$Sexo==1], xlab="Idade atual",
 ylab="Tempo como ativo", col="red",pch=1,  ylim=c(min(TempoAtivo[,1]), max(TempoAtivo[,1])),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
points(DadosServidores$x[DadosServidores$Sexo==2],TempoAtivo[,1][DadosServidores$Sexo==2], col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
legend("topright", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
dev.off()

##Motivo Sai da Atividade
if (length(table(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6])/sum(MotivoSaiAtivo[,1]!=6))==2) legenda=c('Inv.', 'Apos.')
if (length(table(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6])/sum(MotivoSaiAtivo[,1]!=6))==3) legenda=c('Inv.', 'Apos.', 'Conj.')
if (length(table(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6])/sum(MotivoSaiAtivo[,1]!=6))==4) legenda=c('Inv.', 'Apos.', 'Filho', 'Conj.')

png(paste("MotivoBeneficiário 1 - ", pop, "Servidores .png") )
barplot(table(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6])/sum(MotivoSaiAtivo[,1]!=6), xlab="Estado Beneficiário 1",ylab="Frequência" , names=legenda,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
dev.off()

##Dependentes dos ativos mortos
png(paste("Dependentes dos ativos mortos - ", pop, "Servidores .png") )
M=table(MotivoSaiAtivo[,1][MotivoSaiAtivo>3])/sum(MotivoSaiAtivo[,1]>3)
if (length(M)==3) nomes=c("Filhos", "Conjuges", "Sem Dep.")
if (length(M)==2) nomes=c("Conjuges", "Sem Dep.")
barplot(M,names=nomes, ylab="Frequência", xlab="Dependentes dos ativos mortos",cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
dev.off()

}







