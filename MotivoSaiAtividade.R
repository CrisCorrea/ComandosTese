MotivoSaiAtividade1r=function(DadosServidores,r,TempoAtivo,TMDf,TMDm,MorteTMDf,MorteTMDm,ProbFamilia,rodadas) {
##Motivo pelo qual saiu da situação de ativo.
  pop=length(DadosServidores[[1]])
  MotivoSaiAtivo=limite=array(data = 2,  dim=c(pop, rodadas))
  aleatorio=matrix(runif(pop*rodadas, min = 0, max = 1), ncol=rodadas) ##Gera números aleatórios
  
  for (k in 1:rodadas){
    for (j in 1:pop){
      if (DadosServidores$Sexo[j]==1)limite[j,k]=qxt(MorteTMDf, x=r[j], t=1)/qxt(TMDf, x=r[j],t=1)
      if (DadosServidores$Sexo[j]==2)limite[j,k]=qxt(MorteTMDm, x=r[j], t=1)/qxt(TMDm, x=r[j],t=1)
    }
  }
  MotivoSaiAtivo[aleatorio<=limite]=7   ##Morreu                                                                                        
  MotivoSaiAtivo[DadosServidores$x+TempoAtivo-r>=0]=3  ##Se idade que saiu é maior que idade de aposentadoria, assume que aposentou
  
if (length(table(MotivoSaiAtivo[,1])/pop)==3) {
  legenda=c('Inv.', 'Apos.','Morto')
  cores=c("orange","orange","orange","blue","blue","blue")
}
if (length(table(MotivoSaiAtivo[,1])/pop)==2) {
legenda=c( 'Apos.','Morto')
cores=c("orange","orange","blue","blue")
}
if (length(table(MotivoSaiAtivo[,1])/pop)==1) {
legenda=c( 'Apos.')
cores=c("orange","blue")
}

  png(paste("MotivoSaiAtivo - 1 rodada -", pop, "Servidores .png") )
  barplot(table(MotivoSaiAtivo[,1])/pop, xlab="Causa da saída do ativo",ylab="Frequência",names=legenda,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5 )
  dev.off()
  
  IdadeSaiAtivo=DadosServidores$x+TempoAtivo
  png(paste("IdadeSaiAtivo~MotivoSaiAtivo -", pop, "Servidores .png"))
  boxplot(IdadeSaiAtivo[,1]~MotivoSaiAtivo[,1], ylab="Idade de saída da atividade", xlab="Motivo da saída",
  names=legenda,cex.axis=1.5,cex.lab=1.5, cex=2,cex.main=2)
  dev.off()
  
  if (length(table(MotivoSaiAtivo[,1][DadosServidores$Sexo==1]))==length(table(MotivoSaiAtivo[,1][DadosServidores$Sexo==2]))){
     png(paste("SexoSaiAtivo~MotivoSaiAtivo -", pop, "Servidores .png"))
     barplot(c(table(MotivoSaiAtivo[,1][DadosServidores$Sexo==1])/sum(DadosServidores$Sexo==1),table(MotivoSaiAtivo[,1][DadosServidores$Sexo==2])/sum(DadosServidores$Sexo==2))
  , col=cores, names=c(legenda,legenda), ylab="Frequência", ylim=c(0,1),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5 )
    legend("topleft",c("Mulheres"), bty="n",cex=1.5)
   legend("topright",c("Homens"), bty="n",cex=1.5)
        dev.off()
  }
   
  SexoSaiAtivo=matrix(rep(DadosServidores$Sexo, rodadas), ncol=rodadas)

####Avalia se deixou cônjuge ou filho menor de 21 anos
Beneficiario1=ConjugeEFilho (MotivoSaiAtivo,SexoSaiAtivo,IdadeSaiAtivo,ProbFamilia, rodadas)
MotivoSaiAtivo=Beneficiario1[[1]]
SexoBeneficiario1=Beneficiario1[[2]]
IdadeBeneficiario1=Beneficiario1[[3]]

if (length(table(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6])/sum(MotivoSaiAtivo[,1]!=6))==2) legenda=c('Inv.', 'Apos.')
if (length(table(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6])/sum(MotivoSaiAtivo[,1]!=6))==3) legenda=c('Inv.', 'Apos.', 'Conj.')
if (length(table(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6])/sum(MotivoSaiAtivo[,1]!=6))==4) legenda=c('Inv.', 'Apos.', 'Filho', 'Conj.')

#  png(paste("MotivoBeneficiário 1 - ", pop, "Servidores .png") )
#  barplot(table(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6])/sum(MotivoSaiAtivo[,1]!=6), xlab="Estado Beneficiário 1",ylab="Frequência" , names=legenda,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
#  dev.off()
  
  png(paste("IdadeBeneficiario1~MotivoSaiAtivo -", pop, "Servidores .png"))
  boxplot((IdadeBeneficiario1[,1][MotivoSaiAtivo[,1]!=6])~(MotivoSaiAtivo[,1][MotivoSaiAtivo[,1]!=6]), ylab="Idade Beneficiário 1", xlab="Motivo da saída da atividade",names=legenda,cex.axis=1.5,cex.lab=1.5, cex=2,cex.main=2)
  dev.off()

  

return(list(MotivoSaiAtivo,SexoBeneficiario1,IdadeBeneficiario1))
}




