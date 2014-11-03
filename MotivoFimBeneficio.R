MotivoFimBeneficio=function(TempoBeneficiario1,IdadeBeneficiario1,SexoBeneficiario1,MotivoSaiAtivo,ProbFamilia, rodadas){

pop=length(IdadeBeneficiario1)/rodadas
###Se era aposentado ou inválido e morreu, pode ter deixado conjuge e filho.
####Avalia se deixou cônjuge ou filho menor de 21 anos
MotivoSaiBeneficiario1=array(data = NA,  dim=c(pop, rodadas))
MotivoSaiBeneficiario1[MotivoSaiAtivo<=3]=7   ##Motivo pelo qual deixou de ser beneficiário. 7 - morreu. 4-morreu e deixou filho; 5 - morreu e deixou conjuge, 6 - morreu e não deixou dependentes.
MotivoSaiBeneficiario1[MotivoSaiAtivo>3]=6
IdadeMorteBeneficiario1=IdadeBeneficiario1+TempoBeneficiario1
Beneficiario2=ConjugeEFilho(MotivoSaiBeneficiario1,SexoBeneficiario1,IdadeMorteBeneficiario1,ProbFamilia, rodadas)
MotivoSaiBeneficiario1=Beneficiario2[[1]]
SexoBeneficiario2=Beneficiario2[[2]]
IdadeBeneficiario2=Beneficiario2[[3]]

png("IdadeBeneficiario2 x MotivoSaiBeneficiario1.png")
if (length(table(MotivoSaiBeneficiario1[,1][MotivoSaiBeneficiario1!=6]))==1) legenda=c("Conj.")
if (length(table(MotivoSaiBeneficiario1[,1][MotivoSaiBeneficiario1!=6]))==2) legenda=c("Filho","Conj.")
boxplot(IdadeBeneficiario2[,1][MotivoSaiBeneficiario1!=6]~MotivoSaiBeneficiario1[,1][MotivoSaiBeneficiario1!=6], ylab="Idade inicial do beneficiário 2", xlab="Motivo do benefício 2", names=legenda,cex.axis=1.5,cex.lab=1.5)
dev.off()

return(list(MotivoSaiBeneficiario1,IdadeBeneficiario2,SexoBeneficiario2))
}




