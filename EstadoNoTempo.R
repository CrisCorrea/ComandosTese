EstadoNoTempo1IdadeEntrada=function(DadosServidores, TMD,TabuasMorte,ProbFamilia, tempo, rodadas){
###Estima tempo até saída de cada status e indica status para o qual mudou

###Estados observados no decorrer do tempo
##1 = Ativo; 2 = Inválido, 3 - aposentado, 4=Filho beneficiário, 5=Conjuge beneficiário, 6=Morto sem sependentes, 
pop=length(DadosServidores[[1]])

#Transforma tábuas em objetos da classe lifetable
TMDf=new("lifetable",x=TMD$idade,lx=TMD$lxTotalTMD.F, name="TMD Total F")
TMDm=new("lifetable",x=TMD$idade,lx=TMD$lxTotalTMD.M, name="TMD Total M")

MorteTMDf=new("lifetable",x=TMD$idade,lx=TMD$lxMorteTMD.F, name="TMD Morte F")
MorteTMDm=new("lifetable",x=TMD$idade,lx=TMD$lxMorteTMD.M, name="TMD Morte M")

TabuaF=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.F)
TabuaM=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.M)

#Estima idade de entrada  
y=IdadeEntrada1vez(DadosServidores)  ###Idade de entrada só é gerada uma vez
    
## Estima idade em que poderia se aposentar.
r=IdadeMinimaAposentadoria1vez(DadosServidores,y, rodadas)    ##Guarda idades de aposentadoria para cada rodada       

##Estima tempo na atividade
TempoAtivo=TempoA1r(DadosServidores,r,TMDf,TMDm, rodadas)  
MSA= MotivoSaiAtividade1r(DadosServidores,r,TempoAtivo,TMDf,TMDm,MorteTMDf,MorteTMDm,ProbFamilia,rodadas) 
MotivoSaiAtivo=MSA[[1]]
SexoBeneficiario1=MSA[[2]]
IdadeBeneficiario1=MSA[[3]]

#Duração do benefício do primeiro beneficiário
TempoBeneficiario1=TempoB1(MSA,DadosServidores,ProbFamilia,TabuaF, TabuaM, rodadas)
MFB=MotivoFimBeneficio(TempoBeneficiario1,IdadeBeneficiario1,SexoBeneficiario1,MotivoSaiAtivo,ProbFamilia, rodadas)
MotivoSaiBeneficiario1=MFB[[1]]
IdadeBeneficiario2=MFB[[2]]
SexoBeneficiario2=MFB[[3]]

#Duração do benefício do segundo beneficiário
TempoBeneficiario2=TempoB2(IdadeBeneficiario2, SexoBeneficiario2,MotivoSaiBeneficiario1,TabuaF, TabuaM, rodadas)

d=array(dim=c(pop,rodadas,7))
d[,,1]=matrix(rep(y,rodadas), ncol=rodadas)
d[,,2]=matrix(rep(r,rodadas), ncol=rodadas)
d[,,3]=TempoAtivo
d[,,4]=MotivoSaiAtivo
d[,,5]=TempoBeneficiario1
d[,,6]=TempoBeneficiario2
d[,,7]=MotivoSaiBeneficiario1

return (d) 

}



