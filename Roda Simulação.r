##### Resultados .

##Carrega dados iniciais (premissas)
#Escolhe diretório
setwd("C:\\Users\\aroki_000\\Dropbox\\Tese\\Resultados\\Simulações estocásticas\\Premissas")
premissas <- read.table("Premissas.txt",header = TRUE,sep = "",dec=",",)
##Ler tábuas separadamente para evitar erros nas funções do pacote lifecontingencies. Para ler todas de uma vez só algumas tabelas teriam valores iguais a zero, o que dá erro na classe lifetable.  
TMD <- read.table("TMD Simulação.txt",header = TRUE,sep = "",dec=",",)
ProbFamilia <- read.table("Probab de Familia.txt",header = TRUE,sep = "",dec=",",)
TabuasMorte <- read.table("Tabuas de Morte.txt",header = TRUE,sep = "",dec=",",)

#Transforma tábuas em objetos da classe lifetable
TMDf=new("lifetable",x=TMD$idade,lx=TMD$lxTotalTMD.F, name="TMD Total F")
TMDm=new("lifetable",x=TMD$idade,lx=TMD$lxTotalTMD.M, name="TMD Total M")

MorteTMDf=new("lifetable",x=TMD$idade,lx=TMD$lxMorteTMD.F, name="TMD Morte F")
MorteTMDm=new("lifetable",x=TMD$idade,lx=TMD$lxMorteTMD.M, name="TMD Morte M")

TabuaF=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.F)
TabuaM=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.M)


#Lê funções 
library(lifecontingencies)
#library(truncnorm)
setwd("C:\\Users\\aroki_000\\Dropbox\\Tese\\Resultados\\Simulações estocásticas\\Funções")
funcoes<-list.files()
len<-length(funcoes)
for (i in 1:len)
{source(funcoes[i])
}



#Escolhe diretório para guardar resultados
setwd("C:\\Users\\aroki_000\\Desktop\\TesteGráficos")                           

rodadas=5    ###Dá o número de rodadas do processo de simulação. Ex.: 1.000.
tempo=75  # declara o tempo de simulação  em anos 

##Declara tamanhos de população desejados.          			
N=c(50,100,150)


#### Gera população inicial
#DadosServidores <-read.table("C:\\Users\\aroki_000\\Dropbox\\Tese\\Resultados\\Simulações estocásticas\\Premissas\\DadosServidoresA.txt",
#header = TRUE,sep = "",dec=",",) 
#DadosServidores335=DadosServidores
#DadosServidores670=rbind(DadosServidores335,DadosServidores335)
#DadosServidores1005=rbind(DadosServidores335,DadosServidores670)
#PopInicial=list(length=3)
#PopInicial[[1]]=DadosServidores335
#PopInicial[[2]]=DadosServidores670
#PopInicial[[3]]=DadosServidores1005
##Declara tamanhos de população desejados.          			
#N=c(335,670,1005)


#### Gera população inicial  de tamanho N
PopInicial=list(length=length(N))
for (i in 1:(length(N)))  PopInicial[[i]]=assign(paste0("DadosServidores",N[i]), GeraPopInicial(N[i]))

  
  
##Roda simulações com população fechada
REstados=SalarioContribuicao=Beneficio=list(length=length(N))
for (i in 1:(length(N))) {
  REstados[[i]]=EstadoNoTempo1IdadeEntrada(PopInicial[[i]],TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
  BeneficioeContribuicao=Pagamentos(PopInicial[[i]],REstados[[i]],TabuasMorte, premissas$ss, tempo, rodadas)
  Beneficio[[i]]= BeneficioeContribuicao[[1]]
  SalarioContribuicao[[i]]=BeneficioeContribuicao[[2]]
  save(list = ls(all = TRUE), file = "SimulaçõesFechada.RData")
}

##Salva resultados
save(list = ls(all = TRUE), file = "SimulaçõesFechada.RData")

i=2
GraficosMudaEstado(PopInicial[[2]],REstados[[2]])
GraficosNRodadas (REstados[[i]],tempo)
#for (i in 1:(length(N))) GraficosNRodadas (REstados[[i]],tempo)


##Calcula alíquota de contribuição, Contribuição e Reserva Matemática
Tfuturo=200
aliquota=vector(length=length(N))
Contribuicao=RM=RM0=RMPadronizada=RProspecFechada=DiferencaReservaFechada=DifPadronizadaFechada=list(length=length(N))
for (i in 1:(length(N))) {
     aliquota[i]=Aliquota(SalarioContribuicao[[i]],Beneficio[[i]],premissas$i)
     Contribuicao[[i]]=SalarioContribuicao[[i]]*aliquota[i]       ###Valor das contribuições com a alíquota de contribuição definida como alíquota ideal
     RM[[i]]=ReservaMatematica(SalarioContribuicao[[i]],Beneficio[[i]],premissas$i,aliquota[i])
     RM0[[i]]=ReservaMatematica(SalarioContribuicao[[i]],Beneficio[[i]],0,aliquota[i])
     RMPadronizada[[i]]=RM[[i]]/N[i]      ##Padroniza resultados pelo tamanho da população
     SC=rbind(SalarioContribuicao[[i]],matrix(rep(0,(Tfuturo-tempo)*rodadas), ncol=rodadas))
     B=rbind(Beneficio[[i]],matrix(rep(0,(Tfuturo-tempo)*rodadas), ncol=rodadas))
     Pop=cbind(PopInicial[[i]],matrix(rep(0,N[i]*rodadas), ncol=rodadas))
     RProspecFechada[[i]]=ReservaProspectiva(Pop,SC, B, premissas$i, aliquota[i],Tfuturo)    ##Reserva na visão prospectiva
     DiferencaReservaFechada[[i]]=(RM[[i]]-RProspecFechada[[i]][1:75,])   ###Diferença entre as reservas retrospectiva e prospectiva
     DifPadronizadaFechada[[i]]=DiferencaReservaFechada[[i]]/N[i]
  save(list = ls(all = TRUE), file = "SimulaçõesFechada.RData")
}



   
####Calcula variância da Reserva Matemática
VarRMPad=VarDifPad=dpRMPad=dpDifPad=matrix(data = NA, nrow = tempo, ncol = length(N))
for (i in 1:length(N)) for(t in 1:tempo){
      VarRMPad[t,i]=var(RMPadronizada[[i]][t,])
      VarDifPad[t,i]=var(DifPadronizadaFechada[[i]][t,])
      dpRMPad[t,i]=sd(RMPadronizada[[i]][t,])
      dpDifPad[t,i]=sd(DifPadronizadaFechada[[i]][t,])
}
PR=ProbRuina(RMPadronizada[[1]])
PR2=ProbRuina(DifPadronizadaFechada[[1]])   ##Sobre o resultado
TM=TamRuina(RMPadronizada[[1]])
TM2=TamRuina(DifPadronizadaFechada[[1]])
TempoAteRuina=length(PR)-length(PR[PR>0])+1
TempoAteRuina2=length(PR2)-length(PR2[PR2>0])+1
p=0    ##Aceita probabildiade p de déficit
AliquotaRD=ARDp(RM[[1]],SalarioContribuicao[[1]],premissas$i,tempo,rodadas,p)
AliquotaRD2=ARDp(DiferencaReservaFechada[[1]],SalarioContribuicao[[1]],premissas$i,tempo,rodadas,p)
for (i in 2:length(N)){
  PR=cbind(PR,ProbRuina(RMPadronizada[[i]]))
  PR2=cbind(PR2,ProbRuina(DifPadronizadaFechada[[i]]))
  TempoAteRuina=cbind(TempoAteRuina,length(PR[,i])-length(PR[,i][PR[,i]>0])+1)
  TempoAteRuina2=cbind(TempoAteRuina2,length(PR2[,i])-length(PR2[,i][PR2[,i]>0])+1)
  TM=cbind(TM,TamRuina(RMPadronizada[[i]]))        ##Tamanho médio do déficit dado que houve ruína a cada tempo
  TM2=cbind(TM2,TamRuina(DifPadronizadaFechada[[i]])) 
  AliquotaRD=cbind(AliquotaRD,ARDp(DiferencaReservaFechada[[i]],SalarioContribuicao[[i]],premissas$i,tempo,rodadas,p))
  AliquotaRD2=cbind(AliquotaRD2,ARDp(DiferencaReservaFechada[[i]],SalarioContribuicao[[i]],premissas$i,tempo,rodadas,p))
}
TempoRuina=TempoRuinaDifFechada=array(data = 0, dim = c(length(N),rodadas))
for (i in 1: length(N)) for ( k in 1:rodadas) {
    TempoRuina[i,k]=length(RMPadronizada[[i]][,k])-length(RMPadronizada[[i]][,k][RMPadronizada[[i]][,k]<0])+1
        TempoRuinaDifFechada[i,k]=length(DifPadronizadaFechada[[i]][,k])-length(DifPadronizadaFechada[[i]][,k][DifPadronizadaFechada[[i]][,k]<0])+1
}
TempoRuina[TempoRuina==76]=NA
TempoRuinaDifFechada[TempoRuinaDifFechada==76]=NA 



###Medidas de Solvência   pop FECHADA
p=0.90  
VaR=CVaR=TVaR=ES=VaRDifFechada=CVaRDifFechada=TVaRDifFechada=ESDifFechada=matrix(data=0,ncol=length(RMPadronizada), nrow=dim(RMPadronizada[[1]])[1]) 
for (i in 1:length(RMPadronizada)) for (t in 1:tempo){
   Perda=-RMPadronizada[[i]];Perda[RMPadronizada[[i]]>0]=0
   VaR[t,i]=quantile(Perda[t,],p) ##Fórmula 5.35 do Kaas
   ES[t,i]=sum(Perda[t,][Perda[t,]>VaR[t,i]])/rodadas
   TVaR[t,i]=VaR[t,i]+ES[t,i]/(1-p) ##Fórmula 5.42 Kaas
   CVaR[t,i]=mean(Perda[t,][Perda[t,]>VaR[t,i]])   ##Equação 5.44 Kaas
   Perda=-DifPadronizadaFechada[[i]];Perda[DifPadronizadaFechada[[i]]>0]=0
   VaRDifFechada[t,i]=quantile(Perda[t,],p) ##Fórmula 5.35 do Kaas
   ESDifFechada[t,i]=sum(Perda[t,][Perda[t,]>VaRDifFechada[t,i]])/rodadas
   TVaRDifFechada[t,i]=VaRDifFechada[t,i]+ESDifFechada[t,i]/(1-p) ##Fórmula 5.42 Kaas
   CVaRDifFechada[t,i]=mean(Perda[t,][Perda[t,]>VaRDifFechada[t,i]])   ##Equação 5.44 Kaas
}       

##Salva resultados
save(list = ls(all = TRUE), file = "SimulaçõesFechada.RData")




######################################## POPULAÇÃO ABERTA

#Escolhe diretório para guardar resultados
setwd(".\\Aberta")

##Roda simulações com população aberta
PopAberta=EstadosAberto=list(length=length(N))
for (i in 1:(length(N))){
    PopAberta[[i]]=GeraPopEntrada(N[i])
    EstadosAberto[[i]]=EstadoNoTempo1IdadeEntrada(PopAberta[[i]],TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
    PopAberta[[i]]=cbind(PopAberta[[i]],REstados[[i]][,,3])
    TempoAtividade=REstados[[i]][,,3]+EstadosAberto[[i]][,,3]
    while(length(TempoAtividade[TempoAtividade<75])>0) {
       Pop=GeraPopEntrada(N[i])
       EstadosPop=EstadoNoTempo1IdadeEntrada(Pop,TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
       PopAberta[[i]]=rbind(PopAberta[[i]],cbind(Pop,TempoAtividade))
       EA=array(data = NA, dim = c(dim(PopAberta[[i]])[1],rodadas,7))
       for (d in 1:7) EA[,,d]=rbind(EstadosAberto[[i]][,,d],EstadosPop[,,d])
       EstadosAberto[[i]]=EA
       TempoAtividade=TempoAtividade+EstadosPop[,,3]
    }
    save(list = ls(all = TRUE), file = "SimulaçõesAberta.RData")
}

PopTotal=EstadosTotal=list(length=length(N))
Naberta=vector(length=length(N))
for (i in 1:(length(N))){
    EA=array(data = NA, dim = c((dim(PopAberta[[i]])+dim(PopInicial[[i]]))[1],rodadas,7))
    for (d in 1:7) EA[,,d]=rbind(REstados[[i]][,,d],EstadosAberto[[i]][,,d])
    EstadosTotal[[i]]=EA
    PopTotal[[i]]=rbind(cbind(PopInicial[[i]],matrix(rep(0,N[i]*rodadas), ncol=rodadas)),PopAberta[[i]])
    Naberta[i]= dim(PopTotal[[i]])[1]
    #GraficosNRodadasAberta(i,PopTotal[[i]],EstadosTotal[[i]],tempo)
}
i=2   ##Gráficos para pop de 250 servidores
GraficosNRodadasAberta(i,PopTotal[[i]],EstadosTotal[[i]],tempo)

#Estima valores de contribuições e benefícios para os novos entrados. 

Tfuturo=200
SalarioContribuicaoAberta=BeneficioAberta=SalarioContribuicaoTotal=BeneficioTotal=ContribuicaoTotal=RMAberta=RM0Aberta=RMPadronizadaAberta=RProspec=DiferencaReservaAberta=DifPadronizadaAberta=list(length=length(N))
for (i in 1:(length(N))) {
  BeneficioeContribuicao=Pagamentos200anos(PopAberta[[i]],EstadosAberto[[i]],TabuasMorte, premissas$ss, tempo, rodadas,Tfuturo)
  BeneficioAberta[[i]]= BeneficioeContribuicao[[1]]
  SalarioContribuicaoAberta[[i]]=BeneficioeContribuicao[[2]]
  BeneficioTotal[[i]]=Beneficio[[i]]+BeneficioAberta[[i]][1:75,]
  SalarioContribuicaoTotal[[i]]= SalarioContribuicao[[i]]+SalarioContribuicaoAberta[[i]][1:75,] 
  ContribuicaoTotal[[i]]=SalarioContribuicaoTotal[[i]]*aliquota[i]       ###Valor das contribuições com a alíquota de contribuição definida como alíquota ideal
  RMAberta[[i]]=ReservaMatematica(SalarioContribuicaoTotal[[i]],BeneficioTotal[[i]],premissas$i,aliquota[i])
  RM0Aberta[[i]]=ReservaMatematica(SalarioContribuicaoTotal[[i]],BeneficioTotal[[i]],0,aliquota[i])
  RMPadronizadaAberta[[i]]=RMAberta[[i]]/N[i]      ##Padroniza resultados pelo tamanho da população
  SC=rbind(SalarioContribuicao[[i]],matrix(rep(0,(Tfuturo-tempo)*rodadas), ncol=rodadas))
  B=rbind(Beneficio[[i]],matrix(rep(0,(Tfuturo-tempo)*rodadas), ncol=rodadas))
  RProspec[[i]]=ReservaProspectiva(PopTotal[[i]],SC+SalarioContribuicaoAberta[[i]], B+BeneficioAberta[[i]], premissas$i, aliquota[i],Tfuturo)    ##Reserva na visão prospectiva
  DiferencaReservaAberta[[i]]=(RMAberta[[i]]-RProspec[[i]][1:75,])   ###Diferença entre as reservas retrospectiva e prospectiva
  DifPadronizadaAberta[[i]]=DiferencaReservaAberta[[i]]/N[i]
  save(list = ls(all = TRUE), file = "SimulaçõesAberta.RData")
}


  
   
####Calcula variância da Reserva Matemática
VarRMPadAberta=dpRMPadAberta=matrix(data = NA, nrow = tempo, ncol = length(N))
for (i in 1:length(N))  for(t in 1:tempo){
      VarRMPadAberta[t,i]=var(RMPadronizadaAberta[[i]][t,])
      dpRMPadAberta[t,i]=sd(RMPadronizadaAberta[[i]][t,])
}
PRAberta=ProbRuina(RMPadronizadaAberta[[1]])
TMAberta=TamRuina(RMPadronizadaAberta[[1]])
TempoAteRuinaAberta=length(PRAberta)-length(PRAberta[PRAberta>0])+1
p=0    ##Aceita probabildiade p de déficit
AliquotaRDAberta=ARDp(RMAberta[[1]],SalarioContribuicaoTotal[[1]],premissas$i,tempo,rodadas,p)
for (i in 2:length(N)){
  PRAberta=cbind(PRAberta,ProbRuina(RMPadronizadaAberta[[i]]))
  TempoAteRuinaAberta=cbind(TempoAteRuinaAberta,length(PRAberta[,i])-length(PRAberta[,i][PRAberta[,i]>0])+1)
  TMAberta=cbind(TMAberta,TamRuina(RMPadronizadaAberta[[i]]))        ##Tamanho médio do déficit dado que houve ruína a cada tempo
  AliquotaRDAberta=cbind(AliquotaRDAberta,ARDp(RMAberta[[i]],SalarioContribuicaoTotal[[i]],premissas$i,tempo,rodadas,p))
}
TempoRuinaAberta=array(data = 0, dim = c(length(N),rodadas))
for (i in 1: length(N)) for ( k in 1:rodadas) TempoRuinaAberta[i,k]=length(RMPadronizadaAberta[[i]][,k])-length(RMPadronizadaAberta[[i]][,k][RMPadronizadaAberta[[i]][,k]<0])+1
TempoRuinaAberta[TempoRuinaAberta==76]=NA

p=0.90  
VaRFundoAberta=CVaRFundoAberta=TVaRFundoAberta=ESFundoAberta=matrix(data=0,ncol=length(RMPadronizadaAberta), nrow=dim(RMPadronizadaAberta[[1]])[1]) 
for (i in 1:length(RMPadronizadaAberta)) for (t in 1:tempo){
   Perda=-RMPadronizadaAberta[[i]];Perda[RMPadronizadaAberta[[i]]>0]=0
   VaRFundoAberta[t,i]=quantile(Perda[t,],p) ##Fórmula 5.35 do Kaas
   ESFundoAberta[t,i]=sum(Perda[t,][Perda[t,]>VaRFundoAberta[t,i]])/rodadas
   TVaRFundoAberta[t,i]=VaRFundoAberta[t,i]+ESFundoAberta[t,i]/(1-p) ##Fórmula 5.42 Kaas
  CVaRFundoAberta[t,i]=(mean(Perda[t,][Perda[t,]>VaRFundoAberta[t,i]]))   ##Equação 5.44 Kaas
}



##################### DIFERENÇA EM RELAÇÃO À RESERVA PROSPECTIVA

VarDifAberta=dpDiferencaAberta=matrix(data = NA, nrow = tempo, ncol = length(N))
for (i in 1:length(N))  for(t in 1:tempo){
      VarDifAberta[t,i]=var(DifPadronizadaAberta[[i]][t,])
      dpDiferencaAberta[t,i]=sd(DifPadronizadaAberta[[i]][t,])
}

PRDifAberta=ProbRuina(DifPadronizadaAberta[[1]])
TMDifAberta=TamRuina(DifPadronizadaAberta[[1]])
TempoAteRuinaDifAberta=length(PRDifAberta)-length(PRDifAberta[PRDifAberta>0])+1
p=0    ##Aceita probabildiade p de déficit
AliquotaRDDifAberta=ARDp(DiferencaReservaAberta[[1]],SalarioContribuicaoTotal[[1]],premissas$i,tempo,rodadas,p)
for (i in 2:length(N)){
  PRDifAberta=cbind(PRDifAberta,ProbRuina(DifPadronizadaAberta[[i]]))
  TempoAteRuinaDifAberta=cbind(TempoAteRuinaDifAberta,length(PRDifAberta[,i])-length(PRDifAberta[,i][PRDifAberta[,i]>0])+1)
  TMDifAberta=cbind(TMDifAberta,TamRuina(DifPadronizadaAberta[[i]]))        ##Tamanho médio do déficit dado que houve ruína a cada tempo
  AliquotaRDDifAberta=cbind(AliquotaRDDifAberta,ARDp(DiferencaReservaAberta[[i]],SalarioContribuicaoTotal[[i]],premissas$i,tempo,rodadas,p))
}
###Distribuição do tempo até a ruína
TempoRuinaDifAberta=array(data = 0, dim = c(length(N),rodadas))
for (i in 1: length(N)) for ( k in 1:rodadas) TempoRuinaDifAberta[i,k]=length(DifPadronizadaAberta[[i]][,k])-length(DifPadronizadaAberta[[i]][,k][DifPadronizadaAberta[[i]][,k]<0])+1
TempoRuinaDifAberta[TempoRuinaDifAberta==76]=NA


###Medidas de Solvência   pop aberta
p=0.90  
VaRDifAberta=CVaRDifAberta=TVaRDifAberta=ESDifAberta=matrix(data=0,ncol=length(DifPadronizadaAberta), nrow=dim(DifPadronizadaAberta[[1]])[1]) 
for (i in 1:length(DifPadronizadaAberta)) for (t in 1:tempo){
   Perda=-DifPadronizadaAberta[[i]];Perda[DifPadronizadaAberta[[i]]>0]=0
   VaRDifAberta[t,i]=quantile(Perda[t,],p) ##Fórmula 5.35 do Kaas
   ESDifAberta[t,i]=sum(Perda[t,][Perda[t,]>VaRDifAberta[t,i]])/rodadas
   TVaRDifAberta[t,i]=VaRDifAberta[t,i]+ESDifAberta[t,i]/(1-p) ##Fórmula 5.42 Kaas
  CVaRDifAberta[t,i]=(mean(Perda[t,][Perda[t,]>VaRDifAberta[t,i]]))   ##Equação 5.44 Kaas
}


###Tempo médio até ruína
TempoMedioFundoFechada=TempoMedioFundoAberta=TempoMedioDifFechada=TempoMedioDifAberta=vector(length=length(N))
for (i in 1:length(N)){ 
 TempoMedioFundoFechada[i]=mean(na.omit(TempoRuina[i,]))
 TempoMedioDifFechada[i]=mean(na.omit(TempoRuinaDifFechada[i,]))
 TempoMedioFundoAberta[i]=mean(na.omit(TempoRuinaAberta[i,]))
 TempoMedioDifAberta[i]=mean(na.omit(TempoRuinaDifAberta[i,]))
}

##Salva resultados
save(list = ls(all = TRUE), file = "SimulaçõesAberta.RData")




##RESULTADOS
aliquota
#Fechada
##Fundo
c(dpRMPad[75,],TempoAteRuina,TempoMedioFundoFechada,AliquotaRD,CVaR[75,])

#Diferença
c(dpDifPad[75,],TempoAteRuina2,TempoMedioDifFechada,AliquotaRD2,CVaRDifFechada[75,])

#Aberta
##Fundo
c(dpRMPadAberta[75,],TempoAteRuinaAberta,TempoMedioFundoAberta,AliquotaRDAberta,CVaRFundoAberta[75,])

##Diferença
c(dpDiferencaAberta[75,],TempoAteRuinaDifAberta,TempoMedioDifAberta,AliquotaRDDifAberta,CVaRDifAberta[75,])






