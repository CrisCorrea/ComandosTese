ARDp=function(Matriz, SC, i, tempo,rodadas,p){
##Calcula a alíquota de risco demográfico
##Matriz = Matriz de Reserva Matemática a cada tempo em cada rodada
##VPCF=Valor presente dos salários de contribuição de todos os servidores (valor médio nas k rodadas)
##TP = tamanho da população
##i= taxa de rentabilidade
##p=probabilidade de não ter déficit
##t=tempo de análise

limite=0
if (mean(Matriz[tempo,]<0)>0) limite=quantile(Matriz[tempo,][Matriz[tempo,]<0],1-p)   ##Se existem valores abaixo de 0, calcula limite
DE=sum(Matriz[tempo,][Matriz[tempo,]<limite])/rodadas       ###DE = tamanho médio da ruína 
VPDEF=DE*(1/(1+i)^(tempo))  ##Valor presente do déficit esperado futuro

v=1/(1+i)^seq(0,tempo-1)
VPCF=vector(length=rodadas)
for (k in 1:rodadas){
    VPCF[k]=sum(SC[,k]*v)
}
mediaVPCF=mean(VPCF)


aliquotaRD=-VPDEF/mediaVPCF
aliquotaRD

return(aliquotaRD)
}







