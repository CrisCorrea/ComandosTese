ARDp=function(Matriz, SC, i, tempo,rodadas,p){
##Calcula a al�quota de risco demogr�fico
##Matriz = Matriz de Reserva Matem�tica a cada tempo em cada rodada
##VPCF=Valor presente dos sal�rios de contribui��o de todos os servidores (valor m�dio nas k rodadas)
##TP = tamanho da popula��o
##i= taxa de rentabilidade
##p=probabilidade de n�o ter d�ficit
##t=tempo de an�lise

limite=0
if (mean(Matriz[tempo,]<0)>0) limite=quantile(Matriz[tempo,][Matriz[tempo,]<0],1-p)   ##Se existem valores abaixo de 0, calcula limite
DE=sum(Matriz[tempo,][Matriz[tempo,]<limite])/rodadas       ###DE = tamanho m�dio da ru�na 
VPDEF=DE*(1/(1+i)^(tempo))  ##Valor presente do d�ficit esperado futuro

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







