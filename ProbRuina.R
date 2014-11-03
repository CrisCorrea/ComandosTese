ProbRuina=function(RM){
##Calcula a probabilidade de ruína para uma matriz de reservas matemáticas com k simulações

tempo=dim(RM)[1]
P.Ruina=vector(length=tempo)

for (t in 1:tempo) P.Ruina[t]=mean(RM[t,]<0)   ###Diz quantas vezes foi menor que 0 em frequencia e atribui a  P.Ruina

return(P.Ruina)
}


TamRuina=function(RM){
##Calcula o tamanho médio da ruína dado que houve ruina para uma matriz de reservas matemáticas com k simulações

tempo=dim(RM)[1]
Tam.Ruina=rep(0,tempo)

for (t in 1:tempo) if (sum(RM[t,]<0)>0) Tam.Ruina[t]=-mean(RM[t,][RM[t,]<0])

return(Tam.Ruina)
}







