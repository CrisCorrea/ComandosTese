TempoAteSaida=function(idade, sexo, TabuaFeminina, TabuaMasculina, rodadas){
##Estima tempo até saída a aprtir de tábua de vida

Tempo=sexo
###Calcula tempo até a saída pelas tábuas definidas.
for (k in 1:rodadas){
  for (i in 1:dim(sexo)[1]){
    Tempo[i,k][sexo[i,k]==1]=rLife(n=1,object=TabuaFeminina,x=idade[i,k],type="Kx")     ##Tipe=Kx - tempo discreto (Tx=Kx+0.5)
    Tempo[i,k][sexo[i,k]==2]=rLife(n=1,object=TabuaMasculina,x=idade[i,k],type="Kx")    ##Não é possível atribuir vetores de idade. 
  }  
}
return (Tempo)
}









