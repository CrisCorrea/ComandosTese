TempoAteSaida=function(idade, sexo, TabuaFeminina, TabuaMasculina, rodadas){
##Estima tempo at� sa�da a aprtir de t�bua de vida

Tempo=sexo
###Calcula tempo at� a sa�da pelas t�buas definidas.
for (k in 1:rodadas){
  for (i in 1:dim(sexo)[1]){
    Tempo[i,k][sexo[i,k]==1]=rLife(n=1,object=TabuaFeminina,x=idade[i,k],type="Kx")     ##Tipe=Kx - tempo discreto (Tx=Kx+0.5)
    Tempo[i,k][sexo[i,k]==2]=rLife(n=1,object=TabuaMasculina,x=idade[i,k],type="Kx")    ##N�o � poss�vel atribuir vetores de idade. 
  }  
}
return (Tempo)
}









