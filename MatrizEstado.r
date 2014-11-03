MatrizEstado=function(TempoAtivo,MotivoSaiAtivo, TempoBeneficiario1,MotivoSaiBeneficiario1,TempoBeneficiario2, pop, rodadas){

MatrizEstado=array(data = 1,  dim=c(pop, tempo+1, rodadas))        ###Atribui estado do tempo t=0 até o tempo t. Portanto, tem t+1 colunas
for(k in 1:rodadas){   ##Repete para cada rodada
      for (j in 1:pop){
          for (t in (TempoAtivo[j]+1):(tempo+1)){
            if (t>TempoAtivo[j]+TempoBeneficiario1[j]+TempoBeneficiario2[j]+1) MatrizEstado[j,t,k]=6
            if (t<=TempoAtivo[j]+TempoBeneficiario1[j]+TempoBeneficiario2[j]+1) MatrizEstado[j,t,k]=MotivoSaiBeneficiario1[j]
            if (t<=TempoAtivo[j]+TempoBeneficiario1[j]+1) MatrizEstado[j,t,k]=MotivoSaiAtivo[j]
          }
      }
}

GraficosNRodadas(MatrizEstado)
return( MatrizEstado)
}







