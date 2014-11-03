ConjugeEFilho=function (MotivoSaida,SexoQuemMorreu,IdadeQuemMorreu,ProbFamilia, rodadas) {
####Avalia se deixou cônjuge ou filho menor de 21 anos

##MotivoSaida = Motivo pelo qual saiu da situação anterior
##SexoQuemMorreu = Sexo de quem morreu, ou seja, sexo do servidor.
##IdadeQuemMorreu = idade do servidor quando morreu.
pop=dim(SexoQuemMorreu)[1]
aleatorio1=matrix(runif(pop*rodadas, min = 0, max = 1), ncol=rodadas)
aleatorio2=matrix(runif(pop*rodadas, min = 0, max = 1), ncol=rodadas)  #Não posso usar o mesmo número aleatório para avaliar probabilidade de ter cônjuge e de ter filho, porque um resultado enviesaria o outro. Pensei em usar 1-aleatório, mas os resultados também foram enviesados, de forma que todos os servidores deixavam algum beneficiário. Então, achei melhor fazer dois sorteios diferentes, um para filhos e outro para cônjuges.
IdadeBeneficiario=IdadeQuemMorreu
SexoBeneficiario=SexoQuemMorreu

for (k in 1:rodadas){
  for (i in 1:pop){
    if (MotivoSaida[i,k]==7){##Se morreu
       if (SexoQuemMorreu[i]==1){ ##Se é mulher
           if (aleatorio1[i,k]<=ProbFamilia$P.TerConj.F[IdadeQuemMorreu[i,k]]) {
              MotivoSaida[i,k]=5 ##Morreu e deixou cônjuge
              IdadeBeneficiario[i,k]=floor(max(18,rnorm(1,mean=3.45+0.84*IdadeQuemMorreu[i,k]+3.61+0.07*IdadeQuemMorreu[i,k], sd =6.9))) ##Estima idade do cônjuge, com idade mínima de 18 anos, e arredonda para baixo, indicando idade completa
              SexoBeneficiario[i,k]=2
           }
           if (MotivoSaida[i,k]==7 & aleatorio2[i,k]<=ProbFamilia$P.TerFilho.F[IdadeQuemMorreu[i,k]]) {
              MotivoSaida[i,k]=4  ##Morreu e deixou filho menor de 21 anos
              IdadeBeneficiario[i,k]=floor(max(0,rnorm(1, mean =-9.314+0.443*IdadeQuemMorreu[i,k]+1.782, sd=4.773)))  ##Estima idade do filho. Idade mínima=0.
              SexoBeneficiario[i,k]=1  ##Assume tabela de vida de menor mortalidade
              if (IdadeBeneficiario[i,k]>=21) MotivoSaida[i,k]=6 ##Se filho tem 21 anos ou mais, não é beneficiário.
           }
       }
       if (SexoQuemMorreu[i]==2){ ##Se é homem
           if (aleatorio1[i,k]<=ProbFamilia$P.TerConj.M[IdadeQuemMorreu[i,k]]) {
              MotivoSaida[i,k]=5 ##Morreu e deixou cônjuge
              IdadeBeneficiario[i,k]=floor(max(18,rnorm(1,mean=3.45+0.84*IdadeQuemMorreu[i,k], sd =6.9))) ##Estima idade do cônjuge, com idade mínima de 18 anos
              SexoBeneficiario[i,k]=1
           }
           if (MotivoSaida[i,k]==7 & aleatorio2[i,k]<=ProbFamilia$P.TerFilho.M[IdadeQuemMorreu[i,k]]) {
              MotivoSaida[i,k]=4  ##Morreu e deixou filho menor de 21 anos
              IdadeBeneficiario[i,k]=floor(max(0,rnorm(1, mean =-9.314+0.443*IdadeQuemMorreu[i,k], sd=4.773)))  ##Estima idade do filho. Idade mínima=0.
              if (IdadeBeneficiario[i,k]>=21) MotivoSaida[i,k]=6  ##Se filho tem 21 anos ou mais, não é beneficiário.
              SexoBeneficiario[i,k]=1  ##Assume tabela de vida de menor mortalidade
           }
       }
   }
  }
}
MotivoSaida[MotivoSaida==7]=6  ##Se não tem filho nem cônjuge, não deixou dependente
IdadeBeneficiario[IdadeBeneficiario>=110]=109    ##Ajustes idades dos beneficiários
IdadeBeneficiario[MotivoSaida==6]=109        ##Se não existe beneficiário, não existe idade do beneficiário


return(list(MotivoSaida,SexoBeneficiario,IdadeBeneficiario))

}





