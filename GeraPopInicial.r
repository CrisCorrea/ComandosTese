GeraPopInicial=function (N){
##Gera população inicial e seus atributos de sexo, idade e salário

pop=matrix(data = NA, nrow = N, ncol = 3)
colnames (pop)= c('x','Sexo','Salário')

##########Gera sexo
Mulheres=round(N*.64, digits = 0)     ##64% des servidores são mulheres. Os demais, são homens.
Homens=N-Mulheres
Sexo=c(rep(1,Mulheres),rep(2,Homens))

########Gera Idade dado sexo
#gera frequencia de idades
idade=seq(18,70)
freqIdadeM=-0.1259+0.01044*idade-0.0002137*(idade^2)+0.000001285*(idade^3)
freqIdadeH=-0.06639+0.005796*idade-0.0001058*idade^2+0.0000005147*idade^3
freqIdadeM=freqIdadeM+0.00255
freqIdadeH=freqIdadeH+0.00255
somaM=sum(freqIdadeM)
somaH=sum(freqIdadeH)
freqIdadeM=freqIdadeM/somaM
freqIdadeH=freqIdadeH/somaH
#plot(idade,freqIdadeM)
#plot(idade,freqIdadeH)

#Número de pessoas a cada idade e sexo
NIdadeH=NIdadeM=vector(length=(70-17))
NIdadeH[1]= floor(freqIdadeH[1]*Homens)
NIdadeM[1]= floor(freqIdadeM[1]*Mulheres)
for (i in 2:(70-17)) {
  NIdadeH[i]=floor(sum(freqIdadeH[1:i])*Homens)-floor(sum(freqIdadeH[1:(i-1)])*Homens)
  NIdadeM[i]=floor(sum(freqIdadeM[1:i])*Mulheres)-floor(sum(freqIdadeM[1:(i-1)])*Mulheres)
}
##Garante que foram distribuidos servidores por sexo para todos os campos reservados
if (sum(NIdadeH)<Homens)  NIdadeH[(39-17)]=NIdadeH[(39-17)]+Homens-sum(NIdadeH)     ##Se faltou alguém, atribui à idade mediana
if (sum(NIdadeM)<Mulheres)  NIdadeM[(39-17)]=NIdadeM[(39-17)]+Mulheres-sum(NIdadeM)

xM=rep(18,NIdadeM[[1]])
xH=rep(18,NIdadeH[[1]])
for (i in 19:70){
  xM=c(xM,rep(i,NIdadeM[[i-17]]))
  xH=c(xH,rep(i,NIdadeH[[i-17]]))
}
x=c(xM,xH)



##########GERA REMUNERAÇÃO
aleatorioH=rnorm(Homens,mean =0,sd=0.5960748)
aleatorioM=rnorm(Mulheres,mean =0,sd=0.5960748)
RemH=round(exp(5.265e+00 + 9.823e-02*xH-1.868e-03*xH^2+1.194e-05*xH^3+aleatorioH), digits = 2)
RemM=round(exp(5.265e+00 + 9.823e-02*xM-1.868e-03*xM^2+1.194e-05*xM^3-3.084e-02+aleatorioM ), digits = 2)
RemH[RemH<545]=545
RemM[RemM<545]=545
RemH[RemH>26723.13]=26723.13
RemM[RemM>26723.13]=26723.13

Salário=c(RemM,RemH)
pop=as.data.frame(cbind(x,Sexo, Salário))

return(pop)
}


