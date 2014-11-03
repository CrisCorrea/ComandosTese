GeraPopEntrada=function (N) {
##Gera população que entrou e seus atributos de sexo, idade e salário

##########Gera sexo
Mulheres=round(N*.64, digits = 0)     ##64% des servidores são mulheres. Os demais, são homens.
Homens=N-Mulheres
Sexo=c(rep(1,Mulheres),rep(2,Homens))

##Gera idade aleatória de uma distribuição normal truncada em 18

#Pela Weibull
IdadeM=trunc(rweibull(Mulheres,scale=14.287559,shape=1.461185))+18
IdadeH=trunc(rweibull(Homens,scale=15.881910,shape=1.397349))+18
IdadeM[IdadeM>60]=30  ##Se idade estimada for maior que 60, adota idade média de entrada feminina = 30 anos.  
IdadeH[IdadeH>60]=32  ##Se idade estimada for maior que 60, adota idade média de entrada homem = 32 anos. 
#Pela normal
#IdadeM=trunc(rtruncnorm(Mulheres, a=18, b=69, mean = 24.7, sd = 11.9))
#IdadeH=trunc(rtruncnorm(Homens, a=18, b=69, mean = 23.1 , sd = 14.9))
x=c(IdadeM,IdadeH)

##########GERA REMUNERAÇÃO com mesma função de salários da pop inicial
aleatorioH=rnorm(Homens,mean =0,sd=0.5960748)
aleatorioM=rnorm(Mulheres,mean =0,sd=0.5960748)
RemH=round(exp(5.265e+00 + 9.823e-02*IdadeH-1.868e-03*IdadeH^2+1.194e-05*IdadeH^3+aleatorioH), digits = 2)
RemM=round(exp(5.265e+00 + 9.823e-02*IdadeM-1.868e-03*IdadeM^2+1.194e-05*IdadeM^3-3.084e-02+aleatorioM ), digits = 2)
RemH[RemH<545]=545
RemM[RemM<545]=545
RemH[RemH>26723.13]=26723.13
RemM[RemM>26723.13]=26723.13
Salário=c(RemM,RemH)

Pop=as.data.frame(cbind(x,Sexo, Salário))
colnames (Pop)= c('x','Sexo','Salário')
return(Pop)
}
