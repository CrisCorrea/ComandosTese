GeraPopEntrada=function (N) {
##Gera popula��o que entrou e seus atributos de sexo, idade e sal�rio

##########Gera sexo
Mulheres=round(N*.64, digits = 0)     ##64% des servidores s�o mulheres. Os demais, s�o homens.
Homens=N-Mulheres
Sexo=c(rep(1,Mulheres),rep(2,Homens))

##Gera idade aleat�ria de uma distribui��o normal truncada em 18

#Pela Weibull
IdadeM=trunc(rweibull(Mulheres,scale=14.287559,shape=1.461185))+18
IdadeH=trunc(rweibull(Homens,scale=15.881910,shape=1.397349))+18
IdadeM[IdadeM>60]=30  ##Se idade estimada for maior que 60, adota idade m�dia de entrada feminina = 30 anos.  
IdadeH[IdadeH>60]=32  ##Se idade estimada for maior que 60, adota idade m�dia de entrada homem = 32 anos. 
#Pela normal
#IdadeM=trunc(rtruncnorm(Mulheres, a=18, b=69, mean = 24.7, sd = 11.9))
#IdadeH=trunc(rtruncnorm(Homens, a=18, b=69, mean = 23.1 , sd = 14.9))
x=c(IdadeM,IdadeH)

##########GERA REMUNERA��O com mesma fun��o de sal�rios da pop inicial
aleatorioH=rnorm(Homens,mean =0,sd=0.5960748)
aleatorioM=rnorm(Mulheres,mean =0,sd=0.5960748)
RemH=round(exp(5.265e+00 + 9.823e-02*IdadeH-1.868e-03*IdadeH^2+1.194e-05*IdadeH^3+aleatorioH), digits = 2)
RemM=round(exp(5.265e+00 + 9.823e-02*IdadeM-1.868e-03*IdadeM^2+1.194e-05*IdadeM^3-3.084e-02+aleatorioM ), digits = 2)
RemH[RemH<545]=545
RemM[RemM<545]=545
RemH[RemH>26723.13]=26723.13
RemM[RemM>26723.13]=26723.13
Sal�rio=c(RemM,RemH)

Pop=as.data.frame(cbind(x,Sexo, Sal�rio))
colnames (Pop)= c('x','Sexo','Sal�rio')
return(Pop)
}
