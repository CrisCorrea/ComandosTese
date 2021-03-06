
FatorPrevidenciario=function(TabuaFP,Sexo,y,r){
#Calcula fotor previdenci�rio para o indiv�duo. 
ex=exn(TabuaFP,x=r,type="curtate")
if (Sexo==1) f=(r-y+5)*0.31/ex*(1+(r+(r-y+5)*0.31)/100)
if (Sexo==2) f=(r-y)*0.31/ex*(1+(r+(r-y)*0.31)/100)

return(f)
} 


BIntegral=function(y,x,r,sx,ss) {
##Benef�cio integral = m�dia dos 80% maiores sal�rios
BInt=sx*((1+ss)^(r-x)-(1+ss)^(0.2*r+0.8*y-x))/(0.8*(r-y)*ss)
return(BInt)
}


BeneficioIdadeR=function(DadosServidores,y,r,ss) {
##Br � o valor que receberia por aposentadoria programada. Depende do tipo de aposentadoria. 

Sexo=DadosServidores$Sexo
x=DadosServidores$x    ##Considerar tempo em anos 
BrInt=BIntegral(y,x,r,DadosServidores$Sal�rio,ss)

Br=y  ##dimen��es: pop e rodadas
#Se aposentadoria por idade
Br[Sexo==1 & r==60 & y>30]= BrInt[Sexo==1 & r==60 & y>30]*(r[Sexo==1 & r==60 & y>30]-y[Sexo==1 & r==60 & y>30])/30
Br[Sexo==2 & r==65 & y>30]=BrInt[Sexo==2 & r==65 & y>30]*(r[Sexo==2 & r==65 & y>30]-y[Sexo==2 & r==65 & y>30])/35
Br[(Sexo==1 & r==60 & y<=30)]=BrInt[(Sexo==1 & r==60 & y<=30)]
Br[(Sexo==2 & r==65 & y<=30)]=BrInt[(Sexo==2 & r==65 & y<=30)]

#Se aposentadoria  por idade e tempo
Br[(Sexo==1 & r>=55 & y<=r-30)]= BrInt[(Sexo==1 & r>=55 & y<=r-30)]
Br[(Sexo==2 & r>=60 & y<=r-35)]= BrInt[(Sexo==2 & r>=60 & y<=r-35)]

#Se aposentadoria  compuls�ria
Br[(Sexo==1 & r==70 & y<=40)]= BrInt[(Sexo==1 & r==70 & y<=40)]
Br[(Sexo==1 & r==70 & y>40)]= BrInt[(Sexo==1 & r==70 & y>40)]*(r[(Sexo==1 & r==70 & y>40)]-y[(Sexo==1 & r==70 & y>40)])/30
Br[(Sexo==2 & r==70 & y<=35)]= BrInt[(Sexo==2 & r==70 & y<=35)]
Br[(Sexo==2 & r==70 & y>35)]= BrInt[Sexo==2 & r==70 & y>35]*(r[Sexo==2 & r==70 & y>35]-y[Sexo==2 & r==70 & y>35])/35

#matplot(r,Br)

return(Br)
}







Pagamentos=function (DadosServidores,REstados,TabuasMorte, ss, tempo, rodadas){
##Calcula benef�cio pelas regras do RPPS e pelas regras do RGPS. 
#Estima a compensa��o financeira. Calcula o benef�cio pago sem a compensa��o financeira.       

pop=dim(DadosServidores)[1]
x=DadosServidores$x    
Sexo=DadosServidores$Sexo
sx=DadosServidores$Sal�rio*12  ##Transforma sal�rio mensal em anual
TabuaFP=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.AS)
y=REstados[,,1]
r=REstados[,,2]
TempoAtivo=REstados[,,3]
MotivoSaiAtivo=REstados[,,4]
TempoBeneficiario1=REstados[,,5]
TempoBeneficiario2=REstados[,,6]
MotivoSaiBeneficiario1=REstados[,,7]                                         

Beneficios1RPPS=Beneficios2RPPS=Beneficios1RGPS=Beneficios2RGPS=array(data = 0,dim=c(pop,rodadas))  ## Valor do benefc�cio pelas normas do RGPS
Br=12*BeneficioIdadeR(DadosServidores,y,r,ss)
for (k in 1:rodadas){
  for (j in 1:pop){
    #Benef�cio 1 RGPS: 
      ## Se � inv�lido, recebe benef�cio integral calculado � idade de invalidez
      ##Se � pensionista, benef�cio = integral, se ativo, ou �ltimo benef�cio de aposentadoria
      if (MotivoSaiAtivo[j,k]==2 |MotivoSaiAtivo[j,k]==4 | MotivoSaiAtivo[j,k]==5) Beneficios1RGPS[j,k]=BIntegral(y[j,k],x[j],r[j,k],sx[j],ss)
      ## Se se aposentou, benef�cio = m�dia 80% maiores remunera��o * fator previdenci�rio
      if (MotivoSaiAtivo[j,k]==3) Beneficios1RGPS[j,k]=BIntegral(y[j,k],x[j],r[j,k],sx[j],ss)*FatorPrevidenciario(TabuaFP,DadosServidores$Sexo[j],y[j,k],r[j,k])
    #Benef�cio 2 RGPS
      ##Se � pensionista, benef�cio = integral, se ativo, ou �ltimo benef�cio de aposentadoria
      if ((MotivoSaiBeneficiario1[j,k]==4 | MotivoSaiBeneficiario1[j,k]==5)) Beneficios2RGPS[j,k]=Beneficios1RGPS[j,k]
    #Benef�cio RPPS 1: 
      ## Se � inv�lido, recebe benef�cio integral calculado � idade de invalidez
      ##Se � pensionista, benef�cio = integral, se ativo, ou �ltimo benef�cio de aposentadoria
      if (MotivoSaiAtivo[j,k]==2) Beneficios1RPPS[j,k]=BIntegral(y[j,k],x[j],r[j,k],sx[j],ss)
      ## Se se aposentou, benef�cio = m�dia 80% maiores remunera��o * fator previdenci�rio
      if (MotivoSaiAtivo[j,k]==3) Beneficios1RPPS[j,k]=Br[j,k]
    #Benef�cio RPPS 2
      ##Se � pensionista, benef�cio = �ltimo sal�rio da ativa, se ativo, ou �ltimo benef�cio de aposentadoria
      if (MotivoSaiAtivo[j,k]==4 | MotivoSaiAtivo[j,k]==5) Beneficios1RPPS[j,k]= sx[j]*(1+ss)^TempoAtivo[j,k]
      if ((MotivoSaiBeneficiario1[j,k]==4 | MotivoSaiBeneficiario1[j,k]==5)) Beneficios2RPPS[j,k]=Beneficios1RPPS[j,k]
  }
}  

MatrizBeneficio1RPPS=MatrizBeneficio2RPPS=MatrizBeneficio1RGPS=MatrizBeneficio2RGPS=MatrizCompensacao=SalarioContribuicao=array(data = 0, dim = c(pop,tempo,rodadas))
MatrizBeneficioRPPSTotal=MatrizBeneficioRGPSTotal=MatrizCompensacaoTotal=SalarioContribuicaoTotal=array(data = 0, dim = c(tempo, rodadas))
a=t(matrix(rep((1+ss)^seq(0,tempo-1,1),pop),nrow=tempo))


for (k in 1:rodadas) {
  for (j in 1:pop) {
    if (TempoBeneficiario1[j,k]>0) for (t in (TempoAtivo[j,k]+1):(TempoAtivo[j,k]+TempoBeneficiario1[j,k])) if (t<=75) MatrizBeneficio1RPPS[j,t,k]=MatrizBeneficio1RGPS[j,t,k]=1
    if (TempoBeneficiario2[j,k]>0) for (t in (TempoAtivo[j,k]+TempoBeneficiario1[j,k]+1):(TempoAtivo[j,k]+TempoBeneficiario1[j,k]+TempoBeneficiario2[j,k])) if (t<=75) MatrizBeneficio2RPPS[j,t,k]=MatrizBeneficio2RGPS[j,t,k]=1
    for (t in 1:TempoAtivo[j,k]) SalarioContribuicao[j,t,k]=1
  }
  MatrizBeneficio1RGPS[,,k]=Beneficios1RGPS[,k]*MatrizBeneficio1RGPS[,,k]
  MatrizBeneficio2RGPS[,,k]=Beneficios2RGPS[,k]*MatrizBeneficio2RGPS[,,k]
  MatrizBeneficio1RPPS[,,k]=Beneficios1RPPS[,k]*MatrizBeneficio1RPPS[,,k]
  MatrizBeneficio2RPPS[,,k]=Beneficios2RPPS[,k]*MatrizBeneficio2RPPS[,,k]    
  SalarioContribuicao[,,k]=12*DadosServidores$Sal�rio*(a*SalarioContribuicao[,,k])
  for (t in 1:tempo) {
    MatrizBeneficioRGPSTotal[t,k]=sum(MatrizBeneficio1RGPS[,t,k])+sum(MatrizBeneficio2RGPS[,t,k])
    MatrizBeneficioRPPSTotal[t,k]=sum(MatrizBeneficio1RPPS[,t,k])+sum(MatrizBeneficio2RPPS[,t,k])
    for (j in 1:pop) MatrizCompensacao[j,t,k]=min(MatrizBeneficio1RGPS[j,t,k]+MatrizBeneficio2RGPS[j,t,k],MatrizBeneficio1RPPS[j,t,k]+MatrizBeneficio2RPPS[j,t,k])*(x[j]-y[j,k])/(r[j,k]-y[j,k])
    MatrizCompensacaoTotal[t,k]=sum(MatrizCompensacao[,t,k])
    SalarioContribuicaoTotal[t,k]=sum(SalarioContribuicao[,t,k])
  }
}
BeneficioPagoPlano=MatrizBeneficioRPPSTotal-MatrizCompensacaoTotal

##Gr�ficos
ymax=max(BeneficioPagoPlano,MatrizBeneficioRGPSTotal,MatrizBeneficioRPPSTotal,MatrizCompensacaoTotal)
png(paste("Benef�cios RGPS - ", pop,"Servidores.png"))
matplot(MatrizBeneficioRGPSTotal/1000000, type = "l",ylab="R$ Milh�es", main="Beneficios pela regra do RGPS", xlab="Tempo" , ylim=c(0,ymax/1000000),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
dev.off()
png(paste("Benef�cios RPPS - ", pop,"Servidores.png"))
matplot(MatrizBeneficioRPPSTotal/1000000, type = "l",ylab="R$ Milh�es", main="Beneficios pela regra do RPPS", xlab="Tempo" , ylim=c(0,ymax/1000000),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
dev.off()
png(paste("Compensa��o Financeira - ", pop,"Servidores.png"))
matplot(MatrizCompensacaoTotal/1000000, type = "l",ylab="R$ Milh�es", main="Compensa��o Financeira", xlab="Tempo" , ylim=c(0,ymax/1000000),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
dev.off()
png(paste("Benef�cios pagos pelo plano ", pop,"Servidores.png"))
matplot(BeneficioPagoPlano/1000000, type = "l",ylab="R$ Milh�es", main="Beneficios pagos pelo plano", xlab="Tempo" , ylim=c(0,ymax/1000000),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
dev.off()
png(paste("Sal�rio de Contribui��o - ", pop, "Servidores.png"))
matplot(SalarioContribuicaoTotal, type = "l",ylab="R$ Milh�es", main="Sal�rio de Contribui��o Total", xlab="Tempo",cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
dev.off()

png(paste("Percentis Benef�cios RGPS - ", pop,"Servidores.png"))
GraficoPercentilPagamentos(MatrizBeneficioRGPSTotal/1000000, 0.75,0, ymax/1000000, titulo=paste("Benef�cios RGPS -", pop, "Servidores"), eixoY="R$ Milh�es",TipoLegenda="topleft")
dev.off()
png(paste("Percentis Benef�cios RPPS - ", pop,"Servidores.png"))
GraficoPercentilPagamentos(MatrizBeneficioRPPSTotal/1000000, 0.75, 0, ymax/1000000, titulo=paste("Benef�cios RPPS -", pop, "Servidores"), eixoY="R$ Milh�es",TipoLegenda="topleft")
dev.off()
png(paste("Percentis Compensa��o Financeira - ", pop,"Servidores.png"))
GraficoPercentilPagamentos(MatrizCompensacaoTotal/1000000,  0.75,0, ymax/1000000, titulo=paste("Compensa��o Financeira -", pop, "Servidores"), eixoY="R$ Milh�es",TipoLegenda="topleft")
dev.off()
png(paste("Percentis Benef�cios pagos pelo plano ", pop,"Servidores.png"))
GraficoPercentilPagamentos(BeneficioPagoPlano/1000000,  0.75,0, ymax/1000000, titulo=paste("Benef�cios Pagos -", pop, "Servidores"), eixoY="R$ Milh�es",TipoLegenda="topleft")
dev.off()
png(paste("Percentis Sal�rios de Contribui��o ", pop,"Servidores.png"))
GraficoPercentilPagamentos(SalarioContribuicaoTotal/1000000,  0.75,0, ymax=max(SalarioContribuicaoTotal)/1000000, titulo=paste("Sal�rio de Contribui��o -", pop, "Servidores"), eixoY="R$ Milh�es",TipoLegenda="topright")
dev.off()

return(list(BeneficioPagoPlano,SalarioContribuicaoTotal))
}








