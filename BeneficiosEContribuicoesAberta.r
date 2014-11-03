Pagamentos200anos=function (DadosServidores,Estados,TabuasMorte,ss,tempo,rodadas,Tfuturo){
##Calcula benefício e contribuições para pop aberta antes e após t=75
##Dados Servidores = população aberta


##Tfuturo=200
pop=dim(DadosServidores)[1]
x=DadosServidores$x    
Sexo=DadosServidores$Sexo
sx=DadosServidores$Salário*12  ##Transforma salário mensal em anual
TabuaFP=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.AS)
y=Estados[,,1]
r=Estados[,,2]
TempoAtivo=Estados[,,3]
MotivoSaiAtivo=Estados[,,4]
TempoBeneficiario1=Estados[,,5]
TempoBeneficiario2=Estados[,,6]
MotivoSaiBeneficiario1=Estados[,,7]
TE=DadosServidores[4:(rodadas+3)] ###Tempo de entrada de cada indivíduo na população
                                        
##Calcula valor do benefício
Br=12*BeneficioIdadeR(DadosServidores,y,r,ss)
rm(DadosServidores)
rm(Estados)

MatrizBeneficioRPPSTotal=SalarioContribuicaoTotal=array(data = 0, dim = c(Tfuturo, rodadas))

for (k in 1:rodadas){
  ##Calcula valor do benefício
  ValorBeneficios1RPPS=ValorBeneficios2RPPS=array(data = 0,dim=c(pop,rodadas))  ## Valor do benefcício
  MatrizBeneficio1RPPS=MatrizBeneficio2RPPS=ValorSalarioContribuicao=array(data = 0, dim = c(pop,Tfuturo))
  for (j in 1:pop){
    if (TE[j,k]<=tempo){
    #Benefício RPPS 1: 
      ## Se é inválido, recebe benefício integral calculado à idade de invalidez 
      if (MotivoSaiAtivo[j,k]==2) ValorBeneficios1RPPS[j,k]=BIntegral(y[j,k],x[j],r[j,k],sx[j],ss)
      ## Se se aposentou, benefício = 
      if (MotivoSaiAtivo[j,k]==3) ValorBeneficios1RPPS[j,k]=Br[j,k]
    #Benefício RPPS 2
      ##Se é pensionista, benefício = último salário da ativa, se ativo, ou último benefício de aposentadoria
      ##Se é pensionista, benefício = integral, se ativo, ou último benefício de aposentadoria
      if (MotivoSaiAtivo[j,k]==4 | MotivoSaiAtivo[j,k]==5) ValorBeneficios1RPPS[j,k]= sx[j]*(1+ss)^TempoAtivo[j,k]
      if ((MotivoSaiBeneficiario1[j,k]==4 | MotivoSaiBeneficiario1[j,k]==5)) ValorBeneficios2RPPS[j,k]=ValorBeneficios1RPPS[j,k]
  ##Define quando recebe o benefício    
       for (t in TE[j,k]:(TE[j,k]+TempoAtivo[j,k])) if (t<=Tfuturo) ValorSalarioContribuicao[j,t]=sx[j]*(1+ss)^(t-TE[j,k])
       if (TempoBeneficiario1[j,k]>0) for (t in (TE[j,k]+TempoAtivo[j,k]+1):(TE[j,k]+TempoAtivo[j,k]+TempoBeneficiario1[j,k])) if (t<=Tfuturo) MatrizBeneficio1RPPS[j,t]=ValorBeneficios1RPPS[j,k]
       if (TempoBeneficiario2[j,k]>0) for (t in (TE[j,k]+TempoAtivo[j,k]+TempoBeneficiario1[j,k]+1):(TE[j,k]+TempoAtivo[j,k]+TempoBeneficiario1[j,k]+TempoBeneficiario2[j,k])) if (t<=Tfuturo) MatrizBeneficio2RPPS[j,t]=ValorBeneficios2RPPS[j,k]
    }
  }
  ##Calcula benefício total
  for (t in 1:Tfuturo) {
    MatrizBeneficioRPPSTotal[t,k]=sum(MatrizBeneficio1RPPS[,t])+sum(MatrizBeneficio2RPPS[,t])
    SalarioContribuicaoTotal[t,k]=sum(ValorSalarioContribuicao[,t])
  }
}  

return(list(MatrizBeneficioRPPSTotal,SalarioContribuicaoTotal))
}




