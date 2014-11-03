SimulaComMesmaAliquota=function(DadosServidores1,DadosServidores2,DadosServidores3,premissas, TMD,TabuasMorte,ProbFamilia, rodadas, tempo){
###Faz microssimulação para reserva matemática para uma população de servidores



#### Estima estados dos indivíduos no tempo
##Lembrar que em t=0 todos são ativos. Em t=1 alguns já mudaram de estado.
#A simulação começa em t=1.
REstados=EstadoNoTempo(DadosServidores1, TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
Estados=REstados[[1]]
y=REstados[[2]]
IdadeAposenta=REstados[[3]]
#plot(Estados[1,,1])
#write.table(Estados[,,1], "C:\\Users\\Cris Corrêa\\Desktop\\Estados.xls", sep="\t") ##Exporta para arquivo excel




#GraficosNRodadas(Estados)

#Matriz de contribuições
##Estima contribuições considerando alíquota =100%
ContribuicaoMaxima=Contribuicao(Estados, DadosServidores1, premissas)
#matplot(ContribuicaoMaxima/1000, type = "l", ylab="Contribuições Máximas($ Mil)", xlab="Tempo")

### Matriz de benefícios
##Benefícios dos indivíduos com status de beneficiários em t.
BeneficioTotal=Beneficios(Estados,DadosServidores1,y,IdadeAposenta,premissas,TabuasMorte)
#matplot(BeneficioTotal/1000, type="l",ylab="Beneficios Pagos (Mil)", xlab="Tempo" )

####Valor presente das contribuições e dos benefícios
t=seq(0:tempo)
v=1/(1+premissas$i)^(t)
VPB=BeneficioTotal*v
VPC= ContribuicaoMaxima*v
VPBF=vector(length=rodadas)
VPCF=vector(length=rodadas)
for (k in 1:rodadas){
    VPBF[k]=sum(VPB[,k])
    VPCF[k]=sum(VPC[,k])
}

###Alíquota média = alíquota ideal=alíquota média em k simulações
#Pressuposto: alíquota média mantém equilíbrio atuarial do plano considerando a compensação previdenciária, pois assume que contribuições futuras são capazes de arcar com benefícios futuros já descontada a compensação previdenciária.
aliquota=mean(VPBF/VPCF)
print("Alíquota de contribuição")
print(aliquota)

###Valor das contribuições com a alíquota de contribuição definida como alíquota ideal
ContribuicaoTotal=ContribuicaoMaxima*aliquota
#matplot(ContribuicaoTotal/1000, type = "l", ylab="Contribuições ($ Mil)", xlab="Tempo")


#####Compara contribuições e benefícios
SaldoAno=ContribuicaoTotal-BeneficioTotal
#matplot(SaldoAno, type="l")
#abline(h=0)


#par(mfrow=c(1,3))
#matplot(ContribuicaoTotal/1000, type = "l", ylab="Contribuições ($ Mil)", xlab="Tempo meses)")
#abline(h=0)
#matplot(BeneficioTotal/1000, type = "l", ylab="Benefícios ($ Mil)", xlab="Tempo(meses)")
#abline(h=0)
#matplot(SaldoAno/1000, type = "l",ylab="Saldo no ano  ($ Mil)", xlab="Tempo(meses)" )
#abline(h=0)





####Reserva Matemática SEM rentabilidade
ReservaSemRentabilidade=ReservaMatematica(ContribuicaoTotal,BeneficioTotal,0)
#matplot(ReservaSemRentabilidade/1000, type="l", ylab="Reserva Sem Rentabilidade (Mil)", xlab="Tempo")
#abline(h=0)


####Reserva Matemática COM rentabilidade
ReservaComRentabilidade335=ReservaMatematica(ContribuicaoTotal,BeneficioTotal,premissas$i)
#matplot(ReservaComRentabilidade/1000, type="l",ylab="Reserva Com Rentabilidade (Mil)", xlab="Tempo")
#abline(h=0)


#par(mfrow=c(1,2))
#matplot(ReservaSemRentabilidade/1000, type="l", ylab="Reserva Sem Rentabilidade (Mil)", xlab="Tempo")
#abline(h=0)
#matplot(ReservaComRentabilidade/1000, type="l",ylab="Reserva Com Rentabilidade (Mil)", xlab="Tempo")
#abline(h=0)

#GraficosReserva(ReservaSemRentabilidade)
#GraficosReserva(ReservaComRentabilidade)

##Para demais tamanhos populacionais: 

##670 servidores
REstados=EstadoNoTempo(DadosServidores2, TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
Estados=REstados[[1]]
y=REstados[[2]]
IdadeAposenta=REstados[[3]]
ContribuicaoMaxima=Contribuicao(Estados, DadosServidores2, premissas)
ContribuicaoTotal=ContribuicaoMaxima*aliquota
BeneficioTotal=Beneficios(Estados,DadosServidores2,y,IdadeAposenta,premissas,TabuasMorte)

ReservaComRentabilidade670=ReservaMatematica(ContribuicaoTotal,BeneficioTotal,premissas$i)


##1005 servidores
REstados=EstadoNoTempo(DadosServidores3, TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
Estados=REstados[[1]]
y=REstados[[2]]
IdadeAposenta=REstados[[3]]
ContribuicaoMaxima=Contribuicao(Estados, DadosServidores3, premissas)
ContribuicaoTotal=ContribuicaoMaxima*aliquota
BeneficioTotal=Beneficios(Estados,DadosServidores3,y,IdadeAposenta,premissas,TabuasMorte)

ReservaComRentabilidade1005=ReservaMatematica(ContribuicaoTotal,BeneficioTotal,premissas$i)

return(list(ReservaComRentabilidade335,ReservaComRentabilidade670,ReservaComRentabilidade1005))

}













