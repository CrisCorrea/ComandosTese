SimulaComMesmaAliquota=function(DadosServidores1,DadosServidores2,DadosServidores3,premissas, TMD,TabuasMorte,ProbFamilia, rodadas, tempo){
###Faz microssimula��o para reserva matem�tica para uma popula��o de servidores



#### Estima estados dos indiv�duos no tempo
##Lembrar que em t=0 todos s�o ativos. Em t=1 alguns j� mudaram de estado.
#A simula��o come�a em t=1.
REstados=EstadoNoTempo(DadosServidores1, TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
Estados=REstados[[1]]
y=REstados[[2]]
IdadeAposenta=REstados[[3]]
#plot(Estados[1,,1])
#write.table(Estados[,,1], "C:\\Users\\Cris Corr�a\\Desktop\\Estados.xls", sep="\t") ##Exporta para arquivo excel




#GraficosNRodadas(Estados)

#Matriz de contribui��es
##Estima contribui��es considerando al�quota =100%
ContribuicaoMaxima=Contribuicao(Estados, DadosServidores1, premissas)
#matplot(ContribuicaoMaxima/1000, type = "l", ylab="Contribui��es M�ximas($ Mil)", xlab="Tempo")

### Matriz de benef�cios
##Benef�cios dos indiv�duos com status de benefici�rios em t.
BeneficioTotal=Beneficios(Estados,DadosServidores1,y,IdadeAposenta,premissas,TabuasMorte)
#matplot(BeneficioTotal/1000, type="l",ylab="Beneficios Pagos (Mil)", xlab="Tempo" )

####Valor presente das contribui��es e dos benef�cios
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

###Al�quota m�dia = al�quota ideal=al�quota m�dia em k simula��es
#Pressuposto: al�quota m�dia mant�m equil�brio atuarial do plano considerando a compensa��o previdenci�ria, pois assume que contribui��es futuras s�o capazes de arcar com benef�cios futuros j� descontada a compensa��o previdenci�ria.
aliquota=mean(VPBF/VPCF)
print("Al�quota de contribui��o")
print(aliquota)

###Valor das contribui��es com a al�quota de contribui��o definida como al�quota ideal
ContribuicaoTotal=ContribuicaoMaxima*aliquota
#matplot(ContribuicaoTotal/1000, type = "l", ylab="Contribui��es ($ Mil)", xlab="Tempo")


#####Compara contribui��es e benef�cios
SaldoAno=ContribuicaoTotal-BeneficioTotal
#matplot(SaldoAno, type="l")
#abline(h=0)


#par(mfrow=c(1,3))
#matplot(ContribuicaoTotal/1000, type = "l", ylab="Contribui��es ($ Mil)", xlab="Tempo meses)")
#abline(h=0)
#matplot(BeneficioTotal/1000, type = "l", ylab="Benef�cios ($ Mil)", xlab="Tempo(meses)")
#abline(h=0)
#matplot(SaldoAno/1000, type = "l",ylab="Saldo no ano  ($ Mil)", xlab="Tempo(meses)" )
#abline(h=0)





####Reserva Matem�tica SEM rentabilidade
ReservaSemRentabilidade=ReservaMatematica(ContribuicaoTotal,BeneficioTotal,0)
#matplot(ReservaSemRentabilidade/1000, type="l", ylab="Reserva Sem Rentabilidade (Mil)", xlab="Tempo")
#abline(h=0)


####Reserva Matem�tica COM rentabilidade
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













