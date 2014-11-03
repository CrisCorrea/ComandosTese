#####Estimação da reserva matemática prospectiva.
ReservaProspectiva=function(DadosServidores,SC, B, juros, aliquota,Tfuturo){

##Tfuturo=200  ##Reserva para os próximos 200 anos
TE=DadosServidores[4:(rodadas+3)] ###Tempo de entrada de cada indivíduo na população

R=array(data = 0,  dim=c(Tfuturo,rodadas))
#Reserva Matemática Prospectiva.
R[Tfuturo,]=B[Tfuturo,]-SC[Tfuturo,]*aliquota
for (t in (Tfuturo-1):1) R[t,]=R[t+1,]*(1/(1+juros))+B[t,]-SC[t,]*aliquota

return(R)
}









