#####Estima��o da reserva matem�tica prospectiva.
ReservaProspectiva=function(DadosServidores,SC, B, juros, aliquota,Tfuturo){

##Tfuturo=200  ##Reserva para os pr�ximos 200 anos
TE=DadosServidores[4:(rodadas+3)] ###Tempo de entrada de cada indiv�duo na popula��o

R=array(data = 0,  dim=c(Tfuturo,rodadas))
#Reserva Matem�tica Prospectiva.
R[Tfuturo,]=B[Tfuturo,]-SC[Tfuturo,]*aliquota
for (t in (Tfuturo-1):1) R[t,]=R[t+1,]*(1/(1+juros))+B[t,]-SC[t,]*aliquota

return(R)
}









