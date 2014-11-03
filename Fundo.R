ReservaMatematica=function(SC,B,juros,aliquota){
#Reserva sem ajuste para meio do período

tempo = dim(SC)[1]
rodadas=dim(SC)[2]

R=array(data = 0,  dim=c(tempo,rodadas))
#Reserva Matemática em t=0 é 0.
R[1,]=SC[1,]*aliquota-B[1,]
for (t in 2:tempo) R[t,]=R[t-1,]*(1+juros)+SC[t,]*aliquota-B[t,]

return(R)
}




