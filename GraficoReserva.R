GraficosReserva <- function (Matriz){ #faz gráficos para várias rodadas


tempo = dim(Matriz)[1]
rodadas=dim(Matriz)[2]

MatrizMedia=array(data = NA,  dim=c(tempo))  ## Vetor de média de cada status no tempo
for (t in 1:tempo){
        MatrizMedia[t]=mean(Matriz[t,])
}

yMax= max(Matriz)
yMin= min(Matriz)



###Gráficos dos valores observados
par(mfrow=c(1,1))

plot(Matriz[,1]/1000, type="l", xlab="Tempo", ylab=("Reserva Matemática (R$ Mil)"), ylim=c(yMin/1000,yMax/1000))
for (i in 2:rodadas){
    lines(Matriz[,i]/1000)
}
lines(MatrizMedia/1000, col="red")
abline(h=0)


}


