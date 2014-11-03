GraficoPercentilPagamentos=function(RM,P1,ymin, ymax, titulo, eixoY, TipoLegenda){
###Gráficos por percentil
tempo=dim(RM)[1]
P1a=P1b=Media=Minimo=Maximo=vector(length=tempo)
for (t in 1:tempo) {
    P1a[t]=quantile(RM[t,],(1-P1)/2)
    P1b[t]=quantile(RM[t,],P1+(1-P1)/2)
    Media[t]=mean(RM[t,])
    Maximo[t]=max(RM[t,])
    Minimo[t]=min(RM[t,])
}

plot(Media, type="l", ylim=c(ymin,ymax), xlab="Tempo", ylab=eixoY,lwd=2,main=titulo,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
lines (P1a, type="l",lty=2, col=4,lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines (P1b, type="l",lty=2, col=4,lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines (Maximo, type="l", lty=3, col="orange", lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines (Minimo, type="l", lty=3, col="orange", lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
legend (TipoLegenda,lty = c(1,2,3),lwd=2,col=c(1,4,"orange"),c("Média",paste0("IC ", P1,"%"), "Extremos"),bty="n",cex=1.5)
abline(h=0)
}



GraficoPercentil=function(RM, ymin, ymax, titulo, eixoY, xlegenda, ylegenda){
###Gráficos por percentil
tempo=dim(RM)[1]
P10=P90=P01=P99=P25=P75=Media=Minimo=Maximo=vector(length=tempo)
for (t in 1:tempo) {
    P10[t]=quantile(RM[t,],.05)
    P90[t]=quantile(RM[t,],.95)
    P01[t]=quantile(RM[t,],.005)
    P99[t]=quantile(RM[t,],.995)
    Media[t]=mean(RM[t,],.50)
    Maximo[t]=max(RM[t,])
    Minimo[t]=min(RM[t,])
}

plot(Media, type="l", ylim=c(ymin,ymax), xlab="Tempo",
 ylab=eixoY,lwd=2,main=titulo)
lines (P01, type="l",lty=2, col=4,lwd=2)
lines (P99, type="l",lty=2, col=4,lwd=2)
lines (P10, type="l",lty=4, col=2,lwd=2)
lines (P90, type="l",lty=4, col=2,lwd=2)
lines (Maximo, type="l", lty=3, col=3, lwd=1)
lines (Minimo, type="l", lty=3, col=3, lwd=1)
legend (x=xlegenda, y =ylegenda,lty = c(1,2,4,3),lwd=2,col = c(1,4,2,3),c( "Média","IC 90%", "IC 99%", "Extremos"))
abline(h=0)
}










