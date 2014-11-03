Aliquota=function(SC,B,juros){

tempo=dim(B)[[1]]
rodadas=dim(B)[2]
####Valor presente das contribui��es e dos benef�cios
v=(1/(1+juros))^seq(0,tempo-1)
VPCF=VPBF=vector(length=rodadas)
for (k in 1:rodadas){
    VPBF[k]=sum(B[,k]*v)
    VPCF[k]=sum(SC[,k]*v)
}
aliquota=mean(VPBF/VPCF)

###Al�quota m�dia = al�quota ideal=al�quota m�dia em k simula��es
#Pressuposto: al�quota m�dia mant�m equil�brio atuarial do plano considerando a compensa��o previdenci�ria, pois assume que contribui��es futuras s�o capazes de arcar com benef�cios futuros j� descontada a compensa��o previdenci�ria.
png(paste("Distribui��o das Al�quotas - ",N[i],"servidores.png")) 
boxplot(VPBF/VPCF, ylab="Valor da Al�quota",cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
dev.off()
png(paste("Histograma Distribui��o das Al�quotas - ",N[i],"servidores.png")) 
hist(VPBF/VPCF, xlab="Valor da Al�quota", ylab="Frequ�ncia", main=paste(N[i],"servidores"),cex.main=1.5, cex.lab=1.5, cex.axis=1.5,col='orange', xlim=c(0.20,0.35), ylim=c(0,300))
dev.off()

print("Al�quota de contribui��o")
print(aliquota)

print("desvio padr�o")
print(sd(VPBF/VPCF))
print(paste("m�nima", min((VPBF/VPCF))))
print(paste("m�xima", max((VPBF/VPCF))))

return(aliquota)
}      



