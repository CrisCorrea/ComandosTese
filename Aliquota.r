Aliquota=function(SC,B,juros){

tempo=dim(B)[[1]]
rodadas=dim(B)[2]
####Valor presente das contribuições e dos benefícios
v=(1/(1+juros))^seq(0,tempo-1)
VPCF=VPBF=vector(length=rodadas)
for (k in 1:rodadas){
    VPBF[k]=sum(B[,k]*v)
    VPCF[k]=sum(SC[,k]*v)
}
aliquota=mean(VPBF/VPCF)

###Alíquota média = alíquota ideal=alíquota média em k simulações
#Pressuposto: alíquota média mantém equilíbrio atuarial do plano considerando a compensação previdenciária, pois assume que contribuições futuras são capazes de arcar com benefícios futuros já descontada a compensação previdenciária.
png(paste("Distribuição das Alíquotas - ",N[i],"servidores.png")) 
boxplot(VPBF/VPCF, ylab="Valor da Alíquota",cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
dev.off()
png(paste("Histograma Distribuição das Alíquotas - ",N[i],"servidores.png")) 
hist(VPBF/VPCF, xlab="Valor da Alíquota", ylab="Frequência", main=paste(N[i],"servidores"),cex.main=1.5, cex.lab=1.5, cex.axis=1.5,col='orange', xlim=c(0.20,0.35), ylim=c(0,300))
dev.off()

print("Alíquota de contribuição")
print(aliquota)

print("desvio padrão")
print(sd(VPBF/VPCF))
print(paste("mínima", min((VPBF/VPCF))))
print(paste("máxima", max((VPBF/VPCF))))

return(aliquota)
}      



