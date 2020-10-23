#-----------------------------------------------------------------------------
# dados de postássio liberado em função do tempo

klib <- data.frame(k=c(51.03, 57.76, 26.60, 60.65, 87.07, 64.67,
                       91.28, 105.22, 72.74, 81.88, 97.62, 90.14,
                       89.88, 113.22, 90.91, 115.39, 112.63, 87.51,
                       104.69, 120.58, 114.32, 130.07, 117.65, 111.69,
                       128.54, 126.88, 127.00, 134.17, 149.66, 118.25,
                       132.67, 154.48, 129.11, 151.83, 147.66, 127.30),
                   t=rep(c(15, 30, 45, 60, 75, 90,
                           120, 150, 180, 210, 240, 270), each=3))

#-----------------------------------------------------------------------------
# criando função que representa o modelo, retorna gradidente e hessiano

expo.der <- deriv(~A*(1-exp(-log(2)*t/V))+D*t,
                   c("A", "V", "D"),
                   function(t, A, V, D) NULL)
str(expo.der)

#-----------------------------------------------------------------------------
# diagnose gráfica e primeiro chute

plot(k~t, data=klib, xlab="Período de incubacão (dias)",
     ylab="Potássio liberado acumulado (mg/kg de solo)")
A <- 90; V <- 20; D <- 0.2
curve(expo.der(x, A, V, D), add=TRUE, col=2)
start <- list(A=A, V=V, D=D)

#-----------------------------------------------------------------------------
# ajustando o modelo aos dados a partir dos valores iniciais via gráfico

n0 <- nls(k~expo.der(t, A, V, D), data=klib, start=start)
summary(n0)
confint(n0)

#-----------------------------------------------------------------------------
# valores preditos, gradiente e hessiano avaliado nos valores estimados

str(n0)
str(n0$m$fitted())
c(n0$m$fitted())
attr(n0$m$fitted(), "gradient")
attr(n0$m$fitted(), "hessian")

#-----------------------------------------------------------------------------
# obtenção dos valores preditos

pred <- data.frame(t=seq(0,300,l=100))
der <- do.call(expo.der, args=c(list(t=pred$t), as.list(coef(n0))))

F <- attr(der, "gradient") # gradiente avaliado no novo t
U <- chol(vcov(n0))
se <- sqrt(apply(F%*%t(U), 1, function(x) sum(x^2))) # erro padrão

#-----------------------------------------------------------------------------
# gráficos dos observados, preditos com IC, legenda e equações

#png("f004.png", w=500, h=400); par(mar=c(5.1,4.1,2.1,2.1))
plot(k~t, data=klib, xlab="Período de incubacão (dias)",
     ylab="Potássio liberado acumulado (mg/kg de solo)",
     xlim=c(0,300), ylim=c(0,160))
matlines(pred$t, c(der)+
           outer(se, qt(c(.5, .025,.975), df=df.residual(n0))),
         type="l", col=c(1,2,2), lty=c(1,2,2))
legend("bottomright",
       legend=c("valores observados", "valores preditos",
                "intervalo de confiança (95%)"),
       lty=c(NA,1,2), col=c(1,1,2), pch=c(1,NA,NA), bty="n")
cf <- format(coef(n0), digits=3)
text(par("usr")[1], par("usr")[4], adj=c(-0.05,1.5),
     label=substitute(hat(k)[total]==a%.%(1-e^{-ln(2)%.%t/v})+d%.%t,
                      list(a=cf[1], v=cf[2], d=cf[3])))
abline(v=coef(n0)["V"], h=coef(n0)["A"], col="gray70")
curve(expo.der(x, coef(n0)["A"], coef(n0)["V"], 0), add=TRUE, col=3)
curve(expo.der(x, 0, 0, coef(n0)["D"]), add=TRUE, col=3)
text(225, coef(n0)["A"], pos=3,
     label=substitute(hat(k)[fácil]==a%.%(1-e^{-ln(2)%.%t/v}),
                      list(a=cf[1], v=cf[2])))
text(173, 38, pos=3, srt=18,
     label=substitute(hat(k)[difícil]==d%.%t, list(d=cf[3])))
#dev.off()

