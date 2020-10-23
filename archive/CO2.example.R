fm1CO2.lis <- nlsList(SSasympOff, CO2)
fm1CO2.lis

fm1CO2.nlme <- nlme(fm1CO2.lis)
fm1CO2.nlme

fm2CO2.nlme <- update(fm1CO2.nlme, random = Asym + lrc ~ 1)
fm2CO2.nlme

plot(fm2CO2.nlme, id = 0.05, cex = 0.8, adj = -0.5)

fm2CO2.nlmeRE <- ranef(fm2CO2.nlme, augFrame = TRUE)
fm2CO2.nlmeRE

plot(fm2CO2.nlmeRE, form = ~ Type * Treatment)

contrasts(CO2$Type)
contrasts(CO2$Treatment)

fm3CO2.nlme <- update(fm2CO2.nlme, 
                      fixed = list(Asym ~ Type * Treatment, lrc + c0 ~ 1), 
                      start = c(32.412, 0, 0, 0, -4.5603, 49.344))


fm3CO2_.nlme <- nlme(uptake ~ SSasympOff(conc, Asym, lrc, c0),
                    data = CO2,
                    fixed = list(lrc ~ Type, c0 ~ Type, Asym ~ Type),
                    random = Asym + lrc ~ 1,
                    start = list(fixed=c(lrc=-4.5603, Type=-1, c0=49.344, Type=1, Asym=32.412, Type=0)))





summary(fm3CO2.nlme)






anova(fm3CO2.nlme, Terms = 2:4)

fm3CO2.nlmeRE <- ranef(fm3CO2.nlme, aug = TRUE)
plot(fm3CO2.nlmeRE, form = ~ Type * Treatment)

