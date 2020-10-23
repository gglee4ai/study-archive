library("lme4")
library("ggplot2") # Plotting
data("Orthodont",package="MEMSS")
fm1 <- lmer(
  formula = distance ~ age*Sex + (age|Subject)
  , data = Orthodont
)
newdat <- expand.grid(
  age=c(8,10,12,14)
  , Sex=c("Female","Male")
  , distance = 0
)
mm <- model.matrix(terms(fm1),newdat)
newdat$distance <- predict(fm1,newdat,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(fm1),mm))
tvar1 <- pvar1+VarCorr(fm1)$Subject[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
newdat <- data.frame(
  newdat
  , plo = newdat$distance-cmult*sqrt(pvar1)
  , phi = newdat$distance+cmult*sqrt(pvar1)
  , tlo = newdat$distance-cmult*sqrt(tvar1)
  , thi = newdat$distance+cmult*sqrt(tvar1)
)
#plot confidence
g0 <- ggplot(newdat, aes(x=age, y=distance, colour=Sex))+geom_point()
g0 + geom_errorbar(aes(ymin = plo, ymax = phi))+
  labs(title="CI based on fixed-effects uncertainty ONLY")
#plot prediction
g0 + geom_errorbar(aes(ymin = tlo, ymax = thi))+
  labs(title="CI based on FE uncertainty + RE variance")