# standardized time plot
alcohol2 <- sweep(alcohol[,c(4:6)], 2, apply(alcohol[,c(4:6)], 2, mean))
sd1 <- apply(alcohol[,c(4:6)], 2, sd)
alcohol2 <- sweep(alcohol2, 2, sd1, FUN = "/")
alcohol2 <- cbind(alcohol[,c(1:3)], alcohol2)
alcohol2 %>% gather(ND0:ND60, key = "Time", value = "Consumption") %>% 
  ggplot(aes(x = Time, y = Consumption, group = sid, color = Treatment)) +
  geom_line() +
  facet_grid(. ~ Gender)

# median polish
junk1 <- medpolish(alcohol[, c(4:6)])
res <- junk1$res
cols <- rep(1:314, rep(3, 314))
par(mfcol = c(2, 2))
plot(as.vector(t(res)), col = cols, pch = 19, cex = 0.8, 
     xlab = "Children", ylab = "Residuals", main = "(A)")
plot(junk1$col ,xlab = "Time", ylab = "Time Effect", main = "(B)")
plot(junk1$row, xlab = "Subject", ylab = "Subject Effect", main = "(C)")
acf(as.vector(t(res)), xlab = "Lag", main = "(D)")

# GEE model regarding alcohol consumption
alcohol3 <- alcohol %>% 
  gather(ND0:ND60, key = "Time", value = "Consumption")
fit.gee <- gee(Consumption ~ Treatment * Time + Treatment * Gender, 
               data = alcohol3, family = "gaussian", 
               id = sid, corstr = "unstructured")
sum.gee <- summary(fit.gee)
gee.coef <- sum.gee$coefficients %>% 
  as_tibble() %>% 
  mutate(parameters = rownames(sum.gee$coefficients),
         CIlow = Estimate - `Robust S.E.`,
         CIup = Estimate + `Robust S.E.`,
         CI = paste("(", round(CIlow, 3), ", ", round(CIup, 3), ")", sep = ""),
         pvalue = pnorm(`Robust z`, lower.tail = F) * 2) %>% 
  select(parameters, Estimate, `Robust S.E.`, CI, pvalue)
gee.coef %>% write.csv("gee.coef.csv", na = "")

# GLME model regarding relapse
fit.glme <- glmer(Relapse ~ Treatment + (1 | sid), 
                  data = alcohol, family = "binomial")
sum.glme <- summary(fit.glme)
glme.coef <- sum.glme$coefficients %>% 
  as_tibble() %>% 
  mutate(parameters = rownames(sum.glme$coefficients),
         CIlow = Estimate - `Std. Error`,
         CIup = Estimate + `Std. Error`,
         CI = paste("(", round(CIlow, 3), ", ", round(CIup, 3), ")", sep = "")) %>% 
  select(parameters, Estimate, `Std. Error`, CI, `Pr(>|z|)`)
glme.coef %>% write.csv("glme.coef.csv", na = "")