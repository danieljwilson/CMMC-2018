library("magrittr")

y <- rexp(10)+ 1

# difficult to read
res <- sin(sqrt(mean(exp(log(y)), na.rm = T)))

# lots of junk
a1 <- log(y)
aaa <- exp(a1)
interm <- mean(aaa, na.rm=T)
sqint <- sqrt(interm)
res2 <- sin(sqint)

res3 <- y %>% 
  log %>%
  exp %>%
  mean(na.rm=T) %>%
  sqrt %>% 
  sin
