
dgp <- function(sampsize, tau){
  
  var1 <- rnorm(sampsize, mean = 2, sd = 3)
  var2 <- rnorm(sampsize, mean = 1, sd = 10)
  var3 <- rnorm(sampsize, mean = 0, sd = 1)
  var4 <- rnorm(sampsize, mean = 3, sd = 3)
  var5 <- rnorm(sampsize, mean = 0, sd = 1)
  var6 <- rnorm(sampsize, mean = 1, sd = 15)
  var7 <- rnorm(sampsize, mean = 3, sd = 1)
  var8 <- rnorm(sampsize, mean = 3, sd = 2)
  var9 <- rnorm(sampsize, mean = 2, sd = 20)
  var_bi <- sample(c(0, 10), size = sampsize, replace = TRUE)
  
  # beta <- c(0.1,0.01,0.1,0.01,0.01,0.01, 0.01)
  # X <-cbind(var2,var4, var6, var7, var8, var9, var_bi) %*% beta
  
  # let var1,3,5 be noise and other variables be confounders
  beta <- 0.01
  X <- var2*beta + var4*beta +var6*beta + var7*beta +var8*beta +var9*beta + var_bi*beta
  inv.log <- function(x){exp(x)/(1 + exp(x))}
  z.prop <- sapply(X, inv.log)
  Z <- rbinom(sampsize, 1, z.prop)
  Y1 <- tau + var2 + var4 + var6 + var7 + var8 + var9 + var_bi + rnorm(sampsize, 0, 1)
  Y0 <- var2 + var4 + var6 + var7 + var8 + var9 + var_bi + rnorm(sampsize, 0, 1)
  df <- data.frame(var1, var2, var3, var4, var5, var6, var7, var8, var9, var_bi, Y1, Y0, Z)
  df <- df %>% mutate(Y = if_else(Z ==1, Y1, Y0))
  
  return(df)
}