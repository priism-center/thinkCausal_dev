library(shiny)
library(tidyverse)
library(patchwork)

theme_set(theme_bw() + theme(
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank()
                             ))
draw <- function(N){
  
  X <- rnorm(N, 35, 8)
  
  X[X < 18] = 18
  X[X > 55] = 55
  
  dat <- data.frame(age = X, scaled_age = scale(X), pro = rbinom(N, 1, .33))
  
  z.1 <- rbinom(N, 1, .5)
  beta.z <- c(-1, 1)
  p.score <- pnorm(cbind(dat$scaled_age, dat$pro) %*% beta.z)
  z.2 <- rbinom(N, 1, p.score)
  z <- c(z.1, z.2)
  
  dat <- rbind(dat, dat)
  dat$asn <- c(rep('Random', N), rep('Non-Random', N))
  dat$z <- z
  
  dat$y1 <- with(dat, 
                 180 -7 +
                   .5*scaled_age + I((scaled_age-.1)^2)*2  + 
                   pro*-20 +
                   rnorm(nrow(dat))
  )
  
  
  dat$true.1 <- with(dat, 
                     180 -7 +.5*scaled_age + I((scaled_age-.1)^2)*2 + pro*-20 
  )
  
  dat$y0 <- with(dat, 
                 176 
                 +.5*scaled_age + I((scaled_age+.3)^2)*3.2 + 
                   pro*-20 +
                   rnorm(nrow(dat))
  )
  
  dat$true.0 <-  with(dat, 
                      176 +.5*scaled_age + I((scaled_age+.3)^2)*3.2 + pro*-20
  )
  
  dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)
  
  dat$asn <- factor(dat$asn, levels = c('Random', 'Non-Random'))
  return(dat)
}


