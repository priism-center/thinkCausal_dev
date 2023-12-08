set.seed(2)
x <- rbinom(4000, 1, .07)

z <- rbinom(4000, 1, ifelse(x == 1, .9, .05))

y1 <- 240 - 5 - 80*x + rnorm(1000, 0, 5)
y0 <- 240 - 80*x + rnorm(1000, 0, 5)
y <- ifelse(z == 1, y1, y0)


# difference in means 
fit1 <- lm(y ~ z) # ATE = -50
fit1

# regression 
fit2 <- lm(y ~ z + x) # ATE = -5
fit2
# with bars
tibble(x, z, y) %>% 
  group_by(z) %>% 
  summarise(y_hat = mean(y)) %>% 
ggplot(aes(as.factor(z), y_hat, fill = as.factor(z))) + 
  geom_col(col = 'black') + 
  scale_fill_manual(values = c(4, 2)) + 
  theme_bw()

tibble(x, z, y) %>% 
  group_by(z) %>% 
  summarise(y_hat = mean(y)) %>% 
  ggplot(aes(as.factor(z), y_hat, fill = as.factor(z))) + 
  geom_col(col = 'black') + 
  scale_fill_manual(values = c(4, 2)) + 
  theme_bw() + 
  geom_segment(aes(x = 2.5, xend = 2.5, y = 189, yend = 239)) + 
  geom_segment(aes(x = 2.5, xend = 2.47, y = 189, yend = 189)) + 
  geom_segment(aes(x = 2.5, xend = 2.47, y = 239, yend = 239)) + 
  geom_label(aes(label = 'Difference in Means = -50.19 minutes!', x = 2.3 , y = 214), show.legend = F, col = 1) + 
  theme(legend.position = 'bottom')


tibble(x, z, y) %>% 
  group_by(z, x) %>% 
  summarise(y_hat = mean(y)) %>% 
  ggplot(aes(as.factor(z), y_hat, fill = as.factor(z))) + 
  geom_col(col = 'black') + 
  facet_wrap(~x) + 
  scale_fill_manual(values = c(4, 2)) + 
  theme_bw()



# with points 

tibble(x, z, y) %>% 
  group_by(z) %>% 
  summarise(y_hat = mean(y)) %>% 
  mutate(ucl = y_hat + 1.96*sqrt(diag(vcov(fit1)))['z'], 
         lcl = y_hat - 1.96*sqrt(diag(vcov(fit1)))['z']) %>% 
  ggplot(aes(as.factor(z), y_hat, col = as.factor(z))) + 
  geom_point(size = 2) + 
  geom_segment(aes(x = as.factor(z), xend = as.factor(z), 
                    y = ucl, yend = lcl)) +
  geom_segment(aes(x = 1.5, xend = 1.5, y = 239, yend = 189), col = 1) + 
  geom_label(aes(label = 'Difference in Means = -50.19 minutes!', x = 1.5 , y = 214), show.legend = F, col = 1) + 
  scale_color_manual(values = c(4, 2)) + 
  theme_bw()


tibble(x, z, y) %>% 
  group_by(z, x) %>% 
  summarise(y_hat = mean(y)) %>% 
  ggplot(aes(as.factor(z), y_hat, col = as.factor(z))) + 
  geom_point(size = 2) + 
  facet_wrap(~x) + 
  #geom_segment(aes(x = 1.5, xend = 1.5, y = 239, yend = 189), col = 1) + 
  #geom_label(aes(label = 'Difference in Means = -50.19 minutes!', x = 1.5 , y = 214), show.legend = F, col = 1) + 
  scale_color_manual(values = c(4, 2)) + 
  theme_bw()





### DGP 2

age <- c(runif(500, 18, 65), rnorm(1000, 35, 5), runif(500, 20, 37))
age[age < 18] <- 18

get_income <- function(x){
  case_when(
            x < 25 ~ rnorm(1, 40, 5), 
            x < 35 ~ rnorm(1, 60, 10), 
            x < 45 ~ rnorm(1, 80, 10), 
            x < 70 ~ rnorm(1, 50, 5)
  )
}

income <- sapply(age, get_income)

z <- rbinom()

set.seed(2)
x <- rbinom(4000, 1, .07)
state_p <- c(.3, .1, .05, .05, .025, .025,.025, .025, .02, .02, rep(.009, 40))
state <- sample(size = 4000, 1:50, replace = T, prob = state_p)

state_mat <- matrix(nrow = 4000, ncol = 50)
for (i in 1:50) {
  state_mat[, i] <- ifelse(state == i,1, 0)
}

colnames(state_mat) <- paste0('state_', 1:50)
X <- cbind(pro = x, state_mat)

beta <- c(qnorm(.9), qnorm(runif(50, 0, .3)))
z <- rbinom(4000, 1, pnorm(X %*% beta))

y1 <- 240 - 5 - 80*x + rnorm(1000, 0, 5)
y0 <- 240 - 80*x + rnorm(1000, 0, 5)
y <- ifelse(z == 1, y1, y0)

lm(y ~ z + state_mat)

sum(z)


