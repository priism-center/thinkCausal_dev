library(dplyr)
library(ggplot2)

# make all the data
set.seed(2)
X <- runif(1000, 130, 200)
p.score <- ifelse(X < 200, pnorm(scale(X)*-.5), 0)
Z <- rbinom(1000, 1, p.score)
Z <- ifelse(X >=144.2, 0, Z)
scaleX <- 1.5 + as.vector(scale(X))


Y0 <- 160 + scaleX*2  + (scaleX**2)*3 + rnorm(1000)
Y1 <- 160 + -5 + scaleX*2 + rnorm(1000)
Y <- ifelse(Z == 1, Y1, Y0)

dat <- data.frame(X, scaleX, Y = Y, Z = Z, Y1 = Y1, Y0 = Y0)

set.seed(2)
X <- runif(1000, 130, 200)
p.score <- ifelse(X < 200, pnorm(scale(X)*-.5), 0)
Z <- rbinom(1000, 1, p.score)
Z <- ifelse(X >=144.2, 0, Z)
scaleX <- 1.5 + as.vector(scale(X))


Y0 <- 160 + scaleX*2  + (scaleX**2)*3 + rnorm(1000)
Y1 <- 160 + -5 + scaleX*2  + (scaleX**2)*3 + rnorm(1000)
Y <- ifelse(Z == 1, Y1, Y0)

world1 <- data.frame(X, scaleX, Y = Y, Z = Z, Y1 = Y1, Y0 = Y0)

set.seed(2)
X <- runif(1000, 130, 200)
p.score <- ifelse(X < 200, pnorm(scale(X)*-.5), 0)
Z <- rbinom(1000, 1, p.score)
Z <- ifelse(X >=144.2, 0, Z)
scaleX <- 1.5 + as.vector(scale(X))


Y0 <- 160 + scaleX*2  + (scaleX**2)*3 + rnorm(1000)
Y1 <- 160 + -5 + scaleX*2  + rnorm(1000)
Y <- ifelse(Z == 1, Y1, Y0)

world2 <- data.frame(X, scaleX, Y = Y, Z = Z, Y1 = Y1, Y0 = Y0)

set.seed(2)
X <- runif(1000, 130, 200)
p.score <- ifelse(X < 200, pnorm(scale(X)*-.5), 0)
Z <- rbinom(1000, 1, p.score)
Z <- ifelse(X >=144.2, 0, Z)
scaleX <- 1.5 + as.vector(scale(X))


Y0 <- 160 + scaleX*2  + (scaleX**2)*3 + rnorm(1000)
Y1 <- ifelse(X > 145,
             160 + -5 + scaleX*3 +(scaleX**2)*4.5  + rnorm(1000),
             160 + -5 + scaleX*2  + rnorm(1000)
)
Y <- ifelse(Z == 1, Y1, Y0)

world3 <- data.frame(X, scaleX, Y = Y, Z = Z, Y1 = Y1, Y0 = Y0)

p1 <- dat %>%
  ggplot(aes(X, Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200)) +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z', title = 'Marathon Running Data', subtitle = 'The data from our hypothetical study')
p1
ggsave('inst/app/www/learn/estimands2/plots/p1.png', device = 'png', height = 5, width = 8)

p2 <- dat %>%
  filter(Z == 1) %>%
  add_row(Z = 0, X = 0, Y = 0, Y1 = 0, Y0 = 0, scaleX = 0) %>%
  ggplot(aes(X, Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200)) +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z', title = 'Runners that wore HyperShoes')
p2
ggsave('inst/app/www/learn/estimands2/plots/p2.png', device = 'png', height = 5, width = 8)

p3 <- dat %>%
  ggplot() +
  geom_point(data = dat %>% filter(X<145), aes(X, Y, col = as.factor(Z))) +
  geom_point(data = dat %>% filter(X>=145), aes(X, Y, col = as.factor(Z))) +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z') +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200)) +
  annotate("rect", xmin = 129.2, xmax = 146, ymin = 150, ymax = 165,
           alpha = 0, color= "black")

p3
ggsave('inst/app/www/learn/estimands2/plots/p3.png', device = 'png', height = 5, width = 8)


# make table 1
table1 <- dat %>% filter(Z == 1) %>%
  select(X, Z, Y0, Y1, Y) %>%
  mutate_all(~round(., 1)) %>%
  mutate(Y0 = '?')

readr::write_csv(table1, 'inst/extdata/estimands2_table1.csv')


p4 <- dat %>%
  ggplot() +
  geom_point(data = dat %>% filter(X<145), aes(X, Y, col = as.factor(Z))) +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z') +
  annotate("rect", xmin = 129.2, xmax = 146, ymin = 150, ymax = 165,
           alpha = 0, color= "black") +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200))

p4
ggsave('inst/app/www/learn/estimands2/plots/p4.png', device = 'png', height = 5, width = 8)


fit <- bartCause::bartc(Y, Z, X, data = dat, estimand = 'att')
y.cf <- bartCause::extract(fit, 'y.0')
dat %>%
  filter(X < 145)

p5 <- ggplot() +
  geom_point(data = dat %>% filter(X < 145) , aes(X, Y, col = as.factor(Z))) +
  scale_color_manual(values = c(4, 2)) +
  geom_line(data = dat %>% filter(Z == 1) %>% mutate(y.cf =apply(y.cf, 2, mean)),
            aes(X, y.cf), size = 1, col = 2) +
  theme_bw() +
  annotate("rect", xmin = 129.2, xmax = 146, ymin = 150, ymax = 165,
           alpha = 0, color= "black") +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z') +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200))

p5
ggsave('inst/app/www/learn/estimands2/plots/p5.png', device = 'png', height = 5, width = 8)


p6 <- p1
p6
ggplot2::ggsave('inst/app/www/learn/estimands2/plots/p6.png', device = 'png', height = 5, width = 8)


p7 <- dat %>%
  filter(Z == 0) %>%
  add_row(Z = 1, X = 0, Y = 0, Y1 = 0, Y0 = 0, scaleX = 0) %>%
  ggplot(aes(X, Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z', title = 'Runners that did not wear HyperShoes', subtitle = 'What would have happened to these runners if they had worn HyperShoes?') +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200))

p7
ggsave('inst/app/www/learn/estimands2/plots/p7.png', device = 'png', height = 5, width = 8)


table2 <- dat %>% filter(Z == 0) %>%
  select(X, Z, Y0, Y1, Y) %>%
  mutate_all(~round(., 1)) %>%
  mutate(Y1 = '?')

readr::write_csv(table2, 'inst/extdata/estimands2_table2.csv')

p10 <- dat %>%
  ggplot(aes(X, Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z') +
  geom_text(aes(label = '???', x = 180, y = 165), col = 1, size = 6) +
  geom_text(aes(label = '???', x = 190, y = 160), col = 1, size = 6) +
  geom_text(aes(label = '???', x = 160, y = 157), col = 1, size = 6) +
  geom_text(aes(label = '???', x = 160, y = 180), col = 1, size = 6) +
  geom_text(aes(label = '???', x = 180, y = 190), col = 1, size = 6) +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200))

p10

ggsave('inst/app/www/learn/estimands2/plots/p10.png', device = 'png', height = 5, width = 8)

p11 <- ggplot() + theme_void()
p11
ggsave('inst/app/www/learn/estimands2/plots/p11.png', device = 'png', height = 5, width = 8)


p12 <- ggplot() +
  geom_point(data = world1, aes(X, Y, col = as.factor(Z))) +
  geom_point(data = world1 %>% filter(X> max(world1$X[world1$Z == 1]), Z == 0), aes(X, Y1, col = as.factor(Z)), shape = 21, col = 2, alpha = .3) +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z', title = 'HyperShoes work equally well for slower runners!') +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200))

p12
ggsave('inst/app/www/learn/estimands2/plots/p12.png', device = 'png', height = 5, width = 8)


p13 <- ggplot() +
  geom_point(data = world2, aes(X, Y, col = as.factor(Z))) +
  geom_point(data = world2 %>% filter(X> max(world2$X[world3$Z == 1]), Z == 0), aes(X, Y1, col = as.factor(Z)), shape = 21, col = 2, alpha = .3) +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z',
       title = "HyperShoes work even better for slower runners!") +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200))


p13

ggsave('inst/app/www/learn/estimands2/plots/p13.png', device = 'png', height = 5, width = 8)


p14 <- ggplot() +
  geom_point(data = world3, aes(X, Y, col = as.factor(Z))) +
  geom_point(data = world3 %>% filter(X> max(world2$X[world3$Z == 1]), Z == 0),
             aes(X, Y1, col = as.factor(Z)), shape = 21, col = 2, alpha = .3) +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z', title = "HyperShoes make slower runners even slower!")

p14

ggsave('inst/app/www/learn/estimands2/plots/p14.png', device = 'png', height = 5, width = 8)


p15 <- dat %>%
  ggplot(aes(X, Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z') +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200)) +
  geom_text(aes(label = '???', x = 180, y = 165), col = 1, size = 6) +
  geom_text(aes(label = '???', x = 190, y = 160), col = 1, size = 6) +
  geom_text(aes(label = '???', x = 160, y = 157), col = 1, size = 6) +
  geom_text(aes(label = '???', x = 160, y = 180), col = 1, size = 6) +
  geom_text(aes(label = '???', x = 180, y = 190), col = 1, size = 6)

p15
ggsave('inst/app/www/learn/estimands2/plots/p15.png', device = 'png', height = 5, width = 8)


p16 <- p1
p16
ggplot2::ggsave('inst/app/www/learn/estimands2/plots/p16.png', device = 'png', height = 5, width = 8)

p17 <- ggplot() + theme_void()
p17
ggsave('inst/app/www/learn/estimands2/plots/p17.png', device = 'png', height = 5, width = 8)

p18 <- dat %>%
  filter(Z == 1) %>%
  add_row(Z = 0, X = 0, Y = 0, Y1 = 0, Y0 = 0, scaleX = 0) %>%
  ggplot(aes(X, Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z', title = 'Runners that wore HyperShoes', subtitle = 'What would have happened to these runners if they did not wear HyperShoes?') +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200))
p18
ggsave('inst/app/www/learn/estimands2/plots/p18.png', device = 'png', height = 5, width = 8)

p19 <- dat %>%
  filter(Z == 0) %>%
  add_row(Z = 1, X = 0, Y = 0, Y1 = 0, Y0 = 0, scaleX = 0) %>%
  ggplot(aes(X, Y, col = as.factor(Z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color ='Z', title = 'Runners that did not wear HyperShoes', subtitle = 'What would have happened to these runners if they wore HyperShoes?') +
  coord_cartesian(ylim = c(150, 199), xlim = c(129, 200))

p19
ggsave('inst/app/www/learn/estimands2/plots/p19.png', device = 'png', height = 5, width = 8)


table3 <- dat %>%
  select(X, Z, Y0, Y1, Y) %>%
  mutate_all(~round(., 1)) %>%
  mutate(Y0 = ifelse(Z == 1, '?', Y0),
         Y1 = ifelse(Z == 1, Y1, '?'))

readr::write_csv(table3, 'inst/extdata/estimands2_table3.csv')


# quiz plots
N <- 750

X <- rnorm(N, 35, 10)
X <- X[X > 18 & X < 55]

dat <- data.frame(age = X, scaled_age = scale(X))

#beta.z <-c(-1)
asn_z <- function(x){
  if(x <= 30){
    rbinom(1, 1, .77)
  }

  else{rbinom(1, 1, .4)}
}

dat$z <- sapply(X, asn_z)



dat$y1 <- with(dat,
               180 -7 +.5*scaled_age + I((scaled_age-.1)^2)*2  + rnorm(nrow(dat))
)

dat$true.1 <- with(dat,
                   180 -7 +.5*scaled_age + I((scaled_age-.1)^2)*2
)

dat$y0 <- with(dat,
               176 +.5*scaled_age + I((scaled_age+.3)^2)*3.2 + rnorm(nrow(dat))
)

dat$true.0 <-  with(dat,
                    176 +.5*scaled_age + I((scaled_age+.3)^2)*3.2
)

dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)

dat %>%
  filter(scaled_age < 2.1) %>%
  ggplot(aes(scaled_age, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('control', 'treated')) +
  theme_bw() +
  labs(color = NULL, x = 'X the single confounder',y = 'Outcome Y')

ggsave('inst/app/www/learn/estimands2/plots/quiz1.png', device = 'png', height = 5, width = 8)


# quiz question 2
N <- 750

X <- rnorm(N, 35, 10)
X <- X[X > 18 & X < 55]

dat <- data.frame(age = X, scaled_age = scale(X))

#beta.z <-c(-1)
asn_z <- function(x){
  if(x <= 30){
    rbinom(1, 1, .73)
  }

  else{rbinom(1, 1, .4)}
}

dat$z <- sapply(X, asn_z)



dat$y1 <- with(dat,
               180 -10 +2*scaled_age  + rnorm(nrow(dat))
)


dat$y0 <- with(dat,
               176 +.5*scaled_age + I((scaled_age+.3)^2)*1.3 + rnorm(nrow(dat))
)



dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)

dat %>%
  filter(scaled_age > .3 & scaled_age < 1.5 | z == 1) %>%
  ggplot(aes(scaled_age, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('control', 'treated')) +
  theme_bw() +
  labs(color = NULL, x = 'X the single confounder',y = 'Outcome Y')

ggsave('inst/app/www/learn/estimands2/plots/quiz2.png', device = 'png', height = 5, width = 8)


# quiz question 3
N <- 750

X <- rnorm(N, 35, 10)
X <- X[X > 18 & X < 55]

dat <- data.frame(age = X, scaled_age = scale(X))

#beta.z <-c(-1)
asn_z <- function(x){
  if(x <= 30){
    rbinom(1, 1, .6)
  }

  else{rbinom(1, 1, .4)}
}

dat$z <- sapply(X, asn_z)



dat$y1 <- with(dat,
               180 -10 +.5*scaled_age + I((scaled_age-.1)^2)*2  + rnorm(nrow(dat))
)


dat$y0 <- with(dat,
               176 +.5*scaled_age + I((scaled_age+.3)^2)*1.3 + rnorm(nrow(dat))
)


dat$y <- ifelse(dat$z ==1, dat$y1, dat$y0)

dat %>%
  filter(scaled_age <.3 | z == 0) %>%
  ggplot(aes(scaled_age, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('control', 'treated')) +
  theme_bw() +
  labs(color = NULL, x = 'X the single confounder',y = 'Outcome Y')

ggsave('inst/app/www/learn/estimands2/plots/quiz3.png', device = 'png', height = 5, width = 8)


z <- rbinom(500, 1, .5)
X1 <- rnorm(500, 20, 10)
X1 <- ifelse(X1<0, 0, X1)
X0 <- rnorm(500, 40, 10)
X0 <- ifelse(X0>60, 60, X0)
X0 <- ifelse(X0 <0, 0, X0)
X <- ifelse(z==1, X1, X0)
y1 <- rnorm(500, 72 + 3*sqrt(X1), 1)
y0 <- rnorm(500, 90 + exp((.06*X0)), 1)
y <- ifelse(z==1, y1, y0)
dat <- data.frame(X1, X0, X, y1, y0, y, z)

ggplot(dat %>% filter(X<31 | z == 0) %>% mutate(y= y + 70), aes(X, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('control', 'treated')) +
  theme_bw() +
  labs(col = NULL) +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color = NULL)

ggsave('inst/app/www/learn/estimands2/plots/partial_overlap.png', device = 'png', height = 5, width = 8)


ggplot(dat %>% filter(X<31 | z == 0) %>% mutate(y= y + 70), aes(X, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('control', 'treated')) +
  theme_bw() +
  annotate("rect", xmin = 12, xmax = 32, ymin = 150, ymax = 170,
           alpha = 0, color= "black") +
  labs(col = NULL) +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color = NULL)


#
# ggplot(dat %>% filter(X > 8& X<31 | z == 0) %>% mutate(y= y + 70), aes(X, y, col = as.factor(z))) +
#   geom_point() +
#   scale_color_manual(values = c(4, 2), labels = c('control', 'treated')) +
#   annotate('rect', xmin=-Inf, xmax=min(X[z == 0])-.5, ymin=-Inf, ymax=Inf, alpha=.2, fill='red') +
#   theme_bw() +
#   labs(col = NULL) +
#   labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color = NULL)
#

dat %>% filter(X <23 | z == 0) %>% mutate(y= y + 70) %>%
  filter(X>40|z == 1) %>%
  ggplot(aes(X, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2), labels = c('control', 'treated')) +
  #annotate('rect', xmin= 30+ .7, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='blue') +
  # annotate('rect', xmin=-Inf, xmax=min(X[z == 0])-.5, ymin=-Inf, ymax=Inf, alpha=.2, fill='red') +
  theme_bw() +
  labs(col = NULL) +
  labs(x = 'Single confounder X', y = 'Outcome Y (running times)', color = NULL)

ggsave('inst/app/www/learn/estimands2/plots/no_overlap.png', device = 'png', height = 5, width = 8)
