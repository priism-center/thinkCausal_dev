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


p21 <- dat %>%
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

p21

ggsave('inst/app/www/learn/estimands2/plots/p21.png', device = 'png', height = 5, width = 8)

p18 <- ggplot() + theme_void()
p18
ggsave('inst/app/www/learn/estimands2/plots/p18.png', device = 'png', height = 5, width = 8)



N <- 500

X <- rnorm(N, 35, 10)
X <- X[X > 18 & X < 55]

dat <- data.frame(age = X, scaled_age = scale(X))

#beta.z <-c(-1)
asn_z <- function(x){
  if(x <= 30){
    rbinom(1, 1, .8)
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

p19 <- dat %>%
  ggplot(aes(scaled_age, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(color = 'Z', x = 'X are single confounder',y = 'Outcome Y')

p19

ggsave('inst/app/www/learn/estimands2/plots/p19.png', device = 'png', height = 5, width = 8)

fit_ex2 <- bartCause::bartc(y, z, scaled_age, data = dat)
y.cf <- bartCause::extract(fit_ex2, 'y.cf')
y.cf <- cbind.data.frame(y.cf = apply(y.cf, 2, mean), X = dat$scaled_age, Z = dat$z)
y.cf0 <- apply(y.cf, 2, mean)[dat$z == 0]

p20 <- ggplot() +
  geom_point(data = dat, aes(scaled_age, y, col = as.factor(z))) +
  geom_line(data = y.cf %>% filter(Z == 1), aes(X, y.cf), col = 2, size = 1) +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(color = 'Z', x = 'X are single confounder',y = 'Outcome Y')

p20
ggsave('inst/app/www/learn/estimands2/plots/p20.png', device = 'png', height = 5, width = 8)

p21 <-   ggplot() +
  geom_point(data = dat, aes(scaled_age, y, col = as.factor(z))) +
  #geom_line(data = y.cf %>% filter(Z == 1), aes(X, y.cf), col = 2, size = 1) +
  geom_line(data = y.cf %>% filter(Z == 0), aes(X, y.cf), col = 4, size = 1) +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(color = 'Z', x = 'X our single confounder',y = 'Outcome Y')

p21
ggsave('inst/app/www/learn/estimands2/plots/p21.png', device = 'png', height = 5, width = 8)

dat_ex3 <- dat %>% filter(scaled_age > .1 & scaled_age < .8 | z == 1) %>%
  filter(scaled_age > -1)

p22 <- ggplot(dat_ex3, aes(scaled_age, y, col = as.factor(z))) +
  geom_point() +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(color = 'Z', x = 'X our single confounder',y = 'Outcome Y')

p22

ggsave('inst/app/www/learn/estimands2/plots/p22.png', device = 'png', height = 5, width = 8)


fit_ex3 <- bartCause::bartc(y, z, scaled_age, data = dat_ex3, estimand = 'atc')
y.cf <- bartCause::extract(fit_ex3, 'y.cf')
y.cf.uncertain <- cbind.data.frame(
  y.lcl = apply(y.cf, 2, function(i){quantile(i, .025)}),
  y.ucl = apply(y.cf, 2, function(i){quantile(i, .975)}),
  X = dat_ex3$scaled_age[dat_ex3$z == 0])

y.cf <- cbind.data.frame(y.cf = apply(y.cf, 2, mean), X = dat_ex3$scaled_age[dat_ex3$z == 0])


p23 <- ggplot() +
  geom_point(data = dat_ex3, aes(scaled_age, y, col = as.factor(z))) +
  geom_line(data = y.cf, aes(X, y.cf), col = 4, size = 1) +
  #geom_ribbon(data = y.cf.uncertain, aes(x = X, ymin = y.lcl, ymax = y.ucl), alpha = .2) +
  scale_color_manual(values = c(4, 2)) +
  theme_bw() +
  labs(color = 'Z', x = 'X are single confounder',y = 'Outcome Y')

p23
ggsave('inst/app/www/learn/estimands2/plots/p23.png', device = 'png', height = 5, width = 8)







