library(tidyverse)
library(patchwork)
set.seed(21)
n = 400
z <- rep( 0:1 , each=n/2 )
bugs1 <- rbinom(n, 1, .1)
bugs0 <- rbinom(n, 1, .6)
bugs1[bugs0 == 0 & bugs1 == 1] <- 0
bugs <- ifelse(z == 1, bugs1, bugs0)
bugs_lab <- ifelse(bugs == 1, 'bugs' , 'no bugs')

y1 <- 7.5 + rnorm(400) -3*bugs1
y0 <- 7.5 + rnorm(400) -3*bugs0
y <- ifelse(z == 1, y1, y0)

summary(lm(y ~ z))
1.5139 - 1.96*.1540

summary(lm(y ~ z + bugs))
-0.1780    + 1.96*0.1251 


theme_set(theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  text = element_text(size = 16)))

# create plot 1 this is the analysis without bugs
tibble::tibble(bugs, z, y) %>% 
  mutate(z_lab = ifelse(z == 1, 'plants with new fertilizer', 'plants without new fertilizer'), 
         beta = ifelse(z == 1, 5.649 + 1.663, 5.649)) %>% 
  ggplot2::ggplot(aes(reorder(z_lab, z), y, col = z_lab)) + 
  geom_point(position = position_jitter(seed = 2)) + 
  geom_segment(x = .6, xend = 1.4, y = 5.849, yend = 5.849, col = 'black', size = 1) + 
  geom_segment(x = 1.6, xend = 2.4, y = 5.849+ 1.514, yend = 5.849+ 1.514, col = 'black', size = 1) + 
  scale_color_manual(values = c(4, 2), breaks = c('plants without new fertilizer', 'plants with new fertilizer')) + 
  theme(legend.position = 'top') + 
  labs(x = NULL, y = 'pounds of fruit', title = 'No post-treatment variables', color = NULL, 
       subtitle = 'estimated ATE = 1.51 95% CI: 1.21, 1.82') 

# save to learning file
ggsave('p1.png', 
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/post-treatment/', 
      width = 9, 
      height = 7)


tibble::tibble(bugs, z, y) %>% 
  mutate(z_lab = ifelse(z == 1, 'plants with new fertilizer', 'plants without new fertilizer'), 
         beta = case_when(
           bugs == 1 & z == 1 ~ sum(lm(y ~ z + bugs)$coeff), 
           bugs == 1 & z == 0 ~ sum(lm(y ~ z + bugs)$coeff[c(1, 3)]), 
           bugs == 0 & z == 1 ~ sum(lm(y ~ z + bugs)$coeff[c(1, 2)]), 
           bugs == 0 & z == 0 ~ sum(lm(y ~ z + bugs)$coeff[1]),
         ), 
         .x = ifelse(z == 1, 1.6, .6),
         .xend = ifelse(z == 1, 2.4, 1.4),
         bugs = ifelse(bugs == 1, 'plants with bugs', 'plants without bugs'), 
        ) %>% 
  ggplot2::ggplot(aes(reorder(z_lab, z), y, col = z_lab)) + 
  geom_point(position = position_jitter(seed = 2)) + 
  geom_segment(aes(x = .x, xend = .xend, y = beta, yend = beta), col = 'black', size = 1) + 
  scale_color_manual(values = c(4, 2), breaks = c('plants without new fertilizer', 'plants with new fertilizer')) + 
  facet_wrap(~bugs) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  theme(legend.position = 'top') + 
  labs(x = NULL, y = 'pounds of fruit', title = 'Including post-treatment variables', color = NULL, 
       subtitle = 'estimated ATE = -.18 95% CI: -.4, .06')

ggsave('p2.png', 
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/post-treatment/', 
       width = 9, 
       height = 7)



mean(y1 - y0)
tibble::tibble(y1, y0, y, z, bugs1, bugs0) %>% 
  pivot_longer(1:2) %>% 
  mutate(status = 
           case_when(
             z == 1 & name == 'y1' ~ 'factual outcome', 
             z == 0 & name == 'y0' ~ 'factual outcome', 
             TRUE ~ 'counter-factual outcome'
           ), 
         .x = ifelse(name == 'y1', 1.6, .6),
         .xend = ifelse(name == 'y1', 2.4, 1.4),
         po = ifelse(name == 'y1', 'Y(1)', 'Y(0)'), 
         name_lab = ifelse(name == 'y1', 'If all 400 plants had the new fertilizer', 'If none of the 400 plants had the new fertilizer'),
         z_lab = ifelse(z == 1, 'plants with new fertilizer', 'plants without new fertilizer')
         ) %>% 
  group_by(name) %>% 
  mutate(ate = mean(value)) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(name_lab, ate), value, col = po, shape = status)) + 
  geom_point(position = position_jitter(seed = 2)) + 
  geom_segment(aes(x = .x, xend = .xend, y = ate, yend = ate), col = 'black', size = 1) + 
  scale_color_manual(values = c(4, 2), 
                     #breaks = c('Y if Z = 0', 'Y if Z = 1')
                     ) + 
  scale_shape_manual(values = c(21, 19)) + 
  theme(legend.position = 'top') + 
  labs(x = NULL, color = NULL, shape = NULL, y = 'pounds of fruit', 
       title = 'If we could see all potential outcomes', 
       subtitle = 'true ATE = 1.59') + 
  coord_cartesian(ylim = c(2, 10))

ggsave('p3.png', 
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/post-treatment/', 
       width = 9, 
       height = 7)
           

tibble(est = c(lm(y ~ z)$coeff[2], lm(y ~ z + bugs)$coeff[2]), 
       se = c(sqrt(diag(vcov(lm(y ~ z))))[2], sqrt(diag(vcov(lm(y ~ z + bugs))))[2]), 
       lci = est - 1.96*se, 
       uci = est + 1.96*se, 
       analysis = c('no post-treatment variables', 'including post-treatment variables'), 
       ate = mean(y1 - y0), 
       color = c('black', 'white')) %>% 
ggplot(aes(x = est, analysis, col = color)) + 
  geom_point(size = 2) + 
  geom_errorbarh(aes(xmin = lci, xmax = uci, y = analysis), height = .05) + 
  coord_cartesian(xlim = c(-.5, 3.5), ylim = c(1, 2)) + 
  scale_color_manual(values = c('black', 'white')) + 
  geom_vline(xintercept = 1.58, linetype = 2) + 
  geom_label(label = 'True ATE = 1.58', x = 1.58, y = 2.4, color = 'black') + 
  theme(axis.text.y = element_text(color = c('white', 'black')), 
        axis.ticks.y = element_line(color = c('white', 'black')),
        legend.position = 'none') + 
  labs(x = 'Estimated ATE', y = NULL, title = 'Comparing Analyses')

ggsave('p4.png', 
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/post-treatment/', 
       width = 9, 
       height = 7)

tibble(est = c(lm(y ~ z)$coeff[2], lm(y ~ z + bugs)$coeff[2]), 
       se = c(sqrt(diag(vcov(lm(y ~ z))))[2], sqrt(diag(vcov(lm(y ~ z + bugs))))[2]), 
       lci = est - 1.96*se, 
       uci = est + 1.96*se, 
       analysis = c('no post-treatment variables', 'including post-treatment variables'), 
       ate = mean(y1 - y0)) %>% 
  ggplot(aes(x = est, analysis)) + 
  geom_point(size = 2) + 
  geom_errorbarh(aes(xmin = lci, xmax = uci, y = analysis), height = .05) + 
  coord_cartesian(xlim = c(-.5, 3.5), ylim = c(1, 2)) + 
  geom_vline(xintercept = 1.58, linetype = 2) + 
  geom_label(label = 'True ATE = 1.58', x = 1.58, y = 2.4, color = 'black') + 
  labs(x = 'Estimated ATE', y = NULL, title = 'Comparing Analyses')

ggsave('p5.png', 
       path = '~/Dropbox/thinkCausal_dev/thinkCausal/inst/app/www/learn/post-treatment/', 
       width = 9, 
       height = 7)
