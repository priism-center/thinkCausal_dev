shinyServer(function(input, output) {
  
    
    plot_single_draw <- reactive({
      req(input$n > 9)

      dat <- draw(N = input$n)
      dat$z <- factor(dat$z, levels = c(1, 0))
      means <- dat %>% 
        group_by(z) %>% 
        summarise(point = mean(age)) %>% 
        mutate(line = if_else(z == 1, 'treatment average age', 'control average age'))
      
      p1 <- dat %>% 
        ggplot(aes(age, fill = z)) + 
        geom_density(alpha = .6) + 
        theme(legend.position = 'none') + 
        facet_wrap(~ asn,ncol = 1) + 
        #geom_vline(data = means, aes(xintercept = point, linetype = line)) + 
        labs(title = 'Distribution of Age', x = element_blank()) + 
        scale_fill_manual(values = c('red', 'blue'))
        # geom_vline(xintercept = mean(dat$age[dat$z == 1]), aes(linetype = 'treatment average age')) + 
        # geom_vline(aes(xintercept = mean(dat$age[dat$z == 0]), linetype = 'control average age'))
      
      p2 <- dat %>% 
        ggplot(aes(as.factor(pro),  fill = z)) + 
        geom_bar(position = 'dodge', alpha = .7) + 
        scale_x_discrete(labels = c('Amature', 'Professional')) + 
        facet_wrap(~asn, ncol = 1) + 
        labs(title = "Distribution of Professional Runners", x = element_blank(), fill = element_blank()) +
        scale_fill_manual(values = c('red', 'blue'),labels=c('treatment - HyperShoes', 'contorl - No HyperShoes'))
      
      p3 <- dat %>% filter(asn == 'Random') %>% 
        ggplot(aes(age, fill = z)) + 
        geom_density(alpha = .6) + 
        theme(legend.position = 'none') + 
        facet_wrap(~asn, ncol = 1) + 
        #geom_vline(data = means, aes(xintercept = point, linetype = line)) + 
        labs(title = 'Distribution of Age', x = element_blank()) + 
        scale_fill_manual(values = c('red', 'blue'))
      
      p4 <- dat %>% filter(asn == 'Random') %>% 
        ggplot(aes(as.factor(pro),  fill = z)) + 
        geom_bar(position = 'dodge', alpha = .7) + 
        scale_x_discrete(labels = c('Amature', 'Professional')) + 
        facet_wrap(~asn, ncol = 1) + 
        labs(title = "Distribution of Professional Runners", x = element_blank(), fill = element_blank()) +
        scale_fill_manual(values = c('red', 'blue'),labels=c('treatment - HyperShoes', 'contorl - No HyperShoes'))
      
      p5 <- dat %>% filter(asn != 'Random') %>% 
        ggplot(aes(age, fill = z)) + 
        geom_density(alpha = .6) + 
        theme(legend.position = 'none') + 
        facet_wrap(~asn, ncol = 1) + 
        #geom_vline(data = means, aes(xintercept = point, linetype = line)) + 
        labs(title = 'Distribution of Age', x = element_blank()) + 
        scale_fill_manual(values = c('red', 'blue'))
      
      p6 <- dat %>% filter(asn != 'Random') %>% 
        ggplot(aes(as.factor(pro),  fill = z)) + 
        geom_bar(position = 'dodge', alpha = .7) + 
        scale_x_discrete(labels = c('Amature', 'Professional')) + 
        facet_wrap(~asn, ncol = 1) + 
        labs(title = "Distribution of Professional Runners", x = element_blank(), fill = element_blank()) +
        scale_fill_manual(values = c('red', 'blue'),labels=c('treatment - HyperShoes', 'contorl - No HyperShoes'))
      
      both <- p1 + p2
      rct <- p3 + p4
      obs <- p5 + p6
      list(both = both, rct = rct, obs = obs)
      #p1 + p2
      
      })
    
    output$balance <- renderPlot({
      plots <- plot_single_draw()
      if(length(input$rct) > 1) plots$both
      else if(input$rct == 'Random') plots$rct
      else plots$obs

    })
    
    
    

})
