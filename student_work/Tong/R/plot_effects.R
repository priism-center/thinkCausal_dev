
plot_effects <- function(dat_out){
  
  ggplot(dat_out, aes(x = Category, y = Estimate)) + 
    geom_point(aes(color = Category), size = 5) + 
    geom_errorbar(aes(ymin = Estimate - Std.Dev, ymax = Estimate + Std.Dev), 
                  width = .2,
                  position = position_dodge(.9),
                  size = .5) +
    theme_minimal() + 
    xlab("") +
    ylab("Estimated Treatment Effects")
  
}