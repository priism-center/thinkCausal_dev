theme_timeline <- function(){
  theme_classic() + 
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top"
  ) 
}


