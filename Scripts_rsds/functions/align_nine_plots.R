align_nine_plots <- function(list.plots, 
                             family = "",
                             labels=rep("",6), 
                             labels.size=8){
  
  require(tidyverse)
  require(gridExtra)
  
  gg <- ggplot()+
    coord_equal(xlim = c(0, 12), ylim = c(0, 12), expand = c(0,0)) +
    annotation_custom(ggplotGrob(list.plots[[7]]),
                      xmin = 0.25, xmax = 3.75, ymin = 0.25, ymax = 3.75) +
    annotation_custom(ggplotGrob(list.plots[[8]]),
                      xmin = 4.25, xmax = 7.75, ymin = 0.25, ymax = 3.75) +
    annotation_custom(ggplotGrob(list.plots[[9]]),
                      xmin = 8.25, xmax = 11.75, ymin = 0.25, ymax = 3.75) +
    annotation_custom(ggplotGrob(list.plots[[4]]),
                      xmin = 0.25, xmax = 3.75, ymin = 4.25, ymax = 7.75) +
    annotation_custom(ggplotGrob(list.plots[[5]]),
                      xmin = 4.25, xmax = 7.75, ymin = 4.25, ymax = 7.75) +
    annotation_custom(ggplotGrob(list.plots[[6]]),
                      xmin = 8.25, xmax = 11.75, ymin = 4.25, ymax = 7.75) +
    annotation_custom(ggplotGrob(list.plots[[1]]),
                      xmin = 0.25, xmax = 3.75, ymin = 8.25, ymax = 11.75) +
    annotation_custom(ggplotGrob(list.plots[[2]]),
                      xmin = 4.25, xmax = 7.75, ymin = 8.25, ymax = 11.75) +
    annotation_custom(ggplotGrob(list.plots[[3]]),
                      xmin = 8.25, xmax = 11.75, ymin = 8.25, ymax = 11.75) +
    theme_void()
  
  
  # DF with the coordinates of the 5 arrows
  # df.arrows <- data.frame(id=1:9,
  #                         x =    c(3.75, 6,      7.6875, 6,    6,    6,    2,    6,    10),
  #                         xend = c(4.25, 7.6875, 8.25,   2,    6,    10,   2,    6,    10),
  #                         y =    c(10,   8.25,   9.0625,  8.25, 8.25, 8.25, 4.25, 4.25, 4.25),
  #                         yend = c(10,   9.0625,  10,     7.75, 7.75, 7.75, 3.75, 3.75, 3.75))
  
  df.arrows <- data.frame(id=1:25,
                          x =    c(3.75, 4.25, 6,    6,    10,   10,   0.25,  11.75, 11.75, 0.25,  2,    6,    10,   0.25, 3.74,  3.74,  0.25,  4.26,  7.75,  7.75, 4.26,  8.25,  11.75, 11.75, 8.25),
                          xend = c(4.25, 3.75, 6,    6,    10,   10,   11.75, 11.75, 0.25,  0.25,  2,    6,    10,   3.74, 3.74,  0.25,  0.25,  7.75,  7.75,  4.26, 4.26,  11.75, 11.75, 8.25,  8.25),
                          y =    c(10,   10,   8.25, 7.75, 8.25, 7.75, 7.74,  7.74,  0.1,   0.1,   4.25, 4.25, 4.25, 11.75, 11.75, 8.25, 8.25,  11.75, 11.75, 8.25, 8.25,  11.75, 11.75, 8.25,  8.25),
                          yend = c(10,   10,   7.75, 8.25, 7.75, 8.25, 7.74,  0.1,   0.1,   7.74,  3.75, 3.75, 3.75, 11.75, 8.25,  8.25, 11.75, 11.75, 8.25,  8.25, 11.75, 11.75, 8.25,  8.25,  11.75))
  
  # add arrows
  gg <- gg +
    geom_segment(data = df.arrows %>% filter(id < 7 | id > 10 & id < 14),
                 aes(x=x,y=y,xend=xend,yend=yend),
                 arrow = arrow(length = unit(0.25,"cm")),
                 col = 'dimgrey',
                 size=1.1) +
    geom_segment(data = df.arrows %>% filter(id >= 7 & id <= 10 | id >= 14),
                 aes(x=x,y=y,xend=xend,yend=yend),
                 col = 'dimgrey',
                 size=1.1)
  
  # add labes
  gg <- gg + annotate('text',label = labels,
                      x=c(.5,12.5,12.5,.5,.5,12.5)+.5,
                      y=c(29,27.5,18.5,17,8,8)+.1,
                      size=labels.size,hjust=0, vjust=0, family = family)
  
  return(gg)
}

align_cmip6_plots <- function(list.plots, 
                              family = "",
                              labels=rep("",6), 
                              labels.size=8){
  
  require(tidyverse)
  require(gridExtra)
  
  gg <- ggplot() + 
    coord_equal(xlim = c(0, 12), ylim = c(0, 12), expand = c(0,0)) +
    annotation_custom(ggplotGrob(list.plots[[1]]),
                      xmin = 0.25, xmax = 7.75, ymin = 8.25, ymax = 11.75) +
    annotation_custom(ggplotGrob(list.plots[[2]]),
                      xmin = 0.25, xmax = 3.75, ymin = 4.25, ymax = 7.75) +
    annotation_custom(ggplotGrob(list.plots[[3]]),
                      xmin = 4.25, xmax = 7.75, ymin = 4.25, ymax = 7.75) +
    annotation_custom(ggplotGrob(list.plots[[4]]),
                      xmin = 8.25, xmax = 11.75, ymin = 8.25, ymax = 11.75) +
    annotation_custom(ggplotGrob(list.plots[[5]]),
                      xmin = 8.25, xmax = 11.75, ymin = 4.25, ymax = 7.75) +
    annotation_custom(ggplotGrob(list.plots[[6]]),
                      xmin = 0.25, xmax = 3.75, ymin = 0.25, ymax = 3.75) +
    annotation_custom(ggplotGrob(list.plots[[7]]),
                      xmin = 4.25, xmax = 7.75, ymin = 0.25, ymax = 3.75) +
    annotation_custom(ggplotGrob(list.plots[[8]]),
                      xmin = 8.25, xmax = 11.75, ymin = 0.25, ymax = 3.75) +
    theme_void()
  

  
  df.lines <- data.frame(x =    c(.25,   7.75,  7.75, .25),
                         xend = c(7.75,  7.75,  .25,  .25),
                         y =    c(11.42, 11.45, 4.25, 4.25),
                         yend = c(11.42, 4.25,  4.25, 11.42))
  
  # add boxes
  gg <- gg +
    geom_segment(data = df.lines,
                 aes(x=x,y=y,xend=xend,yend=yend),
                 col = 'dimgrey',
                 size=1.1)
  
  # add labes
  gg <- gg + annotate('text',label = labels,
                      x=c(.5,12.5,12.5,.5,.5,12.5)+.5,
                      y=c(29,27.5,18.5,17,8,8)+.1,
                      size=labels.size,hjust=0, vjust=0, family = family)
  
  return(gg)
}

dry_wet_plots <- function(list.plots, 
                          family = "",
                          labels=labels, 
                          labels.size=7.5){
  
  require(tidyverse)
  require(gridExtra)
  
  # gg <- ggplot() + 
  #   coord_equal(xlim = c(0, 12), ylim = c(0, 18), expand = c(0,0)) +
  #   annotation_custom(ggplotGrob(list.plots[[1]]),
  #                     xmin = 0.25, xmax = 11.75, ymin = 16.15, ymax = 17.85) +
  #   annotation_custom(ggplotGrob(list.plots[[2]]),
  #                     xmin = 0.25, xmax = 11.75, ymin = 14.15, ymax = 15.85) +
  #   annotation_custom(ggplotGrob(list.plots[[3]]),
  #                     xmin = 0.25, xmax = 11.75, ymin = 12.15, ymax = 13.85) +
  #   annotation_custom(ggplotGrob(list.plots[[4]]),
  #                     xmin = 0.25, xmax = 3.75, ymin = 10.15, ymax = 11.85) +
  #   annotation_custom(ggplotGrob(list.plots[[5]]),
  #                     xmin = 0.25, xmax = 3.75, ymin = 8.15, ymax = 9.85) +
  #   annotation_custom(ggplotGrob(list.plots[[6]]),
  #                     xmin = 0.25, xmax = 3.75, ymin = 6.15, ymax = 7.85) +
  #   annotation_custom(ggplotGrob(list.plots[[7]]),
  #                     xmin = 4.25, xmax = 7.75, ymin = 10.15, ymax = 11.85) +
  #   annotation_custom(ggplotGrob(list.plots[[8]]),
  #                     xmin = 4.25, xmax = 7.75, ymin = 8.15, ymax = 9.85) +
  #   annotation_custom(ggplotGrob(list.plots[[9]]),
  #                     xmin = 4.25, xmax = 7.75, ymin = 6.15, ymax = 7.85) +
  #   annotation_custom(ggplotGrob(list.plots[[10]]),
  #                     xmin = 8.25, xmax = 11.75, ymin = 10.15, ymax = 11.85) +
  #   annotation_custom(ggplotGrob(list.plots[[11]]),
  #                     xmin = 8.25, xmax = 11.75, ymin = 8.15, ymax = 9.85) +
  #   annotation_custom(ggplotGrob(list.plots[[12]]),
  #                     xmin = 8.25, xmax = 11.75, ymin = 6.15, ymax = 7.85) +
  #   annotation_custom(ggplotGrob(list.plots[[1]]),
  #                     xmin = 0.25, xmax = 11.75, ymin = 4.15, ymax = 5.85) +
  #   annotation_custom(ggplotGrob(list.plots[[2]]),
  #                     xmin = 0.25, xmax = 11.75, ymin = 2.15, ymax = 3.85) +
  #   annotation_custom(ggplotGrob(list.plots[[3]]),
  #                     xmin = 0.25, xmax = 11.75, ymin = 0.15, ymax = 1.85) +
  #   theme_void()
  
  gg <- ggplot() + 
    # coord_equal(xlim = c(0, 12), ylim = c(0, 18)) +
    coord_equal(xlim = c(0, 12), ylim = c(0, 18), expand = c(0,0)) +
    annotation_custom(ggplotGrob(list.plots[[1]]),
                      xmin = 0.25, xmax = 11.75, ymin = 12.15, ymax = 17.55) +
    # annotation_custom(ggplotGrob(list.plots[[2]]),
    #                   xmin = 0.25, xmax = 3.75, ymin = 6.15, ymax = 11.85) +
    # annotation_custom(ggplotGrob(list.plots[[3]]),
    #                   xmin = 4.25, xmax = 7.75, ymin = 6.15, ymax = 11.85) +
    # annotation_custom(ggplotGrob(list.plots[[4]]),
    #                   xmin = 8.25, xmax = 11.75, ymin = 6.15, ymax = 11.85) +
    annotation_custom(ggplotGrob(list.plots[[2]]),
                      xmin = 0.25, xmax = 4.75, ymin = 6.3, ymax = 11.7) +
    annotation_custom(ggplotGrob(list.plots[[3]]),
                      xmin = 5.25, xmax = 8, ymin = 6.3, ymax = 11.7) +
    annotation_custom(ggplotGrob(list.plots[[4]]),
                      xmin = 8.5, xmax = 11.75, ymin = 6.3, ymax = 11.7) +
    annotation_custom(ggplotGrob(list.plots[[5]]),
                      xmin = 0.25, xmax = 11.75, ymin = 0.45, ymax = 5.85) +
    theme_void()
  
  
  
  df.lines <- data.frame(x =    c(.25,   11.75,  11.75,  .25,
                                  .25,    4.75,   4.75,  .25,
                                  5.25,      8,      8, 5.25,
                                   8.5,  11.75,  11.75,  8.5,
                                  .25,   11.75,  11.75,  .25),
                         xend = c(11.75,  11.75,  .25,  .25,
                                   4.75,   4.75,  .25,  .25,
                                      8,      8, 5.25, 5.25,
                                  11.75,  11.75,  8.5,  8.5,
                                  11.75,  11.75,  .25,  .25),
                         y =    c(17.55, 17.55, 12.15, 12.15,
                                  11.70, 11.70,   6.3,   6.3,
                                  11.70, 11.70,   6.3,   6.3,
                                  11.70, 11.70,   6.3,   6.3,
                                   5.85,  5.85,  0.45,  0.45),
                         yend = c(17.55, 12.15,  12.15, 17.55,
                                  11.70,   6.3,    6.3, 11.70,
                                  11.70,   6.3,    6.3, 11.70,
                                  11.70,   6.3,    6.3, 11.70,
                                   5.85,  0.45,   0.45,  5.85))
  
  df.arrows <- data.frame(x =    c(4.75,   8,  10.125),
                          xend = c(5.25, 8.5,  10.125),
                          y =    c(9,       9,    6.3),
                          yend = c(9,       9,   5.85))

  # add arrows
  gg <- gg +
    geom_segment(data = df.lines,
                 aes(x=x,y=y,xend=xend,yend=yend),
                 col = 'dimgrey',
                 size=1.1)
  
  # add arrows
  gg <- gg +
    geom_segment(data = df.arrows,
                 aes(x=x,y=y,xend=xend,yend=yend),
                 arrow = arrow(length = unit(0.25,"cm")),
                 col = 'dimgrey',
                 size=1.1)

  # add labes
  gg <- gg + annotate('text',label = labels,
                      x=c(6, 2.5, 6.625, 10.125, 6),
                      y=c(17.55, 11.7, 11.7, 11.7, 5.85)+.05,
                      size=labels.size,hjust=0.5, vjust=0, family = family)
  
  return(gg)
}
