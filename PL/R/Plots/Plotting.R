# Example of use:
# p <- make_plot(datasets::LifeCycleSavings)
# p + aes(x=sr, y=pop15) + geom_point(size = 3)

library(ggplot2)

make_plot <- function(data) {
  p <- ggplot(data=data)
  p <- p + theme(axis.line = element_line(size=2),
                 axis.text = element_text(size = 20),
                 axis.title = element_text(size=24)
  )
  
  return(p)
}