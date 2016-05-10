library(dplyr)
library(ggplot2)
data <- read.table("data.txt", col.names = c("epoch","item","x","split","loss","exp"))
MAX <- max(data$item)
ggplot(data=data, aes(x=(epoch-1) * MAX + item, y=loss, color=exp)) + 
  geom_point(size=2, alpha=0.3) + 
  geom_line() + 
  ylim(c(0.48,0.6)) + 
  xlab("t")
