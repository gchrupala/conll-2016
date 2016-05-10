library(dplyr)
library(ggplot2)
library(stringr)
data <- read.table("data.txt", col.names = c("epoch","item","x","split","loss","exp")) %>% 
  mutate(kind = str_sub(exp, -4))
MAX <- max(data$item)
ggplot(data=data, aes(x=(epoch-1) * MAX + item, y=loss, color=kind)) + 
  geom_point(alpha=0.3) + 
  #geom_line() + 
  geom_vline(xintercept = (data$epoch-1) * MAX, color='gray') +
  ylim(c(0.48,0.6)) + 
  xlab("t") 
