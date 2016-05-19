library(dplyr)
library(ggplot2)
library(stringr)
setwd("~/repos/conll-2016/src")
data <- read.table("data.txt", col.names = c("epoch","item","x","split","loss","exp")) %>% 
  filter(epoch < 8) %>%
  filter(str_sub(exp,-7) != "ipa-res") %>%
  mutate(Model = str_sub(exp, -4)) %>%
  mutate(Model = plyr::mapvalues(Model, from=c("-res","-sum","word"), to=c("Phon GRU", "Word GRU", "Word Sum")))

MAX <- max(data$item)
ggplot(data=data, aes(x=(epoch-1) * MAX + item, y=loss, color=Model)) + 
  geom_point(alpha=0.33) + 
  #  geom_vline(xintercept = (data$epoch-1) * MAX, color='gray') +
  #ylim(c(0.48,0.6)) + 
  scale_x_continuous(breaks=0:8 * MAX, labels=0:8) +
  xlab("Epoch") +
  ylab("Loss") +
  theme(aspect.ratio=2/3, text=element_text(size=25))
ggsave("../loss.png")

