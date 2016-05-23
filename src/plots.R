library(dplyr)
library(ggplot2)
library(stringr)
setwd("~/repos/conll-2016/src")

size <- Vectorize(function(exp){
  if (str_sub(exp, 11, 11) == "5") {
    return(3)
  } else if (str_sub(exp, 11,11) == "6") {
    return(1)
  } else if (str_sub(exp, 11,11) == "7") {
    return(2)
  } else if (str_sub(exp, 11, 11) == "8") {
    return(4)
  } else if (str_sub(exp, -4) == "-res") {
    return(5)
  } else {
    return(1)
  }
})

parse_exp <- function(data) {
  data %>% filter(str_sub(exp,-7) != "ipa-res") %>%
    mutate(Type = str_sub(exp, -4)) %>%
    mutate(Size = size(exp)) %>%
    mutate(Type = plyr::mapvalues(Type, from=c("-res","-sum","word"), to=c("Phon GRU", "Word Sum", "Word GRU"))) %>%
    mutate(Model = paste(Type, Size)) %>%
    mutate(Model = plyr::mapvalues(Model, from="Word Sum 1", to="Word Sum"))
}
data <- read.table("data.txt", col.names = c("epoch","item","x","split","loss","exp")) %>% 
  filter(epoch < 8) %>%
  parse_exp()

MAX <- max(data$item)
ggplot(data=data %>% filter(epoch < 3 & Model %in% c("Word Sum","Word GRU 1", "Phon GRU 3")),
       aes(x=(epoch-1) * MAX + item, y=loss, color=Model, shape=Model)) + 
  geom_point(alpha=0.5) + 
  #  geom_vline(xintercept = (data$epoch-1) * MAX, color='gray') +
  #ylim(c(0.48,0.6)) + 
  scale_x_continuous(breaks=0:8 * MAX, labels=0:8) +
  xlab("Epoch") +
  ylab("Loss") +
  theme(aspect.ratio=2/3, text=element_text(size=25))
ggsave("../loss.png")

# Zoom in

ggplot(data=data %>% filter(Model %in% c("Word Sum","Word GRU 1", "Phon GRU 3")) , 
       aes(x=(epoch-1) * MAX + item, y=loss, color=Model, shape=Model)) + 
  geom_point(alpha=0.5) + 
  ylim(0.49, 0.6) +
  scale_x_continuous(breaks=0:8 * MAX, labels=0:8) +
  xlab("Epoch") +
  ylab("Loss") +
  theme(aspect.ratio=2/3, text=element_text(size=25))
ggsave("../loss-zoom.pdf")

data %>% group_by(Model) %>% summarize(min_loss=(min(loss))) %>% ggplot(aes(y=min_loss, x=Model)) + geom_boxplot()

recall <- read.table("recall.txt", header=FALSE, 
                     col.names=c("epoch","score","exp"))
  
recall  %>% group_by(exp) %>% summarize(max_score=max(score)) %>%
  parse_exp() %>%
#  mutate(Model=reorder(Model, max_score)) %>%
  ggplot(aes(x=Model, y=max_score)) + 
  geom_point(size=3, alpha=0.5) +
  coord_flip() +
  ylab("Accuracy at 5") +
  theme(aspect.ratio=2/2.5, text=element_text(size=25))
ggsave("../accat5.pdf")

recall %>% parse_exp() %>% group_by(exp) %>% ggplot(aes(x=Model, y=score)) + geom_boxplot()
