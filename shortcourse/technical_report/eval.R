library(tidyverse)
da <- read.table("eval.txt") %>% 
  setNames(c("Class rhythm", "Presentation", "Content", "Material")) %>% 
  gather() %>% 
  group_by(value) %>% 
  count(key)

png("results.png", 
    width = 200, height = 120, units = 'mm', res = 300)

da %>% 
  ggplot(aes(value, n)) +
  geom_bar(fill = "#c03728", stat = "identity") +
  facet_wrap(~key) +
  labs(y = "Counts", x = "Note given") +
  xlim(1, 5.5) +
  theme_bw()


dev.off()
