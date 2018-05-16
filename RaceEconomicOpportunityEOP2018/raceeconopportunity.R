# race and economic opportunity
library(tidyverse)
eop1 <- read.csv("table_1.csv")

ggplot(eop1) +
  geom_line(aes(par_pctile, kid_jail_white_female, color = "springgreen4")) +
  geom_line(aes(par_pctile, kid_jail_black_female, color = "red")) +
  geom_line(aes(par_pctile, kid_jail_white_male, color = "blue")) +
  geom_line(aes(par_pctile, kid_jail_black_male, color = "black")) +
  labs(x = "Parent household income percentile rank",
      y = "Percentage of children incarcerated",
      title = "Race and Gender Disparities in Children's Incarceration Rates Based on Parents' Income",
      caption = "Source: Equality of Opportunity Project") +
  theme_light() +
  scale_color_discrete(name = "Race and Gender", 
                       labels = c("Black males",
                                  "White males",
                                  "Black females",
                                  "White females")) 

ggsave("incarceratedkids.jpeg", width = 8, height = 8)
