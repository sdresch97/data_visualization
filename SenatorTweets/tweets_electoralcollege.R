## twitter 538 data

library(tidyverse)
library(ggthemes)
library(rvest)

url<-"https://en.wikipedia.org/wiki/United_States_presidential_election,_2016" # wikipedia page last updated 14 may 2018
temp<-read_html(url)
tables<-html_nodes(temp, "table")
electoralcollege<-html_table(tables[38], fill = TRUE)[[1]]

electoralcollege.1 <- electoralcollege[-1, ] 
electoralcollege.1 <- electoralcollege.1[,c(1,5,8)] 
names(electoralcollege.1) <- c("states", "clinton", "trump")
electoralcollege.1 <- electoralcollege.1 %>% 
  mutate(clinton = ifelse(grepl("-", clinton), 0, clinton),
         trump = ifelse(grepl("-", trump), 0, trump)) %>% 
  mutate(clinton = as.numeric(clinton),
         trump = as.numeric(trump)) %>% 
  mutate(state1 = ifelse(grepl("Maine", states), "Maine",
                         ifelse(grepl("Nebraska", states), "Nebraska", states))) %>% 
  filter(states != "District of Columbia",
         states != "U.S. Total") %>% 
  group_by(state1) %>% 
  summarise(votesclinton = sum(clinton),
            votestrump = sum(trump)) %>% 
  ungroup() %>% 
  mutate(votesclinton = ifelse(grepl("Nebraska", state1), NA, votesclinton),
         votestrump = ifelse(grepl("Nebraska", state1), NA, votestrump),
         votesclinton = ifelse(grepl("Maine", state1), NA, votesclinton),
         votestrump = ifelse(grepl("Maine", state1), NA, votestrump)) %>% 
  mutate(abb = state.abb,
         abb = as.factor(abb))
  

senators <- read.csv("senators.csv")


states_senators <- data.frame(table(senators$state)) %>% 
  mutate(Freq = as.numeric(Freq)) %>% 
  arrange(desc(Freq)) %>% 
  mutate(abb = as.factor(Var1))

senators_electoral <- merge(states_senators, electoralcollege.1) %>% 
  mutate(dem = ifelse(!is.na(votesclinton), 1, 0),
         rep = ifelse(!is.na(votestrump), 1, 0),
         neither = ifelse(abb == "NE" | abb == "ME", 1, 0),
         fill = ifelse(dem == 1, "dem",
                       ifelse(rep == 1, "rep", 
                              ifelse(neither == 1, "neither", NA)))) 

trumpmentions <- senators %>% 
  filter(grepl(".rump", text)) 

trumpmentions2 <-   data.frame(table(trumpmentions$state)) %>% 
  mutate(Freq = as.numeric(Freq)) %>% 
  mutate(abb = as.factor(Var1)) %>% 
  merge(electoralcollege.1) %>% 
  mutate(dem = ifelse(!is.na(votesclinton), 1, 0),
         rep = ifelse(!is.na(votestrump), 1, 0),
         neither = ifelse(abb == "NE" | abb == "ME", 1, 0),
         fill = ifelse(dem == 1, "dem",
                       ifelse(rep == 1, "rep", 
                              ifelse(neither == 1, "neither", NA)))) %>% 
  arrange(desc(Freq)) %>% 
  rename(trumptweets = Freq)
senators_electoral <- senators_electoral %>% 
  rename(totaltweets = Freq)

trumpmentions3 <- merge(trumpmentions2, senators_electoral) %>% 
  mutate(trumpratio = trumptweets / totaltweets * 100)

trumpmentions3 <- transform(trumpmentions3, state=reorder(Var1, trumpratio) )

ggplot(trumpmentions3) +
  geom_col(mapping = aes(x = state, y = trumpratio)) +
  geom_col(mapping = aes(x = state, y = trumpratio, fill = fill)) +
  scale_fill_manual(values = c("blue", "grey", "red")) +
  labs(x = "State", y = "% of Tweets that Mention Trump",
       title= "Percent of Tweets Posted from US Senators that Mention Trump Over a 2-day Period (October 19-20, 2017)",
       subtitle = "Colors indicate how each state voted in 2016 Presidential Election\nBlue = Clinton; Red = Trump; Grey = split electoral votes",
       caption = "Source: FiveThirtyEight, Wikipedia") +
  guides(fill = FALSE) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  theme_minimal()

ggsave("figure/senators_tweet_trump.jpeg", height = 8, width = 12)
