library(tidyverse)
library(Lahman)


war_pitch <- read_csv("war_daily_pitch.txt")
war_bat <- read_csv("war_daily_bat.txt")
cba <- read_csv("cba_features97-18.csv")


pitch <- war_pitch %>%
  select(player_ID, year_ID, WAA, WAR_rep, G) %>%
  mutate(WAR = as.numeric(WAR_rep) + as.numeric(WAA))

bat <- war_bat %>% filter(pitcher == "N") %>%
  select(player_ID, year_ID, WAA, WAR_rep, G) %>%
  mutate(WAR = as.numeric(WAR_rep) + as.numeric(WAA)) %>%
  mutate(WAR_rep = as.numeric(WAR_rep))

comb_war <- bind_rows(bat, pitch) %>% group_by(player_ID, year_ID) %>% top_n(1, G)


CPI <- read_csv("CPIAUCSL.csv")

CPI_year <- CPI %>%  mutate(year = format(DATE, "%Y")) %>% 
  mutate_if(is.character, as.integer) %>%
  group_by(year) %>% summarize(CPI_yr = mean(CPIAUCSL/100))


vet <- function(data){
  data$veteran <- 0
  
  for(i in unique(data$playerID)){
    c <- 0
    for(j in data[which(data$playerID == i),]$yearID){
      data[which(data$playerID == i & data$yearID == j),]$veteran <- ifelse(c < 7, 0, 1)
      c <- c+1
      #print(c(i, j, c, data[which(data$playerID == i & data$yearID == j),]$veteran))
    }
  }
  return(data)
}


POS <- Fielding %>% 
  group_by(playerID, yearID) %>% 
  top_n(1, G) %>%
  select(playerID, yearID, POS) %>%
  ungroup() %>%
  mutate(POS = as.factor(POS))

dataset <- Salaries %>%
  filter(salary > 0) %>% # Exclude zero salary so we can use log
  left_join(Master, by = "playerID") %>%
  mutate(Age = yearID - birthYear) %>%
  filter(yearID <= as.numeric(str_sub(finalGame, 1, 4))) %>% # Exclude errors in salary table
  left_join(POS, by = c("yearID", "playerID")) %>%
  left_join(comb_war, by = c("bbrefID" = "player_ID", "yearID" = "year_ID")) %>%
  left_join(CPI_year, by = c("yearID" = "year")) %>%
  left_join(cba, by = "yearID") %>%
  mutate(realSalary = salary/CPI_yr) %>% 
  select(yearID, teamID, POS, lgID, playerID, salary, birthYear, birthCountry, nameFirst, nameLast, Age, WAA, WAR_rep, WAR, realSalary, bbrefID, feat1, feat2, feat3, feat4, feat5, feat6, feat7) %>%
  vet()

# Filter to include the relavent player years. 
dataset3 <- dataset %>%
  filter(yearID > 1994)

summary(dataset3)

lmsal <- lm(log(realSalary) ~ WAR + yearID + veteran + feat1 + feat2 + feat5 + feat7 , data = dataset3)
summary(lmsal)



dataset3 %>% 
  select(Age, yearID, WAR, realSalary, veteran, feat1, feat2, feat5) %>% 
  sapply(as.numeric) %>%
  cor(use = "complete.obs") %>% round(3)%>% 
  as.data.frame() %>% 
  add_rownames("var1") %>%
  gather("var2", "cor", 2:9) %>%
  ggplot(aes(var1, var2, fill = cor)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_text(aes(var1, var2, label = cor), color = "black", size = 3)


dataset3 %>% 
  ggplot(aes(WAR)) +
  geom_density()



## Data Exploration Figures
ggplot(Salaries, aes(x=factor(yearID), y=salary)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("Annual Salary") +
  scale_x_discrete(breaks=unique(Salaries$yearID)[c(TRUE, FALSE)]) +
  scale_y_continuous(labels = dollar) + 
  ggtitle("Nominal Salary with Boxplots")

Salaries %>% 
  left_join(Fielding, by = c("yearID", "playerID")) %>%
  group_by(yearID, POS) %>% 
  summarize(avgPay = mean(salary)) %>% 
  ggplot(aes(x=yearID, y=avgPay, col=POS)) + 
  geom_point() + 
  geom_line() +
  xlab("Year") +
  ylab("Annual Salary") +
  scale_x_continuous() +
  scale_y_continuous(labels = dollar) + 
  ggtitle("Average Salary By Position and Year")

Salaries %>%
  left_join(Master, by = "playerID") %>%
  filter(yearID %% 5 == TRUE) %>%
  mutate(Age = yearID - birthYear) %>%
  filter(yearID <= as.numeric(str_sub(finalGame, 1, 4))) %>%
  group_by(Age, yearID) %>% #yearID
  left_join(CPI_year, by = c("yearID" = "year")) %>%
  mutate(realSalary = salary/CPI_yr) %>%
  summarize(avgPay = mean(realSalary)) %>%
  ggplot(aes(x=Age, y=avgPay, col=yearID, group = yearID)) + 
  #geom_point() + 
  geom_line() +
  xlab("Age") +
  ylab("Annual Real Salary") +
  scale_x_continuous() +
  scale_y_continuous(labels = dollar) + 
  ggtitle("Average Salary By Age")


Salaries %>%
  left_join(Master, by = "playerID") %>%
  filter(yearID %% 5 == TRUE) %>%
  mutate(Age = yearID - birthYear) %>%
  filter(yearID <= as.numeric(str_sub(finalGame, 1, 4))) %>%
  #mutate(yearID = as.character(yearID)) %>%
  group_by(Age, yearID) %>%
  add_count(Age) %>%
  ggplot(aes(x=Age, y=n, col=yearID, group=yearID)) + 
  geom_line() +
  xlab("Age") +
  ylab("Total Number of Players") +
  scale_x_continuous() +
  scale_y_continuous() + 
  ggtitle("MLB Players by Age")