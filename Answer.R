library(tidyverse)

# load the data
df <- read_csv(file = "min_wage.csv")

# Comments: typically, when we do empirical work, there is always a "data cleaning" process before we do data analysis. This data also needs cleaning. I won't do the cleaning, so you can see how it looks when we won't. Notice that many piece are repetitive. For instance, categorical variables (called "factors" in R) are written as numbers (e.g. the states), of numeric variables are written as categorical (e.g. wage_st is writte as categorical but it is a number). These type of thigs are usually fixes at the beginning. Look for the "recode" or "mutate" that are simply rewriting our variables in a more convenient way. Thesethings can (and should) be done at the beginning.

# basic examination of the data
str(df)
glimpse(df)
summary(df)
head(df)

# 1) 
# total number of stores
df %>% select(store) %>% unique() %>% summarise(n = n())
# stores in NJ
df %>% filter(state==1) %>% select(store) %>% unique() %>% summarise(n = n())
# stores in PA
df %>% filter(state==0) %>% select(store) %>% unique() %>% summarise(n = n())

# all at once
df %>% group_by(state) %>% select(store) %>% unique() %>% summarise(n=n())

# in each survey
df %>% filter(time==0) %>% select(store) %>% unique() %>% summarise(n=n())
df %>% filter(time==1) %>% select(store) %>% unique() %>% summarise(n=n())
df %>% filter(time==0) %>% group_by(state) %>% select(store) %>% unique() %>% summarise(n=n())
df %>% filter(time==1) %>% group_by(state) %>% select(store) %>% unique() %>% summarise(n=n())

# graph
df %>% mutate(state=recode(state, `0`="PA", `1`="NJ")) %>% 
  group_by(state) %>% select(store) %>% unique() %>% summarise(n=n()) %>% 
ggplot() + 
  geom_bar(mapping = aes(x=state, y=n), stat = "identity") +
  theme_classic()
  
df %>% mutate(state=recode(state, `0`="PA", `1`="NJ"),
              chain=recode(chain, `1`="Burguer_King", `2`="KFC", `3`="Roys", `4`="Wendys")) %>% 
  group_by(state, chain) %>% select(store) %>% unique() %>% summarise(n=n()) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x=state, y=n, fill=chain), stat = "identity") +
  theme_classic()

# 2) Distribution of starting wages
# before-PA
df %>% filter(state == 0, time == 0) %>% 
  select(wage_st) %>% mutate(wage_st = round(as.numeric(wage_st),1)) %>% 
  mutate(wage_st = as.factor(wage_st)) %>% 
  ggplot() + geom_bar(mapping = aes(x=wage_st))
# after-PA
df %>% filter(state == 0, time == 1) %>% 
  select(wage_st) %>% mutate(wage_st = round(as.numeric(wage_st),1)) %>% 
  mutate(wage_st = as.factor(wage_st)) %>% 
  ggplot() + geom_bar(mapping = aes(x=wage_st))
# before-NJ
df %>% filter(state == 1, time == 0) %>% 
  select(wage_st) %>% mutate(wage_st = round(as.numeric(wage_st),1)) %>% 
  mutate(wage_st = as.factor(wage_st)) %>% 
  ggplot() + geom_bar(mapping = aes(x=wage_st))
# after-NJ
df %>% filter(state == 1, time == 1) %>% 
  select(wage_st) %>% mutate(wage_st = round(as.numeric(wage_st),1)) %>% 
  mutate(wage_st = as.factor(wage_st)) %>% 
  ggplot() + geom_bar(mapping = aes(x=wage_st))

# 3) FTE
df <- df %>% mutate(empft = as.numeric(empft),
                    emppt = as.numeric(emppt)) %>% 
  mutate(FTE = empft+0.5*emppt)

# 4) averages
NJ0 <- df %>% filter(state == 1, time == 0) %>% summarise(avg = mean(FTE, na.rm=TRUE)) %>% pull()
NJ1 <- df %>% filter(state == 1, time == 1) %>% summarise(avg = mean(FTE, na.rm=TRUE)) %>% pull()
PA0 <- df %>% filter(state == 0, time == 0) %>% summarise(avg = mean(FTE, na.rm=TRUE)) %>% pull()
PA1 <- df %>% filter(state == 0, time == 1) %>% summarise(avg = mean(FTE, na.rm=TRUE)) %>% pull()

# 5) DiD
(NJ1-NJ0)-(PA1-PA0)

# 6) DiD regression
reg <- lm(formula = FTE ~ state*time, data = df)
summary(reg)

# 7) DiD with controls
reg_2 <- lm(formula = FTE ~ state*time + as.factor(chain) + as.factor(co_owned), data = df)
summary(reg)

