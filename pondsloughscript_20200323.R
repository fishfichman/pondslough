#South Bay Beach Seine 2015 - Ponds v Sloughs
#LEZZGO - Rachel Fichman 
        # started: 11/??/2019
#recently updated: 02/24/2020

library(here)
library(tidyverse)

#allyearsremoved.csv contains the data that Hobbs removed before doing these analyses 
  #(he said to me that it was too much close replication or data looked weird)
#allyears.csv is the dataset but including all of hobbs' excel manually calculated totals for fish
  #gonna code it out and do it all in R babyyyy


rough_pondslough <- read.csv(file="allyearsclean.csv", header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE) %>%
#%>% replace(is.na(26:99), 0)
  rename(towdate = date, polstn = station) %>%
  mutate(seasonr= case_when(month == "4"~"W",
                          month == "3"~"W",
                          month == "2"~"W",
                          month == "1"~"W",
                          month == "12"~"W",
                          month == "5"~"Su",
                          month == "6"~"Su",
                          month == "7"~"Su",
                          month == "8"~"Su",
                          month == "9"~"F",
                          month == "10"~"F",
                          month == "11"~"F"))

long_pondslough <- rough_pondslough %>%
  gather(code, catch, -id, -calyr, -surveyyr, -surveynum, -towdate, -month, -season, -polstn, -lat, -long, -site, -sitetype, -floatct, -depthm, -depthf, -sec, -doc, -dop, -sal, -con, -temp, -time, -tideht, -method, -net, -seasonr) # %>% #make long format
  #drop_na(catch) #drop nas in catch (zeros for code, ie. no catch for that species)

speciesmeta <- read.csv(file="SpeciesMetaData_20200221.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE) %>%
  select(code, comname, family, gensp, fishinv, group, hab, natinv, notes)

pondslough  <- left_join(long_pondslough, speciesmeta, "code")

simple2015 <- pondslough %>%
  filter(., surveyyr==2015) %>%
  select(1, 4, 5, 6, 26, 8:12, 24, 27:36)


n <- ggplot(simple2015, aes(sitetype, color = natinv)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ seasonr)
n

#________________________________________________________________________________

#separate out just the 2015 survery year ---- no longer works for long form
df2015<- rough_pondslough %>% 
  #mutate(count = sum()) %>%
  filter(surveyyr=="2015") %>%
  data.frame %>%  print
#checks with the original season delin.
check2015 <- df2015 %>%
  group_by(season, sitetype) %>% 
  summarise(count = length(sitetype))

check2015$season <- as.character(check2015$season)
check2015$season <- factor(check2015$season, levels=c("Sp", "Su", "F", "W"))
#________________________________________________________________________________

#alter season delineations to match the report - has no spring season and includes months 2,3,6,7,8,9,10 only
df2015r<- rough_pondslough %>% 
  #mutate(count = sum()) %>%
  filter(surveyyr=="2015") %>%
  data.frame %>%  print

check2015r <- df2015r %>%
  group_by(seasonr, sitetype) %>% 
  summarise(count = length(sitetype))

check2015r$seasonr <- as.character(check2015r$seasonr)
check2015r$seasonr <- factor(check2015r$seasonr, levels=c("Su", "F", "W"))

cr <- ggplot(check2015r, aes(x=seasonr, y=sitetype)) +
  geom_tile(aes(fill=count)) +
  geom_text(aes(label = round(count, 1))) +
  theme_classic() +
  scale_fill_gradient( low = "pink", high = "red") +
  ggtitle("Site Type by Season")
cr
ggsave("typexseason.png", cr, bg = "transparent")

#________________________________________________________________________________
#chart of site type compared to number of surveys in each by survey
c <- ggplot(check2015, aes(x=season, y=sitetype)) +
  geom_tile(aes(fill=count)) +
  geom_text(aes(label = round(count, 1))) +
  theme_classic() +
  scale_fill_gradient( low = "pink", high = "red") +
  ggtitle("Site Type by Season")
c
#ggsave("typexseason2.png", c, bg = "transparent")

checksite <- df2015 %>%
  group_by(season, site) %>% 
  summarise(count = length(surveynum))

s <- ggplot(checksite, aes(x=season, y=site)) +
  geom_tile(aes(fill=count)) +
  geom_text(aes(label = round(count, 1))) +
  theme_classic() +
  scale_fill_gradient( low = "pink", high = "red") +
  ggtitle("Site by Season")
s
#ggsave("sitexseason2.png", s, bg = "transparent")

checkmonth <- df2015 %>%
  group_by(month, sitetype) %>% 
  summarise(count = length(sitetype))

checkmonth$month <- as.character(checkmonth$month)
checkmonth$month <- factor(checkmonth$month, levels=c("4", "5", "6", "7", "8", "9", "10", "11", "12", "1", "2", "3")) 
m <- ggplot(checkmonth, aes(x=month, y=sitetype)) +
  geom_tile(aes(fill=count)) +
  geom_text(aes(label = round(count, 1))) +
  theme_classic() +
  geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  scale_fill_gradient( low = "pink", high = "red") +
 # scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  ggtitle("Site Type by Month")
m