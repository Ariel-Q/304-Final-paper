#R setup
library(haven)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(here)
library(readr)
library(knitr)
knitr::opts_chunk$set(warning=FALSE, message=FALSE, echo = FALSE)

#load the raw data

raw <- read.csv(here("inputs/depression data.csv"))
raw_child <- read.csv(here("inputs/childhood.csv")) %>% filter(Year != 2011) %>% filter(Strata != "Education", Strata != "Health Insurance") %>% arrange(Year)
raw_suicide <- read.csv(here("inputs/suicide.csv")) %>% filter(Geography == "CALIFORNIA")
year <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2012-2014", "2015-2017")
raw_suicide <- raw_suicide %>% filter(Year %in% year)

#clean up the data and seperate by differnet population groups
sex <- raw %>% filter(Strata == "Sex")
age <- raw %>% filter(Strata == "Age")
race <- raw %>% filter(Strata == "Race-Ethnicity")
education <- raw %>% filter(Strata == "Education")
income <- raw %>% filter(Strata == "Income")
total <- raw %>% filter(Strata == "Total")
sex_s <- raw_suicide %>% filter(Strata == "Sex")
race_s <- raw_suicide %>% filter(Strata == "Race/Ethnicity")
total_s <- raw_suicide %>% filter(Strata.Name == "Total")

#prepare data for the summry of depression and suicide from 2012-2017
c1217 <- c("2012", "2013", "2014", "2015", "2016", "2017")
suicide_1217 <- raw_suicide %>% filter(Year %in% c1217) %>% filter(Strata.Name != "Total")
depression_1217 <- sex %>% filter(Year != 2018)



#generate figure 1-5, figures for overview of depression data
total %>% ggplot(aes(x=Year, y=Percent)) +
  geom_point() +
  geom_smooth(method='lm' , color="purple", fill="#69b3a2", se=TRUE) +labs(
    x = "Year",
    y = "Percent of population",
    title = "Total Percent of population that reported have depressive disorder",
    caption = "data collected from Let's Get Healthy California indicator"
  ) +scale_x_continuous(breaks = seq(2010, 2020, 1))+ scale_y_continuous(breaks = seq(0, 20, 2))+
  theme_classic()

ggplot(sex, aes(fill=Strata.Name, y=Percent, x=Year)) + 
  geom_bar(position="dodge", stat="identity")+guides(fill=guide_legend(title="Gender"))+
  labs(
    x = "Year",
    y = "Percent of population",
    title = "Percent of population with depression by sex in California from 2012-2018",
    caption = "data collected from Let's Get Healthy California indicator")

income %>%
  ggplot(aes(
    y = Percent,
    x = Year,
    group = Strata.Name,
    color = Strata.Name,
    
  )) +
  geom_line() + 
  guides(color = guide_legend(title = "Income")) +
  labs(
    x = "Year",
    y = "Percent of population",
    title = "Percent of population with depression by income in California from 2012-2018",
    caption = "data collected from Let's Get Healthy California indicator"
  )

race %>%
  ggplot( aes(y=Percent, x=Year, group=Strata.Name, color=Strata.Name, )) +
  geom_line()+ guides(color = guide_legend(title = "Race"))+
  labs(
    x = "Year",
    y = "Percent of population",
    title = "Percent of population with depression by race in California from 2012-2018",
    caption = "data collected from Let's Get Healthy California indicator")

education %>%ggplot( aes(y=Percent, x=Year, group=Strata.Name, color=Strata.Name)) +
  geom_line()+ guides(color = guide_legend(title = "Education level"))+
  labs(
    x = "Year",
    y = "Percent of population",
    title = "Percent of population with depression by education level in California from 2012-2018",
    caption = "data collected from Let's Get Healthy California indicator")

#clean suicide data
d_s <- total[c(1,6)] 
d_s <- d_s %>% filter(Year != 2018)
s_partial <- total_s[1:6,]
d_s$Percent_s <- s_partial$Rate
s_partial <- transform(s_partial, Year = as.numeric(Year))

#generate figures for suicide data, figure 6-7
s_partial%>% ggplot(aes(x=Year, y=Rate)) +
  geom_point() +
  geom_smooth(method='lm' , color="purple", fill="#69b3a2", se=TRUE) +labs(
    x = "Year",
    y = "Percent of population",
    title = "Total Percent of death that caused by suicide",
    caption = "data collected from Let's Get Healthy California indicator"
  ) + theme_classic()

ggplot(suicide_1217, aes(fill=Strata.Name, y=Rate, x=Year)) + 
  geom_bar(position="dodge", stat="identity")+guides(fill=guide_legend(title="Gender"))+
  labs(
    x = "Year",
    y = "Percent of population",
    title = "Percent of population that suicide in California from 2012-2017 by Sex",
    caption = "data collected from Let's Get Healthy California indicator")

#clean suicide data by race for table 1
RC <- c("Asian", "Black", "White", "Hispanic", "Other")
RCS <- race_s %>% filter(Strata.Name == "Asian-NH"|Strata.Name == "Black-NH"|Strata.Name == "White-NH"|Strata.Name == "Hisp")
RCS_1 <- race_s %>% filter(Strata.Name != "Asian-NH"&Strata.Name != "Black-NH"&Strata.Name != "White-NH"&Strata.Name != "Hisp")
RCS_o <- data.frame(c("2012-2014", "2015-2017"), c("other", "other"), c(8.741, 9.009))
RCS <- RCS %>% select(Year, Strata.Name, Rate)
names(RCS_o) <- c("Year", "Strata.Name", "Rate")
RCS <- rbind(RCS, RCS_o) %>% arrange(Year, desc(Rate))

#generate table 1
knitr::kable(RCS, col.names = c("Year", "Race", "Percent of Suicide"), caption = "Percent of Population that Suicide by Race in 2012-2017", digits = 2, align=c(rep('c',times=3)))
}
fig8 <- raw_child %>% filter(Strata == "Total population") %>% ggplot(aes(y=Rate, x=Year)) + 
  geom_bar(position="dodge", stat="identity")+scale_x_continuous(breaks = seq(2013, 2025, 2))+
  labs(
    x = "Year",
    y = "Percent of population",
    title = "Total")


#generate figure8 for childhood experience data overview
fig9 <- raw_child %>% filter(Strata == "Sex") %>% ggplot( aes(fill=Strata.Name, y=Rate, x=Year)) + 
  geom_bar(position="dodge", stat="identity")+guides(fill=guide_legend(title="Gender"))+scale_x_continuous(breaks = seq(2013, 2025, 2))+
  labs(
    x = "Year",
    y = "Percent of population",
    title = "By sex")

fig10 <- raw_child %>% filter(Strata == "Age") %>% ggplot( aes(fill=Strata.Name, y=Rate, x=Year)) + 
  geom_bar(position="dodge", stat="identity")+guides(fill=guide_legend(title="Gender"))+scale_x_continuous(breaks = seq(2013, 2025, 2))+
  labs(
    x = "Year",
    y = "Percent of population",
    title = "By Age")

grid.arrange(fig8,fig9,fig10, ncol=2, top="Percent of population that has adverse childhood experience",layout_matrix = cbind(c(1,3), c(2,3)))


#generate table for the comparison of depression and suicide
ggplot(d_s, aes(Year)) + 
  geom_line(aes(y = Percent), color = "darkviolet", size = 0.8,) +
  geom_line(aes(y = Percent_s), color = "deepskyblue2", size = 0.8) +
  geom_text(
    mapping = aes(x = 2014, y = 14, label = "Depression"),
    size = 3,
    color = "darkviolet"
  ) +
  geom_text(
    mapping = aes(x = 2014, y = 11, label = "Suicide"),
    size = 3,
    color = "deepskyblue2"
  )  +
  labs(
    x = "Year",
    y = "Percent in Population",
    title = "trend of depression and suicide in California",
  )+scale_x_continuous(breaks = seq(2012, 2020, 1))+scale_y_continuous(breaks = seq(10, 18, 1))+
  theme(
    panel.background = element_rect(fill = "gray98"),
    panel.grid.major.y = element_line(
      color = "darkgray",
      size = 0.5,
      linetype = 1
    ), panel.grid.major.x = element_line(
      color = "darkgray",
      size = 0.5,
      linetype = 1
    ),
    axis.line =  element_line(
      color = "black",
      size = 0.5,
      linetype = 1
    ) 
  )

#prepare data and combine data of all three indicators in 2013 and 2015
data_1315 <- sex %>% filter(Year == 2013|Year == 2015)
child_s <- raw_child %>% filter(Strata == "Sex") 
data_1315$Percent_childhood <- child_s$Rate
suicide_s <- sex_s %>% filter((Year == 2013|Year == 2015) & Strata.Name != "Total")
suicide_s <- c(15.720, 4.749, 16.070, 5.765)
data_1315$Percent_suicide <- suicide_s
data_1315 <- data_1315 %>% select(Year, Strata.Name, Percent, Percent_childhood, Percent_suicide)
names(data_1315)[names(data_1315) == "Percent"] <- "Percent_depression"

#generate table 2
knitr::kable(data_1315, col.names = c("Year", "Sex", "Percent of Depression", "Percent of Adverse Childhood Experience", "Percent of Suicide"), caption = "Percent of population with Depression, Adverse Childhood Experience and Suicide in 2013 and 2015", digits = 2, align=c(rep('c',times=5)))


