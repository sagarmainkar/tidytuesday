# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
library(tidyverse)


# tuesdata <- tidytuesdayR::tt_load('2021-02-02')
# tuesdata <- tidytuesdayR::tt_load(2021, week = 6)
# 
# hbcu_all <- tuesdata$hbcu_all

# Or read in the data manually

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')


hbcu_all_new<- hbcu_all %>% 
  rename(TotalEnrollment =`Total enrollment`,
         FourYear=`4-year`,
         TwoYear=`2-year`,
         TotalPublic = `Total - Public`,
         FourYearPublic = `4-year - Public`,
         TwoYearPublic= `2-year - Public`,
         TotalPrivate= `Total - Private`,
         FourYearPrivate = `4-year - Private`,
         TwoYearPrivate = `2-year - Private`
         )

hbcu_piv <-hbcu_all_new %>% 
  pivot_longer(c(TotalEnrollment,TotalPublic,TotalPrivate,
                 FourYear,FourYearPublic,FourYearPrivate,
                 Males,Females,TwoYear,TwoYearPublic,
                 TwoYearPrivate),names_to="Type",values_to="Value")
labels_data <- hbcu_piv %>%  filter(Year == max(Year)) 

hbcu_piv %>% 
  ggplot(aes(x=Year,y=Value)) +
  geom_line(aes(color=Type))+
  geom_text(data=labels_data,  aes(label=Type,color=Type),x=2015,vjust="inward",hjust="inward") +
  labs(
      y="Enrollments",
      title="Enrollments in"
      
         )+
  theme_classic() +
  scale_x_continuous(breaks = seq(1980, 2020, by = 4))
  

hbcu_piv %>% 
  filter(Type)
  ggplot(aes(x=Year,y=Value)) +
  geom_line(aes(color=Type))+
  geom_text(data=labels_data,  aes(label=Type,color=Type),x=2015,vjust="inward",hjust="inward") +
  labs(
    y="Enrollments",
    title="Enrollments in"
    
  )+
  theme_classic() +
  scale_x_continuous(breaks = seq(1980, 2020, by = 4))

  
  





