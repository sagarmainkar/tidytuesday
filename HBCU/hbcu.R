library(tidyverse)
library(janitor)
library(patchwork)



hbcu_all <- 
    readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% 
              clean_names()

### Clean data ----------------------------------------------------------------
# We can separate by gender OR by program length and public/private, not both

### Gender breakdown
hbcu_by_gender <- hbcu_all %>%
  # We only need year and gender columns
  select(year, males, females) %>%
  # Convert to tidy format, collapsing male/female into one descriptor field
  pivot_longer(males:females,
               names_to = "gender",
               values_to = "students") %>%
  # Convert from plural to singular for cleaner data
  # "s$" specifies an "s" character at the end of a string ("$" is end of string in regular expressions)
  mutate(gender = str_remove(gender, "s$"))


### Program breakdown
hbcu_by_program <- hbcu_all %>%
  # We need fields with "public" or "private" in the name
  # (They also have 2- vs 4-year)
  # We DON'T need fields with "total" in the name, since this is redundant
  select(year,
         contains(c("public", "private")),
         -contains("total")) %>%
  # names_pattern argument does the heavy lifting
  # It separates names into groups, as specified by parentheses "(group)"
  # Field names are structured so that program length is followed by public/private
  # We also specify "x_" as an optional argument using regular expressions
  pivot_longer(cols = x4_year_public:x2_year_private,
               names_pattern = "[x_]?(.*)_(.*)",
               names_to = c("program_length", "public_private"),
               values_to = "students") %>%
  # parse_number() finds the number (i.e., 2 or 4) in program_length and discards the rest
  # Then we use paste() to add "years" on the end
  # It's a sorta-convoluted way to get rid of the underscores and extra bits
  mutate(program_length = paste(parse_number(program_length), "years"))




 p1 <-hbcu_by_gender %>% 
   ggplot(aes(year,students,color=gender)) +
   geom_line() +
   geom_text(data=filter(hbcu_by_gender,year==max(year)),  aes(label=gender,color=gender),x=2015,nudge_y = -2000,hjust="inward") +
   labs(
     y="Enrollments",
     x="Year",
     title="Enrollments by Gender"
     
   )+
   theme(plot.title = element_text(hjust = .5, size = 25),
         legend.position = "none",
          panel.grid = element_blank(),
           panel.background = element_rect(fill="#ffffff"))+
   scale_x_continuous(breaks = seq(1980, 2020, by = 4))

 
 p2 <- hbcu_by_program %>% 
   ggplot(aes(year,students,color=public_private)) +
   geom_line() +
   geom_text(data=filter(hbcu_by_program,year==max(year)),aes(label=public_private),x=2015,nudge_y = -2000,hjust="inward")+
   facet_wrap(~ program_length,nrow=2,scales = "free_y")+
   labs(
     y="",
     x="Year",
     title="Public/Private Enrollments"
   )+
   theme(plot.title = element_text(hjust = .5, size = 25),
         legend.position="none",
         panel.grid = element_blank(),
         panel.background = element_rect(fill="#ffffff"))

 
 p1 + p2































