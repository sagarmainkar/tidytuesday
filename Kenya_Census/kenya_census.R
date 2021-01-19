library(tidyverse)

# Or read in the data manually

gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')


bind_cols(
  gender %>% 
  mutate(
        County = tolower(County),
        County = replace(County, County == "total","kenya")
        ) %>% 
  filter(County != "total") %>% 
  arrange(County) %>% 
  pull(County),

crops %>% 
  rename(County = SubCounty) %>% 
  mutate(County = tolower(County)) %>% 
  arrange(County) %>% 
  pull(County),


households %>% 
  mutate(County = tolower(County)) %>% 
  arrange(County) %>% 
  pull(County)
)

gender <- gender %>% 
  mutate(
    County = tolower(County),
    County = replace(County, County == "total","kenya")
  )

crops <- crops %>% 
            rename(County = SubCounty)

counties <- data.frame(cbind( gender %>% 
                    mutate(
                      County = tolower(County)
                    ) %>% 
                    arrange(County) %>% 
                    pull(County),
                  
                  crops %>% 
                    mutate(County = tolower(County)) %>% 
                    arrange(County) %>% 
                    pull(County),
                  
                  
                  households %>% 
                    mutate(County = tolower(County)) %>% 
                    arrange(County) %>% 
                    pull(County)))

names(counties) <- c("Gender","Crops","Households")


clean_counties <- function(data){
  data <-data %>% 
        mutate(
          County = tolower(County),
          County = replace(County,County =="homa bay","homabay"),
          County = replace(County,County == "murang'a", "muranga"),
          County = replace(County,County == "tana river", "tanariver"),
          County = replace(County,County == "trans nzoia", "transnzoia"),
          County = replace(County,County == "uasin gishu", "uasingishu"),
          County = replace(County,County == "west pokot", "westpokot")
                                                                         
        )
  
    return (data)
}

kenya_census <- full_join(clean_counties(gender),clean_counties(crops),by="County")

kenya_census <- full_join(kenya_census,clean_counties(households),by="County")          

kenya_census %>%  View()


kenya_census %>% 
  ggplot()