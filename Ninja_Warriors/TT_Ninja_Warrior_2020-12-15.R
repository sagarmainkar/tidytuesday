library(tidyverse)
library(tidytext)


ninja_warrior

 #Which obstacles are most used at each round stage?
obstacles_by_round <-  ninja_warrior %>% 
                          group_by(round_stage,obstacle_name ) %>% 
                            
                              mutate(round_stage = as.factor(round_stage)) %>% 
                              count(obstacle_name,sort=TRUE) %>% 
                                ungroup()


obstacles_by_round %>% 
                  group_by(round_stage) %>% 
                    top_n(10) %>% 
                      ungroup %>%
                        mutate(round_stage = as.factor(round_stage),
                               obstacle_name = reorder_within(obstacle_name, n,round_stage)) %>%
                                ggplot(aes(obstacle_name, n, fill = round_stage)) +
                                  geom_col(show.legend = FALSE) +
                                      facet_wrap(~round_stage, scales = "free_y") +
                                        coord_flip() +
                                          scale_x_reordered() +
                                          scale_y_continuous(expand = c(0,0)) +
                                            labs(y = "Number of obstacles per stage",
                                                 x = NULL,
                                                 title = "Highest obstacles per stage?"
                                                 )
                                  


# Where are round stages held. Is there a pattern?
by_loc_stage <- ninja_warrior %>% 
                        group_by(location,round_stage) %>% 
                          mutate(round_stage = as.factor(round_stage)) %>% 
                            count(location,sort=TRUE) %>% 
                            ungroup()


by_loc_stage %>% 
            
            mutate(round_stage = as.factor(round_stage),
                   location = reorder_within(location,n,round_stage)) %>% 
                    ggplot(aes(x=location,n,fill=round_stage))+
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~round_stage, scales = "free_y")+
                    coord_flip() +
                      scale_x_reordered() +
                      scale_y_continuous(expand = c(0,0)) +
                          labs(y = "Number of times a stage was held in a location",
                               x = NULL,
                               title = "Pattern for each stage held at a location"
                          )
            