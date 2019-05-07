test <- activity %>% 
    mutate(replace_mean_steps = ifelse(is.na(steps),
                                       SummaryInterval$mean_steps[SummaryInterval$interval==activity$interval],steps))