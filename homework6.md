Homework 6
================
Annie Yu xy2404
11/22/2018

``` r
homicide <- read_csv('./data/homicide-data.csv') %>% 
  janitor::clean_names() %>%
  mutate(city_state = str_c(city, ",", state),
         resolved = as.numeric(disposition == "Closed by arrest")) %>% 
  filter(city_state =="Dallas,TX"|city_state =="Phoenix,AZ"|city_state =="Kansas City,MO"|city_state =="Tulsa,AL") %>%
  mutate(victim_race = as.factor(victim_race),
         victim_sex = as.factor(victim_sex),
         victim_age = as.numeric(victim_age),
    victim_race = fct_recode(victim_race, non_white = 'Asian', non_white = 'Black', non_white = 'Hispanic', non_white = 'Other', non_white = 'Unknown', white = 'White'))
```

    ## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
    ## coercion

    ## Warning: Unknown levels in `f`: Asian, Black, Hispanic, Other
