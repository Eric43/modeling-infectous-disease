

library(tidyverse)

library(tidycensus)
options(tigris_use_cache = TRUE)

library(bnlearn)

# Get the population data for the state

vars <- load_variables(2018, "acs5", cache = TRUE)

wv_pop_county  <- get_acs(geography = "county", 
                   variables = c(total_pop = "B01003_001"), 
                   state = "WV", 
                   year = 2018)

##### Need to figure out why no MOE maybe on census tract

wv_pop_tract  <- get_acs(geography = "tract", 
                         variables = c(total_pop = "B01003_001"), 
                         state = "WV", 
                         year = 2018)

wv_noveh <- get_acs(geography = "tract",
                    variables = c(tot_noveh = "B08014_002"),
                    state = "WV",
                    year = 2018,
                    geometry = TRUE)



       

       
butcher_est <- function (pop_est = ..., pop_sd = ..., n_rep = 100 ))
 
{
  #### Basic simulation of the number infected, 
  
  sim_pop <- tibble(pop = rnorm(n_rep, mean = pop_est, sd = pop_sd),
                    exp_inf = pop * rnorm(n=1, mean = 0.20, sd = 0.2), 
                    exp_hos = exp_inf * rnorm(n = 1, mean = 0.08, sd = 0.0008),
                    exp_mort = exp_inf * runif(1, min = 0.008, max = 0.018)) #hosp or inf?
  
  return(sim_pop)

  
  }



butcher_est2 <- function (pop_est = ..., 
                          pop_sd = ..., 
                          n_rep = 100,
                          mort_min = 0.0008,
                          mort_max = 0.0018)
  
{
  #### Basic simulation of the number infected, 
  
  sim_pop <- tibble(pop = rnorm(n_rep, mean = pop_est, sd = pop_sd))
  
  sim_pop <- sim_pop %>% 
    mutate(exp_inf = pop * rnorm(n=1, mean = 0.40, sd = 0.05)) %>%
    mutate(exp_hos = exp_inf * rnorm(n = 1, mean = 0.08, sd = 0.0008)) %>%
    mutate(exp_mort = exp_inf * runif(1, min = mort_min, max = mort_max))  # 0.008 to 0.18%
    
  return(sim_pop)
  
}


hist(rnorm(n=1000, mean = 0.40, sd = 0.05))





### Re write Butchers Toll to work with a set pop_est r bind?

  
#### Basic simulation of the number infected attached to census tract level data


butchers_toll <- mutate(wv_pop_tract,simu = pmap(list(wv_pop_tract$estimate, wv_pop_tract$moe, 1000), butcher_est2))


saveRDS(butchers_toll, file = "wv_c19sim.RDS")

=

#### How to extract the information from the dataframe

wv_inf <- mutate(butchers_toll, mean_inf = unlist(map(butchers_toll$simu[], ~round(mean(.$exp_inf)))))



#### Adding the summary of the simulation per census tract using map

wv_sim_c19v2 <- butchers_toll %>% 
  mutate(mean_inf = unlist(map(butchers_toll$simu[], ~round(mean(.$exp_inf))))) %>%
  mutate(sd_inf = unlist(map(butchers_toll$simu[], ~sd(.$exp_inf)))) %>%
  mutate(mean_hos = unlist(map(butchers_toll$simu[], ~round(mean(.$exp_hos))))) %>%
  mutate(sd_hos = unlist(map(butchers_toll$simu[], ~sd(.$exp_hos)))) %>%
  mutate(mean_mort = unlist(map(butchers_toll$simu[], ~round(mean(.$exp_mort))))) %>%
  mutate(sd_mort = unlist(map(butchers_toll$simu[], ~sd(.$exp_mort))))

#### Summing the column estimates to use as a statewide estimate

sum(wv_sim_c19v2$mean_mort)   
sum(wv_sim_c19v2$mean_hos)
sum(wv_sim_c19v2$mean_inf)

#### to group by county need to remove census tract number etc.


wv_sim_c19v2 %>% group_by()


#### Getting the amount of households without vehicle and plotting
##### Also using this as a way to merge with butchers toll model

test <- mutate(wv_noveh, p_adj = wv_noveh$estimate / sum(wv_noveh$estimate))


test %>%
  ggplot(aes(fill = p_adj)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle("Probability of no vehicle normalized to state total")


#### Merging the no vehicle and the infection model 

##### Prepare the model by removing the simu column


wv_merge <- left_join(test, select(wv_sim_c19v2, -"simu"), by = c("GEOID", "NAME"))


wv_merge %>%
  ggplot(aes(fill = mean_inf)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle("Expectation of number total infections by CO-19 infection")




+
  facet_(c("mean_inf", "mean_hos", "mean_mort", "p_adj"))

  