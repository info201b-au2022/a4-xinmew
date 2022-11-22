library(tidyverse)
library(dplyr)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

# load in data of jail population
incarceration_trends <- read_csv("~/a4-xinmew/data/incarceration_trends.csv")

# subset the jail population with each year in the U.S.
incarceration_by_year <- incarceration_trends[c("year", "total_pop")]

# load in name of each state
state_names <- read_csv("state_names_and_codes.csv")

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function finds the total amount of people incarcerated in the
# United States each year
get_year_jail_pop <- function() {
  incarceration_by_year <- incarceration_by_year %>%
    group_by(year) %>%
    summarize(total_pop = sum(total_pop))
  
  incarceration_by_year <- incarceration_by_year %>%
    mutate(new_incarceration = total_pop - lag(total_pop))
  incarceration_by_year <- incarceration_by_year[-1,]
return(incarceration_by_year[c("year", "new_incarceration")])   
}
incarceration_by_year <- get_year_jail_pop()

# This function plots the total amount of people incarcerated each year
# in the United States
plot_jail_pop_for_us <- function()  {
  plot <- ggplot(incarceration_by_year, 
                 aes(x = year, y = new_incarceration)) + geom_bar(stat='identity') 
  return(plot)   
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
some_states <- c("AK", "AL")
# This function takes a vector of states as input, and finds the total
# incarcerated population in each state each year
get_jail_pop_by_states <- funzction(states) {
  incarceration_state <- incarceration_trends %>%
    filter(state %in% states) %>%
    select(year, state, total_pop) %>%
    group_by(state, year) %>%
    summarize(total_pop = sum(total_pop))
  
  return(incarceration_state)
}

# This function takes a vector of states, and graph each state's total
# incarcerated population each year as a line chart
plot_jail_pop_by_states <- function(states) {
  incarceration_by_state <- get_jail_pop_by_states(states)
  plot <- ggplot(incarceration_by_state, 
         aes(x = year, y = total_pop, group = state)) + geom_line()
  return(plot)
}
plot_jail_pop_by_states(some_states)


#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
compare_state_race_jail_prop <- function(states) {
  incarceration_state <- incarceration_trends %>%
    filter(state %in% states) %>%
    filter(!is.na(black_pop_15to64)) %>%
    select(state, total_pop, black_pop_15to64, aapi_pop_15to64, latinx_pop_15to64, 
           native_pop_15to64, white_pop_15to64) %>%
    
    group_by(state) %>% 
    summarize(total_pop = sum(total_pop), 
              black_pop_15to64 = sum(black_pop_15to64, na.rm=T)/total_pop,
              aapi_pop_15to64 = sum(aapi_pop_15to64, na.rm=T)/total_pop,
              latinx_pop_15to64 = sum(latinx_pop_15to64, na.rm=T)/total_pop, 
              native_pop_15to64 = sum(native_pop_15to64, na.rm=T)/total_pop, 
              white_pop_15to64 = sum(white_pop_15to64, na.rm=T)/total_pop)
}

plot_state_race_jail_prop <- function(state) {
  race <- c("black_pop_15to64", "aapi_pop_15to64", "latinx_pop_15to64", "native_pop_15to64", "white_pop_15to64")
  incarceration_by_state_with_race_prop <- compare_state_race_jail_prop(state)
  props <- data.frame(incarceration_by_state_with_race_prop$black_pop_15to64,
                 incarceration_by_state_with_race_prop$aapi_pop_15to64,
                 incarceration_by_state_with_race_prop$latinx_pop_15to64,
                 incarceration_by_state_with_race_prop$native_pop_15to64,
                 incarceration_by_state_with_race_prop$white_pop_15to64)
  plot <- barplot(props, aes(x = state, y = value))
                  + geom_bar(stat = 'identity')
  return(plot)
}

props <- data.frame(incarceration_by_state_with_race_prop$black_pop_15to64,
                    incarceration_by_state_with_race_prop$aapi_pop_15to64,
                    incarceration_by_state_with_race_prop$latinx_pop_15to64,
                    incarceration_by_state_with_race_prop$native_pop_15to64,
                    incarceration_by_state_with_race_prop$white_pop_15to64)
plot <- barplot(props, aes(x = state, y = value)) + geom_bar(stat = 'identity')



plot_state_race_jail_prop("AK")

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


