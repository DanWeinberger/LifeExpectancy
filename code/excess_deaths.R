# ==============================================================================
# Import
# ==============================================================================

library(tidyverse)
library(lubridate)
source('code/utils.R')

# https://data.cdc.gov/NCHS/AH-Excess-Deaths-by-Sex-Age-and-Race-and-Hispanic-/m74n-4hbs

# Import excess deaths data from CDC: 
excess_deaths <- read_csv('data/excess_deaths.csv') 

# Format excess deaths table so it's easier to work with: 
excess_deaths_refined <- excess_deaths %>% 
	select(`Time Period`, MMWRyear, MMWRweek, RaceEthnicity, Sex, AgeGroup, `Deaths (unweighted)`, `Average number of deaths (unweighted)`, `Percent above average (unweighted)`) %>% 
	filter(!(AgeGroup %in% c("All Ages","Not stated"))) %>% 
	filter(Sex=="All Sexes") %>% 
	select(-Sex) %>% 
	filter(RaceEthnicity %in% c("All Race/Ethnicity Groups", "Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Non-Hispanic Asian")) %>% 
	filter(MMWRyear %in% 2015:2020) %>% 
	rename(Deaths=`Deaths (unweighted)`) %>% 
	rename(AvgDeaths=`Average number of deaths (unweighted)`) %>% 
	rename(PctExcess=`Percent above average (unweighted)`) %>% 
	rename(TimePeriod=`Time Period`) 

# Calculate the difference in mortality rate between 2020 and 2015-2019 (avg): 
excess_rate_df_raw <- excess_deaths_refined %>% 
	group_by(TimePeriod, RaceEthnicity, AgeGroup) %>% 
	# For each time/demography stratum, calculate total deaths: 
	summarise(NDeaths = sum(Deaths)) %>% 
	# Add correction factor for number of years in time period: 
	mutate(Divisor=case_when(TimePeriod=="2015-2019"~5, TRUE~1)) %>% 
	# Calculate avg deaths per year in 2015-2019 vs 2020: 
	mutate(AvgDeaths=NDeaths/Divisor) %>% 
	# Clean up: 
	select(-NDeaths, -Divisor) %>% 
	# Reshape: 
	pivot_wider(values_from=AvgDeaths, names_from=TimePeriod) %>% 
	# Calculate difference in avg death rate between 2020 and 2015-2019 (avg):
	mutate(RateFoldChange=`2020`/`2015-2019`) %>% 
	# Dummy column to tell how many times to replicate this value in next df:
	mutate(Rep=case_when(
		AgeGroup=="0-14 Years"~15,
		AgeGroup=="85+"~16,
		TRUE~5))

# Make table for excess 2020 death rate for each age/demography group: 
excess_rate_df <- excess_rate_df_raw %>% 
	arrange(RaceEthnicity, AgeGroup) %>% 
	split(.$RaceEthnicity) %>% 
	map(~ tibble(xi=rep(.$RateFoldChange, .$Rep))) %>% 
	map(~ mutate(., age=0:(n()-1))) %>% 
	bind_rows(.id="RaceEthnicity") %>% 
	select(RaceEthnicity,age,xi)
