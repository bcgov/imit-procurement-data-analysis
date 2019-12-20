# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


## Quick exploration of tidy CITZ Procurement data from bcgov Open Information Catalogue,
## which is pulled, generated & tidied with 01-get-data.R and 02-clean-data.R scripts.


## Load Libraries
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(conflicted)
library(stringr)

conflict_prefer("here", "here") #use here::here please
conflict_prefer("filter", "dplyr")


#-------------------------------------------------------------------------------
## Read Data 

#data frame from 01-get-data.R
citz_proc_imit_data <- read_csv(here("tmp/citz_imit_proc_data.csv"))



#-------------------------------------------------------------------------------
## Exploratory Visualization


#all data by year & imit
citz_proc_imit_data %>% 
 group_by(year, imit) %>% 
  summarise(total = sum(annual_contract_value)) %>% 
  ggplot(aes(year, total/1000000, fill = imit)) +
  geom_col(alpha = 0.6) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


#STOB 63 data only by year
citz_proc_imit_data %>% 
  filter(str_detect(description_of_work, "^63")) %>% 
  group_by(year) %>% 
  summarise(total = sum(annual_contract_value)) %>% 
  ggplot(aes(year, total/1000000)) +
  geom_col(alpha = 0.6) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


citz_proc_imit_data %>% filter(str_detect(name_of_the_contractor, "IBM"))
citz_proc_imit_data %>% filter(str_detect(name_of_the_contractor, "TELUS"))
citz_proc_imit_data %>% filter(str_detect(name_of_the_contractor, "ESIT"))



