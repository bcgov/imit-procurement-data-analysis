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


## Quick exploration of CITZ Procurement data from bcgov Open Information Catalogue,
## which is pulled and generated with 01-get-data.R script.


## Load Libraries
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(conflicted)

conflict_prefer("here", "here") #use here::here please


#-------------------------------------------------------------------------------
## Read Data 

#data frame from 01-get-data.R
citz_proc_data <- read_csv(here("tmp/citz_proc_data.csv"))


#-------------------------------------------------------------------------------
## Tidy Data 

#explore duplicates
da_records <- citz_proc_data %>%
  filter(procurement_type == "direct_award") 

da_records %>% 
  group_by(contract_reference_number, contract_value, fiscal_year) %>% 
  count() %>% 
  filter(n > 1) #2 (1) duplicate entries in da data
  
over10_records <-  citz_proc_data %>%
  filter(procurement_type == "over_10k") 

over10_records %>% 
  group_by(contract_reference_number, contract_value, fiscal_year) %>% 
  count() %>% 
  filter(n > 1) #19 (8) duplicate entries in over_10k data


#remove duplicates & contract_value == NA row (1)
citz_proc_data_tidy <- citz_proc_data %>%
  distinct(contract_reference_number, contract_value, fiscal_year, .keep_all = TRUE) %>% 
  filter(!is.na(contract_value))


#generate year columns and add average contract_value per year 
#for multi-year contracts
citz_proc_data_tidy <- citz_proc_data_tidy %>% 
  mutate(start_year = year(start_date),
         delivery_year =  year(delivery_date),
         number_years = (delivery_year - start_year) + 1,
         annual_contract_value = contract_value/number_years)


#-------------------------------------------------------------------------------
## 

citz_proc_data_tidy %>% 
  filter(procurement_type == "over_10k") %>% 
  distinct(description_of_work)

citz_proc_data_tidy %>% 
 group_by(fiscal_year, procurement_type) %>% 
  summarise(total = sum(annual_contract_value)) %>% 
  ggplot(aes(fiscal_year, total/1000000, fill = procurement_type)) +
  geom_col()



