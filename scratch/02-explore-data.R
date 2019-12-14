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


#-------------------------------------------------------------------------------
## Read Data Frames from 01-get-data.R

citz_over10k_data <- read_csv(here("tmp/citz_over10k_data.csv"))
citz_da_data <- read_csv(here("tmp/citz_da_data.csv"))


#-------------------------------------------------------------------------------
## Combine Direct Award & Contracts >10K Data Frames

citz_over10k_data <- read_csv(here("tmp/citz_over10k_data.csv"))
citz_da_data <- read_csv(here("tmp/citz_da_data.csv"))


da_data <- citz_da_data %>%
  rename("direct_award_criteria/procurement_process" = "direct_award_criteria",
         "ministry_and_office_division_or_branch_procuring_the_service" = "office_division_or_branch_procuring_the_service")

over10_data <- citz_over10k_data %>%
  rename("direct_award_criteria/procurement_process" = "procurement_process") %>% 
  mutate(contract_value = case_when(amended_contract_value == 0 ~ initial_contract_value,
                                    TRUE ~ amended_contract_value)) %>% 
  select(-initial_contract_value, -current_amendment, -amended_contract_value)

citz_proc_data <- da_data %>% 
bind_rows(over10_data)


#-------------------------------------------------------------------------------
## Explore

citz_proc_data %>% 
  


