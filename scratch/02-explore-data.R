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
## Merge Direct Award & Contracts >10K Data Frames

citz_over10k_data <- read_csv(here("tmp/citz_over10k_data.csv"))
citz_da_data <- read_csv(here("tmp/citz_da_data.csv"))


#  [1] "start_date"                                     
#  [2] "contract_reference_number"                      
#  [3] "office_division_or_branch_procuring_the_service"
#  [4] "name_of_the_contractor"                         
#  [5] "contract_value"                                 
#  [6] "description_of_work"                            
#  [7] "delivery_date"                                  
#  [8] "direct_award_criteria/procurement_process"                          
#  [9] "fiscal_year"                                    
# [10] "ministry_name"                                  
# [11] "procurement_type" 

foo <- citz_da_data %>%
  rename("direct_award_criteria/procurement_process" = "direct_award_criteria")

data <- citz_over10k_data %>% rename("direct_award_criteria/procurement_process" = "procurement_process",
                             "office_division_or_branch_procuring_the_service" = "ministry_and_office_division_or_branch_procuring_the_service") %>% 
  mutate(contract_value = amended_contract_value) %>% 
bind_rows(foo)





