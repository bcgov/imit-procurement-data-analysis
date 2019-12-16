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
## Read Data Frame from 01-get-data.R

citz_proc_data <- read_csv(here("tmp/citz_proc_data.csv"))


#-------------------------------------------------------------------------------
## Explore

citz_proc_data %>% 
  


