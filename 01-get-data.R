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

library(bcdata) #available from GitHub https://bcgov.github.io/bcdata/
library(janitor)
library(dplyr)
library(lubridate)


## Look at ministry-contract-awards-province-of-british-columbia record 
## in the B.C. Data Catalogue
bcdc_get_record("9bff5da9-fced-4671-8ff3-f6117e5c8266")


## Get the tabular data with bcdata package
awards_raw <- bcdc_get_data(record = "9bff5da9-fced-4671-8ff3-f6117e5c8266",
                        resource = "aee0fed5-e1f5-4e32-b17c-c98eafdd0d36")

## Quick look at columns
skimr::skim(awards_raw)


## Tidy raw data frame
awards_tidy <- awards_raw %>% 
  clean_names() %>%
  mutate(days_open = as_date(closing_date) - as_date(issued_date),
         year = year(date_awarded))  %>%
  select(-amendments, -issued_by_organization, -closing_date, -issued_date)



  