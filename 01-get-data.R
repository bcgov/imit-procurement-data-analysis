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
library(stringr)
library(ggplot2)


## Look at ministry-contract-awards-province-of-british-columbia record 
## in the B.C. Data Catalogue
bcdc_get_record("9bff5da9-fced-4671-8ff3-f6117e5c8266")


## Get the tabular data with bcdata package
## Data released under B.C. Crown Copyright Licence
## https://www2.gov.bc.ca/gov/content?id=1AAACC9C65754E4D89A118B875E0FBDA
awards_raw <- bcdc_get_data(record = "9bff5da9-fced-4671-8ff3-f6117e5c8266",
                        resource = "aee0fed5-e1f5-4e32-b17c-c98eafdd0d36")


## Quick look at columns
skimr::skim(awards_raw)


## Tidy raw data frame
awards_tidy <- awards_raw %>% 
  clean_names() %>%
  mutate(year = year(date_awarded),
         award_total = str_remove_all(award_total, "[,]"),
         award_total = as.numeric(str_remove(award_total, "[$]")))  %>%
  select(year, issued_for_organization, title, successful_vendor,
         successful_vendor_city, award_total, province, country)


## Total Awarded by Year
awards_tidy %>% 
  group_by(year) %>% 
  summarise(total = sum(award_total)/1000000) %>% 
  ggplot() +
  geom_col(aes(x = year, y = total), alpha = 0.6, fill = "blue") +
  labs(y = "Total Awarded (million $)",
       x = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


## Build IM/IT Categories from `title` text


  