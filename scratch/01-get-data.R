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


## Grab CITZ Procurement xlsx files from bcgov Open Information Catalogue
## https://www2.gov.bc.ca/gov/content?id=9710AD9FEF33424589928C639851205B
## and create machine-readable data frames


## Load Libraries
library(readxl)
library(httr)
library(janitor)
library(dplyr)
library(stringr)
library(rvest)
library(purrr)
library(readr)
library(here)


#-------------------------------------------------------------------------------
## Contracts Over $10K


## Do it for one resource URL
## Download file to temporary_file using URL, read in file, delete temporary_file file
# url <- "http://docs.openinfo.gov.bc.ca/CO14507_Contracts_Over_10000_Ministry_of_Citizens'_Services_October_December_2018.xlsx"
# 
# temporary_file <- tempfile(fileext = ".xlsx")
# 
# GET(url, write_disk(temporary_file))
# 
# yr <- str_sub(url,-9,-6)
# 
# data <- read_xlsx(temporary_file, range = cell_cols("B:M")) %>%
#   mutate(year = yr)
# 
# unlink(temporary_file)


## Do it for ALL resource URLs
## Get main web page with URLs to pages with resources
citz_over10k_main <- read_html("https://www2.gov.bc.ca/gov/search?q=%2Binmeta%3Adc.contributor%3DMinistry+of+Citizens%27+Services&id=DECF07E95C5641F09270121869929025&tab=1")


## Create df of URLs for web pages with resources
citz_over10k_pages <- citz_over10k_main %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  tibble() %>%
  filter(str_detect(., "enSearch"))


## Map over each html page and create a df of resource download URLs
citz_over10k_urls <- map_dfr(.x = citz_over10k_pages$.,
                             .f = ~ {
                               read_html(paste0("https://www2.gov.bc.ca/", .x)) %>%
                                 html_nodes("a") %>%
                                 html_attr("href") %>%
                                 tibble() %>%
                                 filter(str_detect(., "xlsx"))
                             })  


## Note: not all 9 resources are xlsx files, null report in PDF format (dropped)
## Note: some of the 8 xlsx resources have differing number of empty (non-data) leading rows,
## different column names and different number of columns (empty, trailing columns)


## Supply column names to mitigate differing column names
column_names <- c("start_date",                                                  
"contract_reference_number",                                  
"ministry_and_office_division_or_branch_procuring_the_service",
"name_of_the_contractor",                                    
"initial_contract_value",                                     
"current_amendment",                                         
"amended_contract_value",                                    
"description_of_work",                                       
"detailed_description",                                      
"delivery_date",                                          
"comments_optional_as_required",                             
"procurement_process")


# Supply column data types to mitigate empty rows generating data type "guessing"
column_types_over10 = c("date","text","text", "text","numeric","numeric",
                  "numeric","text","text", "date","text","text")


## Map over each download URL, download and read in xlsx file, create a df with data from all files
citz_over10k_data_raw <- map_dfr(.x = citz_over10k_urls$.,
                             .f = ~{temporary_file <- tempfile(fileext = ".xlsx")
                               GET(.x, write_disk(temporary_file))
                              
yr <- str_sub(.x, -9, -6) #grab year from file name

read_xlsx(temporary_file,
    range = cell_cols("B:M"), #grab select columns to mitigate empty, trailing columns
    col_names = column_names,
                col_types = column_types_over10) %>%
  mutate(year = yr, #add fy to df
         ministry_name = "citz",
         procurement_type = "over_10k")

})
  

## Remove non-data rows
citz_over10k_data <- citz_over10k_data_raw %>% filter(!is.na(start_date)) 


## Write to tmp folder
write_csv(citz_over10k_data, here::here("tmp/citz_over10k_data.csv"))


#-------------------------------------------------------------------------------
## Directly Awarded Contracts 


## Read main web pages that have URLs to web pages with resources
citz_da_main_pages <- tibble(urls = c("https://www2.gov.bc.ca/gov/search?q=%2Binmeta%3Adc.contributor%3DMinistry+of+Citizens%27+Services&id=30E96E61F14C436B9727AAA4FEE2E251",
"https://www2.gov.bc.ca/gov/search?id=30E96E61F14C436B9727AAA4FEE2E251&page=2&q=%2Binmeta%3Adc.contributor%3DMinistry+of+Citizens%27+Services&sort=D&tab=1",
"https://www2.gov.bc.ca/gov/search?id=30E96E61F14C436B9727AAA4FEE2E251&page=3&q=%2Binmeta%3Adc.contributor%3DMinistry+of+Citizens%27+Services&sort=D&tab=1"))


## Create df of URLs for web pages with resources
citz_da_pages <- map_dfr(.x = citz_da_main_pages$urls, 
                    .f = ~{read_html(.x) %>% 
  html_nodes("a") %>%
  html_attr("href") %>%
  tibble() %>%
  filter(str_detect(., "enSearch"))
})
  

## Map over each html page and create a df of resource download URLs
citz_da_urls <- map_dfr(.x = citz_da_pages$.,
                             .f = ~ {
                               read_html(paste0("https://www2.gov.bc.ca/", .x)) %>%
                                 html_nodes("a") %>%
                                 html_attr("href") %>%
                                 tibble() %>%
                                 filter(str_detect(., "xlsx"))
                             })  
 

# Supply column data types to mitigate empty rows generating data type "guessing"
column_types_da = c("date", "text", "text", "text", "numeric", "text", "date", "text")


## Map over each download URL, download and read in xlsx file, create a df with data from all files
citz_da_data_raw <- map_dfr(.x = citz_da_urls$.,
                             .f = ~{temporary_file <- tempfile(fileext = ".xlsx")
                               GET(.x, write_disk(temporary_file))
                               
yr <- str_sub(.x, -9, -6) #grab year from file name

read_xlsx(temporary_file, skip = 5,  col_types = column_types_da) %>%
  clean_names() %>% 
  mutate(year = yr, #add year to df
         ministry_name = "citz",
         procurement_type = "direct_award")
})


## Remove non-data rows
citz_da_data <- citz_da_data_raw %>%filter(!is.na(start_date)) 
 

## Write to tmp folder
write_csv(citz_da_data, here::here("tmp/citz_da_data.csv"))


#-------------------------------------------------------------------------------
## Combine Direct Award & Contracts >10K Data Frames

# citz_over10k_data <- read_csv(here("tmp/citz_over10k_data.csv"))
# citz_da_data <- read_csv(here("tmp/citz_da_data.csv"))


da_data <- citz_da_data %>%
  rename("direct_award_criteria/procurement_process" = "direct_award_criteria",
         "ministry_and_office_division_or_branch_procuring_the_service" = "office_division_or_branch_procuring_the_service")

over10_data <- citz_over10k_data %>%
  rename("direct_award_criteria/procurement_process" = "procurement_process") %>% 
  mutate(contract_value = case_when(amended_contract_value == 0 ~ initial_contract_value,
                                    TRUE ~ amended_contract_value)) %>% 
  select(-initial_contract_value, -current_amendment, -amended_contract_value)


citz_proc_data <- da_data %>% bind_rows(over10_data)


## Write to tmp folder
write_csv(citz_proc_data, here::here("tmp/citz_proc_data.csv"))




