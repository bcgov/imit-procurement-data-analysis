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



## Load Libraries
library(readr)
library(here)
library(dplyr)
library(lubridate)
library(conflicted)
library(stringr)
library(tibble)

conflict_prefer("here", "here") #use here::here please
conflict_prefer("filter", "dplyr")


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
  group_by(contract_reference_number, contract_value, year) %>% 
  count() %>% 
  filter(n > 1) #2 duplicate entries in da data (1 dupe each) 
  
over10_records <-  citz_proc_data %>%
  filter(procurement_type == "over_10k") 

over10_records %>% 
  group_by(contract_reference_number, contract_value, year) %>% 
  count() %>% 
  filter(n > 1) #2 duplicate entries in da data (1 dupe each) 


#remove duplicates & contract_value == NA row (1)
citz_proc_data_tidy <- citz_proc_data %>%
  distinct(contract_reference_number, contract_value, year, .keep_all = TRUE) %>% 
  filter(!is.na(contract_value))


#generate year columns and add average contract_value per year 
#for multi-year contracts
citz_proc_data_tidy <- citz_proc_data_tidy %>% 
  mutate(start_year = year(start_date),
         delivery_year =  year(delivery_date),
         number_years = (delivery_year - start_year) + 1,
         annual_contract_value = contract_value/number_years)


#-------------------------------------------------------------------------------
## IMIT contracts? Manually generate a "source of truth" for use in modelling?

#All STOB 63 == IMIT
# citz_proc_data_tidy %>% 
#   mutate(imit = case_when(str_detect(description_of_work, "^63") ~ TRUE,
#                           TRUE ~ FALSE))

#Manually Assign all but STOB63 to IMIT

#STOB61
citz_proc_data_tidy %>% filter(str_detect(description_of_work, "^61")) %>%
  distinct(detailed_description) %>%
  pull(detailed_description)

#STOB60
citz_proc_data_tidy %>% filter(str_detect(description_of_work, "^60")) %>%
  distinct(detailed_description) %>%
  pull(detailed_description)

#direct_awards
citz_proc_data_tidy %>% filter(procurement_type == "direct_award") %>%
  distinct(detailed_description) %>%
  pull(detailed_description)

imit_contracts <- tribble(
  ~detailed_description, ~imit,
"Provision of negotiation and other support to Chief Information Officer Council.", TRUE,                                                                                                                       
"Research business analyst.", FALSE,                                                                                                                                                                              
"Technical editor services.", TRUE,                                                                                                                                                                              
"Financial modeling and trend analysis.", FALSE,                                                                                                                                                                  
"Community network analyst.", FALSE,                                                                                                                                                                             
"Senior digital consultant.", TRUE,                                                                                                                                                                             
"Align and update freedom of information web content in response to feedback from a recent user experience service design project", TRUE,                                                                        
"Provide information management business analysis services and to build a branch service catalogue", TRUE,                                                                                                       
"Continuous improvement laboratory analysis and comparison to global laboratories", FALSE,                                                                                                                        
"Develop assistant deputy minister development plan, including strategy and task development, deputy minister level reporting, executive advice, guidance and feedback as well as preparation for reviews", FALSE,
"Senior level project manager to assist the director of eHealth contract management by providing advisory services and associated project management deliverables", TRUE,                                        
"Business advisory team to support the Hosting Administrator's Office and the Workstation Support Services Office for upcoming procurement activities", TRUE,                                                   
"Business advisory team for the Procurement and Supply Division", FALSE,                                                                                                                                         
"Advise on development of leadership framework", FALSE,                                                                                                                                                           
"Test lead for BCBid implementation", TRUE,                                                                                                                                                                      
"Review organization structure & recommend future state", FALSE,                                                                                                                                                  
"Align and update freedom-of-information web content to respond to feedback from a user experience service design project", TRUE,                                                                                
"Event management at a ministry staff learning event", FALSE,                                                                                                                                                     
"Build financial models to conduct sensitivity analysis, that informs and supports executive direction, and do related training of staff.", FALSE,                                                                
"Financial modeller for two applications, namely the System Integration Services Agreement (SISA) and Maintenance Support Statement of Work (MS SOW)", TRUE,                                                     
"Procurement advisory service to provide business advice, change management and negotiation support for Systems Integration Services Agreement (SISA)", TRUE,                                                    
"Provide senior communications consultant as a shared resource between ministries", FALSE,                                                                                                                       
"Senior portfolio manager for HIBC Re-procurement Office at Ministry of Health", FALSE,                                                                                                                           
"Financial analysis and modelling for the next generation HIBC service solution", FALSE,                                                                                                                          
"Provide strategic advice and input into the Agreements with Young Adults (AYA) program redesign and develop an implementable framework which harmonizes family-based rate structures", FALSE,                    
"Conduct industry research related to risk within large  Information Management/Information Technology (IM/IT) contracts", TRUE,                                                                                 
"Policy advice and development, including research and stakeholder engagement.", FALSE,                                                                                                                           
"Procurement consulting services", FALSE,                                                                                                                                                                         
"Strategic leadership mentoring", FALSE,                                                                                                                                                                          
"Content strategy and development of web content to support citizens when dealing with the affairs of someone who has died", TRUE,                                                                               
"Device management support services", TRUE,                                                                                                                                                                      
"Stakeholder engagement support, including training, workshops & knowledge transfer, to the Real Property Division", FALSE,                                                                                       
"Senior level project manager to assist the Director of eHealth Contract Management by providing advisory services and associated project management deliverables", TRUE,                                        
"Assist in procurement transformation that will be used to successfully modernize the procurement practices and tools available to the Province", TRUE,                                                          
"Negotiation support for the BCBid Replacement Project", TRUE,                                                                                                                                                   
"Advisory services to create predictive staffing models for Ministry of Children & Family Development", FALSE,                                                                                                    
"Procurement advisory team to provide business advice, change management and negotiation support for Systems Integration Services Agreement (SISA)", TRUE,                                                       
"Business advisory team to support the Procurement and Supply Division", FALSE,                                                                                                                                   
"Senior Health Informatics Advisor for the Digital Health Strategy", TRUE,                                                                                                                                       
"Change management lead to assist in providing change management and stakeholder engagement services", FALSE,                                                                                                     
"Business advisor for the Procurement Modernization Project", TRUE,                                                                                                                                              
"Advisory services to create predictive financial models for Ministry of Children & Family Development", FALSE,                                                                                                   
"Procurement consultant for virtual document library administration", TRUE,                                                                                                                                     
"Technical writer", FALSE,                                                                                                                                                                                        
"Journey mapping, user testing and service design with supplier stakeholders", TRUE,                                                                                                                             
"Consulting and support for LEAN project", FALSE,                                                                                                                                                                 
"Event management services for multi-day employee learning event", FALSE,                                                                                                                                         
"Device management support", TRUE,                                                                                                                                                                               
"Assistance with developing a strategy for granting and managing access to Cloud and on-premise applications", TRUE,                                                                                             
"Procurement advisory services for BC Bid", FALSE,                                                                                                                                                                
"Assistance with modernisation of procurement practices & tools", TRUE,                                                                                                                                          
"Next-Generation Telecommunications assessment", TRUE,                                                                                                                                                           
"Stakeholder surveys for Telus Alternate Service Delivery Agreement", TRUE,                                                                                                                                      
"Business advisor to the Procurement Modernization Project, which is an initiative to modernize the procurement practices and tools", TRUE,                                                                      
"Advisory and project management services for the BC Bid Replacement Project", TRUE,                                                                                                                            
"Negotiation support for the Oracle & Adobe master agreements", TRUE,                                                                                                                                            
"Business advisory services for the BC Bid Project", TRUE,                                                                                                                                                       
"Review of project process", FALSE,                                                                                                                                                                              
"Digital archives management consulting services", TRUE,                                                                                                                                                         
"Build various financial models to conduct sensitivity analysis that informs and supports executive direction. Train, transition and build knowledge capacity of internal staff resources.", FALSE,               
"Expert assistance to conduct a jurisdictional scan and comparison of public procurement programs", FALSE,                                                                                                        
"End of term report, including a value for money summary", FALSE,                                                                                                                                                 
"Project process review", FALSE,                                                                                                                                                                                 
"Consultant support for Lean process improvement projects", FALSE,                                                                                                                                                
"Provide project manager and business analysis services", FALSE,                                                                                                                                                  
"Third party evaluation of PharmaNet", FALSE,                                                                                                                                                                     
"Facilitator support for proponent workshops on the Devices Procurement", TRUE,                                                                                                                                  
"Procurement consultant services for CLOUDBC", TRUE,                                                                                                                                                             
"Procurement consulting services for various Alternate Service Delivery Projects", TRUE,                                                                                                                         
"Support the Hosting Administrator Office in the development of the Office of the Chief Information Officer (OCIO) hosting strategy", TRUE,                                                                      
"Business case development, market research, scan and analysis", FALSE,                                                                                                                                           
"Project management services to manage the procurement stream of the renewal project", FALSE,                                                                                                                     
"Provide senior-level project manager to assist the Director of eHealth Contract Management", TRUE,
"Analysis of online feedback & engagement summary report regarding daylight savings time initiative.", FALSE,                                                                                                          
"Analysis and report for Child Employment project.", FALSE,                                                                                                                                                            
"Report writing for Private Managed Forest Land project.", FALSE,                                                                                                                                                      
"Development of a virtual town hall.", TRUE,                                                                                                                                                                          
"Analysis of public and stakeholder feedback and development of \"What We Heard\" report for Domestic - Sexual Violence Leave.", FALSE,                                                                                
"Cross jurisdictional scan research.", FALSE,                                                                                                                                                                          
"Logistics planning and materials for a public open house & analysis of public and stakeholder feedback received and the development of a \"What We Heard\" report  for Radium Hot Springs Roundabout project.", FALSE,
"Analysis of public and stakeholder feedback and development on plastics engagement and the development of a \"What We Heard\" report.", FALSE,                                                                        
"Face-to-face meetings for CleanBC engagement.", FALSE,                                                                                                                                                                
"CleanBC public engagement.", FALSE,                                                                                                                                                                                   
"Old forest growth public engagement.", FALSE,                                                                                                                                                                         
"Updates to Chinese Canadian Museum website.", TRUE,                                                                                                                                                                  
"Virtual session for Royal BC Museum modernization project.", TRUE,                                                                                                                                                   
"\"What We Heard\" report translation and graphic for Chinese Canadian Museum.", FALSE,                                                                                                                                
"Consultant to provide business analysis and change management services in support of the Catastrophic Response Action (CRA) process.", FALSE,                                                                         
"User research and service resources to support the CITZ/HEALTH/POPDATABC project team.", TRUE,                                                                                                                       
"Speaker at learning event.", FALSE,                                                                                                                                                                                   
"Project director support for hosting initiatives.", TRUE,                                                                                                                                                            
"Business case development and facilitation.", FALSE,                                                                                                                                                                  
"Project management consulting services.", FALSE,                                                                                                                                                                      
"Provide human resource recruitment services", FALSE,                                                                                                                                                                  
"Audit services covering financial, security and privacy compliance", TRUE,                                                                                                                                           
"Strategic planning support for the Integrated Data Division", FALSE,                                                                                                                                                  
"Provide specialized cross-jurisdictional research, analysis, report writing and related support for the development of governance and implementation of corporate data programs and services", TRUE,                 
"Independent review of operations manual", FALSE,                                                                                                                                                                      
"Project management for mainframe application modernization project", TRUE,                                                                                                                                           
"Consultant to conduct an evaluation of Strategic Real Estate Services current surplus properties program", FALSE,                                                                                                     
"Off-site record storage", TRUE,                                                                                                                                                                                      
"Strategic planning support", FALSE,                                                                                                                                                                                   
"Review, validation and benefits measurement framework for Strategic Framework for Digital Government" , TRUE,                                                                                                        
"Facilitation of group training sessions and development of a team charter, roles and responsibilities matrix, and other team foundation documents", TRUE,                                                            
"Coaching services to excluded management employees", FALSE,                                                                                                                                                           
"Planning, recruitment and assessment services for senior management positions", FALSE,                                                                                                                                
"Logistic planning and coordination to support the OCIO Connect event", TRUE,                                                                                                                                         
"Provide a Cloud workshop that is non-vendor specific", TRUE,                                                                                                                                                         
"Delivery of \"Disciplined Agile\" training and certification", TRUE,                                                                                                                                                 
"Develop guidelines for environmental graphics design", FALSE,                                                                                                                                                         
"Off-site storage of government records", TRUE,                                                                                                                                                                       
"Evaluation support and technical advice for start-up and residence program", TRUE,                                                                                                                                   
"Stakeholder engagement and communications support", FALSE,                                                                                                                                                            
"Data Visualization training workshop", TRUE,                                                                                                                                                                         
"Environmental graphics design", FALSE,                                                                                                                                                                                
"Evaluation of Strategic Real Estate Services current surplus properties program", FALSE,                                                                                                                              
"Organizational planning and development", FALSE,                                                                                                                                                                      
"Software procurement specialist for Customer Relationship Management (CRM) platform & Layered Service Provider (LSP) procurement", TRUE,                                                                             
"Registrar of Real Property Division's ISO 14001 registered Environmental Management System", TRUE,                                                                                                                   
"Engagement of certified Agile coaches to support new project initiatives", TRUE,                                                                                                                                     
"Invigilation of exam sessions on behalf of Service BC", FALSE,                                                                                                                                                        
"Records storage", TRUE,                                                                                                                                                                                             
"Audit services (security & compliance)", TRUE,                                                                                                                                                                       
"Communications writing", FALSE,                                                                                                                                                                                       
"Invigilation of exam sessions", FALSE,                                                                                                                                                                                
"Event management services for the OCIO Connect conference", TRUE,                                                                                                                                                   
"Coaching services for excluded management employees", FALSE,                                                                                                                                                          
"Invigilation of examination sessions", FALSE,                                                                                                                                                                         
"Internal audit of ISO 14001 Environmental Management System", TRUE,                                                                                                                                                  
"Building demolition", FALSE,                                                                                                                                                                                          
"Develop new and enhance existing Smart forms using Adobe Livecycle Developer", TRUE,
"Service excellence workshop", FALSE,                                                                                                                                                        
"Microsoft premier support for Dynamics DSE", TRUE,                                                                                                                                         
"Modernization roadmap development", FALSE,                                                                                                                                                  
"Consulting services during contract negotiation process", FALSE,                                                                                                                            
"Move online Freedom-of-Information form into production environment", TRUE,                                                                                                                
"Vendor direct server monitoring tool", TRUE,                                                                                                                                               
"Lead a restricted time line negotiation", FALSE,                                                                                                                                            
"Jurisdictional scan of services for upcoming procurement preparation", FALSE,                                                                                                               
"Keynote speaking presentation at staff learning conference", FALSE,                                                                                                                         
"Convene, develop and deliver community forums", FALSE,                                                                                                                                      
"Develop an economic development impact model to assess the economic benefits of connectivity in rural BC", FALSE,                                                                           
"Impact assessment for data residing in the BC Geographic Warehouse (BCGW)", TRUE,                                                                                                          
"Provide high speed optical network capabilities to British Columbia higher education & research institutions and also cabinet operations video conferencing.", TRUE,                       
"Microsoft premier support services", TRUE,                                                                                                                                                 
"Provision of in-house Agile training and the development of Agile curriculum based on the established course.", TRUE,                                                                      
"Change management training", FALSE,                                                                                                                                                         
"Speaker at PCOP Symposium", FALSE,                                                                                                                                                          
"Consulting services for program development", FALSE,                                                                                                                                       
"Replacement of electrical/power meters", FALSE,                                                                                                                                             
"Consulting for electric vehicle stations", FALSE,                                                                                                                                           
"Modifications to courtrooms", FALSE,                                                                                                                                                        
"Customised contact centre training", FALSE,                                                                                                                                                 
"Update the “Q,” which is a system to capture analytics about services provided in Service BC Offices.", TRUE,                                                                              
"Legal services for BC Registries to help move off the mainframe and modernize the suite of registry applications.", TRUE,                                                                  
"Operational services for the Data Innovation Program which functions as a source of analysis, research and statistical support for provincial government ministries.", FALSE,               
"Confidential Review of a Program", FALSE,                                                                                                                                                   
"Executive coaching services", FALSE,                                                                                                                                                        
"Consulting services for Victoria Law Courts project", FALSE,                                                                                                                                
"Performance measurement certification training", FALSE,                                                                                                                                     
"Present indigenous tradition workshop at multi-day staff learning event", FALSE,                                                                                                            
"Present/facilitate/speak at multi-day staff learning event", FALSE,                                                                                                                         
"Speaker/Educator for the Procurement Community of Practice symposium", FALSE,                                                                                                               
"Diagnostic and troubleshooting software for SPAN/BC", TRUE,                                                                                                                               
"Electrical upgrades to 506 Government Street", FALSE,                                                                                                                                       
"Consultation regarding lessons learned through large procurements", FALSE,                                                                                                                 
"Briefing note training", FALSE,                                                                                                                                                             
"Develop project narrative, including refined core value proposition, key message, and standard language", FALSE,                                                                            
"Information technology general controls review", TRUE,                                                                                                                                     
"Speaker for the Procurement Community of Practice symposium", FALSE,                                                                                                                        
"Panel member for the Procurement Community of Practice symposium", FALSE,                                                                                                                   
"Keynote speaker for the Procurement Community of Practice symposium", FALSE,                                                                                                                
"Consulting services for central heating plant & district energy system", FALSE,                                                                                                             
"Strategic advisory services in support of Surrey Pretrial project agreement", FALSE,                                                                                                        
"Provision of legal services", FALSE,                                                                                                                                                        
"Research, statistical analysis, algorithm design, testing and validation for topics related to public programs and policies", FALSE,                                                        
"Financial monitoring and audit services", FALSE,                                                                                                                                            
"Consultation service to assist government in establishing criteria against which ministries can be assessed, with respect to meeting their information management (IM) requirements", TRUE,
"Guidance and training for Data Innovation program users", FALSE,                                                                                                                            
"Consultation for Real Estate Services", FALSE,                                                                                                                                              
"Executive coaching", FALSE,                                                                                                                                                                 
"Provision of voice and data network services within a defined geographical area", TRUE,                                                                                                    
"Comprehensive recruitment, attraction, planning and assessment services", FALSE,                                                                                                            
"Review of forthcoming information management initiatives", TRUE,                                                                                                                           
"Training for Open Text Module", TRUE,                                                                                                                                                      
"Present/speak at staff learning event", FALSE,                                                                                                                                              
"Communications for Data Innovation Program governance proposals", FALSE,                                                                                                                    
"Installation of courthouse E-comm system", TRUE,                                                                                                                                           
"Deliver training courses for strategic technology roles across the BC Government to address Chief Information Officer succession planning requirements", TRUE,                             
"Consulting services to work with stakeholders and parties with vested interests, and guide repatriation of headstones to the Woodlands Memorial Garden in New Westminster.", FALSE,         
"Provide digital telecommunications services to Gold River and Tahsis for PLNet", TRUE,                                                                                                    
"Travel expenses associated with attending workshops & meetings for BC Bid Renewal Project", TRUE,                                                                                          
"Training & mentoring on systems that are specific to the division", TRUE,                                                                                                                  
"Business process mapping linked to roles, responsibilities & accountabilities for BC Services Card issuance", TRUE,                                                                        
"Consulting services for contract negotiation", FALSE,                                                                                                                                       
"Develop conceptual model with core functional elements of emergency management", FALSE,                                                                                                     
"Consultation services for potential land sub-division", FALSE,                                                                                                                              
"Fees for providing the service of property appraisals", FALSE,                                                                                                                              
"Tenant improvements to building in Surrey", FALSE,                                                                                                                                          
"Assist in the development of Service BC's strategic service offering by facilitating the process and compiling a final report", FALSE,                                                      
"Acoustical testing Capital Park building a-1", FALSE,                                                                                                                                       
"Business Case Analysis Consulting", FALSE,                                                                                                                                                  
"Consulting Services", FALSE,                                                                                                                                                                
"Advisory Services - Cost Code Functionality", TRUE,                                                                                                                                        
"Six week contract to service the \"devOps\" system in the bridging period, while the competitive sourcing process is run.", TRUE,                                                          
"Assistance with development of three year strategic business plan", FALSE,                                                                                                                  
"Upgrade of SiteMinder and HealthCheck software", TRUE,                                                                                                                                     
"Consulting services for building project", FALSE,                                                                                                                                           
"Financial advisory services", FALSE,                                                                                                                                                        
"Maintenance of equipment", FALSE,                                                                                                                                                          
"BC Bid proposal evaluation - reimbursement for travel expenses", TRUE,                                                                                                                     
"Training on negotiations and vendor relationship advice", FALSE,                                                                                                                            
"Advisory services for the Sourcing/Tech Strategy", TRUE,                                                                                                                                   
"Stakeholder engagement, communications and materials preparation", FALSE,                                                                                                                   
"Prosci Canada change management certification course", FALSE,                                                                                                                               
"Professional development online courses", FALSE,                                                                                                                                            
"Videographer", TRUE,                                                                                                                                                                      
"Consulting services for professional development", FALSE,                                                                                                                                   
"Design of templates and materials", FALSE,                                                                                                                                                  
"Demographics profiling and analytics on British Columbia population, lifestyle, lifestage and clusters segmentation.", FALSE,                                                               
"Develop and deliver webinars to the BC Municipalities on how to create winning proposals and generate access to the Infrastructure Canada Smart City Challenge Program", FALSE,             
"Offsite records storage", TRUE,                                                                                                                                                           
"Speaker for Leadership Council Conference", TRUE,                                                                                                                                          
"Technical writing and communications support for Procurement Transformation", FALSE,                                                                                                        
"Great Northern Way Appraisal", FALSE,                                                                                                                                                       
"Development of a procurement strategy to modernize the procurement practices and tools", TRUE,                                                                                            
"Speaker for employee learning and development event (OCIO Connect 2017)", TRUE,                                                                                                            
"Correspondence tracking and reporting service (known as CLIFF)", TRUE,                                                                                                                     
"Annual maintenance and support for  HP TRIM licenses", TRUE,                                                                                                                               
"Financial year 17/18 consulting services", FALSE,                                                                                                                                           
"Facilitate executive-level workshops and provide instruction related to industry best practices for outsourcing including transition management and effective governance", FALSE,           
"Training and coaching for leaders and teams", FALSE,                                                                                                                                        
"Speaker/Presenter at Procurement Community of Practice Conference", FALSE,                                                                                                                  
"Supply and install 400 entrance pairs (voice and data cables) to Surrey Courthouse", TRUE,                                                                                                 
"Biometric Consulting for Okanagan Correctional Centre (year 2017/18)", FALSE,                                                                                                               
"Download, print, insert & deliver pay advices", FALSE,                                                                                                                                      
"Event coordinator for ministry-wide learning & development event", FALSE,                                                                                                                   
"Appraisal fee", FALSE  
)

imit_contracts %>% group_by(detailed_description) %>% count()


#-------------------------------------------------------------------------------
## Merge IMIT logical data
citz_imit_proc <- citz_proc_data_tidy %>%
  left_join(imit_contracts) %>%
  mutate(imit = case_when(str_detect(description_of_work, "^63") ~ TRUE,
                                    TRUE ~ imit))


#-------------------------------------------------------------------------------
## Write to tmp folder
write_csv(citz_imit_proc, here::here("tmp/citz_imit_proc_data.csv"))

















































