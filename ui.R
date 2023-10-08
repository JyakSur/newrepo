require(formattable)
require(htmlwidgets)
require(shiny)
require(ggplot2)
require(dplyr)
require(DT)
require(readr)
require(openxlsx)
require(tidyverse)
require(leaflet)
require(sf)
require(waiter)
require(gt)

 dat <- readRDS("data/megafile.rds")
 map_data <- readRDS("data/megafile_geo.rds")
 my_map <- readRDS("data/gb_map.rds")

ui <- tagList(
    navbarPage(title=HTML(paste0("<i>Survation x Lodestone MRP Dashboard</i>")), windowTitle = "MRP Dashboard",
               
               tabPanel("Summary",
                        
                        waiter::use_waiter(),
                        
                        tags$style(HTML("
          body {
            margin-left: 2%;
            margin-right: 2%;
            background-color: #E0E2DA;
          }
          .leaflet-container {
            background: #E0E2DA !important;
          }
          .center-content {
            display: flex;
            align-items: center;
          }
              .gt_table {
          background-color: #E0E2DA !important;
        }
        .gt_col_headings {
          background-color: #E0E2DA !important;
        }
        .gt_table_title {
          background-color: #E0E2DA !important;
        }
        ")), 
                        # tabPanel(
                        #   tags$div(
                        #     style = "display: flex; justify-content: space-between; align-items: center; background-color: #f2f2f2; padding: 20px; margin-bottom: 20px;",
                        #     tags$a(href = "https://www.survation.com", target = "_blank", tags$img(src = "https://cdn.survation.com/wp-content/theme/images/logo.png", height = "40px", width = "230px")),
                        #     tags$h1("October 2023 MRP Results", style = "font-weight: bold; margin: auto;"),
                        #     tags$a(href = "https://www.lodestonecommunications.com/", target = "_blank", tags$img(src = "https://s3.amazonaws.com/blab-impact-published-production/QCMdf8ZA8lHLjRv5ICQrGqeqiEDlILkG", height = "60px", width = "290px"))
                        #   )
                        # ),
                        
                        fluidRow(
                            column(width = 4,  
                                   wellPanel(id = "explainer", class = "panel panel-default", fixed = FALSE,
                                             draggable = FALSE, style = "padding-top:10px; padding-left:20px; padding-right:20px; padding-bottom:10px; border-radius: 15px;",
                                             height = "auto",
                                             
                                             h3("Show me results for..."),
                                             
                                             # br(),
                                             
                                             selectInput("constituency", label = NULL, 
                                                         choices = list(
                                                             "Great Britain" = "All of Great Britain",
                                                             "Constituencies" = sort(unique(dat$PCON25NM))
                                                         )),
                                             selected = "All of Great Britain",
                                             width = 420),
                                   
                                   br(),
                                   
                                   leafletOutput("uk_map", height = "900px"),
                                   
                                   br()),
                            
                            column(width = 8,
                                   h1(textOutput("constituency_heading")),
                                   conditionalPanel(
                                       h1(textOutput("constituency_heading")),
                                       
                                       br(),
                                       
                                       fluidRow(
                                           column(6, 
                                                  uiOutput("vote_share_text"),
                                                  plotOutput("vote_share")),
                                           column(6, 
                                                  uiOutput("ptv_seat_text"),
                                                  plotOutput("propensity_to_vote"))
                                       ),
                                       
                                       br(),
                                       
                                       fluidRow(
                                           column(6, uiOutput("tactical_conditional")),
                                           column(4, plotOutput("top_issues")),
                                           column(2, div(class = "center-content", uiOutput("economy")))
                                       ))),
                            
                            fluidRow(
                                column(12, textOutput(h1(" ")))),
                            
                            
                            fluidRow(
                                column(4, uiOutput("contextual_info")),
                                column(5, plotOutput("labour_missions", height = 700, width = "100%")),
                                column(3, 
                                       uiOutput("table_explanation"),
                                       gt_output("policy_areas")))
                        )
               ),
               
               tabPanel("Data Playground"),
               
               tabPanel("What is MRP?",
                        
                        h4("Multilevel regression and post-stratification (MRP) is a way of 
                producing estimates of opinion and attitudes for small defined 
                geographic areas. It works by combining information from large 
                national samples (for example tens of thousands of respondents) 
                with ONS and census data."),
                        
                        br(),
                        
                        h5("The MR (Multi-level Regression) part"),
                        
                        h6("The responses given by respondents are modelled on the basis of their 
                demographic characteristics and what we know about their 
                area (its past voting history, how it voted in the EU referendum, and so on). 
                This is the “multilevel regression” part."),
                        
                        h6("For example, a 23 year old female living in London who works in the media sector and has a 
                university education has a higher probability of being a remain voter than a 72 year old male 
                living in Grimsby who is a retired former fisherman that left school at 16."),  
                        
                        h6("There are elements of a person’s lifestyle, background and life experience that 
                may provide an indication as to their likelihood to vote in a certain way (or 
                choose not to vote at all). “Multi-level regression” examines to what extent 
                each of these elements has an effect on behaviour."),
                        
                        br(),
                        
                        h5("The P (Post-stratification) part"),
                        
                        h6("In the subsequent “post-stratification” stage, we use census data to calculate 
                how many people of each demographic type live in each area and combine this with 
                additional relevant contextual information to predict how many of these people 
                will vote for each party (or have a certain opinion)."),
                        
                        h6("In this way, the estimates, although they are derived from a national sample, 
                end up being representative of the demographic make-up of each constituency."),
                        
                        br(),
                        
                        h5("New Westminster Boundaries"),
                        
                        h6("Survation developed an MRP model based on the 2023 final proposals of the independent 
                boundary commissions for England, Scotland, and Wales. The 2019 General Election results
                have been remapped onto the 2023 constituency boundaries using areal interpolation. 
                Demographic information has been taken from the 2021 census in England and Wales, and the 
                2011 census for Scotland, as well as mid-year population estimations."),
                        
                        br(),
                        
                        h6("Survation used MRP to correctly predict 94.3% of seats in the 2019 General Election.")
               ),
               
               
               tabPanel(title=HTML(paste0(
                   "<li>",
                   "<a href='https://www.survation.com/' target='_blank'><i>Made by Survation</i>",
                   "<i> on behalf of </i>",
                   "<href='https://www.lodestonecommunications.com/' target='_blank'><i>Lodestone Communications</i>",
                   "</a></li>"
               )))
               
    )
)
