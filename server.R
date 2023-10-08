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
require(ggtext)
require(gt)


server <- function(input, output, session) {
 
 dat <- readRDS("data/megafile.rds")
 map_data <- readRDS("data/megafile_geo.rds")
 my_map <- readRDS("data/gb_map.rds")
 seatcomp <- read.xlsx("data/mrp_outputs.xlsx", sheet = "SeatComp")
 
 # Add a spinner:
 Waiter$new(id = "my_waiter")
 my_waiter <- Waiter$new(id = "my_waiter")
 
 selected_data <- reactive({
  dat[dat$PCON25NM == input$constituency,]
 })
 
 # Constituency Heading
 output$constituency_heading <- renderText({
  if (input$constituency != 'All of Great Britain') {
   
   df <- selected_data()
   
   return(df$const_headings)
   
  } else {
   
   return("Great Britain: Labour Majority 210")
   
  }
 })
 
 
 # Text explanations
  # Vote share
  output$vote_share_text <- renderUI({
   
   tags$div(
     HTML('<style>
            .body-text {
              font-family: "Public Sans Thin";
              font-size: 16px;
              margin-left: 40px;
              margin-right: 20px;
            }
          </style>'),
     
     
     tags$div(class = "body-text", 
              
              "We asked respondents ", HTML("<i>'If the Westminster Election was taking place tomorrow, and there was a candidate from all political parties standing in your constituency, which party do you think you would vote for?'</i>"),
              
              tags$br(), tags$br(),
              
              "The graph below shows the outcome of an MRP model on voting intention in the 632 newly formed constituencies in Great Britain, based on a sample of XXX adults.",
              
              tags$br(), tags$br(),
              
              HTML("To find out how MRP modelling works, please go to the <i>What is MRP?</i> tab above."),
              
              tags$br()
     )
   )
 })
  
  # Seat count/PTV
  output$ptv_seat_text <- renderUI({
    
    if (input$constituency != 'All of Great Britain') {
    
    tags$div(
      HTML('<style>
            .body-text {
              font-family: "Public Sans Thin";
              font-size: 16px;
              margin-left: 40px;
              margin-right: 20px;
            }
          </style>'),
      
      
      tags$div(class = "body-text", 
               
               "Our propensity to vote measure aims to uncover the relative feeling towards each party in the constituency.", 
               
               HTML("Respondents were asked <i>'How likely is it that you would ever vote for each of the following parties?' (On a scale from 0 to 10)</i>"),
               
               tags$br(), tags$br(),
               
               "The results below show the estimated share of the constituency likely to vote for each party, relative to their current vote share.",
               
               tags$br(), tags$br(),
               
               HTML("<i>Higher estimates indicate general overall excitement for a party within the constituency.</i>")
               
      )
    )
      
    } else {
      
      tags$div(
        HTML('<style>
            .body-text {
              font-family: "Public Sans Thin";
              font-size: 16px;
              margin-left: 40px;
              margin-right: 20px;
            }
          </style>'),
        
        
        tags$div(class = "body-text", 
                 
                 "Based on the 2024 constituency boundaries, and if the election were taking place today, Labour would be the largest party in parliament with a 210-strong majority.", 
                 
                 HTML("The results below show the combined probability of each party winning each seat. In terms of seat changes, the results indicate that:"),
                 
                 tags$br(), tags$br(),
                 
                 HTML("<ul>
                        <li>Lab would <b>hold 204</b> seats</li>
                        <li>Lab would <b>gain 215</b> seats from Con</li>
                        <li>Lab would <b>gain 20</b> seats from SNP</li>
                        <li>LDem would <b>gain 12</b> seats from Con</li>
                        </ul>")
        )
      )
      
    }
    
    
    
  })
  
  # 
 
 
 # Vote Share Data
 output$vote_share <- renderPlot({
  
  waiter::Waiter$new(id = "vote_share", html = spin_inner_circles())$show()
  
  if (input$constituency != 'All of Great Britain') {
   df <- selected_data()
   
   if (nrow(df) == 0) return()
   
   # Format the data to be ready for graph
   df_long <- df |> 
    rename(LDem19 = LD19,
           Reform19 = TBP19) |> 
    select(starts_with("vi_mean_"), Con19:Reform19) %>% 
    pivot_longer(
     everything(),
     names_to = "party") |> 
    mutate(
     year = case_when(
      str_starts(party, "vi_mean_") ~ "vote23",
      str_ends(party, "19") ~ "vote19",
      TRUE ~ NA_character_),
     party = str_remove_all(party, "vi_mean_|[0-9]+")) %>% 
    filter(value > 0.00) |> 
    pivot_wider(
     names_from = year,
     values_from = value) |> 
    mutate(across(everything(), ~replace_na(., 0.0)))
   
   # Plot Voting Intention Headline
   ggplot(df_long, aes(x = reorder(party, -vote23), y = vote23, fill = party)) +
    geom_bar(stat = "identity", width = 0.5, colour = NA) +
    geom_bar(aes(x = as.character(reorder(party, -vote23)), y = vote19), stat = "identity", alpha = 0.5, width = 0.5, position = position_nudge(x = 0.15), colour = NA) +
    geom_text(aes(label = ifelse(vote23 < 1, sprintf("%.2f", vote23), sprintf("%.0f", vote23))),
              vjust = -0.5, colour = "black", fontface = "bold", size = 5) +
    scale_fill_manual(values = c("Con" = "#0087DC", "Lab" = "#DC241F", "LDem" = "#FDBB30", "Green" = "#6AB023", "Reform" = "#12B6CF", "Other" = "#BEC3BF", "SNP" = "#FDF38E", "Plaid" = "#008142")) +
    labs(title = "Voting Intention",
         subtitle = "Current Prediction vs. 2019 Notionals",
         x = "Party",
         y = "Vote Share (%)") +
 
    ylim(0, max(c(df_long$vote23, df_long$vote19)) + 5) +
   
    theme_minimal() +
    
    hrbrthemes::theme_ipsum_pub() +
    
    theme(legend.position = "none",
          panel.background = element_rect(fill = "#E0E2DA"),
          plot.background = element_rect(fill = "#E0E2DA", color=NA))
  }
  else{
   
   # Plot Voting Intention Headline - Great Britain
   ggplot(seatcomp, aes(x = reorder(Party, -Vote23), y = Vote23, fill = Party)) +
    
    geom_bar(stat = "identity", width = 0.5, colour = NA) +
    
    geom_bar(aes(x = as.character(reorder(Party, -Vote23)), y = Vote19), stat = "identity", alpha = 0.5, width = 0.5, position = position_nudge(x = 0.15), colour = NA) +
    
    geom_text(aes(label = ifelse(Vote23 < 1, sprintf("%.2f", Vote23), sprintf("%.0f", Vote23))),
              vjust = -0.5, colour = "black", fontface = "bold", size = 5) +
    
    scale_fill_manual(values = c("Con" = "#0087DC", "Lab" = "#DC241F", "LDem" = "#FDBB30", "Green" = "#6AB023", "Reform" = "#12B6CF", "Other" = "#BEC3BF", "SNP" = "#FDF38E", "Plaid" = "#008142")) +
    
    labs(title = "Voting Intention",
         subtitle = "Current Prediction vs. 2019 Results",
         x = "Party",
         y = "Vote Share (%)") +
    
    ylim(0, max(seatcomp$Vote23) + 10) +
    
    hrbrthemes::theme_ipsum_pub() +
    
    theme(legend.position = "none",
          panel.background = element_rect(fill = "#E0E2DA"),
          plot.background = element_rect(fill = "#E0E2DA", color=NA))
   
  }
  
 })
 
 #### Propensity to Vote Plot
 output$propensity_to_vote <- renderPlot({
  
  waiter::Waiter$new(id = "propensity_to_vote", html = spin_inner_circles())$show()
  
  if (input$constituency != 'All of Great Britain') {
  
   df <- selected_data()
  
   if (nrow(df) == 0) return()
  
  ### If we go with the mean values, use this:
  df_long <- df %>%
    rename(LDem19 = LD19,
           Reform19 = TBP19) |>
    mutate(ptv_mean_Other = ptv_mean_UKIP + ptv_mean_Alba) |>
    select(-ptv_mean_UKIP, -ptv_mean_Alba) |>
    select(starts_with("vi_mean_"), starts_with("ptv_mean_")) |>
    pivot_longer(
      everything(),
      names_to = "party") |>
    mutate(year = case_when(
      str_starts(party, "vi_mean_") ~ "vote23",
      str_starts(party, "ptv_mean_") ~ "ptv",
      TRUE ~ NA_character_),
      party = str_remove_all(party, "vi_mean_|[0-9]+|ptv_mean_")) |>
    filter(value > 1) |>
    pivot_wider(
      names_from = year,
      values_from = value) |>
    mutate(across(everything(), ~replace_na(., 0.0)))
  
  # Plot PTV
  ggplot(df_long, aes(x = reorder(party, -vote23), y = vote23, fill = party)) +
   
   geom_bar(stat = "identity", width = 0.5, colour = "black", size = 0.5) +
   
   geom_segment(aes(x = as.numeric(reorder(party, -vote23)), xend = as.numeric(reorder(party, -vote23)), 
                    y = vote23, yend = ptv), colour = "black") +
   geom_point(aes(y = ptv, fill = party), colour = "black", shape = 21, size = 3, stroke = 1) + 
   
   geom_text(aes(y = (vote23 + ptv) / 2,
                 label = ifelse((ptv - vote23) < 1 & (ptv - vote23) > -1, 
                                sprintf("%+.2f%%", ptv - vote23), 
                                sprintf("%+.1f%%", ptv - vote23))),
             vjust = 0, nudge_x = 0.5, colour = "black", size = 4) +
   
   
   scale_fill_manual(values = c("Con" = "#0087DC", "Lab" = "#DC241F", "LDem" = "#FDBB30", "Green" = "#6AB023", "Reform" = "#12B6CF", "Other" = "#BEC3BF", "SNP" = "#FDF38E", "Plaid" = "#008142")) +
   
   scale_color_manual(values = c("Con" = "#0087DC", "Lab" = "#DC241F", "LDem" = "#FDBB30", "Green" = "#6AB023", "Reform" = "#12B6CF", "Other" = "#BEC3BF", "SNP" = "#FDF38E", "Plaid" = "#008142")) +
   
   labs(title = "Vote Share and Propensity to Vote",
        subtitle = "Share of constituency likely to vote for the party (7-10)",
        x = "Party",
        y = "Vote Share / Propensity to Vote (%)") +
   
   ylim(0, max(c(df_long$vote23, df_long$ptv)) + 5) +
  
  scale_x_discrete(expand = c(0.05, 0.5)) +
   
   theme_minimal() +
   
   hrbrthemes::theme_ipsum_pub() +
   
   theme(legend.position = "none",
         panel.background = element_rect(fill = "#E0E2DA"),
         plot.background = element_rect(fill = "#E0E2DA", color=NA)) 
  
  }
  
  else{
   
   # Plot Seats - Great Britain
   ggplot(seatcomp, aes(x = reorder(Party, -Seats23), y = Seats23, fill = Party)) +
    
    geom_bar(stat = "identity", width = 0.5, colour = NA) +
    
    geom_bar(aes(x = as.character(reorder(Party, -Seats23)), y = Seats19), stat = "identity", alpha = 0.5, width = 0.5, position = position_nudge(x = 0.15), colour = NA) +
    
    geom_text(aes(label = ifelse(Seats23 < 1, sprintf("%.2f", Seats23), sprintf("%.0f", Seats23))),
              vjust = -0.5, colour = "black", fontface = "bold", size = 5) +
    
    scale_fill_manual(values = c("Con" = "#0087DC", "Lab" = "#DC241F", "LDem" = "#FDBB30", "Green" = "#6AB023", "Reform" = "#12B6CF", "Other" = "#BEC3BF", "SNP" = "#FDF38E", "Plaid" = "#008142")) +
    
    labs(title = "Seat Prediction",
         subtitle = "Current Seat Prediction vs. 2019 GE",
         x = "Party",
         y = "Seats",
         caption = "Note: Based on average probabilities from MRP model. Total seats may not sum to 632.") +
    
    ylim(0, max(seatcomp$Seats23) + 40) +
    
    hrbrthemes::theme_ipsum_pub() +
    
    theme(legend.position = "none",
          panel.background = element_rect(fill = "#E0E2DA"),
          plot.background = element_rect(fill = "#E0E2DA", color=NA))
   
  }
   
   
 })
 
 # Tactical Voting Plot
 output$tactical_conditional <- renderUI({
  
  waiter::Waiter$new(id = "tactical_conditional", html = spin_inner_circles())$show()
  
  df <- selected_data()
  
  if (any(df$country == "England")) {
   # Clean the data for England tactical voting
   
   df_long <- df |> 
    select(starts_with("vi_mean_"), starts_with("et_mean_")) %>% 
    pivot_longer(
     everything(),
     names_to = "party") |> 
    mutate(
     year = case_when(
      str_starts(party, "vi_mean_") ~ "vote23",
      str_starts(party, "et_mean_") ~ "tactical",
      TRUE ~ NA_character_),
     party = str_remove_all(party, "vi_mean_|et_mean_")) %>% 
    filter(value > 0.00) |> 
    pivot_wider(
     names_from = year,
     values_from = value) |> 
    mutate(across(everything(), ~replace_na(., 0.0)))  
   
   # Plot England Tactical Vote
   eng_plot <- ggplot(df_long, aes(x = reorder(party, -tactical), y = tactical, fill = party)) +
    
    geom_bar(stat = "identity", width = 0.5, colour = NA) +
    
    geom_bar(aes(x = as.character(reorder(party, -tactical)), y = vote23), stat = "identity", alpha = 0.5, width = 0.5, position = position_nudge(x = 0.15), colour = NA) +
    
    geom_text(aes(label = ifelse(tactical < 1, sprintf("%.2f", tactical), sprintf("%.0f", tactical))),
              vjust = -0.5, colour = "black", fontface = "italic", size = 5) +
    
    scale_fill_manual(values = c("Con" = "#0087DC", "Lab" = "#DC241F", "LDem" = "#FDBB30", "Green" = "#6AB023", "Reform" = "#12B6CF", "Other" = "#BEC3BF", "SNP" = "#FDF38E", "Plaid" = "#008142")) +
    
    labs(title = "Tactical Voting Comparison vs Voting Intention",
         subtitle = "If you thought that in your constituency, the contest was really\nbetween Labour & the Conservatives, and no other party stood a realistic\nchance of winning the seat, how do you think you would vote?",
         x = "Party",
         y = "Vote Share (%)") +
    
    ylim(0, max(df_long$tactical) + 10) +
    
    # theme_minimal() +
    
    hrbrthemes::theme_ipsum_pub() +
    
    theme(
     legend.position = "none",
     panel.background = element_rect(fill = "#E0E2DA"),
     plot.background = element_rect(fill = "#E0E2DA", color = NA),
     plot.subtitle = element_text(face = "italic", family = "Helvetica")
    )
   
   renderPlot(eng_plot)
   
  } else if (any(df$country == "Scotland")) {
   # Clean the data for Scotland tactical voting
   
   df_long <- df |> 
    select(starts_with("vi_mean_"), starts_with("st_mean_")) %>% 
    pivot_longer(
     everything(),
     names_to = "party") |> 
    mutate(
     year = case_when(
      str_starts(party, "vi_mean_") ~ "vote23",
      str_starts(party, "st_mean_") ~ "tactical",
      TRUE ~ NA_character_),
     party = str_remove_all(party, "vi_mean_|st_mean_")) %>% 
    filter(value > 0.00) |> 
    pivot_wider(
     names_from = year,
     values_from = value) |> 
    mutate(across(everything(), ~replace_na(., 0.0)))  
   
   # Plot Tactical Vote
   sco_plot <-  ggplot(df_long, aes(x = reorder(party, -tactical), y = tactical, fill = party)) +
    
    geom_bar(stat = "identity", width = 0.5, colour = NA) +
    
    geom_bar(aes(x = as.character(reorder(party, -tactical)), y = vote23), stat = "identity", alpha = 0.5, width = 0.5, position = position_nudge(x = 0.15), colour = NA) +
    
    geom_text(aes(label = ifelse(tactical < 1, sprintf("%.2f", tactical), sprintf("%.0f", tactical))),
              vjust = -0.5, colour = "black", fontface = "italic", size = 5) +
    
    scale_fill_manual(values = c("Con" = "#0087DC", "Lab" = "#DC241F", "LDem" = "#FDBB30", "Green" = "#6AB023", "Reform" = "#12B6CF", "Other" = "#BEC3BF", "SNP" = "#FDF38E", "Plaid" = "#008142")) +
    
    labs(title = "Tactical Voting Comparison vs Voting Intention",
         subtitle = "If you thought that in your constituency, the contest was really\nbetween Labour & the SNP, and no other party stood a realistic\nchance of winning the seat, how do you think you would vote?",
         x = "Party",
         y = "Vote Share (%)") +
    
    ylim(0, max(df_long$tactical) + 10) +
    
    hrbrthemes::theme_ipsum_pub() +
    
    theme(legend.position = "none",
          panel.background = element_rect(fill = "#E0E2DA"),
          plot.background = element_rect(fill = "#E0E2DA", color = NA),
          plot.subtitle = element_text(face = "italic", family = "Helvetica"))
   
   renderPlot(sco_plot)
   
  } else renderUI({
    
    tags$div(
     HTML('<style>
            .header {
              font-family: "Public Sans Bold";
              font-size: 24px;
              margin-left: 40px;
            }
            
            .body-text {
              font-family: "Public Sans Thin";
              font-size: 16px;
              margin-left: 40px;
            }
          </style>'),
     
     tags$div(class = "header", "Tactical Voting"),
     
     tags$br(),
     
     tags$div(class = "body-text", 
              "In ", HTML("<b>England</b>"), " and ", HTML("<b>Scotland,</b>"), " we asked respondents how they would vote if they thought that in their constituency, the contest was really between ",
              
              HTML("<i>Labour</i>"), " and the ", HTML("<i>Conservatives/SNP</i>"), ", and no other party stood a realistic chance of winning the seat.",
              
              tags$br(), tags$br(),
              
              HTML("The results show that under this scenario in England, <b>Labour would win 458 seats - 65 more</b> than under current voting intention."),
              
              tags$br(), tags$br(),
              
              HTML("In Scotland, this would increase Labour’s seat count to <b><i>28 - 6 more</b></i> than current voting intention."),
              
              tags$br(), tags$br(),
              
              HTML("<i>To see the results, please select a constituency in England or Scotland from the field above.</i>"),
              
              tags$br(), tags$br()
     )
    )
    
   })
   
  
 })
 
 
 
 # Top 3 Issues Plot
 output$top_issues <- renderPlot({
  
  waiter::Waiter$new(id = "top_issues", html = spin_inner_circles())$show()
  
  if (input$constituency != 'All of Great Britain') {
   
  df <- selected_data()
  
  if (nrow(df) == 0) return()
  
  # Create the issue data frame
  issues <- c(df$top_Issue, df$second_Issue, df$third_Issue)
  
  values <- c(df$top_Value, df$second_Value, df$third_Value)
  
  issue_df <- data.frame(issues, values)
  
  issue_df$issues <- gsub(" ", "\n", issue_df$issues)
  
  # Plot the issues:
  ggplot(issue_df, aes(x = reorder(issues, values), y = values)) +
   
   geom_bar(fill = "#078a09", colour = "#078a09", size = 0.002, width = 0.5, stat = "identity") +
   
   geom_text(aes(label = sprintf("%.0f", values)), vjust = 0.5, hjust = 1.5, colour = "white", fontface = "bold", size = 6) +
   
   coord_flip() +
   
   labs(title = "Top Three Issues",
        subtitle = "Which 3 issues will most affect how you vote at \nthe next Westminster General Election? \nPlease rank them in order of importance.",
        x = "",
        y = "Share of constituency selecting as top three (%)") +
   
   hrbrthemes::theme_ipsum_pub() +
   
   theme(legend.position = "none",
         panel.background = element_rect(fill = "#E0E2DA"),
         plot.background = element_rect(fill = "#E0E2DA", color=NA),
         axis.text.y = element_text(hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold.italic", size = 12),
         plot.margin = margin(1, 1, 1, -1)
   ) +
   
   scale_y_continuous(limits = c(0, max(issue_df$values) + 5), expand = c(0, 0))
  
  } else {
  
  # For Great Britain
  
  issues <- c("Cost of Living", "Health and the NHS", "The economy generally")
  
  values <- c(63, 54, 37)
  
  issue_df <- data.frame(issues, values)
  
  issue_df$issues <- gsub(" ", "\n", issue_df$issues)
  
  # Plot the issues:
  ggplot(issue_df, aes(x = reorder(issues, values), y = values)) +
   
   geom_bar(fill = "#078a09", colour = "#078a09", size = 0.002, width = 0.5, stat = "identity") +
   
   geom_text(aes(label = sprintf("%.0f", values)), vjust = 0.5, hjust = 1.5, colour = "white", fontface = "bold", size = 6) +
   
   coord_flip() +
   
   labs(title = "Top Three Issues",
        subtitle = "Which 3 issues will most affect how you vote at \nthe next Westminster General Election? \nPlease rank them in order of importance.",
        x = "",
        y = "Share of constituency selecting as top three (%)") +
   
   hrbrthemes::theme_ipsum_pub() +
   
   theme(legend.position = "none",
         panel.background = element_rect(fill = "#E0E2DA"),
         plot.background = element_rect(fill = "#E0E2DA", color=NA),
         axis.text.y = element_text(hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold.italic", size = 12),
         plot.margin = margin(1, 1, 1, -1)  # Adjust the left margin; here, -1 removes space
   ) +
   
   scale_y_continuous(limits = c(0, max(issue_df$values) + 5), expand = c(0, 0))
  
  }
  
 })
 
 
 # Economy Output
 output$economy <- renderUI({
  
  waiter::Waiter$new(id = "economy", html = spin_inner_circles())$show()
  
  if (input$constituency != 'All of Great Britain') {
  
   df <- selected_data()
  
  if (nrow(df) == 0) return()
  
  tagList(
   div(style = "text-align: center; margin-top: 50px; font-family: Public Sans Bold;",  # Moved the whole field down
       div(style = "width: 100px; height: 100px; border-radius: 50%; background-color: #0087DC; position: relative; margin: 0 auto;",  # Circle div
           div(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-size: 24px; color: white;",
               paste0(round(as.numeric(df$q1_mean)), "%")
           )
       ),
       div(style = "margin-top: 40px; font-size: 16px",  # Separated the text under the circle
           tags$em(" of voters in ", df$PCON25NM, " believe that the "),
           tags$b("economy will get worse in the next year."),
           br(),
           tags$em("Compared to 46% nationally.")
       )
   )
  )
  
  } else {
   
   tagList(
    div(style = "text-align: center; margin-top: 50px; font-family: Public Sans Bold;",  # Moved the whole field down
        div(style = "width: 100px; height: 100px; border-radius: 50%; background-color: #0087DC; position: relative; margin: 0 auto;",  # Circle div
            div(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-size: 24px; color: white;",
                paste0("46%")
            )
        ),
        div(style = "margin-top: 40px; font-size: 16px",  # Separated the text under the circle
            tags$em(" of voters in Great Britain believe that the "),
            tags$b("economy will get worse in the next year.")
        )
    )
   )
  
  }
  
 })
 
 # Labour Missions Plot
 output$labour_missions <- renderPlot({
  
  waiter::Waiter$new(id = "labour_missions", html = spin_inner_circles())$show()
  
  if (input$constituency != 'All of Great Britain') {
  
  df <- selected_data()
  
  if (nrow(df) == 0) return()
  
  missions <- c("Secure the <b>highest sustained growth<br>
                  in the G7</b> with good jobs and productivity<br>
                  growth in every part of the country<br> 
                  making everyone, not just a few,<br> 
                  better off.", 
                
                "Make Britain a <b>clean energy <br>
                  superpower</b> to create jobs, cut<br> 
                  bills and boost energy security <br>
                  with zero-carbon electricity by 2030,<br>
                  accelerating to net zero",
                
                "Build an <b>NHS fit for the future</b><br>
                  that is there when people need<br>
                  it with fewer lives lost to the<br> 
                  biggest killers in a fairer Britain,<br>
                  where everyone lives well for longer.",
                
                "Make <b>Britain’s streets safe by<br>
                  halving serious violent crime</b> and<br> 
                  raising confidence in the police and<br> 
                  criminal justice system to its<br> 
                  highest levels",
                
                "Break down barriers to opportunity<br>
                  at every stage for every child, by<br> 
                  <b>reforming the childcare and education<br> 
                  systems</b>, raising standards everywhere,<br>
                  and preparing young people for work<br> 
                  and life.")
  
  mission_values <- c(df$q2.1_mean, 
                      df$q2.2_mean, 
                      df$q2.3_mean, 
                      df$q2.4_mean, 
                      df$q2.5_mean)
  
  mission_df <- data.frame(missions, mission_values)
  
  ggplot(mission_df, aes(x = reorder(missions, mission_values), y = mission_values)) +
   
   geom_bar(size = 0.002, width = 0.6, stat = "identity", position = "identity", fill = '#DC241F', colour = "#DC241F") +
   
   geom_text(aes(label = sprintf("%.0f", mission_values)), vjust = 0.5, hjust = 1.5, colour = "white", fontface = "bold", size = 5) +
   
   coord_flip() +
   
   labs(title = "Labour's Five Missions",
        subtitle = "Ranked by highest personal importance to voters",
        x = "",
        y = "Share of constituency ranking the mission\nas most important to them personally (%)") +
   
   hrbrthemes::theme_ipsum_pub() +
   
   theme(legend.position = "none",
         panel.background = element_rect(fill = "#E0E2DA"),
         plot.background = element_rect(fill = "#E0E2DA", color=NA),
         # plot.margin = margin(1, 1, 1, 50, "pt"),  # Adjust this line for plot margins
         axis.text.y = element_markdown(size = 12, face = "italic", hjust = 0, margin = margin(0, 50, 0, 0, "pt"), family = "Public Sans Thin"),
         plot.margin = margin(1, 1, 1, -2))
  
  } else {
   
   tags$div(tags$br(), tags$br())
   
   missions <- c("Secure the <b>highest sustained growth<br>
                  in the G7</b> with good jobs and productivity<br>
                  growth in every part of the country<br> 
                  making everyone, not just a few,<br> 
                  better off.", 
                 
                 "Make Britain a <b>clean energy <br>
                  superpower</b> to create jobs, cut<br> 
                  bills and boost energy security <br>
                  with zero-carbon electricity by 2030,<br>
                  accelerating to net zero",
                 
                 "Build an <b>NHS fit for the future</b><br>
                  that is there when people need<br>
                  it with fewer lives lost to the<br> 
                  biggest killers in a fairer Britain,<br>
                  where everyone lives well for longer.",
                 
                 "Make <b>Britain’s streets safe by<br>
                  halving serious violent crime</b> and<br> 
                  raising confidence in the police and<br> 
                  criminal justice system to its<br> 
                  highest levels",
                 
                 "Break down barriers to opportunity<br>
                  at every stage for every child, by<br> 
                  <b>reforming the childcare and education<br> 
                  systems</b>, raising standards everywhere,<br>
                  and preparing young people for work<br> 
                  and life.")
   
   mission_values <- c(13.75, 
                       14.44, 
                       44.08, 
                       16.69, 
                       11.03)
   
   mission_df <- data.frame(missions, mission_values)
   
   ggplot(mission_df, aes(x = reorder(missions, mission_values), y = mission_values)) +
    
    geom_bar(size = 0.002, width = 0.6, stat = "identity", position = "identity", fill = '#DC241F', colour = "#DC241F") +
    
    geom_text(aes(label = sprintf("%.0f", mission_values)), vjust = 0.5, hjust = 1.5, colour = "white", fontface = "bold", size = 5) +
    
    coord_flip() +
    
    labs(title = "Labour's Five Missions",
         subtitle = "Ranked by highest personal importance to voters",
         x = "",
         y = "Share of constituency ranking the mission\nas most important to them personally (%)") +
    
    hrbrthemes::theme_ipsum_pub() +
    
    theme(legend.position = "none",
          panel.background = element_rect(fill = "#E0E2DA"),
          plot.background = element_rect(fill = "#E0E2DA", color=NA),
          # plot.margin = margin(1, 1, 1, 0, "pt"),  # Adjust this line for plot margins
          axis.text.y = element_markdown(size = 12, face = "italic", hjust = 0, margin = margin(0, 10, 0, 0, "pt"), family = "Public Sans Thin"),
          plot.margin = margin(1, 1, 1, -2))
   
  }
 })
 
 # Policy Areas Table
 output$policy_areas <- render_gt({
  
  waiter::Waiter$new(id = "policy_areas", html = spin_inner_circles())$show()
  
  
  if (input$constituency != 'All of Great Britain') {
  df <- selected_data()
  
  if (nrow(df) == 0) return()
  
  policy_names <- c("Education",
                    "The Environment",
                    "Immigration",
                    "Industrial Relations",
                    "The NHS",
                    "Refugees & Asylum seekers",
                    "Environment & Climate Change",
                    "The Economy generally",
                    "National Security",
                    "Housing",
                    "Cost of Living")
  
  policy_values <- c(df$q3.1_mean, 
                     df$q3.2_mean, 
                     df$q3.3_mean, 
                     df$q3.4_mean, 
                     df$q3.5_mean, 
                     df$q3.6_mean, 
                     df$q3.7_mean, 
                     df$q3.8_mean, 
                     df$q3.9_mean, 
                     df$q3.10_mean, 
                     df$q3.11_mean)
  
  policy_df <- data.frame(policy_names, policy_values)
  
  # datatable(policy_df, options = list(
  #  paging = FALSE, 
  #  searching = FALSE),
  #  class = 'cell-border stripe hover',
  #  rownames = FALSE
  # ) %>% formatStyle(
  #  'policy_values',
  #  backgroundColor = styleColorBar(range(policy_df$policy_values), 'red')
  # )
  
  formattable(policy_df, list(
   policy_values = color_bar("red")))
  
  } else {
   
   policy_names <- c("Education",
                     "The Environment",
                     "Immigration",
                     "Industrial Relations",
                     "The NHS",
                     "Refugees & Asylum seekers",
                     "Environment & Climate Change",
                     "The Economy generally",
                     "National Security",
                     "Housing",
                     "Cost of Living")
   
   policy_values <- c(mean(dat$q3.1_mean), 
                      mean(dat$q3.2_mean), 
                      mean(dat$q3.3_mean), 
                      mean(dat$q3.4_mean), 
                      mean(dat$q3.5_mean), 
                      mean(dat$q3.6_mean), 
                      mean(dat$q3.7_mean), 
                      mean(dat$q3.8_mean), 
                      mean(dat$q3.9_mean), 
                      mean(dat$q3.10_mean), 
                      mean(dat$q3.11_mean))
   
   policy_df <- data.frame(policy_names, policy_values)
   
   # datatable(policy_df, options = list(
   #  paging = FALSE, 
   #  searching = FALSE),
   #  class = 'cell-border stripe hover',
   #  rownames = FALSE
   # ) %>% formatStyle(
   #  'policy_values',
   #  backgroundColor = styleColorBar(range(policy_df$policy_values), 'red')
   # )
   
   policy_df <- policy_df |> 
    arrange(desc(policy_values))

   gt(policy_df) %>%
    tab_header(title = "Who do voters trust more on policies?") %>%
     cols_label(
       policy_values = "Share of constituency",
       policy_names = "Policy area"
     ) %>%
    fmt_number(
     columns = c(policy_values),
     decimals = 0,
     suffixing = TRUE
    ) %>%
    fmt(
     columns = c(policy_values),
     fns = function(x) {
      ifelse(
       x > 50,
       paste0("<span style='color: #DC241F'>", round(x), "%</span>"),
       paste0("<span style='color: #0087DC'>", round(x), "%</span>")
      )
     }
    ) |> 
    tab_options(
    table.background.color = "#E0E2DA")
   
   
  }
  
 })
 
 
 # Update the Leaflet map based on the selected constituency
 output$uk_map <- renderLeaflet({
  
  waiter::Waiter$new(id = "uk_map", html = spin_inner_circles())$show()
  
  map_data_filtered <- map_data[map_data$PCON25NM == input$constituency, ]
  
  if (input$constituency == 'All of Great Britain') {
   
   my_map %>% setView(lng = -2.5, lat = 55, zoom = 5.5) 
  
   } else {
   
    if (is.null(st_bbox(map_data_filtered))) {
    
     print("Bounding box is null")
   
     } else {
    
      my_map %>% setView(lng = mean(st_coordinates(map_data_filtered)[, "X"]), 
                       lat = mean(st_coordinates(map_data_filtered)[, "Y"]), 
                       zoom = 9) 
   }
  }
  
 })
 
 output$contextual_info <- renderUI({
   
   tags$div(
     HTML('<style>
            .header {
              font-family: "Public Sans Bold";
              font-size: 24px;
              margin-left: 40px;
            }
            
            .body-text {
              font-family: "Public Sans Thin";
              font-size: 16px;
              margin-left: 40px;
            }
          </style>'),
     
     tags$br(),
     tags$br(),
     
     tags$div(class = "header", "Get The Data"),
     
     tags$br(),
     
     tags$div(class = "body-text", 
              
              "Survation partnered with Lodestone Communications to conduct a poll of 6,170 adults on their voting intentions and topical issues.",
              
              HTML("We carried out MRP modelling to analyse the results of the poll for each of the 632 parliamentary constituencies
                   in Great Britain, based on boundaries for the 2024 General Election."),
              
              tags$br(), tags$br(),
              
              HTML("For the voting intention, we oversampled from a number of harder to reach seats in order to provide accurate estimates."),
              
              tags$br(), tags$br(),
              
              HTML("You can download the data in accessible format from our archive."),
              
              tags$br(), tags$br(),
              
              HTML("<i>Survation is a Market Research Society company partner. Survation is a  member of the British Polling Council and abides by its rules.</i>"),
              
              tags$br(),
              
              HTML("<i>Survation Ltd Registered in England & Wales Number 07143509</i>"),
              
              tags$br(), tags$br()
     )
   )
   
 })
 
 output$table_explanation <- renderUI({
   
   tags$div(
     HTML('<style>
            .header {
              font-family: "Public Sans Bold";
              font-size: 24px;
              margin-left: 5px;
            }
            
            .body-text {
              font-family: "Public Sans Thin";
              font-size: 16px;
              margin-left: 5px;
            }
          </style>'),
     
     tags$div(class = "header",
              "Trust on Policies"),
     
     tags$br(),
     
     tags$div(class = "body-text", 
              
              HTML("The table below shows the modelled share of voters who trust Labour more than the Conservatives on a policy area."),
              
              tags$br()
     )
   )
   
 })
 
 
}