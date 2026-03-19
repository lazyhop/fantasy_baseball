
# done ----
# add select all team filter for the Full League Data Visuals PAge and for Full Leage Tables page
# make the backgrounds transparent, put words on tbale. remove the values
# on the mean z score chart on the comparison page. remove the legend
# week number charts are showing the wrong order!


# to do -----

# make the values show up when you hover over them  https://shiny.posit.co/r/articles/build/plot-interaction/
# on the mean z score chart on the comparison page. make it a vertical table. put the names In the bars?
# add a  "would you have won?" table
# add columns to team specific viz!
# resize tables on tab 1
# delete the third table on the front page?
# flip the 4th table so you can sort by z score!
# create a "replacement level" identifier for players on your roster. 
  # To "Flag" players who you should probably drop. So like 12 x (# of players on roster) = replacement ranking
# compare your record, compared to your z score total YTD. How unlucky are you?!!!!! "unluckiness"
# list of rosters, and current all player ranking. filterable by 15+30 days

# maybe ----
# add a max z score on charts to help with comparison?
# add a table to team specific vid?



# Loading Libraries ----
library(tidyverse)
#library(knitr)
#library(kableExtra)
library(RColorBrewer)

library(shiny)
library(shinyWidgets)
library(plotly)
#library(reactable)
library(DT)

# Loading Datasets  ----
 # load("Output Data/teams.RDS") # for debugging
 # load("Output Data/allWeeks.RDS") # for debugging
 # load("Output Data/stat_cats.RDS") # for debugging
load("teams.RDS")
load("allWeeks.RDS")
load("stat_cats.RDS")

# Data Manipulation ----
# getting data for every team, for every completed week
allWeekStats <- allWeeks %>%
  #filter(team_completed_games > 0) %>% # getting infor for any week with completed gams
  filter(team_remaining_games == 0) %>% # only getting info for completed weeks # change this in the future!!!!!
  select(team_stats_week, team_key, team_id, team_name, team_stats) %>%
  unnest(team_stats)

allWeekStats2 <- left_join(allWeekStats, stat_cats, by = 'stat_id') %>%
  filter(display_name != 'H/AB') %>%
  mutate(value = as.numeric(value)) %>%
  select(-stat_id) %>%
  rename(stat_category = display_name)

# ranking results by week and category. creating z scores
allWeekStats3 <- allWeekStats2 %>%
  group_by(team_stats_week, stat_category) %>%
  mutate(Rank = ifelse(stat_category %in% c('ERA', 'WHIP'),
                       rank(value),
                       rank(-value)),
         zScore = ifelse(stat_category %in% c('ERA', 'WHIP'),
                         -(value - mean(value))/sd(value),
                         (value - mean(value))/sd(value)))

# getting max range of zScore for data viz
# Using to create chart range minimum expand_limits() later on
zScoreRange <- allWeekStats3 %>%
  ungroup() %>%
  select(zScore) %>%
  mutate(zScore = round(abs(zScore), 0)) %>%
  filter(zScore == max(zScore)) %>%
  arrange(desc(zScore)) %>%
  distinct(zScore) %>%
  as.numeric()


# finding the highest mean zscore range by team. 
# Using to create chart range minimum expand_limits() later on
zScoreRangeMean <- allWeekStats3 %>%
  ungroup() %>%
  group_by(team_name) %>%
  summarise(mean_zScore = mean(abs(zScore), na.rm = TRUE)) %>%
  filter(mean_zScore == max(mean_zScore)) %>% 
  select(mean_zScore) %>%
  as.numeric() %>%
  ceiling() #rounding to whole number

# this is for the charts where zz-scores are summed
zScoreRangeSum <- allWeekStats3 %>%
  ungroup() %>%
  group_by(team_name, position_type, team_stats_week) %>%
  summarise(zScoreSum = sum(zScore)) %>%
  ungroup() %>%
  filter(zScoreSum == max(abs(zScoreSum))) %>%
  select(zScoreSum) %>%
  as.numeric() %>%
  ceiling()

# creating function for a results table by week for all source stats! ----
actualValuesWeek <- function(weekNum, teamsIncluded){
  actualValues <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum,
           team_name %in% c(teamsIncluded)) %>%
    select(-c(Rank, zScore, team_stats_week, team_key, position_type)) %>%
    pivot_wider(names_from = stat_category, values_from = value) %>%
    mutate(Week = paste('Week', weekNum), .before = team_name) %>%
    relocate(R, HR, RBI, SB, OPS,
             IP,
             W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
    rename(SVH = `SV+H`)
  
  actualValues
  
}

# creating function for a results table by week for rankings! ----
rankingsWeek <- function(weekNum, teamsIncluded){
  
  rankings <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum,
           team_name %in% c(teamsIncluded)) %>%
    select(-c(value, zScore, team_stats_week, team_key, position_type)) %>%
    spread(key = stat_category, value = Rank) %>%
    mutate(Week = paste('Week', weekNum), .before = team_name) %>%
    relocate(R, HR, RBI, SB, OPS,
             IP,
             W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
    rename(SVH = `SV+H`) 
  
  rankings
}

# creating function for a results table by week for z scores! ----
zScoreWeek <- function(weekNum, teamsIncluded){
  
  zScore <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum,
           team_name %in% c(teamsIncluded)) %>%
    select(-c(Rank, value, team_stats_week, team_key, position_type)) %>%
    spread(key = stat_category, value = zScore) %>%
    mutate(Week = paste('Week', weekNum), .before = team_name) %>%
    relocate(R, HR, RBI, SB, OPS,
             IP,
             W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
    rename(SVH = `SV+H`)
  
  zScore
}

# creating function for a results table by week for total z scores by hitting/pitching! ----
zScoreSumsWeek <- function(weekNum, teamsIncluded){
  
  zScoreSumsCat <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum,
           team_name %in% c(teamsIncluded)) %>%
    mutate(Week = paste('Week', weekNum),
           weekNumber = weekNum) %>%
    group_by(Week, weekNumber, team_name, position_type) %>%
    summarise(zScoreSum = sum(zScore)) %>%
    spread(key = team_name, value = zScoreSum)
  
  
  zScoreSumsCat2 <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum,
           team_name %in% c(teamsIncluded)) %>%
    mutate(Week = paste('Week', weekNum),
           weekNumber = weekNum) %>%
    group_by(Week, weekNumber, team_name) %>%
    summarise(zScoreSum = sum(zScore)) %>%
    spread(key = team_name, value = zScoreSum) %>%
    mutate(position_type = 'Total')
  
  zScoreSumsCat3 <- rbind(zScoreSumsCat, zScoreSumsCat2)
  
}

# Creating Ranges for the page
completedWeeks <- allWeeks %>%
  #filter(team_completed_games > 0) %>% # getting infor for any week with completed gams # replace next week
  filter(team_remaining_games == 0) %>% # only getting info for completed weeks
  select(team_stats_week) %>%
  distinct(team_stats_week) %>%
  unlist()

# creating list of team names
teamNameList <- allWeeks %>%
  distinct(team_name) %>%
  unlist() %>%
  as.vector()


# creating function for a results table by week for total z scores by hitting/pitching! ----
# selected list of teams for below functions

zScoreSums <- tibble()

for (c in completedWeeks){
  zScoreSumsadd <- zScoreSumsWeek(c, teamNameList)
  
  zScoreSums <- rbind(zScoreSums, zScoreSumsadd)
}

zScoreSums2 <- zScoreSums %>%
  gather(key = 'team_name', value = 'zScoreSum', 4:15)


# creating colors for the data viz ----
myColors <- brewer.pal(12, "Paired")
names(myColors) <- levels(allWeeks$team_name)
colScale <- scale_colour_manual(name = 'team_name', values = myColors)
colScaleFill <- scale_fill_manual(name = 'team_name', values = myColors)

# creating line type for the data viz ----
myLines <- c('solid', 'twodash','solid', 'twodash','solid', 'twodash',
             'solid', 'twodash','solid', 'twodash','solid', 'twodash')
names(myLines) <- levels(allWeeks$team_name)
lineType <- scale_linetype_manual(name = 'team_name', values = myLines)


table4Length <- length(zScoreSumsWeek(min(completedWeeks), teamNameList))

# Categories for Hitting & Pitching
statsListHitting <- c('R', 'HR', 'RBI', 'SB', 'OPS')
statsListPitching <- c('W', 'K', 'ERA', 'WHIP', 'SV+H')


# re-used cosmetics for ggplot
# Saving individual components then combining later
hline_motif <- geom_hline(yintercept = 0, color = 'dark gray')

above_avg_text <- annotate("text", x = -Inf, y = Inf, label = "Above Average", 
                           vjust = 2, hjust = -0.1, color = "#747474", size = 4,
                           family = 'Roboto', fontface = 'italic')

below_avg_text <- annotate("text", x = -Inf, y = -Inf, label = "Below Average", 
                           vjust = -1, hjust = -0.1, color = "#747474", size = 4,
                           family = 'Roboto', fontface = 'italic')

# makes background transparent
theme_motif <- theme(panel.background = element_rect(fill = 'transparent'),
                     plot.background = element_rect(fill = 'transparent', color = NA),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     legend.background = element_rect(fill = 'transparent'),
                     axis.text.y = element_blank(),  # Remove y-axis tick labels
                     axis.ticks.y = element_blank())

# Combine the motifs into a list (optional but organized)
visualMotifs <- list(hline_motif, above_avg_text, below_avg_text, theme_motif)



# ggplot(aes(x = team_name, y = zScoreSum, fill = team_name)) +
#   geom_col() +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
#   labs(title = 'Total Z-Score for Selected Week Range') +
#   
#   
#   geom_hline(yintercept = 0, color = 'dark gray') +
#   # Add "above average" text above the x-axis line
#   annotate("text", x = -Inf, y = Inf, label = "Above Average", 
#            vjust = 2, hjust = -0.1, color = "#747474", size = 4,
#            family = 'Roboto', fontface = 'italic') +
#   # Add "below average" text below the x-axis line
#   annotate("text", x = -Inf, y = -Inf, label = "Below Average", 
#            vjust = -1, hjust = -.1, color = "#747474", size = 4,
#            family = 'Roboto', fontface = 'italic') +
#   #colScaleFill + 
#   theme(panel.background = element_rect(fill='transparent'),
#         plot.background = element_rect(fill='transparent', color=NA),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.background = element_rect(fill='transparent'),
#         axis.text.y = element_blank(),  # Remove y-axis tick labels
#         axis.ticks.y = element_blank()  # Remove y-axis ticks (optional)
#   )


# shiny Application ----
# ui section ----
ui <- fluidPage(
  tabsetPanel(
    tabPanel('Full League Tables',
             fluidRow(
               titlePanel("Fantasy Baseball Analyzer")  # Application title
             ),
             
             fluidRow(
               column(width = 3,
                      selectInput('weeksToAnalyze', 'What week do you want to see?', choices =  as.vector(completedWeeks),
                                  selected = max(as.vector(completedWeeks)))
               ),
               column(width = 3,
                      pickerInput('teamListTables', 'Which teams do you want to see info for?',
                                  choices = teamNameList,
                                  multiple = TRUE,
                                  selected = teamNameList,
                                  width = '100%',
                                  options = list(
                                    `actions-box` = TRUE,  # Add select/deselect all buttons
                                    size = 10,  # Number of visible items in the dropdown
                                    `selected-text-format` = "count > 4"  # Show count if more than 3 items selected
                                  ))
               )),
             
             # fluidRow(
             #   selectInput('weeksToAnalyze', 'What week do you want to see?', choices =  as.vector(completedWeeks),
             #               selected = max(as.vector(completedWeeks)))
             # ),
             
             fluidRow(
               h3('Results for Selected Week'),
               DTOutput("actualValuesTable"),
               style = "padding-bottom:20px"
             ),
             
             fluidRow(
               h3('Rankings for Selected Week'),
               DTOutput("rankingsTable"),
               style = "padding-bottom:20px"
             ),
             
             fluidRow(
               h3('Z-Scores for Selected Week'),
               DTOutput("zScoreTable"),
               style = "padding-bottom:20px"
             ),
             
             fluidRow(
               h3('Sum of Z-Scores by Stat Category Type for Selected Week'),
               DTOutput("zScoreSumTable"),
               style = "padding-bottom:20px"
             )
    ),
    tabPanel('Full League Data Viz',
             
             fluidRow(style = "padding-top:20px",
                      h2('Full League Data Visuals')),
             
             fluidRow(
               column(width = 3,
                      sliderInput('weekRangeToAnalyze', 'Choose Week(s) of Data to View',
                                  value = c(min(as.numeric(completedWeeks)), max(as.numeric(completedWeeks))),
                                  min = min(as.numeric(completedWeeks)),
                                  max = max(as.numeric(completedWeeks)))
               ),
               column(width = 3,
                      pickerInput('teamListFull', 'Which teams do you want to see info for?',
                                  choices = teamNameList,
                                  multiple = TRUE,
                                  selected = teamNameList,
                                  width = '100%',
                                  options = list(
                                    `actions-box` = TRUE,  # Add select/deselect all buttons
                                    size = 10,  # Number of visible items in the dropdown
                                    `selected-text-format` = "count > 4"  # Show count if more than 3 items selected
                                  ))
               )),
             
             fluidRow(
               plotOutput('viz1')
             ),
             
             fluidRow(
               plotOutput('viz2'),
               style = "padding-bottom:40px"
             ),
             
             # hitting categories
             fluidRow(
               selectInput('hittingCat', 'What Hitting Category do you want to see?',
                           choices = statsListHitting,
                           selected = 'R'),
               plotOutput('vizB1'),
               style = "padding-bottom:20px"
             ),
             fluidRow(
               plotOutput('vizB2'),
               style = "padding-bottom:20px"
             ),
             
             # pitching categories
             fluidRow(
               selectInput('pitchingCat', 'What Pitching Category do you want to see?',
                           choices = statsListPitching,
                           selected = 'W'),
               plotOutput('vizP1'),
               style = "padding-bottom:20px"
             ),
             fluidRow(
               plotOutput('vizP2'),
               style = "padding-bottom:20px"
             )
    ),
    
    # team specific visuals
    tabPanel('Single Team Data Viz',
             
             fluidRow(style = "padding-top:20px",
                      h2('Team Specific Data Viz')),
             
             fluidRow(
               column(width = 3,
                      sliderInput('weekRangeToAnalyzeTeamSp', 'Choose Week(s) of Data to View',
                                  value = c(min(as.numeric(completedWeeks)), max(as.numeric(completedWeeks))),
                                  min = min(as.numeric(completedWeeks)),
                                  max = max(as.numeric(completedWeeks)))
               ),
               column(width = 3,
                      selectInput('teamList', 'Which team do you want to see info for?',
                                  choices = teamNameList)
               )
             ),
             
             fluidRow(
               plotOutput('vizTSp1'),
               style = "padding-bottom:40px"
             ),
             
             
             # hitting team specific categories
             fluidRow(
               selectInput('hittingCatTeamSp', 'What Hitting Category do you want to see?',
                           choices = statsListHitting,
                           multiple = TRUE,
                           selected = statsListHitting),
               style = "padding-top:20px"
             ),
             fluidRow(
               column(width = 8,
                      plotOutput('vizTSpB1'),
               ),
               column(width = 4,  
                      plotOutput('vizTSpB2')
               ),
               style = "padding-bottom:20px"
             ),
             
             # pitching team specific categories
             fluidRow(
               #style = "padding-top:20px",
               selectInput('pitchingCatTeamSp', 'What Pitching Category do you want to see?',
                           choices = statsListPitching,
                           multiple = TRUE,
                           selected = statsListPitching),
               style = "padding-top:20px"
             ),
             fluidRow(
               column(width = 8,
                      plotOutput('vizTSpP1'),
               ),
               column(width = 4,  
                      plotOutput('vizTSpP2')
               ),
               style = "padding-bottom:40px"
             ),
    )
  )   
)

# server function ----
server <- function(input, output, session) {
  
  
  output$actualValuesTable <- renderDT(
    datatable(
      actualValuesWeek(input$weeksToAnalyze, input$teamListTables), # function used to create the table
      options = list(
        paging = FALSE,
        dom = 't'
      ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
    )
  )
  
  output$rankingsTable <- renderDT(
    datatable(
      rankingsWeek(input$weeksToAnalyze, input$teamListTables), # function used to create the table
      options = list(
        paging = FALSE,
        dom = 't'
      ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
    )
  )
  
  output$zScoreTable <- renderDT(
    datatable(
      zScoreWeek(input$weeksToAnalyze, input$teamListTables), # function used to create the table
      options = list(
        paging = FALSE,
        dom = 't'
      ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
    ) %>%
      formatRound(columns = 4:14 , digits = 3)
  )
  
  output$zScoreSumTable <- renderDT(
    datatable(
      zScoreSumsWeek(input$weeksToAnalyze, input$teamListTables), # function used to create the table
      options = list(
        paging = FALSE,
        dom = 't'
      ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
    ) %>%
      #formatRound(columns = 4:table4Length , digits = 3) #using length of the table
      formatRound(columns = 4:length(zScoreSumsWeek(min(completedWeeks), input$teamListTables)), digits = 3)
  )
  
  output$viz1 <- renderPlot(
    zScoreSums2 %>%
      filter(position_type == 'Total',
             team_name %in% c(input$teamListFull),
             weekNumber %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>% # add the variable here!
      group_by(team_name) %>%
      summarise(zScoreSum = sum(zScoreSum)) %>%
      ggplot(aes(x = team_name, y = zScoreSum, fill = team_name)) +
        geom_col() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        labs(title = 'Total Z-Score for Selected Week Range') +
        visualMotifs +
          colScaleFill +
          expand_limits(y = c(-zScoreRangeSum,zScoreRangeSum)),
    res = 96)
  
  output$viz2 <- renderPlot(
    zScoreSums2 %>%
      filter(position_type != 'Total',
             team_name %in% c(input$teamListFull),
             weekNumber %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      group_by(team_name, position_type) %>%
      summarise(zScoreSum = sum(zScoreSum)) %>%
      ggplot(aes(x = team_name, y = zScoreSum, fill = position_type)) +
      geom_col(position = 'dodge') +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(title = 'Total Z-Score for Selected Week Range by Stat Type') +
      visualMotifs +
      expand_limits(y = c(-zScoreRangeSum,zScoreRangeSum)) +
      theme(#text=element_text(size=6),
        legend.key.size = unit(.5, 'cm')),
    res = 96)
  
  # hitting visuals
  output$vizB1 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category == input$hittingCat,
             team_name %in% c(input$teamListFull),
             team_stats_week %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      ggplot(aes(x = as.numeric(team_stats_week), y = zScore, color = team_name,
                 group = team_name, linetype = team_name)) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      labs(title = paste('Z-Score for Selected Week Range for', input$hittingCat, sep = ' ')) +
      visualMotifs +
      expand_limits(y = c(-zScoreRange, zScoreRange)) +
      colScale +
      lineType +
      xlab(label = 'Week Number') +
      scale_x_continuous(breaks=seq(min(input$weekRangeToAnalyze), max(input$weekRangeToAnalyze), 1),
                         limits=c(min(input$weekRangeToAnalyze), max(input$weekRangeToAnalyze))),
    res = 96)
  
  output$vizB2 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category == input$hittingCat,
             team_name %in% c(input$teamListFull),
             team_stats_week %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      group_by(team_name) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
      ggplot(aes(x = team_name, y = zScore, fill = team_name)) +
      geom_col() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(title = paste('Mean Z-Score for Selected Week Range for', input$hittingCat, sep = ' ')) +
      visualMotifs +
      #scale_color_discrete(name = "Category") +
      theme(legend.position = "none") +
      expand_limits(y = c(-zScoreRangeMean, zScoreRangeMean)) +
      colScaleFill,
    res = 96)
  
  # pitching visuals
  output$vizP1 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category == input$pitchingCat,
             team_name %in% c(input$teamListFull),
             team_stats_week %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      ggplot(aes(x = as.numeric(team_stats_week), y = zScore, color = team_name,
                 group = team_name, linetype = team_name)) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      labs(title = paste('Z-Score for Selected Week Range for', input$pitchingCat, sep = ' ')) +
      visualMotifs +
      expand_limits(y = c(-zScoreRange, zScoreRange)) +
      geom_hline(yintercept = 0, color = 'dark gray') +
      colScale +
      lineType +
      xlab(label = 'Week Number') +
      scale_x_continuous(breaks=seq(min(input$weekRangeToAnalyze), max(input$weekRangeToAnalyze), 1),
                         limits=c(min(input$weekRangeToAnalyze), max(input$weekRangeToAnalyze))),
    res = 96)
  
  output$vizP2 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category == input$pitchingCat,
             team_name %in% c(input$teamListFull),
             team_stats_week %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      group_by(team_name) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
      ggplot(aes(x = team_name, y = zScore, fill = team_name)) +
      geom_col() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(title = paste('Mean Z-Score for Selected Week Range for', input$pitchingCat, sep = ' ')) +
      visualMotifs +
      #scale_color_discrete(name = "Category") +
      theme(legend.position = "none") +
      expand_limits(y = c(-zScoreRangeMean, zScoreRangeMean)) +
      colScaleFill,
    res = 96)
  
  # team specific visuals
  output$vizTSp1 <- renderPlot(
    zScoreSums2 %>%
      filter(team_name == input$teamList,
             weekNumber %in% c(min(input$weekRangeToAnalyzeTeamSp):max(input$weekRangeToAnalyzeTeamSp))) %>%
      ggplot(aes(x = as.numeric(weekNumber), y = zScoreSum, color = position_type, group = position_type)) +
      geom_line() +
      geom_point() +
      labs(title = paste('Z-Score Sum for Selected Week Range by Stat Category for', input$teamList, sep = ' ')) +
      visualMotifs +
      expand_limits(y = c(-zScoreRangeSum, zScoreRangeSum)) +
      xlab(label = 'Week Number') +
      scale_x_continuous(breaks=seq(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp), 1),
                         limits=c(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp))),
    res = 96)
  
  # team specific - hitting
  output$vizTSpB1 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category %in% c(input$hittingCatTeamSp),
             team_name == input$teamList,
             team_stats_week %in% c(min(input$weekRangeToAnalyzeTeamSp):max(input$weekRangeToAnalyzeTeamSp))) %>%
      ggplot(aes(x = as.numeric(team_stats_week), y = zScore, color = stat_category)) +
      geom_line() +
      geom_point() +
      labs(title = paste('Z-Score for Selected Week Range and Team for Hitting Categories', sep = ' ')) +
      visualMotifs +
      xlab(label = 'Week Number') +
      scale_x_continuous(breaks=seq(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp), 1),
                         limits=c(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp))) +
      scale_color_discrete(name = "Category") +
      expand_limits(y = c(-zScoreRange, zScoreRange)),
    res = 96)
  
  output$vizTSpB2 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category %in% c(input$hittingCatTeamSp),
             team_name == input$teamList,
             team_stats_week  %in% c(min(input$weekRangeToAnalyzeTeamSp):max(input$weekRangeToAnalyzeTeamSp)))  %>%
      group_by(stat_category) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
      ggplot(aes(x = stat_category, y = zScore, fill = stat_category)) +
      geom_col() +
      labs(title = str_wrap(paste('Mean Z-Score for Selected Week Range and Team for Hitting Categories', sep = ' '),
                            width = 55)) +
      visualMotifs +
      scale_color_discrete(name = "Category") +
      theme(legend.position = "none") +
      expand_limits(y = c(-zScoreRangeMean, zScoreRangeMean)),
    res = 96)
  
  
  # team specific - pitching
  output$vizTSpP1 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category %in% c(input$pitchingCatTeamSp),
             team_name == input$teamList,
             team_stats_week %in% c(min(input$weekRangeToAnalyzeTeamSp):max(input$weekRangeToAnalyzeTeamSp))) %>%
      ggplot(aes(x = as.numeric(team_stats_week), y = zScore, color = stat_category)) +
      geom_line() +
      geom_point() +
      labs(title = paste('Z-Score for Selected Week Range and Team for Pitching Categories', sep = ' ')) +
      visualMotifs +
      xlab(label = 'Week Number') +
      scale_x_continuous(breaks=seq(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp), 1),
                         limits=c(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp))) +
      scale_color_discrete(name = "Category") +
      expand_limits(y = c(-zScoreRange, zScoreRange)),
    res = 96)
  
  output$vizTSpP2 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category %in% c(input$pitchingCatTeamSp),
             team_name == input$teamList,
             team_stats_week  %in% c(min(input$weekRangeToAnalyzeTeamSp):max(input$weekRangeToAnalyzeTeamSp)))  %>%
      group_by(stat_category) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
      ggplot(aes(x = stat_category, y = zScore, fill = stat_category)) +
      geom_col() +
      labs(title = str_wrap(paste('Mean Z-Score for Selected Week Range and Team for Pitching Categories', sep = ' '), 55)) +
      visualMotifs +
      scale_color_discrete(name = "Category") +
      theme(legend.position = "none") +
      expand_limits(y = c(-zScoreRangeMean, zScoreRangeMean)),
    res = 96)
}

# Run the application ----
shinyApp(ui = ui, server = server)








# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Fantasy Baseball Analyzer"),
#   
#   fluidRow(
#     column(4,
#            selectInput('weeksToAnalyze', 'What week do you want to see?', choices =  as.vector(completedWeeks))
#     )
#   ),
#   
#   fluidRow(
#     column(8,
#            htmlOutput("actualValuesTable", width = '100%')
#     )
#   )
# )
# 
# server <- function(input, output) {
#   
#   output$actualValuesTable <- renderText(
#     reactive({
#       kable(actualValuesWeek(input$weeksToAnalyze), caption = "Actual Results by Team for Selected Week")
#     })
#   )
#   
# }



