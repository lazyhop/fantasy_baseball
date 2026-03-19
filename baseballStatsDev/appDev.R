
# Loading Libraries ----
library(tidyverse)
#library(knitr)
#library(kableExtra)
library(RColorBrewer)

library(shiny)
#library(reactable)
library(DT)

# Loading Datasets  ----
# load("Output Data/teams.RDS")
# load("Output Data/allWeeks.RDS")
# load("Output Data/stat_cats.RDS")
load("teams.RDS")
load("allWeeks.RDS")
load("stat_cats.RDS")

# Data Manipulation ----
# getting data for every team, for every completed week
allWeekStats <- allWeeks %>%
  filter(team_remaining_games == 0) %>% # only getting info for completed weeks
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
zScoreRange <- allWeekStats3 %>%
  ungroup() %>%
  select(zScore) %>%
  mutate(zScore = round(abs(zScore), 0)) %>%
  filter(zScore == max(zScore)) %>%
  arrange(desc(zScore)) %>%
  distinct(zScore) %>%
  as.numeric()

# creating function for a results table by week for all source stats! ----
actualValuesWeek <- function(weekNum){
  actualValues <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
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
rankingsWeek <- function(weekNum){
  
  rankings <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
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
zScoreWeek <- function(weekNum){
  
  zScore <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
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
zScoreSumsWeek <- function(weekNum){
  
  zScoreSumsCat <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
    mutate(Week = paste('Week', weekNum),
           weekNumber = weekNum) %>%
    group_by(Week, weekNumber, team_name, position_type) %>%
    summarise(zScoreSum = sum(zScore)) %>%
    spread(key = team_name, value = zScoreSum)
  
  
  zScoreSumsCat2 <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
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
  filter(team_remaining_games == 0) %>% # only getting info for completed weeks
  select(team_stats_week) %>%
  distinct(team_stats_week) %>%
  unlist()

# creating function for a results table by week for total z scores by hitting/pitching! ----
zScoreSums <- tibble()

for (c in completedWeeks ){
  zScoreSumsadd <- zScoreSumsWeek(c)
  
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


# creating list of team names
teamNameList <- allWeeks %>%
  distinct(team_name) %>%
  unlist() %>%
  as.vector()



table4Length <- length(zScoreSumsWeek(min(completedWeeks)))

# Categories for Hitting & Pitching
statsListHitting <- c('R', 'HR', 'RBI', 'SB', 'OPS')
statsListPitching <- c('W', 'K', 'ERA', 'WHIP', 'SV+H')


# adding functions for server!

# shiny Application ----
# ui section ----
ui <- fluidPage(
  tabsetPanel(
    tabPanel('Full League Tables',
             fluidRow(
               titlePanel("Fantasy Baseball Analyzer")  # Application title
             ),
             
             fluidRow(
               selectInput('weeksToAnalyze', 'What week do you want to see?', choices =  as.vector(completedWeeks),
                           selected = max(as.vector(completedWeeks)))
             ),
             
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
             fluidRow(
               h2('Full League Data Visuals'),
               sliderInput('weekRangeToAnalyze', 'Choose Week(s) of Data to View',
                           value = c(min(as.numeric(completedWeeks)), max(as.numeric(completedWeeks))),
                           min = min(as.numeric(completedWeeks)),
                           max = max(as.numeric(completedWeeks))),
               plotOutput('viz1'),
               style = "padding-top:20px"
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
    tabPanel('Single Team Data Viz',
             # team specific visuals
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
               plotOutput('vizTSpB1'),
               style = "padding-top:20px;padding-bottom:20px"
             ),
             fluidRow(
               plotOutput('vizTSpB2'),
               style = "padding-bottom:20px"
             ),
             
             # pitching team specific categories
             fluidRow(
               #style = "padding-top:20px",
               selectInput('pitchingCatTeamSp', 'What Pitching Category do you want to see?',
                           choices = statsListPitching,
                           multiple = TRUE,
                           selected = statsListPitching),
               plotOutput('vizTSpP1'),
               style = "padding-top:20px;padding-bottom:20px"
             ),
             fluidRow(
               plotOutput('vizTSpP2'),
               style = "padding-bottom:40px"
             )
    )
  )   
)

# server function ----
server <- function(input, output, session) {
  
  
  output$actualValuesTable <- renderDT(
    datatable(
      actualValuesWeek(input$weeksToAnalyze), # function used to create the table
      options = list(
        paging = FALSE,
        dom = 't'
      ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
    )
  )
  
  output$rankingsTable <- renderDT(
    datatable(
      rankingsWeek(input$weeksToAnalyze), # function used to create the table
      options = list(
        paging = FALSE,
        dom = 't'
      ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
    )
  )
  
  output$zScoreTable <- renderDT(
    datatable(
      zScoreWeek(input$weeksToAnalyze), # function used to create the table
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
      zScoreSumsWeek(input$weeksToAnalyze), # function used to create the table
      options = list(
        paging = FALSE,
        dom = 't'
      ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
    ) %>%
      formatRound(columns = 4:table4Length , digits = 3) #using length of the table
  )
  
  output$viz1 <- renderPlot(
    zScoreSums2 %>%
      filter(position_type == 'Total',
             weekNumber %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>% # add the variable here!
      group_by(team_name) %>%
      summarise(zScoreSum = sum(zScoreSum)) %>%
      ggplot(aes(x = team_name, y = zScoreSum, fill = team_name)) +
      geom_col() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(title = 'Total Z-Score for Selected Week Range') +
      geom_hline(yintercept = 0, color = 'dark gray') +
      colScaleFill,
    res = 96)
  
  output$viz2 <- renderPlot(
    zScoreSums2 %>%
      filter(position_type != 'Total',
             weekNumber %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      group_by(team_name, position_type) %>%
      summarise(zScoreSum = sum(zScoreSum)) %>%
      ggplot(aes(x = team_name, y = zScoreSum, fill = position_type)) +
      geom_col(position = 'dodge') +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(title = 'Total Z-Score for Selected Week Range by Stat Type') +
      theme(#text=element_text(size=6),
        legend.key.size = unit(.5, 'cm')) +
      geom_hline(yintercept = 0, color = 'dark gray'),
    res = 96)
  
  # hitting visuals
  output$vizB1 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category == input$hittingCat,
             team_stats_week %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      ggplot(aes(x = team_stats_week, y = zScore, color = team_name,
                 group = team_name, linetype = team_name)) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      labs(title = paste('Z-Score for Selected Week Range for', input$hittingCat, sep = ' ')) +
      xlab(label = 'Week Number') +
      ylim(-zScoreRange, zScoreRange) +
      geom_hline(yintercept = 0, color = 'dark gray') +
      colScale +
      lineType,
    res = 96)
  
  output$vizB2 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category == input$hittingCat,
             team_stats_week %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      group_by(team_name) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
      ggplot(aes(x = team_name, y = zScore, fill = team_name)) +
      geom_col() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(title = paste('Mean Z-Score for Selected Week Range for', input$hittingCat, sep = ' ')) +
      ylim(-zScoreRange, zScoreRange) +
      geom_hline(yintercept = 0, color = 'dark gray') +
      colScaleFill,
    res = 96)
  
  # pitching visuals
  output$vizP1 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category == input$pitchingCat,
             team_stats_week %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      ggplot(aes(x = team_stats_week, y = zScore, color = team_name,
                 group = team_name, linetype = team_name)) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      labs(title = paste('Z-Score for Selected Week Range for', input$pitchingCat, sep = ' ')) +
      xlab(label = 'Week Number') +
      ylim(-zScoreRange, zScoreRange) +
      geom_hline(yintercept = 0, color = 'dark gray') +
      colScale +
      lineType,
    res = 96)
  
  output$vizP2 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category == input$pitchingCat,
             team_stats_week %in% c(min(input$weekRangeToAnalyze):max(input$weekRangeToAnalyze))) %>%
      group_by(team_name) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
      ggplot(aes(x = team_name, y = zScore, fill = team_name)) +
      geom_col() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(title = paste('Mean Z-Score for Selected Week Range for', input$pitchingCat, sep = ' ')) +
      ylim(-zScoreRange, zScoreRange) +
      geom_hline(yintercept = 0, color = 'dark gray') +
      colScaleFill,
    res = 96)
  
  # team specific visuals
  output$vizTSp1 <- renderPlot(
    zScoreSums2 %>%
      filter(team_name == input$teamList,
             weekNumber %in% c(min(input$weekRangeToAnalyzeTeamSp):max(input$weekRangeToAnalyzeTeamSp))) %>%
      ggplot(aes(x = weekNumber, y = zScoreSum, color = position_type, group = position_type)) +
      geom_line() +
      geom_point() +
      labs(title = paste('Z-Score Sum for Selected Week Range by Stat Category for', input$teamList, sep = ' ')) +
      geom_hline(yintercept = 0, color = 'dark gray'),
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
      xlab(label = 'Week Number') +
      scale_x_continuous(breaks=seq(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp), 1),
                         limits=c(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp))) +
      scale_color_discrete(name = "Category") +
      ylim(-zScoreRange, zScoreRange) +
      geom_hline(yintercept = 0, color = 'dark gray'),
    res = 96)
  
  output$vizTSpB2 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category %in% c(input$hittingCatTeamSp),
             team_name == input$teamList,
             team_stats_week  %in% c(min(input$weekRangeToAnalyzeTeamSp):max(input$weekRangeToAnalyzeTeamSp)))  %>%
      group_by(stat_category) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
      ggplot(aes(x = stat_category, y = zScore)) +
      geom_col() +
      labs(title = paste('Mean Z-Score for Selected Week Range and Team for Hitting Categories', sep = ' ')) +
      ylim(-zScoreRange, zScoreRange) +
      geom_hline(yintercept = 0, color = 'dark gray'),
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
      xlab(label = 'Week Number') +
      scale_x_continuous(breaks=seq(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp), 1),
                         limits=c(min(input$weekRangeToAnalyzeTeamSp), max(input$weekRangeToAnalyzeTeamSp))) +
      scale_color_discrete(name = "Category") +
      ylim(-zScoreRange, zScoreRange) +
      geom_hline(yintercept = 0, color = 'dark gray'),
    res = 96)
  
  output$vizTSpP2 <- renderPlot(
    allWeekStats3 %>%
      filter(stat_category %in% c(input$pitchingCatTeamSp),
             team_name == input$teamList,
             team_stats_week  %in% c(min(input$weekRangeToAnalyzeTeamSp):max(input$weekRangeToAnalyzeTeamSp)))  %>%
      group_by(stat_category) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
      ggplot(aes(x = stat_category, y = zScore)) +
      geom_col() +
      labs(title = paste('Mean Z-Score for Selected Week Range and Team for Pitching Categories', sep = ' ')) +
      ylim(-zScoreRange, zScoreRange) +
      geom_hline(yintercept = 0, color = 'dark gray'),
    res = 96)
}


# Run the application ----
shinyApp(ui = ui, server = server)


# title stuff!!!
# title1Paste <- reactive({
#   paste('Actual Results for Week', input$weeksToAnalyze)
# })
# 
# 
# output$table1title <- renderText({
#   title1Paste()
# }


# at some point, try functions!!!!





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
<<<<<<< HEAD
# }
=======
# }



>>>>>>> 2a51ab035c0aac4810af3baa1ea300363af81c04
