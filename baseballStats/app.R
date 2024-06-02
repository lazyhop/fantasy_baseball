
# Loading Libraries ----
library(tidyverse)
library(knitr)
#library(kableExtra)
library(RColorBrewer)

library(shiny)
#library(reactable)
library(DT)

# Loading Datasets  ----
load("Output Data/teams.RDS")
load("Output Data/allWeeks.RDS")
load("Output Data/stat_cats.RDS")

# Data Manipulation ----
# getting data for every team, for every completed week
allWeekStats <- allWeeks %>%
  filter(team_remaining_games == 0) %>% # only getting info for completed weeks
  select(team_stats_week, team_key, team_id, team_name, team_stats) %>%
  unnest(team_stats)

allWeekStats2 <- left_join(allWeekStats, stat_cats, by = 'stat_id') %>%
  filter(display_name != 'H/AB') %>%
  mutate(value = as.numeric(value)) %>%
  select(-stat_id)

# ranking results by week and category. creating z scores
allWeekStats3 <- allWeekStats2 %>%
  group_by(team_stats_week, display_name) %>%
  mutate(Rank = rank(-value),
         zScore = ifelse(display_name %in% c('ERA', 'WHIP'),
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
    pivot_wider(names_from = display_name, values_from = value) %>%
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
    spread(key = display_name, value = Rank) %>%
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
    spread(key = display_name, value = zScore) %>%
    mutate(Week = paste('Week', weekNum), .before = team_name) %>%
    relocate(R, HR, RBI, SB, OPS,
             IP,
             W, K, ERA, WHIP, `SV+H`,, .after= team_name) %>%
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


# creating function for a results table by week for total z scores by hitting/pitching! ----
# zScoreSums <- tibble()
# 
# for (c in weekRangeAnalyze ){
#   zScoreSumsadd <- zScoreSumsWeek(c)
#   
#   zScoreSums <- rbind(zScoreSums, zScoreSumsadd)
# }
# 
# zScoreSums2 <- zScoreSums %>%
#   gather(key = 'team_name', value = 'zScoreSum', 4:15)




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

# Creating Ranges for the page
completedWeeks <- allWeeks %>%
  filter(team_remaining_games == 0) %>% # only getting info for completed weeks
  select(team_stats_week) %>%
  distinct(team_stats_week) %>%
  unlist()



actualValuesWeek(2)

# adding functions for server!

# new ui and server using DT package ----

ui <- fluidPage(
  
  # Application title
  titlePanel("Fantasy Baseball Analyzer"),
  
  
  selectInput('weeksToAnalyze', 'What week do you want to see?', choices =  as.vector(completedWeeks),
              selected = 8),
  
  textOutput('table1title'),
  DTOutput("actualValuesTable"),
  
  textOutput('table2title'),
  DTOutput("rankingsTable")
  
)


server <- function(input, output) {
 
  rValues <- reactiveValues()
  
  observe(
    rValues$week <-  paste('Actual Results for Week', input$weeksToAnalyze)
  )
  
  
  output$table1title <- renderText({
    rValues$week
  }
  )
  
  
  title2Paste <- reactive({
    paste('Rankings for Week', input$weeksToAnalyze, '[1 High]')
  })
  
  
  output$table2title <- renderText({
    title2Paste()
  }
  )
   
  output$actualValuesTable <- renderDT(
    datatable(
      actualValuesWeek(input$weeksToAnalyze),
      options = list(
        paging = FALSE,
        dom = 't'
        ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
      )
    )
  
  output$rankingsTable <- renderDT(
    datatable(
      rankingsWeek(input$weeksToAnalyze),
      options = list(
        paging = FALSE,
        dom = 't'
      ),
      rownames = FALSE #only including the table, not the info summary, or the pagination control!
    )
  )
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)



title1Paste <- reactive({
  paste('Actual Results for Week', input$weeksToAnalyze)
})


output$table1title <- renderText({
  title1Paste()
}





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



