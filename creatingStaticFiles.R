

#assignInNamespace("is_interactive", function() { TRUE }, "httr")
#rlang::with_interactive({ options(httr_oob_default=TRUE)  })
#options(httr_oob_default=TRUE) 

# Logging ----
tryCatch({
    # Set up logging
    log_dir <- "D:/hopki/Documents/R_Projects/fantasy_baseball/logs"
    dir.create(log_dir, showWarnings = FALSE)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path(log_dir, paste0("dataUpdateExecution_", timestamp, ".log"))
    sink(file = log_file, append = TRUE, type = "output", split = TRUE)

    cat("Data Update Script Execution Started\n")
    cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

    # Actual Script ---------------------------------------------------------------------------------------------
    
    
    # Loading Libaries ----
    library(tidyverse)
    #devtools::install_github("https://github.com/macraesdirtysocks/YFAR")
    library(YFAR)
    
    # Set non-interactive mode
    #Sys.setenv("HTTR_SIMULATE_NON_INTERACTIVE" = "TRUE")
    #options(httr_oob_default = TRUE)
    
    # Creating Connection to Yahoo ----
    
    # creating token
    # my_token <- y_create_token(my_key = 'your-key-here',
    #                            my_secret = 'your-secret-here',
    #                            app_name = 'your-app-name-here')

    # Load token with error handling
    token_path <- "D:/hopki/Documents/R_Projects/fantasy_baseball/Source Files/tokenValue.rds"
    if(!file.exists(token_path)) stop("Token file not found at: ", token_path)
    
    my_token <- readRDS(token_path)
    
    # Verify and refresh token
    if(!my_token$can_refresh()) stop("Token cannot be refreshed")
    my_token$refresh()
    
    # league information ----
    games <- list()
    games <- y_games(my_token)
    
    # Pulling stats from Yahoo ----
    # I can create static versions of these files to use while not online!
    # so like...right now only lol
    #teams <- y_teams(league_key = '431.l.93970', token_name = my_token) # this is 2024
    teams <- y_teams(league_key = '458.l.90108', token_name = my_token)
    
    allWeeks <- y_team_stats(key = teams$team_key, my_token, week = c(1:24))
    
    stat_cats <- y_stats_categories(game_key = '431', my_token) %>%
      select(stat_id, display_name, position_type)
    
    
    # exporting files
    # general files
    save(teams, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/Output Data/teams.RDS')
    save(allWeeks, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/Output Data/allWeeks.RDS')
    save(stat_cats, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/Output Data/stat_cats.RDS')
    
    # files for the web App
    save(teams, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/baseballStats/teams.RDS')
    save(allWeeks, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/baseballStats/allWeeks.RDS')
    save(stat_cats, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/baseballStats/stat_cats.RDS')
    
    # files for the dev web App
    save(teams, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/baseballStatsDev/teams.RDS')
    save(allWeeks, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/baseballStatsDev/allWeeks.RDS')
    save(stat_cats, file = 'D:/hopki/Documents/R_Projects/fantasy_baseball/baseballStatsDev/stat_cats.RDS')
    
    
    # Logging -------------------------------------------------------------------------------------
    
  }, error = function(e) {
    cat("\nSCRIPT EXECUTION FAILED:\n")
    cat("Error:", e$message, "\n")
  }, finally = {
    cat("\nScript execution completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    sink()
  })






