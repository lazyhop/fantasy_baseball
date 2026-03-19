
# Function to publish to shinyapps.io
publish_to_shinyapps <- function() {
  if (!require("rsconnect")) install.packages("rsconnect")
  library(rsconnect)
  
  tryCatch({
    deployApp(
      appDir = "D:/hopki/Documents/R_Projects/fantasy_baseball/baseballStats",
      appName = "baseballStats",
      account = "lazyhop",
      forceUpdate = TRUE#,
      #logLevel = 'verbose'
    )
    #Y # this answers a follow-up question Do you want to proceed with deployment? [y/N]: Y
    message("Successfully published to shinyapps.io")
    return(TRUE)
  }, error = function(e) {
    message("Failed to publish: ", e$message)
    return(FALSE)
  })
}

# Call the publish function
publish_to_shinyapps()
