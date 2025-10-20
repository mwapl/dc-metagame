library(rsconnect)

source("app.R")

# Deploy
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINY_ACCOUNT"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(appDir = ".", appName = "DCMetagame-app", forceUpdate = TRUE)
