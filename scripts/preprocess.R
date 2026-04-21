library(rsconnect)

source("app.R")
Sys.setenv(USE_BUNDLED_LIBUV = "1")
# Deploy
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINY_ACCOUNT"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(appDir = ".", appName = "DCMetagame-app", forceUpdate = TRUE)
