# config.R
# To work locally, copy config.R, rename it to config.R.local, add it to the 
# .gitignore file and update with your local paths

getConfig <- function() {
  if (Sys.getenv("APP_ENV") == "container") {
    # Container configuration. The container must have the environment variable 
    # APP_ENV = "container"
    list(
      xema_data_path = "/app/data/xema",
      geojson_data_path = "/app/data/geojson",
      environment = "container"
    )
  } else {
    # Local configuration
    list(
      xema_data_path = "local-path/data/xema",
      geojson_data_path = "local-path/data/geojson",
      environment = "local"
    )
  }
}