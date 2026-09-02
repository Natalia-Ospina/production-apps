source("requirements.R")
invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)
source("global.R")
source("funciones.R")

source("ui/dashboard.R")
source("server/dashboard.R")

shinyApp(ui, server)