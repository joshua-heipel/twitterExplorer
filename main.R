# install packages
packages <- c("shiny", "rtweet", "DT", "readr")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
}

ipak(packages)


source("explorer.R")
# # load code
# source("ui.R")
# source("server.R")
# 
# # run app
# shinyApp(ui, server)
