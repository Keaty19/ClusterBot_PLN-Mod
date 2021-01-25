library(shiny)
library(dplyr)
library(igraph)
library(network)
library(visNetwork)
library(shinythemes)
library(ggplot2)

source("plnUI.R",local=TRUE)
source("plnServer2.R",local=TRUE)

# Run the application 
shinyApp(
    ui = plnUI, 
    server = plnServer
)