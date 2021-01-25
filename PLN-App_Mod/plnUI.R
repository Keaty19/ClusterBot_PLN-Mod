library(shiny)
library(shinythemes)
source('plnRows2.R')

plnUI <- shinyUI({
  fluidPage(theme = shinytheme("cosmo"),
      Row1,
      Row2,
      Row3
  )
})