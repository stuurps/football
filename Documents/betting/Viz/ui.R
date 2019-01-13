library(shiny)
library(DT)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("yeti"),
  tabsetPanel(type = "tabs",
              tabPanel("Championship", DT::dataTableOutput("CH")),
              tabPanel("League 1", DT::dataTableOutput("L1")),
              tabPanel("League 2", DT::dataTableOutput("L2")),
              tabPanel("All", DT::dataTableOutput("All"))
    )
  )
)
