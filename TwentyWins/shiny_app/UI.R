## Ryan Elmore
## Shiny UI for Nuggets app

shinyUI(pageWithSidebar(
  headerPanel("Nuggets Playoff Probabilities"),
  
  sidebarPanel(
    wellPanel(
    sliderInput("WINS", 
                "Number of wins - losses:", 
                min = -20,
                max = 20, 
                value = 0),
                
    sliderInput("POINTDIFF", 
                "Point differential:", 
                min = -10,
                max = 10, 
                value = 0, 
                step = 0.25),

    dateInput("DATE", 
              "Date:", 
              value = as.POSIXct(Sys.Date()))
  )),
  
  mainPanel(
    plotOutput("LOGISTICPLOT")
  )
))
