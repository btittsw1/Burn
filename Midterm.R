library(fpp3)
library(shiny)
library(seasonal)
library(shinydashboard)
#load data
FE <- read.csv("Midterm_Fire_Emblem_USA.csv") #USA "Fire Emblem" Google Trends from 2004-present
names(FE) <- c("month", "interest")
FE$month <- yearmonth(FE$month)
FE <- tsibble(FE, index = month)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Google Trends Analysis - Fire Emblem", titleWidth = 400),
  dashboardSidebar(width = 175,
                   sidebarMenu(
                     menuItem("Welcome!", tabName = "welcome"),
                     menuItem("Full Time Series", tabName = "full"),
                     menuItem("Choose Your Own Plot", tabName = "choose"),
                     menuItem("Analysis", tabName = "analyze")
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
              strong(em(h2("Background"))),
              p("", style = "font-family: 'times'; font-si16pt"),
              h4(em("Fire Emblem"),"is a fantasy RPG developed by Intelligent Systems and published by
              Nintendo. The first game was released in 1990 for the Famicom, and overall 16 core
              games have been released.",em("Fire Emblem"),"was exclusively released in Japan until",
              em("Super Smash Bros. Melee"),"released in 2001, when it introduced the",em("Fire Emblem"),
              "characters Marth and Roy to its roster. From there,",em("Fire Emblem"),"was sold
              worldwide, with the most recent entry being released in 2019."),
              h4("Fun Fact!", em("Fire Emblem"), "technically debuted worldwide in the form of an anime series.
              In 1997, a two-episode anime series based on the third game,", 
              em("Fire Emblem: Mystery of the Emblem,"), "was released worldwide. It was even dubbed in English!"),
              strong(em(h2("Instructions"))),
              p("", style = "font-family: 'times'; font-si16pt"),
              h4("Navigate the pages using the tabs to the left as follows:"),
              h5(strong("Full Time Series")),
              h6("View the full time series data. Use the date range selector within to narrow your view."),
              h5(strong("Choose Your Own Plot")),
              h6("Click the buttons to display different types of time series analysis plots."),
              h5(strong("Analysis")),
              h6("Read an analysis of the four plots you view.")),
      
      tabItem(tabName = "full",
              box(plotOutput("fe_plot"), width = 12,
                  status = "primary", title = "Full Time Series Plot", solidHeader = TRUE),
              
              box(dateRangeInput(
                inputId = "selected_date_range",
                label = "Select Date Range:",
                start = min(FE$month),
                end = max(FE$month),
                min = min(FE$month),
                max = max(FE$month),
              ),status = "primary", title = "Change Dates")),
      
      tabItem(tabName = "choose",
              box(plotOutput("choose_plot"), width = 12,
                  status = "primary", title = "The Plot You Choose Will Display Here",
                  solidHeader = TRUE),
              
              box(radioButtons(
                inputId = "select_plot",
                label = "Select a plot type to display",
                choices = c("Autocorrelation", "Decomposition", "Seasonality")
              ), status = "primary", title = "Plot Type")
              ),
      
      tabItem(tabName = "analyze",
              h4("The time series plot is very irregular, but it is cyclical in that there are generally spikes 
                  every time a new game is released. The autocorrelation plot shows that the most recent variables
                  are the most accurate for analysis. The decomposition plot shows that the time series is mostly affected by
                  randomness. The trend analysis shows that between 2012 and 2013 the data started to trend upward.
                  However, it started to trend downward again two years later. There is monthly seasonality, 
                  but it has little, if any, effect on the overall series. The seasonality plot confirms that 
                  seasonality isn't present in the overall series."))
  )
 )
)

server <- function(input, output) {
  
  output$fe_plot <- renderPlot({
    min_date <- input$selected_date_range[1]
    max_date <- input$selected_date_range[2]
    
    filter_plot <- FE[
      FE$month >= min_date &
        FE$month <= max_date,]
    
    autoplot(filter_plot)
  })
  
  output$choose_plot <-renderPlot({
    if(input$select_plot == "Autocorrelation"){
      acf(FE)
    }
    else if(input$select_plot == "Decomposition"){
      FE %>%
        model(classical_decomposition(interest)) %>%
        components()%>%
        autoplot()
    }
    else if(input$select_plot == "Seasonality"){
      gg_season(FE)
    }
  })
}
shinyApp(ui, server)