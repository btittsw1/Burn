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
                     menuItem("Simple Models", tabName = "simple"),
                     menuItem("Exponential Smoothing" ,tabName = "ETS"),
                     menuItem("Auto ARIMA", tabName = "ARIMA"),
                     menuItem("Manual ARIMA", tabName = "Manual_ARIMA"),
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
              h5(strong("Simple Models")),
              h6("Use the buttons to display different types of simple models."),
              h5(strong("Exponential Smoothing")),
              h6("Use the buttons to display the different methods of exponential smoothing."),
              h5(strong("Auto ARIMA")),
              h6("View a computer-generated ARIMA model"),
              h5(strong("Manual ARIMA")),
              h6("Input different parameters for an ARIMA model to see what parameters look best."),
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
      tabItem(tabName = "simple",
              box(plotOutput("simple_plots"), width = 12,
                  status = "primary", title = "The Plot You Choose Will Display Here",
                  solidHeader = TRUE),
              
              box(radioButtons(
                inputId = "simple_choose",
                label = "Select a plot type to display",
                choices = c("Naive", "Seasonal Naive", "Mean", "Drift")
              ), status = "primary", title = "Plot Type")),
      tabItem(tabName = "ETS",
              box(plotOutput("ets_plots"), width = 12,
                  status = "primary", title = "The Method You Choose Will Display Here",
                  solidHeader = TRUE),
              
              box(radioButtons(
                inputId = "ets_choose",
                label = "Select a method to use to display the data",
                choices = c("Holt's", "Holt's / Winter's")
              ), status = "primary", title = "Method Type")),
      tabItem(tabName = "ARIMA",
              box(plotOutput("auto_arima_plot"), width = 12,
                  status = "primary", title = "Auto ARIMA",
                  solidHeader = TRUE)),
      
      tabItem(tabName = "Manual_ARIMA",
              box(plotOutput("manual_arima"), width = 12,
                  status = "primary", title = "Manual ARIMA",
                  solidHeader = TRUE),
              
              box(numericInput(
                inputId = "AR",
                label = "Enter your desired autoregression value between 0 and 2",
                value = 0,
                min = 0,
                max = 2,
                step = 1
              ), status = "primary", title = "Autoregression Input"),
              
              box(numericInput(
                inputId = "I",
                label = "Enter your desired integration value between 0 and 2",
                value = 0,
                min = 0,
                max = 2,
                step = 1
              ), status = "primary", title = "Integration Input"),
              
              box(numericInput(
                inputId = "MA",
                label = "Enter your desired moving average value between 0 and 2",
                value = 0,
                min = 0,
                max = 2,
                step = 1
              ), status = "primary", title = "Moving Average Input")),
      
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
  
  output$simple_plots <- renderPlot({
    if(input$simple_choose == "Naive"){
      fit <- FE %>%
        model(NAIVE(interest))
      
      fit %>%
        forecast() %>%
        autoplot(FE) %>%
        labs(title = "Naive Model")
    }
    else if(input$simple_choose == "Seasonal Naive"){
      fit <- FE %>%
        model(SNAIVE(interest))
      
      fit %>%
        forecast() %>%
        autoplot(FE) %>%
        labs(title = "Seasonal Naive Model")
    }
    else if(input$simple_choose == "Mean"){
      fit <- FE %>%
        model(MEAN(interest))
      
      fit %>%
        forecast() %>%
        autoplot(FE) %>%
        labs(title = "Mean Model")
    }
    else if(input$simple_choose == "Drift"){
      fit <- FE %>%
        model(RW(interest ~ drift()))
      
      fit %>%
        forecast() %>%
        autoplot(FE) %>%
        labs(title = "Drift Model")
    }
  })
  
  output$ets_plots <- renderPlot({
    if(input$ets_choose == "Holt's"){
      fit <- FE %>%
        model(ETS(interest ~ trend()))
      
      fit %>%
        forecast() %>%
        autoplot(FE) %>%
        labs(title = "Holt's Method")
    }
    else if(input$ets_choose == "Holt's / Winter's"){
      fit <- FE %>%
        model(ETS(interest ~ trend() + season()))
      
      fit %>%
        forecast() %>%
        autoplot(FE) %>%
        labs(title = "Holt's / Winter's Method")
    }
  })
  
  output$auto_arima_plot <- renderPlot({
      fit <- FE %>%
        model(ARIMA(interest))
      
      fit %>%
        forecast() %>%
        autoplot(FE)
  })
  
  output$manual_arima <- renderPlot({
    fit <- FE %>%
      model(ARIMA(interest ~ 0 + pdq(input$AR, input$I, input$MA)))
    
    fit %>%
      forecast() %>%
      autoplot(FE)
  })
}
shinyApp(ui, server)