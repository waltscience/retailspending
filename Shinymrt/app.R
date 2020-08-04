library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
dat <- read.csv("spending.csv", header = TRUE)[-c(343:348), ]
dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
nam <- gsub(".", " ", colnames(dat), fixed=TRUE)[-18]
rs <- data.frame(recession = c("r1", "r2", "r3"),
                 start = as.Date(c("2001-03-01", "2007-12-01", "2020-02-01"), "%Y-%m-%d"),
                 end = as.Date(c("2001-12-01", "2009-06-01", "2020-06-01"), "%Y-%m-%d")
                 )
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Retail Sales"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "y",
                  label = "Years",
                  min = 1992,
                  max = 2020,
                  value = c(2000, 2020),
                  step = 1,
                  sep=""
                  ),
      
      radioButtons(inputId = "s",
                  choices = nam,
                  label = "Sector",
                  )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      textOutput("selected_var")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    dat %>%
      select(everything()) %>%
      filter(Date > as.Date(paste(input$y[1], "-01-01", sep = ""), "%Y-%m-%d") & Date < as.Date(paste(input$y[2], "-07-01", sep = ""), "%Y-%m-%d")) -> d
    ggplot(data = d, aes(Date, d[, gsub(" ", ".", input$s, fixed = TRUE)])) +
      geom_rect(data = rs, aes(NULL, NULL, xmin = start, xmax = end),
                ymin = 0, 
                ymax = Inf, 
                fill = "grey50", 
                size = 0.7, 
                alpha = 0.3) +
      geom_line(data = d,
                aes(x = Date, y = d[, gsub(" ", ".", input$s, fixed = TRUE)]),
                colour = "firebrick3",
                size = 1) +
      labs(y = "Spending (millions of dollars)", x = "Year") +
      scale_x_date(date_breaks = "2 years",
                   breaks = "1 years",
                   limits = c(as.Date(min(d$Date)), as.Date(max(d$Date))),
                   date_labels = "%Y") +
      theme_few()
    
  })
  
  output$selected_var <- renderText({ 
    
    paste("Monthly spending in ", gsub("  ", " ", tolower(input$s)), ". Shaded areas represent U.S. recessions", sep = "")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



