library(shiny)
library(bslib)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggExtra)
library(thematic)

### Grqphs to create: Scatterplot and Parallel Coordinates Plot

### Datasets: penguins, airquality

### this is a test comment



# shiny app showing scatterplot on airquality dataset

thematic_shiny(font = "auto")


#airquality = data("airquality")
airquality$Date = as.Date(sprintf("1973-%02d-%02d", airquality$Month, airquality$Day), format="%Y-%m-%d")
airquality$Month <- fct_recode(as.factor(airquality$Month),
                               "May" = "5",
                               "June" = "6",
                               "July" = "7",
                               "August" = "8",
                               "September" = "9")

selection_x_data <- airquality |>
  select(-Month, -Day)

selection_y_data <- airquality |>
  select(-Month, -Day, -Date)

ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "darkly"),
  
  titlePanel("Analyzing Airquality on Roosevelt Island 1973"),
  
  tabsetPanel(
    type = "pills",
    
    tabPanel(
      "Univariate", 
      sidebarLayout(
        sidebarPanel(
          varSelectInput("var", "Variable?", data = selection_y_data, selected = "Wind"),
          sliderInput("bins", "Number of Bins?", value = 40, min = 1, max =
                        100),
        ),
        mainPanel(
          plotOutput("histogram")
        )
        
      )
    ),
    
    tabPanel(
      "Bivariate", 
      sidebarLayout(
        sidebarPanel(
          varSelectInput("xvar", "X Variable?", data = selection_x_data, selected = "Date"),
          varSelectInput("yvar", "Y Variable?", data = selection_y_data, selected = "Temp"),
          checkboxGroupInput("month", "Filter by Month"
                             , choices = levels(airquality$Month)
                             , selected = levels(airquality$Month)
                             )
        ), 
        mainPanel(
          plotOutput("scatterplot")
        )

      )
    )
    
  )
)



server <- function(input, output) {
  
  output$histogram <- renderPlot({
    
    histplot <- ggplot(data = airquality, mapping=aes(x=!!input$var)) +
                  geom_histogram(bins=input$bins) +
                  labs(title='Histogram of Selected Weather Feature')
              
    histplot
  }, res = 100, height = 700, width = 700)
  
  output$scatterplot <- renderPlot({
    airquality |>
      drop_na() |>
      filter(Month %in% input$month) |>
      ggplot(mapping = aes(x = !!input$xvar, y = !!input$yvar)) +
      geom_point() + list(
        labs(title='Scatterplot of Roosevelt Island Weather Data (1973)'),
        if(input$xvar == "Date") geom_line()
      ) -> scatter
    
    scatter
    
  }, res = 100, height = 700, width = 700)
  
  
}



shinyApp(ui, server)