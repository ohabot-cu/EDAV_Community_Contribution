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
airquality$Month <- as.factor(airquality$Month)


ui <- fluidPage(
  
  theme = bslib::bs_theme(version = 5, bootswatch = "quartz"),
  
  titlePanel("Analyzing Airquality on Roosevelt Island 1973"),
  
  tabsetPanel(
    type = "pills",
    
    tabPanel(
      "Univariate", 
      sidebarLayout(
        sidebarPanel(
          varSelectInput("var", "Variable?", data = airquality, selected = "Wind"),
          sliderInput("bins", "Number of Bins?", value = 40, min = 0, max =
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
          varSelectInput("xvar", "X Variable?", data = airquality, selected = "Wind"),
          varSelectInput("yvar", "Y Variable?", data = airquality, selected = "Temp"),
          checkboxGroupInput("month", "Filter by Month?",
                             choices = levels(airquality$Month), selected =
                               5)
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
                  geom_histogram(bins=input$bins)
              
    histplot
    
  })
  
  output$scatterplot <- renderPlot({
    
    airquality |>
      filter(Month %in% input$month) |>
      ggplot(mapping = aes(x = !!input$xvar, y = !!input$yvar)) +
      geom_point() -> scatter
    
    scatter
    
  })
  
  
}



shinyApp(ui, server)