library(shiny)
library(bslib)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggExtra)
library(thematic)

### Grqphs to create: Scatterplot and Parallel Coordinates Plot

### Datasets: penguins, airquality



# shiny app showing scatterplot on airquality dataset

thematic_shiny(font = "auto")


# prepare airquality data
airquality$Date = as.Date(sprintf("1973-%02d-%02d", airquality$Month, airquality$Day), format="%Y-%m-%d")
airquality$Month <- fct_recode(as.factor(airquality$Month),
                               "May" = "5",
                               "June" = "6",
                               "July" = "7",
                               "August" = "8",
                               "September" = "9")

# subsets of columns suitable for ui selection
selection_x_data <- airquality |>
  select(-Month, -Day)
selection_y_data <- airquality |>
  select(-Month, -Day, -Date)


# prepare penguins data
penguins_csv <- "https://raw.githubusercontent.com/jcheng5/simplepenguins.R/main/penguins.csv"
penguins <- drop_na(readr::read_csv(penguins_csv))
# subsets of columns suitable for ui selection
penguins_num <- penguins |> select(where(is.numeric), -Year)
penguins_cat <- penguins |> select(where(is.character))


ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "cerulean"),
  
  headerPanel("Demonstration of Interactive Graphs Using Shiny"),
  
  tabsetPanel(
    type = "pills",
    
    tabPanel(
      "Univariate", 
      titlePanel("Airquality on Roosevelt Island 1973"),
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
      titlePanel("Airquality on Roosevelt Island 1973"),
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
    ),
    
    tabPanel("|"),
    
    tabPanel(
      "Multivariate",
      titlePanel("Penguins Data"),
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "species", "Filter by Species",
            choices = unique(penguins$Species),
            selected = unique(penguins$Species)
          ),
          checkboxGroupInput(
            "sex", "Filter by Sex",
            choices = unique(penguins$Sex),
            selected = unique(penguins$Sex)
          ),
          checkboxGroupInput(
            "island", "Filter by Island",
            choices = unique(penguins$Island),
            selected = unique(penguins$Island)
          ),
          varSelectInput("colorby", "Color By", penguins_cat, selected = "Species"),
          checkboxInput("by_species", "Show colors", TRUE),
        ),
        mainPanel(
          plotOutput('parcoordplot')
        )
      )
    )
  )
)



server <- function(input, output) {
  subsetted_penguins <- reactive({
    req(input$species)
    req(input$island)
    req(input$sex)
    req(input$colorby)
    penguins |> 
      filter(Species %in% input$species) |> 
      filter(Island %in% input$island) |> 
      filter(Sex %in% input$sex) |> 
      select(input$colorby, where(is.numeric), -Year)
  })
  
  output$parcoordplot <- renderPlot({
    data <- drop_na(subsetted_penguins())
    p <- ggparcoord(data
                    , scale = "std"
                    , missing = "exclude"
                    , alphaLines = 0.3
                    , groupColumn = if(input$by_species) 1 else NULL
                    , columns = 2:ncol(data)
    ) +
      list(
        if (input$by_species) scale_color_brewer(palette = "Set1"),
        xlab(''),
        ylab('Standard Deviations'),
        # labs(title='Parallel Coordinates Plot for Penguins Data'),
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 20, hjust = 1)
        )
      )
    
    p
  }, res = 100, height = 600)
  
  
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