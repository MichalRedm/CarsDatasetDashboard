library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(d3treeR)
library(dplyr)

df <- read.csv("CarsData.csv") %>%
  filter(year >= 2002, year <= 2020)%>%
  rename("Model" = "model", "Year" = "year") %>% 
  mutate(Manufacturer = recode(Manufacturer,
                               "hyundi" = "Hyundai",
                               "volkswagen" = "Volkswagen",
                               "skoda" = "Skoda",
                               "ford" = "Ford",
                               "toyota" = "Toyota",
                               "merc" = "Mercedes-Benz",
                               "vauxhall" = "Vauxhall"
  )) %>%
  group_by(Manufacturer, Year) %>%
  summarise(avgPrice = mean(price),
            avgMileage = mean(mileage),
            avgTax = mean(tax),
            avgMPG = mean(mpg),
            avgEngineSize = mean(engineSize),
            count = n())


dashboardPage(
  dashboardHeader(
    title = "90,000+ Cars Dataset"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description", icon = icon("file-lines"), tabName = "desc"),
      menuItem("Bubble chart", tabName = "viz", icon = icon("gauge-simple-high")),
      menuItem("Treemap", tabName = "treemap", icon = icon("tree")),
      menuItem("Line graph", tabName = "line", icon = icon("chart-line"))
      
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tags$script(HTML("
        $(document).on('shiny:connected', function(event) {
          function resizePlot() {
            var width = $('#treemap').width();
            var height = $(window).height() * 0.75; // Adjust height as needed
            Shiny.onInputChange('plot_width', width);
            Shiny.onInputChange('plot_height', height);
          }
          $(window).on('resize', resizePlot);
          resizePlot();
        });
      ")),
    tabItems(
      tabItem(tabName = "treemap",
              fluidRow(
                box(width = 12,
                    d3tree2Output("treemap", height = "85vh", width = "100%"))
              )),
      tabItem(tabName = "viz",
        fluidRow(
          style = "display: flex; align-items: stretch;",  # Make the fluidRow a flex container and align items to stretch
          column(9,
                 style = "display: flex; flex-direction: column;",  # Make the column a flex container and set direction to column
                 wellPanel(style = "background-color: #454352;",
                   fluidRow(
                     column(6,
                            selectInput("xaxis",
                                        label = h3("x axis"),
                                        choices = list(
                                          "Year" = "year",
                                          "Price" = "price",
                                          "Mileage" = "mileage",
                                          "MPG" = "mpg",
                                          "Engine size" = "engineSize"
                                        ),
                                        selected = "price")
                     ),
                     column(6,
                            selectInput("yaxis",
                                        label = h3("y axis"),
                                        choices = list(
                                          "Year" = "year",
                                          "Price" = "price",
                                          "Mileage" = "mileage",
                                          "MPG" = "mpg",
                                          "Engine size" = "engineSize"
                                        ),
                                        selected = "mileage")
                     )
                   ),
                   sliderInput("year", "Years:",
                               min = 1970, max = 2024,
                               value = c(1970, 2024),
                               round = TRUE,
                               sep = ""),
                   actionButton("backButton", "Back")
                 )
          ),
          column(3,
                 style = "display: flex;",  # Make the column a flex container
                 wellPanel(style = "background-color: #454352;",
                   style = "flex-grow: 1; display: flex; justify-content: center; align-items: center;",  # Make the wellPanel take full height and center content
                   plotOutput("gauge", width = "12vw", height = "12vw")
                 )
          )
        ),
        fluidRow(
          column(6,
            wellPanel(style = "background-color: #454352;",
              plotlyOutput("bubbleChart")
            ),
            wellPanel(style = "background-color:#454352;",
             fluidRow(
               column(6,
                 plotlyOutput("transmissionPie")
               ),
               column(6,
                 plotlyOutput("fuelTypePie")
               )
             )
            )
          ),
          column(6,
            wellPanel(
              style = "background-color: #454352; color: #cccccc;",
              DTOutput("dataTable")
            )
          )
        )
      ),
      tabItem(tabName = "desc",
              tags$a(href = 'https://www.put.poznan.pl/',
                     tags$img(src = 'PP_logotyp_ANG_CMYK.svg', title = "https://www.put.poznan.pl/", height = "80px")),
              h3("Authors: Deniz Aksoy and Michał Redmer"),
              h2("About the dataset"),
              h3("Cars Dataset from 1970 to 2024"),
              p("This dataset is a collection of over 90,000 used cars spanning from the year 1970 to 2024. It offers a comprehensive glimpse into the world of automobiles, providing valuable insights for researchers, enthusiasts, and industry professionals alike."),
              h4("Features Included:"),
              tags$ul(
                tags$li("Model: The model of the car."),
                tags$li("Year: The manufacturing year of the car."),
                tags$li("Price: The price of the car."),
                tags$li("Transmission: The type of transmission used in the car."),
                tags$li("Mileage: The mileage of the car."),
                tags$li("FuelType: The type of fuel used by the car."),
                tags$li("Tax: The tax rate applicable to the car."),
                tags$li("MPG: The miles per gallon efficiency of the car."),
                tags$li("EngineSize: The size of the car's engine."),
                tags$li("Manufacturer: The manufacturer of the car.")
              ),
              h3("Source"),
              p(
                "This dataset comes from Kaggle and can be found",
                a("here", href = "https://www.kaggle.com/datasets/meruvulikith/90000-cars-data-from-1970-to-2024", style = "color: #097cad;"),
                "."
              )
      ),
      tabItem(tabName = "line",
        fluidRow(
          column(
            width = 3,
            wellPanel(
              style = "background-color: #454352; color: #cccccc;",
              selectInput("yaxis_line", "Select Y-axis variable:", 
                          choices = list(
                            "Average Price" = "avgPrice", 
                            "Average Mileage" = "avgMileage", 
                            "Average Tax" = "avgTax", 
                            "Average MPG" = "avgMPG", 
                            "Average Engine Size" = "avgEngineSize"),
                          selected = "avgPrice"),
              checkboxGroupInput("manufacturers_line", "Select Manufacturers:", 
                                 choices = unique(df$Manufacturer), 
                                 selected = unique(df$Manufacturer))
            )
          ),
          column(
            width = 9,
            wellPanel(
              style = "background-color: #454352; color: #cccccc;",
              plotlyOutput("linePlot", height = "85vh")
            )
          )
        )
      )
    )
  )
)
