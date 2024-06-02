library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(rlang)
library(DT)
library(treemap)
library(d3treeR)

make_gauge <- function(x, n, color =  "#097cad", title = NULL) {
  data.frame(x = c(5 * cos(seq(-pi, 0, len = 100)), 
                   3 * cos(seq(0, -pi, len = 100)),
                   5 * cos(seq(-pi, -pi + pi * x/n, len = 100)),
                   3 * cos(seq( -pi + pi * x/n, -pi, len = 100))),
             y = c(5 * -sin(seq(-pi, 0, len = 100)), 
                   3 * -sin(seq(0, -pi, len = 100)),
                   5 * -sin(seq(-pi, -pi + pi * x/n, len = 100)),
                   3 * -sin(seq( -pi + pi * x/n, -pi, len = 100))),
             group = rep(c("off", "on"), each = 200)) |>
    ggplot(aes(x, y, fill = group)) +
    geom_polygon() +
    scale_fill_manual(values = c("#eceaea", color), guide = "none") +
    annotate("text", x = 0, y = 0.6,
             label = x, size = 4,
             colour = "#cccccc", 
             fontface = 2) +
    ggtitle(title) +
    coord_equal() +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, color = "#cccccc",
                                vjust = 2, face = 2),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Data loading and preprocessing.
data <- read.csv("CarsData.csv") %>% 
  rename("Model" = "model") %>%
  mutate(
    Manufacturer = recode(
      Manufacturer,
      "hyundi" = "Hyundai",
      "volkswagen" = "Volkswagen",
      "skoda" = "Skoda",
      "ford" = "Ford",
      "toyota" = "Toyota",
      "merc" = "Mercedes-Benz",
      "vauxhall" = "Vauxhall"
    )
  ) %>% 
  mutate(across(c(transmission, fuelType), factor))

data_tree <- read.csv("CarsData.csv") %>% 
  rename("Model" = "model") %>% 
  mutate(Manufacturer = recode(Manufacturer,
                               "hyundi" = "Hyundai",
                               "volkswagen" = "Volkswagen",
                               "skoda" = "Skoda",
                               "ford" = "Ford",
                               "toyota" = "Toyota",
                               "merc" = "Mercedes-Benz",
                               "vauxhall" = "Vauxhall"
  )) %>%
  group_by(Model, Manufacturer) %>%
  summarise(count = n()) %>%
  mutate(Model = ifelse(count < 100 , "Other", Model)) %>%
  group_by(Model, Manufacturer) %>%
  summarise(count = sum(count))

p <- treemap(data_tree,
             index=c("Manufacturer","Model"),
             vSize="count",
             type="index",
             palette = "Paired",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )
)

inter <- d3tree2(p, rootname = "Manufacturers")

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

year_range <- range(df$Year)

colordict = c("Audi" = "chartreuse4",
              "BMW" = "red",
              "Ford" = "navyblue",
              "Hyundai" = "orange",
              "Mercedes-Benz" = "orchid",
              "Skoda" = "brown",
              "Toyota" = "blue",
              "Vauxhall" = "black",
              "Volkswagen" = "#C0C0C0"
)
choiceNames = c(
  "avgPrice" = "Average Price", 
  "avgMileage" = "Average Mileage", 
  "avgTax" = "Average Tax", 
  "avgMPG" = "Average MPG", 
  "avgEngineSize" = "Average Engine Size")

current_data <- data
current_transmission_labels <- c()
current_fuelType_labels <- c()

total_cars <- nrow(data)

shinyServer(function(input, output, session) {
  
  observeEvent(input$xaxis, {
    updateSelectInput(session, "yaxis",
                      choices = setdiff(c("price", "mileage", "mpg", "engineSize"), input$xaxis),
                      selected = isolate(input$yaxis))
  })
  
  observeEvent(input$yaxis, {
    updateSelectInput(session, "xaxis",
                      choices = setdiff(c("price", "mileage", "mpg", "engineSize"), input$yaxis),
                      selected = isolate(input$xaxis))
  })
  
  selected_manufacturer <- reactiveVal(NULL)
  selected_model <- reactiveVal(NULL)
  hover_transmission <- reactiveVal(NULL)
  hover_fuelType <- reactiveVal(NULL)
  
  observeEvent(input$backButton, {
    if (!is.null(selected_model())) {
      selected_model(NULL)
    } else if (!is.null(selected_manufacturer())) {
      selected_manufacturer(NULL)
    }
  })
  
  scatter_data <- reactiveVal(data.frame())
  
  filtered_data <- reactive({
    data %>%
      filter(year >= input$year[1], year <= input$year[2])
  })
  
  output$bubbleChart <- renderPlotly({
    x_var <- input$xaxis
    y_var <- input$yaxis
    
    if (is.null(selected_manufacturer())) {
      summary_data <- filtered_data() %>%
        group_by(Manufacturer) %>%
        summarize(
          avg_x = mean(.data[[x_var]], na.rm = TRUE),
          avg_y = mean(.data[[y_var]], na.rm = TRUE),
          count = n()
        )
      
      p <- plot_ly(
        summary_data, 
        x = ~avg_x, 
        y = ~avg_y, 
        size = ~count, 
        type = 'scatter', 
        mode = 'markers', 
        marker = list(opacity = 0.7, sizemode = 'diameter', sizeref = 2),
        text = ~Manufacturer,
        key = ~Manufacturer
      ) %>%
        layout(
          title = "Bubble Chart of Car Models",
          xaxis = list(title = paste("Average", x_var), gridcolor = "#ffffff33"),
          yaxis = list(title = paste("Average", y_var), gridcolor = "#ffffff33"),
          showlegend = FALSE,
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)",
          font = list(color = '#cccccc')
        ) %>% 
        event_register("plotly_click")
      
      current_data <- filtered_data() %>%
        filter(Manufacturer %in% summary_data$Manufacturer)
      current_cars <- sum(summary_data$count)
    } else if (is.null(selected_model())) {
      manufacturer_data <- filtered_data() %>%
        filter(Manufacturer == selected_manufacturer()) %>%
        group_by(Model) %>%
        summarize(
          avg_x = mean(.data[[x_var]], na.rm = TRUE),
          avg_y = mean(.data[[y_var]], na.rm = TRUE),
          count = n()
        )
      
      p <- plot_ly(
        manufacturer_data, 
        x = ~avg_x, 
        y = ~avg_y, 
        size = ~count, 
        type = 'scatter', 
        mode = 'markers', 
        marker = list(opacity = 0.7, sizemode = 'diameter', sizeref = 2),
        text = ~Model,
        key = ~Model
      ) %>%
        layout(
          title = paste("Bubble Chart of", selected_manufacturer(), "Models"),
          xaxis = list(title = paste("Average", x_var), gridcolor = "#ffffff33"),
          yaxis = list(title = paste("Average", y_var), gridcolor = "#ffffff33"),
          showlegend = FALSE,
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)",
          font = list(color = '#cccccc')
        ) %>% 
        event_register("plotly_click")
      
      current_data <- filtered_data() %>%
        filter(Manufacturer == selected_manufacturer())
      current_cars <- sum(manufacturer_data$count)
    } else {
      model_data <- filtered_data() %>%
        filter(Manufacturer == selected_manufacturer(), Model == selected_model())
      
      scatter_data(model_data)
      
      p <- plot_ly(
        model_data, 
        x = ~.data[[x_var]], 
        y = ~.data[[y_var]], 
        type = 'scatter', 
        mode = 'markers', 
        marker = list(opacity = 0.7, size = 5),
        text = ~paste(Manufacturer, Model, sep = " - "),
        key = ~rownames(model_data)
      ) %>%
        layout(
          title = paste("Scatter Plot of", selected_manufacturer(), selected_model(), "Cars"),
          xaxis = list(title = x_var, gridcolor = "#ffffff33"),
          yaxis = list(title = y_var, gridcolor = "#ffffff33"),
          showlegend = FALSE,
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)",
          font = list(color = '#cccccc')
        ) %>% 
        event_register("plotly_click")
      
      if (!is.null(hover_transmission())) {
        highlight_data <- model_data %>% filter(transmission == hover_transmission())
        
        p <- add_trace(p,
                       data = highlight_data,
                       x = ~.data[[input$xaxis]],
                       y = ~.data[[input$yaxis]],
                       type = 'scatter',
                       mode = 'markers',
                       marker = list(opacity = 1, size = 6, color = "red"),
                       text = ~paste(Manufacturer, Model, sep = " - "),
                       key = ~rownames(highlight_data))
      }
      if (!is.null(hover_fuelType())) {
        highlight_data <- model_data %>% filter(fuelType == hover_fuelType())
        
        p <- add_trace(p,
                       data = highlight_data,
                       x = ~.data[[input$xaxis]],
                       y = ~.data[[input$yaxis]],
                       type = 'scatter',
                       mode = 'markers',
                       marker = list(opacity = 1, size = 6, color = "red"),
                       text = ~paste(Manufacturer, Model, sep = " - "),
                       key = ~rownames(highlight_data))
      }
      
      current_data <- model_data
      current_cars <- nrow(model_data)
    }
    
    output$gauge <- renderPlot({
      make_gauge(current_cars, total_cars, title = "Cars shown")
    }, bg = "transparent")
    
    output$dataTable <- renderDT({
      datatable(current_data, options = list(pageLength = 10, autoWidth = TRUE, style = "background-color: #454352; color: #cccccc;"),
                extensions = c('Responsive'))
    })
    
    output$transmissionPie <- renderPlotly({
      transmission_data <- current_data %>%
        count(transmission) %>%
        filter(!is.na(transmission))
      
      current_transmission_labels <<- transmission_data$transmission
      
      plot_ly(
        source = "plot_transmission",
        transmission_data, 
        labels = ~transmission, 
        values = ~n, 
        type = 'pie',
        textinfo = 'label+percent',
        insidetextorientation = 'radial',
        hoverinfo = 'none',
        ids = ~transmission
      ) %>%
        layout(title = "Transmission",
               showlegend = F,
               plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               font = list(color = '#cccccc'),
               margin = list(l = 0, r = 0)) %>% 
        event_register("plotly_hover") %>% 
        event_register("plotly_unhover") %>%
        config(displayModeBar = F)
    })
    
    output$fuelTypePie <- renderPlotly({
      fuelType_data <- current_data %>%
        count(fuelType) %>%
        filter(!is.na(fuelType))
      
      current_fuelType_labels <<- fuelType_data$fuelType
      
      plot_ly(
        source = "plot_fuelType",
        fuelType_data, 
        labels = ~fuelType, 
        values = ~n, 
        type = 'pie',
        textinfo = 'label+percent',
        insidetextorientation = 'radial',
        hoverinfo = 'none',
        ids = ~fuelType
      ) %>%
        layout(title = "Fuel Type",
               showlegend = F,
               plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               font = list(color = '#cccccc'),
               margin = list(l = 0, r = 0)) %>% 
        event_register("plotly_hover") %>% 
        event_register("plotly_unhover") %>%
        config(displayModeBar = F)
    })
    
    ggplotly(p, tooltip = "key") %>% 
      event_register("plotly_click")
  })
  
  observeEvent(event_data("plotly_click"), {
    event <- event_data("plotly_click")
    if (!is.null(event)) {
      if (is.null(selected_manufacturer())) {
        selected_manufacturer(event$key)
      } else if (is.null(selected_model())) {
        selected_model(event$key)
      }
    }
  })
  
  observeEvent(event_data("plotly_hover", source = "plot_transmission"), {
    if (!is.null(selected_manufacturer()) && !is.null(selected_model())) {
      event <- event_data("plotly_hover", source = "plot_transmission")
      hover_transmission(current_transmission_labels[event$pointNumber + 1])
    }
  })
  
  observeEvent(event_data("plotly_unhover", source = "plot_transmission"), {
    hover_transmission(NULL)
  })
  
  observeEvent(event_data("plotly_hover", source = "plot_fuelType"), {
    if (!is.null(selected_manufacturer()) && !is.null(selected_model())) {
      event <- event_data("plotly_hover", source = "plot_fuelType")
      hover_fuelType(current_fuelType_labels[event$pointNumber + 1])
    }
  })
  
  observeEvent(event_data("plotly_unhover", source = "plot_fuelType"), {
    hover_fuelType(NULL)
  })
  
  output$treemap <- renderD3tree2({
    input$plot_width  # Re-render when width changes
    input$plot_height # Re-render when height changes
    inter
  })
  
  output$linePlot <- renderPlotly({
    if (length(input$manufacturers_line) == 0) {
      p <- ggplot() +
        ggtitle("No Manufacturers Selected") +
        theme_minimal() +
        theme(text = element_text(color = "#cccccc"),
              axis.text = element_text(color = "#cccccc"))
    } else {
      filtered_df <- df %>%
        filter(Manufacturer %in% input$manufacturers_line)
      
      yaxis_label <- choiceNames[input$yaxis_line]
      
      p <- ggplot(filtered_df, aes_string(x = "Year", y = input$yaxis_line, color = "Manufacturer")) +
        geom_line() +
        geom_point() +
        labs(y = yaxis_label, title = "Car Statistics Over Years")+
        scale_x_continuous(breaks = unique(df$Year[df$Year %% 2 ==0]), limits = year_range)+
        scale_y_continuous(labels = scales::comma) +
        scale_color_manual(values = colordict)+
        theme_minimal() +
        theme(text = element_text(color = "#cccccc"),
              axis.text = element_text(color = "#cccccc"),
              panel.grid.major = element_line("#ffffff33"))
    }
    
    ggplotly(p) %>% layout(
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
  })
  
})
