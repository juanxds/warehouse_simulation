# Author: Juan Luis Herrera
# Purpose:  App to simulate the management of a warehouse 


# Import packages ####
library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(naniar)
library(UpSetR)
library(combinat)
library(extraDistr)
library(stringr)
library(shiny)
library(shinydashboard)
#library(flexdashboard)
library(DT)



# Functions ####

getmode <- function(v) {
    uniqv <- unique(v)
    x <- uniqv[which.max(tabulate(match(v, uniqv)))]
    return(as.numeric(x))
}

warehouse_positions <- function(rack_size = 5, capacity = 500){
    # Function to create the all possible positions in a warehouse
    
    total_positions <- (capacity / rack_size)
    y <- ifelse((total_positions/10) <= 0,
                1,
                ceiling(total_positions/10) )
    
    
    position_x <- seq(from = 1, to =  10)
    position_y <- seq(from = 1, to = y)
    
    positions <-data.frame(pos_x = c(0), pos_y = c(0))
    
    pos <- 0
    
    for (i in 1:length(position_x)){
        for (j in 1:length(position_y)){
            pos <- pos + 1
            positions[pos,1] <- position_x[i]
            positions[pos,2] <- position_y[j]
            
        }
    }
    return(positions)
}

stock_distribution <- function(positions, rack_size = 5, stock = 100) {
    # Function to distribute stock in a warehouse
    
    capacity <- nrow(positions) * rack_size 
    
    if (stock <= capacity){
        used_positions <- floor(stock / rack_size)
        remaining_stock <- stock - floor(used_positions*rack_size)
        filled_racks <- rep(rack_size, used_positions)
        filled_racks <- append(filled_racks,remaining_stock) 
        unfilled_positions <- nrow(positions) - length(filled_racks)
        racks <- append(filled_racks, rep(0,unfilled_positions))
        positions$units <-racks 
        return(positions)
    } else if (stock > capacity){
        surplus_stock <- stock - capacity
        filled_racks <- rep(rack_size, nrow(positions))
        positions$units <- filled_racks
        overflow <- nrow(positions) + 1
        positions[overflow,1] <- "overflow"
        positions[overflow,2] <- "overflow"
        positions[overflow,3] <- surplus_stock
        return(positions)
        
    }
    
}

warehouse_floor <- function(df, rack_size = 5){
    # Function to create a visualization of a warehouse
    
    df <- df %>% 
        mutate(status = case_when(units > rack_size ~ "Overflow",
                                  units == rack_size  ~ "Fully Used",
                                  units < rack_size & units != 0 ~ "Partially Used",
                                  units == 0 ~ "Empty"))
    
    df$status <- factor(x = df$status,
                        levels = c("Overflow",
                                   "Fully Used",
                                   "Partially Used",
                                   "Empty"))
    
    limit_x <- factor(unique(df[,1], incomparables = FALSE))
    
    limit_y <- factor(unique(df[,2], incomparables = FALSE))
    
    plot <- ggplot(data = df, aes_string(x = "pos_x",
                                         y = "pos_y",
                                         color = "status")) +
        geom_point(shape =15, size = 10) +
        geom_label(aes(label = units), size = 3, color = "black") +
        scale_x_discrete(limits = limit_x) +
        scale_y_discrete(limits = limit_y) +
        theme_minimal() +
        labs(title = "Warehouse Simulation",
             subtitle = "",
             x = "Columns",
             y = "Rows")
    
    return(plot)
    
}

stock_histogram <- function(df, var, x_title = NULL,  y_title = NULL){
    
    # Function to create a histogram
    
    xtitle <- ifelse(is.null(x_title), "Bins", x_title)
    
    ytitle <- ifelse(is.null(y_title), "Frequency", y_title)
    
    lower_limit_y <- 1
    
    y <- getmode(df[[var]])
    y1 <- df[[var]] == y
    y2 <- length(y1[y1==TRUE])
    
    
    upper_limit_y <- y2
    
    name1 <- colnames(df[,..var])
    
    title <- paste(name1,"Frequency",sep = " ")
    
    histo <-ggplot(data = df, 
                   aes_string(x = name1)) + 
        geom_histogram(fill = "blue", binwidth = 1) +
        theme_minimal() +
        scale_y_discrete(limits = factor(lower_limit_y:upper_limit_y)) +
        labs(title = title,
             subtitle = " ",
             x = xtitle,
             y = ytitle)
    
    return(histo)
    
}

stock_trend <- function(df, var_x, var_y, x_title = NULL, y_title = NULL){
    # function to create a trend line plot
    
    xtitle <- ifelse(is.null(x_title), "Var X", x_title)
    
    ytitle <- ifelse(is.null(y_title), "Var Y", y_title)
    
    name_y <- colnames(df[,..var_y])
    name_x <- colnames(df[,..var_x])
    
    title <- paste(name_y, "Trend", sep = " ")
    
    trend <-ggplot(data = df, 
                   aes_string(x = name_x, y = name_y)) + 
        geom_point(color = "red") +
        geom_line(color = "red") +
        geom_smooth(method = "loess") +
        theme_minimal() +
        labs(title = title,
             subtitle = " ",
             x = xtitle,
             y = ytitle)
    
    return(trend)
    
}

stock_bar <- function(df, var_x, var_y, x_title = NULL, y_title = NULL, color = "blue"){
    # function to create a trend line plot
    
    xtitle <- ifelse(is.null(x_title), "Var X", x_title)
    
    ytitle <- ifelse(is.null(y_title), "Var Y", y_title)
    
    name_y <- colnames(df[,..var_y])
    name_x <- colnames(df[,..var_x])
    
    title <- paste(name_y, "position", sep = " ")
    
    bar_trend <-ggplot(data = df, 
                       aes_string(x = name_x, y = name_y)) + 
        geom_col(fill = color) +
        theme_minimal() +
        labs(title = title,
             subtitle = " ",
             x = xtitle,
             y = ytitle)
    
    return(bar_trend)
    
}



# UI ####
ui <- fluidPage(
    # Header ####
    h1(p(strong("Warehouse and stock levels simulation"))),
    
    
    # Application title ####
    titlePanel("Simulation Variables"),
    
    
    
    # Sidebar ####
    sidebarLayout(
        sidebarPanel(
            width = 2,
            #Slider input - Simulation Days ####
            sliderInput(
                inputId = "simulation_days",
                label = "How many days do you want to simulate?",
                min = 5,
                max = 90,
                value = 10,
                step = 1,
                round = TRUE,
                ticks = FALSE,
                animate = FALSE,
                sep = ","
            ),
            #Slider input - AVG Daily Demand  ####
            sliderInput(
                inputId = "avg_daily_demand",
                label = "What is you average daily demand (units)?",
                min = 1,
                max = 100,
                value = 10,
                step = 1,
                round = TRUE,
                ticks = FALSE,
                animate = FALSE,
                sep = ","
            ),
            
            #Slider input - Starting stock ####
            sliderInput(
                inputId = "starting_stock",
                label = "How many units do you have in stock at the start of this simulation?",
                min = 1,
                max = 100,
                value = 10,
                step = 1,
                round = TRUE,
                ticks = FALSE,
                animate = FALSE,
                sep = ","
            ),
            
            #Slider input - Safety stock ####
            sliderInput(
                inputId = "safety_stock",
                label = "What is your safety stock level (units)?",
                min = 0,
                max = 100,
                value = 0,
                step = 1,
                round = TRUE,
                ticks = FALSE,
                animate = FALSE,
                sep = ","
            ),
            
            #Slider input - Order Size ####
            sliderInput(
                inputId = "order_size",
                label = "What is your purchase order size (units)?",
                min = 1,
                max = 100,
                value = 10,
                step = 1,
                round = TRUE,
                ticks = FALSE,
                animate = FALSE,
                sep = ","
            ),
            
            #Slider input - Lead Time ####
            sliderInput(
                inputId = "lead_time",
                label = "How long do you have to wait for a purchase to arrive (days)?",
                min = 1,
                max = 90,
                value = 3,
                step = 1,
                round = TRUE,
                ticks = FALSE,
                animate = FALSE,
                sep = ","),
            
            #Slider input - Warehouse Capacity  ####
            sliderInput(
                inputId = "capacity",
                label = "What is the capacity of your warehouse (units)?",
                min = 1,
                max = 1000,
                value = 500,
                step = 1,
                round = TRUE,
                ticks = FALSE,
                animate = FALSE,
                sep = ","),
            
            #Slider input - rack size  ####
            sliderInput(
                inputId = "rack_size",
                label = "How many units do you store per rack?",
                min = 1,
                max = 50,
                value = 10,
                step = 1,
                round = TRUE,
                ticks = FALSE,
                animate = FALSE,
                sep = ",")
            
            
        ), #End of sidebar
        
        
        
        
        
        
        
        
        
        
        
        
        # Main Panel ####
        mainPanel(
            width = 10,
            #Tabs ####
            tabsetPanel(
                # Demand & Sales Dashboard ####
                tabPanel("Demand & Sales Dashboard",
                         fluidRow(width = 12,
                                  valueBoxOutput(outputId = "total_demand"),
                                  valueBoxOutput(outputId = "total_sales"),
                                  valueBoxOutput(outputId = "total_missed_sales"),
                                  valueBoxOutput(outputId = "ending_stock"),
                                  valueBoxOutput(outputId = "demand_captured"),
                                  valueBoxOutput(outputId = "demand_missed")
                                  
                         ),
                         
                         fluidRow(width = 12,
                                  column(6,
                                         plotlyOutput("Plot1")), # Demand histogram
                                  column(6,
                                         plotlyOutput("Plot2"))),  # Demand trend
                         fluidRow(width = 12, 
                                  column(6,
                                         plotlyOutput("Plot3")), # Sales position
                                  column(6,
                                         plotlyOutput("Plot4"))), # Missing sales position 
                         fluidRow(width = 12, 
                                  column(6,
                                         plotlyOutput("Plot5")), # ending stock position
                                  column(6,
                                         plotlyOutput("Plot6"))), # Purchases position
                         
                ), # End of dashboard
                
                # Warehouse dashboard ####
                tabPanel("Warehouse Dashboard",
                         
                         #Slider input - Simulation Days ####
                         fluidRow(width = 12,
                                  uiOutput("ware_day")
                         ),
                         
                         
                         # Key Numbers ####
                         fluidRow(width = 12,
                                  valueBoxOutput(outputId = "Day"),
                                  valueBoxOutput(outputId = "warehouse_stock"),
                                  valueBoxOutput(outputId = "warehouse_utilization"),
                                  valueBoxOutput(outputId = "warehouse_overflow"),
                                  valueBoxOutput(outputId = "overflow_perc"),
                                  valueBoxOutput(outputId = "warning"),
                                  #renderPlot("warehouse_dist")
                                  
                         ),
                         
                         fluidRow(width = 12,
                                  column(12,
                                         plotOutput("warehouse_dist")))
                                  
                         
                         
                         # Warehouse plot
                         #plotOutput("warehouse_position"),
                         #tableOutput("warehouse_data")
                         
                ), # End of warehouse dashboard
                
                # Data ####
                tabPanel("Stock and Demand Data",
                         tableOutput("data_simulation")), # End of tab data
                
                # Documentation ####
                tabPanel("Documentation",
                         
                         tags$p(HTML("Do you want to check the code of this 
                                     simulation. Follow this link: <a href=\"https://github.com/juanxds/warehouse_simulation\">GitHub</a> ")),
                         
                         tags$p(HTML("Questions or comments, please email me at
                                     <b>juanluhe@outlook.com</b>")),
                         tags$p(HTML("Connect with me at
                                     <a href=\"https://www.linkedin.com/in/juanlherreram\">https://www.linkedin.com/in/juanlherreram/</a>.")),
                         
                         tags$h4(HTML("<b>Simulation variables:</b>")),
                         
                         tags$p(HTML("<b>Simulation days – How many days you want
                         to simulate? -</b>
                         In this simulation you can select the numbers of days
                         you want to run this simulation. The default is 10 days,
                         and you can simulate up to 90 days.")),
                         
                         tags$p(HTML("<b>Daily demand – What is your average daily 
                                     demand (units)? –</b> The daily demand conveys
                                     the numbers of units consumers or customers
                                     are willing to buy. Based on this number,
                                     the simulation produces an estimation of 
                                     the daily demand using a standard deviation
                                     of 1 unit. The simulation assumes the demand
                                     follows a discrete normal distribution.")),
                         
                         tags$p(HTML("<b>Starting stock - How many units do you 
                                     have in stock at the start of this simulation? –</b>
                                     The starting stock refers to the units
                                     stored ate warehouse at day zero of the
                                     simulation. The default is 10 units.")),
                         
                         tags$p(HTML("<b>Safety stock level - What is your safety
                                     stock level (units)? –</b> Usually, 
                                     companies have a minimum amount of stock
                                     to be at the warehouse. In other words, a
                                     cushion, or a buffer to serve any peaks of
                                     demand. When the ending stock is under the
                                     safety stock level, it triggers a purchase 
                                     order to replenish stock. The default 
                                     safety stock level is zero units. ")),
                         
                         tags$p(HTML("<b>Purchase order size - What is your 
                                     purchase order size (units)? –</b> This is 
                                     the size, in units, of the purchase order to
                                     be sent to the supplier when stock is
                                     needed to capture customer demand. The
                                     default purchase order size is 10 units.")),
                         
                         tags$p(HTML("<b>Lead time - How long do you have to
                                     wait for a purchase to arrive (days)? -</b>
                                     The lead time refers the time it takes a 
                                     purchase order to reach the warehouse. The 
                                     default lead time is 3 days. ")),
                         
                         tags$p(HTML("<b>Warehouse capacity – What is the 
                                     capacity of your warehouse (units)? –</b>
                                     This variable entails how many can take the
                                     warehouse without going overflow. If the
                                     warehouse is reached, an overflow position
                                     will appear inside the warehouse dashboard.
                                     The default warehouse capacity is 500 units.")),
                         
                         tags$p(HTML("<b>Rack size – How many units do store per
                                     rack? –</b> the rack size conveys how many 
                                     units can be stored in shelf or stand. 
                                     The default rack size is 10 units.")),
                         
                         tags$h4(HTML("<b>Key calculations:</b>")),
                         
                         tags$p(HTML("<b>Simulated demand:</b> The simulated 
                                     demand is produced based on a discrete 
                                     normal distribution. This demand uses the 
                                     average provided by the user and a standard
                                     deviation of one unit.")),
                         
                         tags$p(HTML("<b>Available stock:</b> The available 
                                     stock is the sum the units stored in the
                                     warehouse at the beginning of the day and
                                     the purchases arriving in that day.")),
                         
                         tags$p(HTML("<b>Sales:</b> the units sold is the lowest value
                                     between the simulated demand and the
                                     available stock in a specific day.")),
                         
                         tags$p(HTML("<b>Demand missed:</b> the demand missed is the
                                     difference between the simulated demand and
                                     the available stock provided that the
                                     demand was higher than the available stock.")),
                         
                         tags$p(HTML("<b>Ending stock:</b> The ending stock is the
                                     available stock in the warehouse minus
                                     the units sold in that day.")),
                         
                         tags$p(HTML("<b>Reorder level:</b> The reorder level is
                                     the product of multiplying the daily demand
                                     by the lead time and adding the safety stock
                                     level provided by the user 
                                     (avg. demand * lead time + safety stock level).")),
                         
                        
                         )
                
                
            ) # End of tabs
            
        )
    ), #End of sidebarlayout
    
)




# SERVER ####
server <- function(input, output) {
    
    # Dynamically generate table when sliders change ####
    
    # Sales and demand data ####
    dataInput <- reactive({
        
        # Processing ####
        
        demand <- rdnorm(input$simulation_days,input$avg_daily_demand,1)
        
        starting_stock <- append(input$starting_stock, rep(0,length(demand)-1))
        
        Reorder_level <- input$avg_daily_demand * input$lead_time + input$safety_stock 
        
        missing_sales <- rep(0,length(demand))
        
        final_stock <-c(0) 
        
        purchase <- rep(0,length(demand))
        
        
        # Stock Calculation ####
        
        for (i in 1:input$simulation_days){
            
            stock <- (starting_stock[i] + purchase[i] - demand[i])
            
            
            if (stock > 0){
                
                final_stock[i] <- starting_stock[i] + purchase[i] - demand[i]
                missing_sales[i] <- 0
                
            } 
            else if (stock == 0){
                
                final_stock[i] <- 0 
                missing_sales[i] <- 0
                
            } 
            else if (stock < 0){
                
                final_stock[i] <- 0 
                missing_sales[i] <- abs(stock)
                
            }
            
            if (final_stock[i] <= Reorder_level){
                purchase[i+input$lead_time] <- input$order_size
            }
            else { 
                purchase[i+input$lead_time] <- 0
            }
            
            starting_stock[i+1] <- final_stock[i] 
            
        }
        
        # stock reporting ####
        starting_stock <- starting_stock[1:input$simulation_days]
        
        purchase <- purchase[1:input$simulation_days]
        
        missing_sales <- missing_sales[1:input$simulation_days]
        
        warehouse <- data.table(day = c(1:input$simulation_days),
                                starting_stock,
                                purchase,
                                demand,
                                final_stock, missing_sales)
        
        dat1 <- warehouse %>% 
            mutate(available_stock = starting_stock + purchase,
                   sales = demand - missing_sales,
                   ending_stock = available_stock-sales) %>%
            select(day,
                   starting_stock,
                   purchase,
                   available_stock,
                   demand,
                   sales,
                   ending_stock,
                   missing_sales)
        
        colnames(dat1) <- c("Day", "Starting_Stock", "Purchases",
                            "Available_Stock", "Demand", "Sales", 
                            "Ending_Stock", "Missed_Sales")
        
        # Return table ####
        return(dat1)
        
    })
    
    # Stock data ####
    datainput2 <- reactive({
        
        df <- dataInput()
        
        stock_1 <- df$Ending_Stock[input$warehouse_day] 
        
        positions <- warehouse_positions(rack_size = input$rack_size,
                                         capacity = input$capacity)
        
        dist_1 <- stock_distribution(positions,
                                     stock = stock_1,
                                     rack_size = input$rack_size)
        
        return(dist_1)
        
    })
    
    # Data simulation table ####
    output$data_simulation <- renderTable({ 
        dataInput()
    })
    
    # Demand and sales Dashboard ####
    
    output$Plot1 <- renderPlotly({
        
        df <- dataInput()
        
        # plot 1 - Histogram of demand
        ggplotly(stock_histogram(df = df,
                                 var = "Demand",
                                 x_title = "daily demand", 
                                 y_title = "count of days"))
        
    }) # End of plot 1
    
    output$Plot2 <- renderPlotly({
        
        df <- dataInput()
        
        # plot 2 - variable trend
        ggplotly(stock_trend(df = df,
                             var_x = "Day",
                             var_y = "Demand",
                             x_title = "Day", 
                             y_title = "Units"))
        
    }) # End of plot 2
    
    output$Plot3 <- renderPlotly({
        
        df <- dataInput()
        
        # plot 3 - variable position
        ggplotly(stock_bar(df = df,
                           var_x = "Day",
                           var_y = "Sales",
                           x_title = "Day", 
                           y_title = "Units"))
        
    }) # End of plot 3
    
    output$Plot4 <- renderPlotly({
        
        df <- dataInput()
        
        # plot 4 - variable position
        ggplotly(stock_bar(df = df,
                           var_x = "Day",
                           var_y = "Missed_Sales",
                           x_title = "Day", 
                           y_title = "Units",
                           color = "red"))
        
    }) # End of plot 4
    
    output$Plot5 <- renderPlotly({
        
        df <- dataInput()
        
        # plot 5 - variable position
        ggplotly(stock_bar(df = df,
                           var_x = "Day",
                           var_y = "Ending_Stock",
                           x_title = "Day", 
                           y_title = "Units",
                           color = "blue"))
    }) # End of plot 5
    
    output$Plot6 <- renderPlotly({
        
        df <- dataInput()
        
        # plot 6 - variable position
        ggplotly(stock_bar(df = df,
                           var_x = "Day",
                           var_y = "Purchases",
                           x_title = "Day", 
                           y_title = "Units",
                           color = "red"))
    }) # End of plot 6
    
    output$total_demand <- renderValueBox({
        df <- dataInput()
        
        t_demand <- paste(sum(df$Demand, na.rm = TRUE),
                          "units",
                          sep = " ")
        
        valueBox(value =  t_demand,
                 subtitle = "Total Demand",
                 #icon = icon("exclamation-triangle"),
                 #color = "red",
                 width = 2)
        
    }) # End of value box  - total demand
    
    output$total_sales <- renderValueBox({
        df <- dataInput()
        
        t_sales <- paste(sum(df$Sales, na.rm = TRUE),
                         "units",
                         sep = " ")
        
        valueBox(value =  t_sales,
                 subtitle = "Total Sales",
                 #icon = icon("exclamation-triangle"),
                 #color = "green",
                 width = 2)
        
    }) # End of value box  - total sales
    
    output$total_missed_sales <- renderValueBox({
        df <- dataInput()
        
        t_missed_sales <- paste(sum(df$Missed_Sales, na.rm = TRUE),
                                "units",
                                sep = " ")
        
        valueBox(value =  t_missed_sales,
                 subtitle = "Total Missed Sales",
                 #icon = icon("exclamation-triangle"),
                 #color = "red",
                 width = 2)
        
    }) # End of value box  - total missed sales
    
    output$ending_stock <- renderValueBox({
        df <- dataInput()
        
        t_ending_stock <- paste(tail(df$Ending_Stock, n = 1),
                                "units",
                                sep = " ")
        
        valueBox(value =  t_ending_stock,
                 subtitle = "Ending Stock",
                 #icon = icon("exclamation-triangle"),
                 #color = "red",
                 width = 2)
        
    }) # End of value box  - ending stock
    
    output$demand_captured <- renderValueBox({
        df <- dataInput()
        
        t_captured <- sum(df$Sales, na.rm = TRUE)/sum(df$Demand, na.rm = TRUE) *100
        
        t_captured <- paste(round(t_captured,1), "%", sep = "")
        
        valueBox(value =  t_captured,
                 subtitle = "Demand Captured",
                 #icon = icon("exclamation-triangle"),
                 #color = "green",
                 width = 2)
        
    }) # End of value box  - demand captured
    
    output$demand_missed <- renderValueBox({
        df <- dataInput()
        
        t_missed <- sum(df$Missed_Sales, na.rm = TRUE)/sum(df$Demand, na.rm = TRUE) *100
        
        t_missed <- paste(round(t_missed,1), "%", sep = "")
        
        valueBox(value =  t_missed,
                 subtitle = "Demand missed",
                 #icon = icon("exclamation-triangle"),
                 #color = "green",
                 width = 2)
        
    }) # End of value box  - demand missed
    
    # Warehouse Dashboard ####
    output$ware_day <- renderUI({
        
        df <- dataInput()
        
        last_day <- nrow(df)
        
        sliderInput(
            inputId = "warehouse_day",
            label = "Select the day when you want to see the warehouse position - press the play button simulate stock position over time",
            min = 1,
            max = last_day,
            value = 1,
            step = 1,
            round = TRUE,
            ticks = FALSE,
            animate = TRUE,
            sep = ","
        )
        
        
        
    })
    
    output$Day <- renderValueBox({
        
        stock_day <- paste("Day", input$warehouse_day, sep = " ")
        
        valueBox(value =  stock_day,
                 subtitle = "Simulation Day",
                 #icon = icon("exclamation-triangle"),
                 #color = "green",
                 width = 2)
        
    }) # End of value box  - stock day
    
    output$warehouse_stock <- renderValueBox({
        
        df <- dataInput()
        
        
        stock <- paste(df$Ending_Stock[input$warehouse_day],
                       "units",
                       sep = " ")
        
        valueBox(value =  stock,
                 subtitle = "Total Stock",
                 #icon = icon("exclamation-triangle"),
                 #color = "green",
                 width = 2)
        
    }) # End of value box  - Warehouse Stock
    
    output$warehouse_utilization <- renderValueBox({
        
        df <- dataInput()
        
        utilization <- round(df$Ending_Stock[input$warehouse_day] / input$capacity*100,
                             1)
        
        util_perc <- paste(utilization,
                           "%",
                           sep = "")
        
        valueBox(value =  util_perc,
                 subtitle = "Warehouse utilization",
                 #icon = icon("exclamation-triangle"),
                 #color = "green",
                 width = 2)
        
    }) # End of value box  - Warehouse utilization
    
    output$data_warehouse <- renderTable({
        datainput2()
    })
    
    output$warehouse_dist <- renderPlot({
        df <- datainput2()
        
        rack_size <- input$rack_size
        
        df <- df %>% 
            mutate(status = case_when(units > rack_size ~ "Overflow",
                                      units == rack_size  ~ "Fully Used",
                                      units < rack_size & units != 0 ~ "Partially Used",
                                      units == 0 ~ "Empty"))
        
        df$status <- factor(x = df$status,
                            levels = c("Overflow",
                                       "Fully Used",
                                       "Partially Used",
                                       "Empty"))
        
        limit_x <- factor(unique(df[,1], incomparables = FALSE))
        
        limit_y <- factor(unique(df[,2], incomparables = FALSE))
        
        ggplot(data = df, aes_string(x = "pos_x",
                                             y = "pos_y",
                                             color = "status")) +
            geom_point(shape = 15, size = 15) +
            geom_label(aes(label = units), size = 3, color = "black") +
            scale_x_discrete(limits = limit_x) +
            scale_y_discrete(limits = limit_y) +
            theme_minimal() +
            labs(title = "Warehouse Position",
                 subtitle = "",
                 x = "Columns",
                 y = "Rows")
    })
    
 
    
}

# Run the application 
shinyApp(ui = ui, server = server)


