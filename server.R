#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://tianran.shinyapps.io/midterm_project/
#

# The github repository link is :https://github.com/Creatran/Shiny-App-for-HVMC.git

library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(wordcloud)
library(RColorBrewer)
library(plotly)
#Importing of Data Files
carcass_calculator_data <- read_csv("./carcass_calculator_data.csv")
wrs <- read.csv("Per_tonne_Protein_Consumed.csv")
wrs2 <- read.csv('Per_million_kilocalories_consumed.csv')
cut_price <- read.csv("Cut_Price.csv")
cut_ratio <- read.csv("cut_ratio.csv")
percent<-read.csv("sales_percent.csv")
details <- read.csv("details.csv")

wrs$Food.type <- as.character(wrs$Food.type)

water.rate <- 6.355078
co2.emision <- 102.959
land.use <- 77

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #Cleaning of Data for Environmental Impact by per ton protein consumed  
    wrs_summary <- reactive({
        data <- wrs %>%
            rename(land = Total.land.use..ha., water = Total.water..m3., 
                   CO2 = Production.emissions..CO2e.) %>%
            select(Food.type, land, water, CO2) %>%
            mutate(water = log(water))
        
        if ('All' %in% input$food_type == FALSE){
            data <- filter(data, Food.type %in% input$food_type)
        }       
        data <- gather(data, key = Envi.type, value = impact, -Food.type)
        
        if ("All" %in% input$plot_var == FALSE){
            data <- data %>%
                filter(Envi.type %in% input$plot_var)
        } 
        return(data)
        
        
    })
    # Environmental Impact Plot for per ton of protein consumed
    output$Envi_plot1 <- renderPlotly({
      ggplotly({
        ggplot(wrs_summary(), 
               aes(x = reorder(Food.type, impact), y = impact, group = Envi.type, fill = Envi.type)) + 
          geom_bar(stat = 'identity', position = 'dodge') + 
          theme(axis.text.x = element_text(size = 10, angle = 90),
                axis.text.y = element_text(size = 5),
                plot.title = element_text(size = 18),
                axis.title = element_text(size = 16)) + 
          labs(title= "Environmental Impact per Ton of\nProtein Consumed",
               x= "Food Type",
               y= "Impact on Environment")  +
          scale_fill_discrete(name = "Environment Factors", labels = c("CO2", "Land", "Water")) +
          scale_y_continuous(trans = 'log10')
      })
        
      
    })
    #Cleaning of Data for Environmental Impact by Kilocalories of Protein Consumed  
    wrs_summary2 <- reactive({
        data <- wrs2 %>%
            rename(land = Total.land.use..ha., water = Total.freshwater.consumption..m3., 
                   CO2 = Agricultural.production.emissions..CO2e.) %>%
            select(Food.type, land, water, CO2)  %>%
            mutate(water = log(water))
        
        if ('All' %in% input$food_type == FALSE){
            data <- filter(data, Food.type %in% input$food_type)
        }       
        data <- gather(data, key = Envi.type, value = impact, -Food.type)
        
        if ("All" %in% input$plot_var == FALSE){
            data <- data %>%
                filter(Envi.type %in% input$plot_var)
        } 
        return(data)
        
    })
    #Plot Environmental Impact by Kilocalories of Protein Consumed 
    output$Envi_plot2 <- renderPlotly({
      ggplotly({
        ggplot(wrs_summary2(), 
               aes(x = reorder(Food.type, impact), y = impact, group = Envi.type, fill = Envi.type)) + 
          geom_bar(stat = 'identity', position = 'dodge') + 
          theme(axis.text.x = element_text(size = 10, angle = 90),
                axis.text.y = element_text(size = 5),
                plot.title = element_text(size = 18),
                axis.title = element_text(size = 16)) + 
          labs(title= "Environmental Impact per Million\nKilocalories of Protein Consumed", x= "Food Type", y= "Impact on Environment")+
          scale_fill_discrete(name = "Environment Factors", labels = c("CO2", "Land", "Water")) 
      })
        
    })
    
    
    #Data for Carcass Calculator
    input_data <- reactive({
        if ('All' %in% input$cuts){
            data <- carcass_calculator_data
        } else {
            data <- carcass_calculator_data %>%
                filter(cut %in% input$cuts)
        }
        return(data)
    })
    
    #Plot for Carcuss Calculator
    plot_data <- reactive({
        input_data() %>%
            rename("Cut" = "cut") %>%
            mutate(`Number of Cows` = ceiling(input$weight/total_weight)) %>%
            mutate(`Water Use` = water.rate * `Number of Cows`) %>%
            mutate(`CO2 Emission` = co2.emision * `Number of Cows`) %>%
            mutate(`land Use` = land.use * `Number of Cows`)
          #  mutate(number.cows = ceiling(input$weight/total_weight)) %>%
          #  mutate(water.rate = water.rate * number.cows) %>%
          #  mutate(co2.emision = co2.emision * number.cows) %>%
          #  mutate(land.use = land.use * number.cows)
          
    #Usage plots
    })
    output$table <- DT::renderDataTable({
        plot_data() %>%
            select(-total_weight)
    })
    # plot for Carbon Dioxide Emissions
    output$plot_CO2 <- renderPlotly({
      ggplotly({
        ggplot(plot_data(), aes(x = reorder(Cut, -`CO2 Emission`), y = `CO2 Emission`)) + 
          geom_bar(stat = 'identity', fill = 'maroon') + 
          coord_flip() + 
          theme(axis.text.x = element_text(hjust = 1, size = 14),
                axis.text.y = element_text( size = 7),
                axis.title = element_text(size = 18),
                plot.title = element_text(size = 20)) +
          labs(title= "Carbon Dioxide Emissions", x= "Cut of Beef", y= "CO2 Emission (in thousands of lbs")
        
      })
        
        
    })
    #Plot for Water  Usage 
    output$plot_water <- renderPlotly({
      ggplotly({
        ggplot(plot_data(), aes(x = reorder(Cut, -`Water Use`), y = `Water Use`)) + 
          geom_bar(stat = 'identity', fill = 'navy') + 
          coord_flip() +
          ggtitle(paste("Water Usage per", input$weight, "\nPounds of Beef")) + 
          ylab("Water Usage (in millions of gallons") + 
          xlab("Cut of Beef") + 
          theme(axis.text.x = element_text(hjust = 1, size = 14),
                axis.text.y = element_text( size = 7),
                axis.title = element_text(size = 18),
                plot.title = element_text(size = 18))
      })
        
    })
    
    # Plot for Land Usage
    output$plot_land <- renderPlotly({
      ggplotly({
        ggplot(plot_data(), aes(x = reorder(Cut, -`land Use`), y = `land Use`)) + 
          geom_bar(stat = 'identity', fill = 'brown') + 
          coord_flip() +
          ggtitle(paste("Land Usage per", input$weight, "Pounds\nof Beef")) + 
          ylab("Land (in acres)") + 
          xlab("Cut of Beef") + 
          theme(axis.text.x = element_text(hjust = 1, size = 14),
                axis.text.y = element_text( size = 7),
                axis.title = element_text(size = 18),
                plot.title = element_text(size = 20))
      })
        
    })
    
    # Plot for Number of Cows Required
    output$plot_ani <- renderPlotly({
      ggplotly({
        ggplot(plot_data(), aes(x = reorder(Cut, -`Number of Cows`), y = `Number of Cows`)) + 
          geom_bar(stat = 'identity', fill = 'orange') + 
          coord_flip() +
          ggtitle(paste("Number of Cows Required for\n", input$weight, "Pounds of Beef")) + 
          ylab("Number of Cows") + 
          xlab("Cut of Beef") + 
          theme(axis.text.x = element_text(hjust = 1, size = 14),
                axis.text.y = element_text( size = 7),
                axis.title = element_text(size = 18),
                plot.title = element_text(size = 20))
      })
        
    })
    #Data Cleaning for Price Plot 
    price_summary <- reactive({
        if ('All' %in% input$cut_price == FALSE){
            data <- filter(cut_price, cut %in% input$cut_price)
            return(data)
        }       
        return(cut_price)
        
    })
    #Plot Price per Pound by cut of beef
    output$price_plot <- renderPlotly({
      ggplotly({
        ggplot(price_summary(), 
               aes(x = reorder(cut, lbprize), y = lbprize)) + 
          geom_bar(stat = 'identity', fill = "blue") + 
          theme(axis.text.x = element_text(size = 10, angle = 90),
                axis.text.y = element_text(size = 5),
                plot.title = element_text(size = 20),
                axis.title = element_text(size = 16)) + 
          labs(title= "Price per Pound for Various\nCuts of Beef", x= "Cut of Beef", y= "Price per lb")
      })
       
    })
    #Data for Wordcloud
    output$cloud <- renderPlot({
    cut_ratio <- details %>%
        mutate(ratio = 1/(log(lbprize) * input$k + (input$k1 * log(water.rate)) + (input$k2 * log(co2.emision)) + (input$k3 * log(land.use))))%>%
        select(cut, ratio) %>%
        arrange(-ratio)
      #Wordcloud for Amounts of Beef
      wordcloud(cut_ratio$cut, cut_ratio$ratio, max.words = input$max,
                colors = brewer.pal(8, "Dark2"), scale=c(3,.05))
               # rot.per=.15, random.order=FALSE)
      png("wordcloud_packages.png", width=20, height=5, units='in', res=300)
    })
    #Plot for Portion of Sales
    output$sales <- renderPlot({
      percent%>%
        ggplot(aes(x = By,y=Percent, fill=Ground_Type))+
        geom_bar(stat = "identity",color = "black",size = 0.5,alpha = 0.6)+
        geom_text(aes(label =Percent),position = position_stack(vjust = 0.5),size = 4)+
        labs(x = "Share of",y = "Percent of Total(%)",title = "2018 Share of Sales by Ground Type")
    
    
  })
    
})
