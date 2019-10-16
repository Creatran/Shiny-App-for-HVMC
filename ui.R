#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(wordcloud2)
library(shinyWidgets)
library(plotly)

## Importing Data Files
carcass_calculator_data <- read_csv("./carcass_calculator_data.csv")
wrs <- read.csv("Per_tonne_Protein_Consumed.csv")
wrs$Food.type <- as.character(wrs$Food.type)

cut_price <- read.csv("Cut_Price.csv")
cut_price$cut <- as.character(cut_price$cut)

sales_percent <- read_csv("sales_percent.csv")

my_CSS <- "
    # table{
        background: yellow;
        font-size: 24px;
    # plot{
    }
    }
"

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    # 
    theme = "bootstrap.min.css",
    
    #Main Layout: Background Image and Buttons for Social Media
    titlePanel(span(tags$img(src = "HVMC_logo_green.png",height = "200px",width="200px"),
                    actionButton("Instagram",
                               tags$img(src = "insta.png",height="30px",width="30px"),
                               onclick ="window.open('https://www.instagram.com/happyvalleymeat/', '_blank')"),
                    actionButton("Facebook",
                               tags$img(src = "face.png",height = "30px",width = "30px"),
                               onclick ="window.open('https://www.facebook.com/HappyValleyMeats', '_blank')"),
                    actionButton("Twitter",
                               tags$img(src = "twitt.png",height = "30px",width = "30px"),
                               onclick ="window.open('https://twitter.com/happyvalleymeat', '_blank')"),
                    actionButton("BCorporate",
                               tags$img(src = "bcorp.jpg",height = "30px",width = "30px"),
                               onclick ="window.open('https://bcorporation.net/directory/happy-valley-meat-company', '_blank')"))),
    setBackgroundImage(src = "farm.jpg"),
    


    # Sidebar with a slider input for number of bins
    column(12, offset = 2,
    mainPanel(
        style = "max-height: 80vh; overflow-y: auto;background-color:white" ,
    tabsetPanel(
        # Environmental Impact Plot
        tabPanel(h1("Environmental Impact",style = 'background-color:white;font-size:180%'),
                 titlePanel(h1("Environmental Impact Calculator",style = 'background-color:white')),
                 sidebarLayout(
                     sidebarPanel(
                         tags$img(src="HVMC_logo_green.png",height = "170px",width = "170px"),
                         h4("Happy Valley Meat Company"),
                         a(actionButton(inputId = "email1", label = "Contact Us", 
                                        icon = icon("envelope", lib = "font-awesome")),
                           href="mailto:meat@happyvalleymeat.com"),
                         
                         selectInput('food_type', 'Food Type', c('All', wrs$Food.type),
                                     multiple = TRUE, selected = "All"),
             helpText("Click on the above field to select different Food Type "),
                        
                         selectInput('plot_var', 'Environmental Component', c('All', "land", "water", "CO2"),
                                     multiple = TRUE, selected = "All")
                         ,helpText("Click on the above field to select different Environmental Component ")
                     ),
                     mainPanel(
                         plotlyOutput('Envi_plot1'),
                         plotlyOutput("Envi_plot2")
                     )
                 )
        ),
        # Carcass Calculator Tab
        tabPanel(h1("Carcass Calculator",style = 'background-color:white;font-size:180%'),
            titlePanel(h1("Carcass Calculator",style = 'background-color:white')),
            sidebarLayout(
                sidebarPanel(
                    tags$img(src="HVMC_logo_green.png",height = "110px",width = "110px"),
                    h4("Happy Valley Meat Company"),
                    a(actionButton(inputId = "email1", label = "Contact Us", 
                                   icon = icon("envelope", lib = "font-awesome")),
                      href="mailto:meat@happyvalleymeat.com"),
                    numericInput('weight', 'Pounds of Meat:', 100),
      
                    selectInput('cuts', 'Cuts of Meat', c('All', carcass_calculator_data$cut),
                               multiple = TRUE, selected = "All"),
                    helpText("Click on the above field to select different cuts of meat ")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                    tags$style(my_CSS),
                    tabsetPanel(
                        tabPanel(
                            title = 'Table',
                            DT::dataTableOutput("table")
                        ),
                        tabPanel(
                            title = "Charts",
                            tabsetPanel(
                                tabPanel(
                                    title = "Carbon Dioxide Emissions",
                                    plotlyOutput('plot_CO2')
                                ),
                                tabPanel(
                                    title = "Water Use",
                                    plotlyOutput("plot_water")
                                ),
                                tabPanel(
                                    title = "Land Use",
                                    plotlyOutput("plot_land")
                                ),
                                tabPanel(
                                    title = "Number of Cows",
                                    plotlyOutput("plot_ani")
                                )
                            )
                        )
                    )
                )
            )
            
        ),
        #Price and Sales Tab
        tabPanel(h1("Price and Sales",style = 'background-color:white;font-size:180%'),
                 tabsetPanel(
                 tabPanel(h1("Price Comparison",style = 'background-color:white;font-size:130%'),
                 sidebarLayout(
                     sidebarPanel(
                         tags$img(src="HVMC_logo_green.png",height = "170px",width = "170px"),
                         h4("Happy Valley Meat Company"),
                         a(actionButton(inputId = "email1", label = "Contact Us", 
                                        icon = icon("envelope", lib = "font-awesome")),
                           href="mailto:meat@happyvalleymeat.com"),
                         
                         
                         selectInput('cut_price', 'Cuts of Beef', c('All', cut_price$cut),
                                     multiple = TRUE, selected = "All")
                     ),
                     mainPanel(
                         plotlyOutput('price_plot')
                     )
                 )),
                 tabPanel(h1("Sales Percent",style = 'background-color:white;font-size:130%'),
                          titlePanel(h1("2018 U.S. Sales Percent by Dollar and by Pound",style = 'background-color:white')),
                          sidebarLayout(
                            sidebarPanel(
                              tags$img(src="HVMC_logo_green.png",height = "170px",width = "170px"),
                              h4("Happy Valley Meat Company"),
                              a(actionButton(inputId = "email1", label = "Contact Us", 
                                             icon = icon("envelope", lib = "font-awesome")),
                                href="mailto:meat@happyvalleymeat.com")
                            ),
                            mainPanel(
                              h6("The plot shows the sales ratios of different ground types in U.S. by two different units, pound and dollar"),
                              h6("The sales ratios of different ground types add up to 100%."),
                              plotOutput("sales")
                            )
                          ))
                 
        )),
        #Price and Environment Ratio Tab
        tabPanel(h1("Price Environment Ratio",style = 'background-color:white;font-size:180%'),
                 titlePanel(h1("Cut Cloud",style = 'background-color:white')),
                 sidebarLayout(
                     sidebarPanel(
                         tags$img(src="HVMC_logo_green.png",height = "170px",width = "170px"),
                         h4("Happy Valley Meat Company"),
                         a(actionButton(inputId = "email1", label = "Contact Us", 
                                        icon = icon("envelope", lib = "font-awesome")),
                           href="mailto:meat@happyvalleymeat.com"),
                         sliderInput("max", "Maximun Number of Cuts: ",
                                     min = 1, max = 35, value = 35),
                         sliderInput("k", "lb price: ",
                                     min = 1, max = 20, value = 1),
                         sliderInput("k1", "Water Use: ",
                                     min = 1, max = 20, value = 1),
                         sliderInput("k2", "CO2: ",
                                     min = 200, max = 400, value = 250),
                         sliderInput("k3", "Land Use: ",
                                     min = 200, max = 400, value = 250)
                     ),
                     mainPanel(
                       h6("Beef cut cloud will show the most environment-friendly and economic-friendly beef cut with the biggest font-size."),
                       h6("Please choose the number of beef cuts."),
                         plotOutput("cloud")
                         
                     )
                 )
        )
        
    ))
    )
))

