#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readxl)
library(stringr)

setwd("C:/Users/james/OneDrive/Documents/Important_Files/Life")
e_number_df = read_xlsx('emans_info.xlsx', sheet = 'e') %>% 
    data.frame(row.names = 1)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How many Numbers are known of e are memorized?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textInput("solution_number", label = h3("Enter the first number"), value = ""),
           verbatimTextOutput("evaluation")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$evaluation <- renderPrint({ 
        
        ## Get My answer
        emans_solution = str_extract_all(input$solution_number, boundary("character"))[[1]]
        
        ## The Solution
        reals_solution = str_extract_all('271828', boundary("character"))[[1]]
        
        numbers_tested = length(reals_solution)
        
        ## Check
        paste0(round(sum(emans_solution == reals_solution)/ numbers_tested,3) * 100, ' %')
        
        
        })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
