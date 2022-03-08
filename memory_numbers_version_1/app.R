#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(readxl)

library(tidyverse)

setwd("C:/Users/james/OneDrive/Documents/Important_Files/Life")
e_number_df = read_xlsx('emans_info.xlsx', sheet = 'e') #%>% 
# data.frame(row.names = 1)



##--------------------------------------
## Name: select_set_fun
## Purpose: Get set
## Input: set number
## Output: set in number concatenated
##--------------------------------------

select_set_fun <- function(specified_set) {
    ## The Solution
    value_to_test = e_number_df %>% 
        filter(set == specified_set) %>% 
        select(-set) %>% 
        unite('Merged', `col-1`:`col-4`,remove =FALSE,sep = '') %>% 
        mutate(Merged = str_remove_all(Merged,'\'')) %>% 
        pull(Merged)
    return(value_to_test)
}
select_set_fun('set-1')


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("How many Numbers are known of e are memorized?"),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
        selectInput('which_set','Select Set to Test',choices = e_number_df$set),
        
        
        textInput("solution_number", label = h2("Set Numbers"), value = ""),
        
        
        verbatimTextOutput("evaluation")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$evaluation <- renderPrint({ 
        
        ## Get My answer
        emans_solution = str_extract_all(input$solution_number, boundary("character"))[[1]]
        
        
        
        ## The Solution
        reals_solution = str_extract_all(select_set_fun(input$which_set), boundary("character"))[[1]] %>% 
            str_remove('\\.') %>% 
            str_subset( ".+")
        
        numbers_tested = length(reals_solution)
        
        ## Check
        paste0(round(sum(emans_solution == reals_solution)/ numbers_tested,3) * 100, ' %')
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
