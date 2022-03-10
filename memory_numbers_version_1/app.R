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

odd_e_number_df = e_number_df %>% 
    mutate(number = as.numeric(str_remove(set,'set-'))) %>% 
    filter(number %% 2 != 0 )
even_e_number_df = e_number_df  %>% 
    anti_join(odd_e_number_df) %>%  
    select(-set)


df = bind_cols(odd_e_number_df,even_e_number_df) %>%  
    select(-contains('set'),-number) %>% 
    unite('Merged', `col-1...2`:`col-4...10`,remove =TRUE, sep = '') %>% 
    mutate(Merged = str_remove_all(Merged,'\'')) %>% 
    mutate(labeled = c(rep('A',5),rep('B',5),rep('C',5) ) )

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
        ## Two Types 1 at a time & A set of 5 40 number sequences 
        radioButtons(inputId = 'question_style',label = 'Which style do you want to test',
                     choices = c('Regular', 'Sequence of 5'),'Regular'),
        
        ## Condition for regular
        conditionalPanel(
            condition = 'input.question_style == "Regular" ',
            selectInput('which_set','Select Set to Test',choices = e_number_df$set),
            
            
            textInput("solution_number", label = h2("Set Numbers"), value = "",width = "400px"),
            
            
            verbatimTextOutput("evaluation")
        ),
        
        
        

        
        
        conditionalPanel(
            condition = 'input.question_style == "Sequence of 5"',
            ## Two Types 1 at a time & A set of 5 40 number sequences 
            radioButtons(inputId = 'set_combination',label = 'Which combination are you interested in?',
                         choices = c('A', 'B', 'C'),'A'),
            
            
            
            
            textInput("solution_number1", label = h2("Set Numbers 1"), value = "",width = "400px"),
            verbatimTextOutput("evaluation1"),

            textInput("solution_number2", label = h2("Set Numbers 2"), value = "",width = "400px"),
            verbatimTextOutput("evaluation2"),


            textInput("solution_number3", label = h2("Set Numbers 3"), value = "",width = "400px"),
            verbatimTextOutput("evaluation3"),



            textInput("solution_number4", label = h2("Set Numbers 4"), value = "",width = "400px"),
            verbatimTextOutput("evaluation4"),


            textInput("solution_number5", label = h2("Set Numbers 5"), value = "",width = "400px"),
            verbatimTextOutput("evaluation5")
        ),
        
        
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
 
    
    output$evaluation1 <- renderPrint({ 
        number_row = 1
        ## Get My answer
        emans_solution = str_extract_all(input$solution_number1, boundary("character"))[[1]]

        
        tmp_df = df %>% 
            filter(labeled == input$set_combination)
        
        ## The Solution
        reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
            str_remove('\\.') %>% 
            str_subset( ".+")
        
        numbers_tested = length(reals_solution)
        ## Check
        paste0(round(sum(emans_solution == reals_solution)/ numbers_tested,3) * 100, ' %')
        
    })
    
   
    
    ## 2
    output$evaluation2 <- renderPrint({ 
        number_row = 2
        ## Get My answer
        emans_solution = str_extract_all(input$solution_number2, boundary("character"))[[1]]
        
        
        tmp_df = df %>% 
            filter(labeled == input$set_combination)
        
        ## The Solution
        reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
            str_remove('\\.') %>% 
            str_subset( ".+")
        
        numbers_tested = length(reals_solution)
        ## Check
        paste0(round(sum(emans_solution == reals_solution)/ numbers_tested,3) * 100, ' %')
        
    })
    
    
    ## 3
    output$evaluation3 <- renderPrint({ 
        number_row = 3
        ## Get My answer
        emans_solution = str_extract_all(input$solution_number3, boundary("character"))[[1]]
        
        
        tmp_df = df %>% 
            filter(labeled == input$set_combination)
        
        ## The Solution
        reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
            str_remove('\\.') %>% 
            str_subset( ".+")
        
        numbers_tested = length(reals_solution)
        ## Check
        paste0(round(sum(emans_solution == reals_solution)/ numbers_tested,3) * 100, ' %')
        
    })
    
    ## 4
    output$evaluation4 <- renderPrint({ 
        number_row = 4
        ## Get My answer
        emans_solution = str_extract_all(input$solution_number4, boundary("character"))[[1]]
        
        
        tmp_df = df %>% 
            filter(labeled == input$set_combination)
        
        ## The Solution
        reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
            str_remove('\\.') %>% 
            str_subset( ".+")
        
        numbers_tested = length(reals_solution)
        ## Check
        paste0(round(sum(emans_solution == reals_solution)/ numbers_tested,3) * 100, ' %')
        
    })
    
    ## 5
    
    output$evaluation5 <- renderPrint({ 
        number_row = 5
        ## Get My answer
        emans_solution = str_extract_all(input$solution_number5, boundary("character"))[[1]]
        
        
        tmp_df = df %>% 
            filter(labeled == input$set_combination)
        
        ## The Solution
        reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
            str_remove('\\.') %>% 
            str_subset( ".+")
        
        numbers_tested = length(reals_solution)
        ## Check
        paste0(round(sum(emans_solution == reals_solution)/ numbers_tested,3) * 100, ' %')
        
    })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
