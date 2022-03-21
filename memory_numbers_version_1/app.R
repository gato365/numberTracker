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

library(lubridate)

setwd("C:/Users/james/OneDrive/Documents/Important_Files/Life/01_thoughts_self/numberTracker")
source('bring_in_e.R')

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
      actionButton('start','Start'),
      actionButton('stop','Stop'),
      textOutput('timeleft')
      
      # verbatimTextOutput("evaluation"),
      # verbatimTextOutput("time_elapsed")
    ),
    
    
    
    
    
    
    conditionalPanel(
      condition = 'input.question_style == "Sequence of 5"',
      ## Two Types 1 at a time & A set of 5 40 number sequences 
      radioButtons(inputId = 'set_combination',label = 'Which combination are you interested in?',
                   choices = c('A', 'B', 'C'),'A'),
      
      
      
      
      textInput("solution_number1", label = h2("Set Numbers 1"), value = "",width = "400px"),
      verbatimTextOutput("evaluation1"),
      radioButtons('answer1','Show Answer', c('Nothing','Answer','Location'), selected = 'Nothing'),
      htmlOutput("eval1_answer"),
      
      textInput("solution_number2", label = h2("Set Numbers 2"), value = "",width = "400px"),
      verbatimTextOutput("evaluation2"),
      radioButtons('answer2','Show Answer', c('Nothing','Answer','Location'), selected = 'Nothing'),
      htmlOutput("eval2_answer"),
      
      
      
      textInput("solution_number3", label = h2("Set Numbers 3"), value = "",width = "400px"),
      verbatimTextOutput("evaluation3"),
      radioButtons('answer3','Show Answer', c('Nothing','Answer','Location'), selected = 'Nothing'),
      htmlOutput("eval3_answer"),
      
      
      
      
      textInput("solution_number4", label = h2("Set Numbers 4"), value = "",width = "400px"),
      verbatimTextOutput("evaluation4"),
      radioButtons('answer4','Show Answer', c('Nothing','Answer','Location'), selected = 'Nothing'),
      htmlOutput("eval4_answer"),
      
      
      
      textInput("solution_number5", label = h2("Set Numbers 5"), value = "",width = "400px"),
      verbatimTextOutput("evaluation5"),
      radioButtons('answer5','Show Answer', c('Nothing','Answer','Location'), selected = 'Nothing'),
      htmlOutput("eval5_answer")
      
      
    ),
    
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  # start_time_info <- eventReactive(input$start, {
  #     ## Define start Time
  #     start_time <- Sys.time()
  #      return(start_time) 
  # })
  
  # finished_time_info <- eventReactive(input$finished, {
  #     ## Define End Time
  #     end_time <- Sys.time()
  #     
  #     
  #     ## Gather start time
  #     start_time <- start_time_info()
  #     
  #     ## How much time has transpired
  #     elapsed_time = end_time - start_time
  #    
  #     return(elapsed_time) 
  # })
  
  
  # output$time_elapsed <- renderPrint({ 
  #     elapsed_time <- finished_time_info()
  #    
  #     # elapsed_time
  #     # start_time_info <- start_time_info()
  #     # start_time_info
  #     # start_time = start_time_info$start_time
  #     # paste0(start_time_info)
  #     # paste0('love')
  # })
  
  timer <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  
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
  
  
  ## Answer Box
  output$eval1_answer <- renderUI({
    
    
    number_row = 1
    tmp_df = df %>% 
      filter(labeled == input$set_combination)
    
    ## Get My answer
    emans_solution = str_extract_all(input$solution_number1, boundary("character"))[[1]]
    
    
    ## The Solution
    reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
      str_remove('\\.') %>% 
      str_subset( ".+")
    
    ## User wants to see answer
    if(input$answer1 == 'Answer'){
      HTML(paste(reals_solution,collapse = '')) 
    } else if(input$answer1 == 'Location') {
      HTML(paste(ifelse(emans_solution == reals_solution,1,0),collapse = ''))
    } else if(input$answer1 == 'Nothing'){
      HTML('')
    }
    
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
  
  
  
  ## Answer Box
  output$eval2_answer <- renderUI({
    number_row = 2
    tmp_df = df %>% 
      filter(labeled == input$set_combination)
    
    ## Get My answer
    emans_solution = str_extract_all(input$solution_number2, boundary("character"))[[1]]
    
    
    
    
    ## The Solution
    reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
      str_remove('\\.') %>% 
      str_subset( ".+")
    
    ## User wants to see answer
    if(input$answer2 == 'Answer'){
      HTML(paste(reals_solution,collapse = '')) 
    } else if(input$answer2 == 'Location') {
      HTML(paste(ifelse(emans_solution == reals_solution,1,0),collapse = ''))
    } else if(input$answer2 == 'Nothing'){
      HTML('')
    }
    
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
  
  
  
  ## Answer Box
  output$eval3_answer <- renderUI({
    number_row = 3
    tmp_df = df %>% 
      filter(labeled == input$set_combination)
    
    
    ## Get My answer
    emans_solution = str_extract_all(input$solution_number3, boundary("character"))[[1]]
    
    
    
    ## The Solution
    reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
      str_remove('\\.') %>% 
      str_subset( ".+")
    ## User wants to see answer
    ## User wants to see answer
    if(input$answer3 == 'Answer'){
      HTML(paste(reals_solution,collapse = '')) 
    } else if(input$answer3 == 'Location') {
      HTML(paste(ifelse(emans_solution == reals_solution,1,0),collapse = ''))
    } else if(input$answer3 == 'Nothing'){
      HTML('')
    }
    
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
  
  
  ## Answer Box
  output$eval4_answer <- renderUI({
    number_row = 4
    tmp_df = df %>% 
      filter(labeled == input$set_combination)
    
    ## Get My answer
    emans_solution = str_extract_all(input$solution_number4, boundary("character"))[[1]]
    
    
    
    ## The Solution
    reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
      str_remove('\\.') %>% 
      str_subset( ".+")
    ## User wants to see answer
    if(input$answer4 == 'Answer'){
      HTML(paste(reals_solution,collapse = '')) 
    } else if(input$answer4 == 'Location') {
      HTML(paste(ifelse(emans_solution == reals_solution,1,0),collapse = ''))
    } else if(input$answer4 == 'Nothing'){
      HTML('')
    }
    
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
  
  ## Answer Box
  output$eval5_answer <- renderUI({
    number_row = 5
    tmp_df = df %>% 
      filter(labeled == input$set_combination)
    
    ## Get My answer
    emans_solution = str_extract_all(input$solution_number5, boundary("character"))[[1]]
    
    
    
    
    ## The Solution
    reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
      str_remove('\\.') %>% 
      str_subset( ".+")
    ## User wants to see answer
    if(input$answer5 == 'Answer'){
      HTML(paste(reals_solution,collapse = '')) 
    } else if(input$answer5 == 'Location') {
      HTML(paste(ifelse(emans_solution == reals_solution,1,0),collapse = ''))
    } else if(input$answer5 == 'Nothing'){
      HTML('')
    }
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
