#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://gist.github.com/marcionicolau/4541016


library(shiny)

shinyServer(function(input, output) {
    
    output$intense <- renderPrint(function() {
        if(input$panel==2){
            Sys.sleep(10)
            return('Finished')
        }else({return(NULL)})
    })
    
})
