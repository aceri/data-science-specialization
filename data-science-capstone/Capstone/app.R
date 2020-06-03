#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(cowsay)
library(corpus)


ng1 <- readRDS("./data/ng1.rds")
#-------------------------------------------------------------------
#-------------------------------------------------------------------
ng2a <- readRDS("./data/ng2a.rds")
ng3a <- readRDS("./data/ng3a.rds")
ng4a <- readRDS("./data/ng4a.rds")
ng5a <- readRDS("./data/ng5a.rds")
ng6a <- readRDS("./data/ng6a.rds")
#-------------------------------------------------------------------
ng2b <- readRDS("./data/ng2b.rds")
ng3b <- readRDS("./data/ng3b.rds")
ng4b <- readRDS("./data/ng4b.rds")
ng5b <- readRDS("./data/ng5b.rds")
ng6b <- readRDS("./data/ng6b.rds")
#-------------------------------------------------------------------
#-------------------------------------------------------------------
ng2c <- readRDS("./data/ng2c.rds")
ng3c <- readRDS("./data/ng3c.rds")
ng4c <- readRDS("./data/ng4c.rds")
ng5c <- readRDS("./data/ng5c.rds")
ng6c <- readRDS("./data/ng6c.rds")
#-------------------------------------------------------------------
ng2d <- readRDS("./data/ng2d.rds")
ng3d <- readRDS("./data/ng3d.rds")
ng4d <- readRDS("./data/ng4d.rds")
ng5d <- readRDS("./data/ng5d.rds")
ng6d <- readRDS("./data/ng6d.rds")
#-------------------------------------------------------------------



if (interactive()) {
ui <- fluidPage(
    shinythemes::themeSelector(),
    #theme=shinytheme("darkly"),
    
    titlePanel("Capstone: SwiftKey - Predicting Next Word"),

    textInput("ti","Hi!, write some text here",value="",placeholder = "Enter some text and I will try to predict next word"),
    verbatimTextOutput("response"),
    
    radioButtons("animals","",c("clippy","hypnotoad","spider","pig","chicken","facecat","trilobite"),inline=T)
    )


server <- function(input, output) {
    

    something <- "here should come the prediction"
    output$response <- renderText({say(paste("Q:> ",input$ti,"\nA:> ",something,sep=""),by=input$animals,type="string")})

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)

}

