library(shiny)
library(shinythemes)
library(quantmod)
library(colourpicker)


ui <- fluidPage(
    #' tags$head(
    #'     tags$style(HTML("
    #'   @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    #' "))
    #' ),
    theme=shinytheme("darkly"),
    titlePanel(
        h3("Yahoo Finance Visualizer") 
        
        #     style = "font-family: 'Passion One';
        # font-weight: 500; line-height: 1.1; 
        # color: #992222;"),
    ),    
    h5("David Pellon @ May 15, 2020") ,
    
        # style = "font-family: 'Passion One';
        # font-weight: 500; line-height: 1.1; 
        # color: #000000;"),	
    
    sidebarLayout(
        sidebarPanel(
            
            textInput("symb", "Enter Stock Symbol", "TSLA"),
            helpText("[?] You can enter stock symbols like 'GOOG', 'AAPL', 'FB', 'KO', etc. (without quotes)."),
            hr(),
            br(),
            
            sliderInput("mon","Last N Months",min=100,max=500,value=365,step=1),
            helpText("[?] Select how many days from today the instrument should be charted back. i.e: 365 would be data for the last year"),
            hr(),
            br(),
            
            radioButtons("MA_type","Select a Moving Average Type:",c("SMA"="1","EMA"="2","WMA"="3","EVWMA"="4","ZLEMA"="5"),inline=T,selected="1"),
            helpText("[?] Select a Moving Average type of the instrument price to be displayed in the chart"),
            hr(),
            br(),
            
            colourInput("MA_col","Select Moving Average Color",value="red"),
            helpText("[?] Change here the colour of the Moving Average line in the chart"),            
            hr(),
            br(),
            
            sliderInput("period","Moving Average Period",min=3,max=50,value=20,step=1),
            helpText("[?] You can specify here in how many periods back should the Moving Average be based on"),            
            hr(),
            br(),
            
            radioButtons("type","Select a Chart Type",c("Candlesticks"="1","Matchsticks"="2","Bars"="3","Line"="4"),inline=T,selected="1"),
            helpText("[?] You can switch here between different types of financial charts.")            
        ),
        mainPanel(plotOutput("plot"),br(),br())
        
    )
)
