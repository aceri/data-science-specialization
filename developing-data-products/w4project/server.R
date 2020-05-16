library(shiny)
library(quantmod)
library(mondate)


plot_chart<-function(symb,mon,type,period,MA_type,MA_col)
{
    get_data<-function(symb,mon)
    {
        ret <- getSymbols(symb,src="yahoo",auto.assign=F,from=Sys.Date()-mon,to=Sys.Date())
        return(ret)
    }
    chart_type <- function(type)
    {
         if(type=="1"){ret <- "candlesticks"}
         else if(type=="2"){ret <- "matchsticks"}
         else if(type=="3"){ret <- "bars"}
         else if(type=="4"){ret <- "line"}
         return(ret)
    }
    chartSeries(get_data(symb,mon),
                type = chart_type(type), 
                theme = chartTheme("black"),
                name=toupper(symb),
                up.col="green",
                dn.col="red")
    if (MA_type==1) {addSMA(n=period,col=MA_col)}
    else if (MA_type==2) {addEMA(n=period,col=MA_col)}
    else if (MA_type==3) {addWMA(n=period,col=MA_col)}
    else if (MA_type==4) {addEVWMA(n=period,col=MA_col)}
    else if (MA_type==5) {addZLEMA(n=period,col=MA_col)}
}

shinyServer(function(input, output, session) 
    {
        optionMA<-reactive({ plot_chart(input$symb,input$mon,input$type,input$period,input$MA_type,input$MA_col) })
        output$plot <- renderPlot({ optionMA() })
	})
