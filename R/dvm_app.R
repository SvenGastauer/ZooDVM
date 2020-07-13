#' Shiny App to detect DVM
#' @param path Path to the ZOnar csv files
#' @return shiny app
#' @import shiny
#' @import ggplot2
#' @export
#' @author Sven Gastauer
#' @examples
#' path='Z:/zonar_sven/zonar/data/csv_zonar'
#' dvm_app(path)

dvm_app <- function(path) {
  require(shiny)
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput('miss','Select mission',mm$missions),
          selectInput('svvar','Sv variable',names(Sv$data)),
          sliderInput("Svrange", label = h3("Sv Range"), min = -95, max = -20, value = c(-85, -45)),
          numericInput('perc','Percentage',82),
          numericInput('dcut','Depth cut',300),
          numericInput('scut','Surface cut',10),
          numericInput('dskip','# Dives to Skip',3),
          numericInput('dend','# Dives to ignore from end',3)),

        mainPanel(
          plotOutput("svplot"),
          plotOutput("dplot"))
        )),
    server = function(input, output) {
      dvm <- reactive({
        Sv = get_sv(path,input$miss)
        dvm = pdvm(ac_group=Sv$data[input$svvar][[1]],
                   vmin=as.numeric(input$Svrange[1]),
                   vmax=as.numeric(input$Svrange[2]),
                   perc=as.numeric(input$perc),
                   dcut=as.numeric(input$dcut),
                   scut=as.numeric(input$scut),
                   dskip=as.numeric(input$dskip),
                   dend=as.numeric(input$dend))
        return(list(Sv,dvm))
      })
      output$svplot <- renderPlot({
        Sv=dvm()[[1]]
        #print(Sv[[1]])
        p=plot_sv(Sv$data[input$svvar][[1]],Sv$gps,svmin=input$Svrange[1], svmax=input$Svrange[2])
        dvm = dvm()[[2]]
        p<-p+geom_line(data=dvm, aes(x=Dive, y=Depth),size=1)
        p
      })
    output$dplot <- renderPlot({
      dvm = dvm()[[2]]
      ggplot(data=dvm, aes(x=sun,y=Depth_r))+
        xlab('Dive #')+ylab('Depth [m]')+
        geom_point()+theme_classic()+
        theme(text=element_text(size=16))
    })
    }
  )
}

