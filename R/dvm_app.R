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
          tabsetPanel(
            tabPanel("Mission Settings",
                     selectInput('miss','Select mission',mm$missions),
                     selectInput('svvar','Sv variable',names(Sv$data)),
                     hr()),
            tabPanel('DVM settings',
                     sliderInput("Svrange", label = ("Sv Range"), min = -95, max = -20, value = c(-85, -45)),
                     numericInput('perc','Percentage',82),
                     radioButtons("updown", "Compute percentage from:",
                                  c("Bottom" = "bottom",
                                    "Surface" = "surface")),
                     numericInput('dcut','Depth cut',300),
                     numericInput('scut','Surface cut',10),
                     numericInput('dskip','# Dives to Skip',3),
                     numericInput('dend','# Dives to ignore from end',3)),
          tabPanel('Filter',
                   numericInput('xwin','Window size x',3),
                   numericInput('ywin','Window size y',3),
                   selectInput('func','Filter function',c('None','mean','median','max','min','sd','var')),
                   )
          )),

        mainPanel(
          plotOutput("svplot"),
          plotOutput("dplot"))
        )),
    server = function(input, output) {
      dvm <- reactive({
        print(input$updown)
        Sv = get_sv(path,input$miss)

        if (input$func != 'None'){
          Svval = Sv$data[input$svvar][[1]]
          Svval = filter2d(Svval, x=input$xwin,y=input$ywin,xval='Dive',yval='Depth_r', val='Sv', log=TRUE,fun=input$func)
        }else{
          Svval = Sv$data[input$svvar][[1]]
        }

        dvm = pdvm(ac_group=Svval,
                   vmin=as.numeric(input$Svrange[1]),
                   vmax=as.numeric(input$Svrange[2]),
                   perc=as.numeric(input$perc),
                   dcut=as.numeric(input$dcut),
                   scut=as.numeric(input$scut),
                   dskip=as.numeric(input$dskip),
                   dend=as.numeric(input$dend),
                   updown=input$updown)
        return(list(Svval,dvm, Sv$gps))
      })
      output$svplot <- renderPlot({
        dvm=dvm()
        Sv = dvm[[1]]
        gps = dvm[[3]]
        #print(Sv[[1]])
        p=plot_sv(Sv,gps,svmin=input$Svrange[1], svmax=input$Svrange[2])
        dvm = dvm[[2]]
        p<-p+geom_line(data=dvm, aes(x=Dive, y=Depth),size=1)
        p
      })
    output$dplot <- renderPlot({
      dvmdat = dvm()[[2]]
      ggplot(data=dvmdat, aes(x=sun,y=Depth_r))+
        xlab('Dive #')+ylab('Depth [m]')+
        geom_point()+theme_classic()+
        theme(text=element_text(size=16))
    })
    }
  )
}

