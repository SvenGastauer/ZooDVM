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

dvm_app <- function() {
  '%ni%' <- Negate('%in%')
  require(shiny)
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            tabPanel("Mission Settings",
                     selectInput('miss','Select mission','Mission'),
                     selectInput('svvar','Sv variable','Sv'),#names(Sv$data)),
                     hr()),
            tabPanel('DVM settings',
                     sliderInput("Svrange", label = ("Sv Range"), min = -95, max = -20, value = c(-85, -45)),
                     numericInput('perc','Percentage',25),
                     radioButtons("updown", "Compute percentage from:",
                                  c("Bottom" = "bottom",
                                    "Surface" = "surface")),
                     numericInput('dcut','Depth cut',300),
                     numericInput('scut','Surface cut',10),
                     numericInput('dskip','# Dives to Skip',3),
                     numericInput('dend','# Dives to ignore from end',3),
                     checkboxInput('freeze','Keep DVM',value = FALSE)),
          tabPanel('Filter',
                   hr(),
                   h3('Running function'),
                   numericInput('xwin','Window size x',3),
                   numericInput('ywin','Window size y',3),
                   selectInput('func','Filter function',c('mean','median','max','min','sd','var')),
                   shiny::actionButton('addRun','Add'),
                   hr(),
                   h3('Anomality'),
                   selectInput('funcAn','Background FUnction', c('mean','median','max','min')),
                   radioButtons('rfscale','Scaled values', c('Normal','scaled')),
                   shiny::actionButton('addAn','Add'),
                   hr(),
                   textAreaInput("filterlist", "Filter list", "None"),
                   )
          )),

        mainPanel(
          textInput("pathIn", "Path", "C:/Users/sven/Documents/Zonar/data/csv_zonar/"),#"Z:\\zonar_sven\\zonar\\data\\csv_zonar\\"),

          plotOutput("svplot"),
          plotOutput("dplot"))
        )),


    server = function(session, input, output) {

      observeEvent(input$pathIn,{
        print(input$pathIn)
        mission = ZooDVM::get_mission(input$pathIn)
        print(mission$missions)
        updateSelectInput(session, "miss",#label='sdfdsfdsf')
                          choices = mission$missions,
                          selected = mission$missions[1])
        Sv = get_sv(input$pathIn,1)
         updateSelectInput(session, "svvar",#label='sdfdsfdsf')
                           choices = names(Sv$data),
                           selected=names(Sv$data)[1])
      })

      #filter buttons
      observeEvent(input$addRun, {
        task = paste0("filter2d(Svval, x=",input$xwin,", y=",input$ywin,
                      ", xval='Dive',yval='Depth_r', val='Sv', log=TRUE,fun='",input$func,"')")
        if(input$filterlist %ni% c('None','') ){
          task = paste(input$filterlist,task, sep=';\n')
        }
        updateTextAreaInput(session, "filterlist", value = task)
      })

      observeEvent(input$addAn, {
        v=if(input$rfscale == 'Normal'){'SvAnomal'}else{'scaledAnomaly'}
        task = paste0("anomaly(Svval, fun='",input$funcAn,"', replace=TRUE,v='", v,"')") #Svval$Sv = Svval[,'",v,"']")
        if(input$filterlist %ni% c('None','') ){
          task = paste(input$filterlist,task, sep=';\n')
        }
        updateTextAreaInput(session, "filterlist", value = task)
      })

      #process Sv
      sv_proc <- reactive({
        if(input$miss != 'Mission'){
          Sv = get_sv(input$pathIn,input$miss)
          print('got Sv')
          Svval = Sv$data[input$svvar][[1]]
          print('svval is there')

          if (input$filterlist %ni% c('None','') ){
            funs = strsplit(input$filterlist,';\n')
            for(f in funs[[1]]){
              print(f)
              Svval = eval(parse(text=f))
            }
          }
          print('no filter')
          updateSliderInput(session, "Svrange", #value = c(floor(min(na.omit(Svval$Sv))),ceiling(max(na.omit(Svval$Sv)))),
                            min = floor(min(na.omit(Svval$Sv))),
                            max = ceiling(max(na.omit(Svval$Sv))),
                            step = 0.1)
          return(list(Svval, Sv$gps))
        }
      })

      #get dvm
      dvm <- reactive({
        print('getting dvm')
        if (input$freeze==FALSE){
            print('no freezing')
          Svval=sv_proc()[[1]]
          print('got it again')
          dvmdat <- pdvm(ac_group=Svval,
                     vmin=as.numeric(input$Svrange[1]),
                     vmax=as.numeric(input$Svrange[2]),
                     perc=as.numeric(input$perc),
                     dcut=as.numeric(input$dcut),
                     scut=as.numeric(input$scut),
                     dskip=as.numeric(input$dskip),
                     dend=as.numeric(input$dend),
                     updown=input$updown)
          }


          return(list(dvmdat))

      })

      output$svplot <- renderPlot({
        print('getting plot')
        sv = sv_proc()
        print('got sv')
        Sv = sv[[1]]
        gps = sv[[2]]
        dvm=dvm()[[1]]
        if(input$freeze==FALSE){dvm =dvm=dvm()[[1]]}
        #print(Sv[[1]])
        p=plot_sv(Sv,gps,svmin=input$Svrange[1], svmax=input$Svrange[2], variable='Sv')

        p<-p+geom_line(data=dvm, aes(x=Dive, y=Depth_r),size=1)
        p
      })

    output$dplot <- renderPlot({
      #dvmdat = dvmdat
      dvmdat = dvm()[[1]]#sv_proc[[1]]
      ggplot(data=dvmdat, aes(x=sun,y=Depth_r))+
        xlab('Dive #')+ylab('Depth [m]')+
        geom_point()+theme_classic()+
        theme(text=element_text(size=16))

    })
    })
}

