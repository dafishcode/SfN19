#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Housekeeping and loading  / restructuring of data 
#==========================================================================
setwd("~/Google Drive File Stream/My Drive/Presentations/1910 SfN/Local/NMF_States_Shiny")
W   <- read.csv('factor_weights.csv')
H   <- read.csv('temp_expression.csv')
ID  <- data.frame(wt=seq(from=1,to=10000, by=2),ga1=seq(from=10001,to=20000, by=2),gg2=seq(from=20001,to=30000, by=2))
id  <- data.frame(wt=seq(from=1,to=10000, by=16),ga1=seq(from=10001,to=20000, by=16), 
                  gg2=seq(from=20001,to=30000, by=16))
cs  <- list('wt', 'ga1', 'gg2')
cl  <- list('rgb(48,165,126,1)','rgb(122,116,185,1)','rgb(237,43,143,1)')

# Page layout
#==========================================================================
ui <- fluidPage(

  fluidRow(
    # Sidebar
    #-----------------------------------------------------------------------
    column(5, wellPanel(checkboxGroupInput("gene", "Genotype to show:", c("Wildtype"="wt",
                                 "GABRA1"="ga1", "GABRG2"="gg2"), selected=c("wt"), inline=T),
              plotlyOutput("NMF_timeseries"))),       
  
    # Main panel
    #-----------------------------------------------------------------------    
    column(7,plotlyOutput("NMF_statespace"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  # Full NMF plot
  #-----------------------------------------------------------------------   
  output$NMF_statespace <- renderPlotly({
    ax <- list(zeroline=T,showline=F,showticklabels=F,showgrid=T) 
    mg <- list(l=-100, r=0, b=0, t=0, pad=0)
    xax <- ax;  xax$range <- c(0, 2);   xax$title <-'Factor 2'
    yax <- ax;  yax$range <- c(0, 1.5); yax$title <-'Factor 3'
    zax <- ax;  zax$range <- c(0, 4);   zax$title <-'Factor 1'
    
    p  <- plot_ly(source='statespace',H, x=10,y=10,z=10,hoverinfo='none',width=500, height=450)
    cid <- 0
    for(c in cs){
      cid <- cid + 1
      if(c%in%input$gene){
        mk  <- list(size = 2, color = cl[[cid]], opacity=.5)
        p   <- add_markers(p,x=H$X2[ID[,c[1]]], y=H$X3[ID[,c[1]]], z=H$X1[ID[,c[1]]], marker=mk)
      }
    }
    p <- add_markers(p)
    p <- layout(p, scene = list(xaxis=xax,yaxis=yax,zaxis=zax), showlegend=F, autosize=F, margin=mg)
  })  
  
  # Sideplot to select time points
  #-----------------------------------------------------------------------
   output$NMF_timeseries <- renderPlotly({
     eventdata <- event_data("plotly_hover", source="statespace")
     p <-  plot_ly(source='timeseries', H, x=-1, y=-1, type='scatter',mode='lines',hoverinfo='none')
      cid <- 0; 
      for(c in cs){
        cid <- cid+1
        if(c%in%input$gene){
          p <- add_trace(p, x=sqrt(seq(from=1,to=10000, by=16)), y=H$X1[id[,c[1]]], line=list(color=cl[[cid]]))
          p <- add_trace(p, x=sqrt(seq(from=1,to=10000, by=16)), y=H$X2[id[,c[1]]]+3, line=list(color=cl[[cid]]))
          p <- add_trace(p, x=sqrt(seq(from=1,to=10000, by=16)), y=H$X3[id[,c[1]]]+6, line=list(color=cl[[cid]]))
          }
      }
      p <- layout(p, showlegend=F, xaxis=list(tickvals=sqrt(c(2400,4800,9600)), ticktext=c('1min', '2min', '4min')),
                  yaxis=list(range=c(-.5,8),tickmode='array', tickvals=c(0,3,6),tickangle=270,ticktext=c('               Factor 1', '                Factor 2', '               Factor 3')))
      
      if(!is.null(eventdata)){p <- add_trace(p, x=sqrt(c(eventdata$pointNumber*2, eventdata$pointNumber*2)),y=c(0,10),
                                             line=list(color='grey',dash='dot'))}
      p <- add_markers(p)
  })
}

# Run the application 
shinyApp(ui,server,options=list(height=440,width=750))
