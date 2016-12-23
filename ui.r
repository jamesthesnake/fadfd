
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(shotsignR)
library(scatterD3)
library(ggplot2)
library(jsonlite)

ui <-shinyUI(pageWithSidebar(headerPanel(
  h1( a( "OKC SHOTS 2016",style = "font-family: 'Cyborg', cursive; font-weight: 500; line-height: 1.1; color: #FF0000;")),
                                         sidebarPanel()),
                             sidebarPanel(
                               checkboxInput("ShotsOn","See Shots "),
                               checkboxInput("onlyMade","See made "),
                               checkboxInput("arrowsAdd","add Arrows Offense"),
                               checkboxInput("addDef","add defense"),
                               
                               checkboxInput("bySize","size arrows by speed"),

                              # actionButton("scatterD3-lasso-toggle", HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span> Toggle Lasso"), "data-toggle" = "button"),
                               selectInput("colorVar","Choose the variable for color",c("dribbles_before","defender_distance","shooters")),
                               selectInput("symVar","Choose the variable for color",c("dribbles_before","made")),
                               #selectInput("height","height",selected=100,c(100,1000)),
                               #selectInput("width","width",selected=100,c(100,1000)),
                               
                               selectInput("chooseColor", "Choose Color Scheme:", c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GnBu", "BuPu", "BuGn", "Blues")),
                               downloadButton("downloadMap","download Map")
                               
                               
                               
                             ),mainPanel(
                               
                               tabsetPanel(id="tabs",
                                           
                                           tabPanel("Instructions", value="int", textOutput("text1") ,textOutput("text2"),textOutput("text3"),textOutput("text4")),
                                           tabPanel("Graph", value="graph", plotOutput("plot")),
                                           tabPanel(" Half Graph", value="graph", plotOutput("plots")),
                                           tabPanel("Scatter Interactive",scatterD3Output("scatterBasket")),
                                           tabPanel("widgeter", value="shotSigns",shotsignOutput("widgets",height = 80))
                               )
                               
                             )
))