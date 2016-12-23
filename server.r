library(ggplot2)
library(htmlwidgets)
library(jsonlite)
library(shotsignR)
library(shiny)
library(plyr)
library(webshot)
library(scatterD3)
server<-shinyServer(function(input, output){
  #webshot::install_phantomjs()
  test_data <<- jsonlite::fromJSON(
    paste0(
      'https://gist.githubusercontent.com/',
      'timelyportfolio/d00488979c22fc4ed900/raw/',
      'a7838e5ea1abe9f0d836e55499bd618efaeae5b7/shotsign.json'
    )
  )
  shots<-read.csv("shots.csv")
  shots$defX<-shots$shot_x - shots$defender_distance*cos(shots$defender_angle)
  shots$defY<-shots$shot_y - shots$defender_distance*sin(shots$defender_angle)+4.75
  shots$offX<-shots$shot_x - shots$shooter_velocity_ft_sec *cos(shots$shooter_velocity_angle)
  shots$offY<- shots$shot_y - shots$shooter_velocity_ft_sec *sin(shots$shooter_velocity_angle)+4.75
  shots$defXMov<-shots$defX - shots$defender_velocity_ft_sec*cos(shots$defender_velocity_angle)
  shots$defYMov<-shots$defY - shots$defender_velocity_ft_sec*sin(shots$defender_velocity_angle)
  xPos<-data.frame(append(shots$shot_x,shots$defX))
  yPos<-data.frame(append(shots$shot_y+4.75,shots$defY))
  bots<-cbind((xPos),(yPos))
  bots$players<-"Shooter"
  bots$players[10001:20000]<-"Defender"
  names(bots)<-c("xPos","yPos","players")
  output$text1<-renderText({ "analysis of the OKC thunder data using custom metrics and color schemes"})
  output$text2<-renderText({ "Yeah!"})
  output$text3<-renderText({ "loads of analysis" })
  output$text4<-renderText({ "By James Hennessy" })
  output$scatterBasket<-renderScatterD3({
    if(input$onlyMade){
      shots=shots[shots$made==1,]
    }
    if(input$colorVar=="defender_distance" ){
      colorVars=shots$defender_distance
    }
    if(input$colorVar=="dribbles_before"){
      colorVars=shots$dribbles_before
    }
    if(input$colorVar=="shooters"){
      colorVars=bots$players
    }
    if(input$symVar =="made" ){
      symVars=shots$made
    }
    if(input$symVar=="dribbles_before"){
      symVars=shots$dribbles_before
    }
    if(input$colorVar=="shooters"){
    scatterD3(x= bots$Xpos ,y=bots$yPos ,col_var= colorVars,lines=data.frame(slope=c(0,0,Inf,Inf),intercept=c(94,0,-25,0,47,0,0,25),stroke=c("yellow","blue","green","red"),stroke_width=1,lasso = TRUE))
    }
   else{
     scatterD3(x= shots$shot_x ,y=shots$shot_y+4.75 ,col_var= colorVars, symbol_var = symVars,lines=data.frame(slope=c(0,0,Inf,Inf),intercept=c(94,0,-25,0,47,0,0,25),stroke=c("yellow","blue","green","red"),stroke_width=1,lasso = TRUE))
                                                                                                    
   } 
    })
  output$halfPlot<-renderPlot({
     pat()
  })
  pat<-function()({
    circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
      angles = seq(0, 2 * pi, length.out = npoints)
      return(data.frame(x = center[1] + radius * cos(angles),
                        y = center[2] + radius * sin(angles)))
    }
    
    theme_court = function(base_size = 16) {
      theme_bw(base_size) +
        theme(
          text = element_text(color = "#f0f0f0"),
          plot.background = element_rect(fill = "blue", color = "red"),
          panel.background = element_rect(fill = "black", color = "white"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks.length = unit(0, "lines"),
          legend.background = element_rect(fill = "orange", color = "purple"),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.text = element_text(size = rel(1.0))
        )
    }
    
    width = 50
    height = 94 / 2
    key_height = 19
    inner_key_width = 12
    outer_key_width = 16
    backboard_width = 6
    backboard_offset = 4
    neck_length = 0.5
    hoop_radius = 0.75
    hoop_center_y = backboard_offset + neck_length + hoop_radius
    three_point_radius = 23.75
    three_point_side_radius = 22
    three_point_side_height = 14
    
    short_three_radius = 22
    short_three_seasons = c("1994-95", "1995-96", "1996-97")
    print("Hame")
    court_points = data.frame(
      x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
      y = c(height, 0, 0, height, height),
      desc = "perimeter"
    )
    
    court_points = rbind(court_points , data.frame(
      x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
      y = c(0, key_height, key_height, 0),
      desc = "outer_key"
    ))
    
    court_points = rbind(court_points , data.frame(
      x = c(-backboard_width / 2, backboard_width / 2),
      y = c(backboard_offset, backboard_offset),
      desc = "backboard"
    ))
    
    court_points = rbind(court_points , data.frame(
      x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
    ))
    
    foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
    foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
    foul_circle_bottom = filter(foul_circle, y < key_height) %>% mutate(desc = "foul_circle_bottom")
    
    hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop")
    
    restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
      filter(y >= hoop_center_y) %>%
      mutate(desc = "restricted")
    
    three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height)
    short_three_circle = circle_points(center = c(0, hoop_center_y), radius = short_three_radius) %>% filter(y >= hoop_center_y)
    
    three_point_line = data.frame(
      x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
      y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
      desc = "three_point_line"
    )
    
    short_three_line = data.frame(
      x = c(three_point_side_radius, three_point_side_radius, short_three_circle$x, -three_point_side_radius, -three_point_side_radius),
      y = c(0, hoop_center_y, short_three_circle$y, hoop_center_y, 0),
      desc = "short_three_line"
    )
    
    court_without_three = rbind(court_points , foul_circle_top, foul_circle_bottom, hoop, restricted)
    
    court_points = rbind(court_without_three, three_point_line)
    court_points = mutate(court_points , dash = (desc == "foul_circle_bottom"))
    
    short_three_court_points = rbind(court_without_three, short_three_line)
    short_three_court_points = mutate(short_three_court_points , dash = (desc == "foul_circle_bottom"))
    
    court <<- ggplot() +
      geom_path(data = court_points,
                aes(x = x, y = y, group = desc, linetype = dash),
                color = "#999999") +
      scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
      coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
      theme_court(base_size = 22)
    
    short_three_court = ggplot() +
      geom_path(data = short_three_court_points,
                aes(x = x, y = y, group = desc, linetype = dash),
                color = "#999999") +
      scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
      coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
      theme_court(base_size = 22)
    courter<<-court
  })
  
    bigPlot<-reactive({
    ggplot(data=data.frame(x=1,y=1),aes(x,y))+
             ###outside box:
             geom_path(data=data.frame(x=c(-250,-250,250,250,-250),y=c(-470,470,470,-470,-470)))+
             ###halfcourt line:
             geom_path(data=data.frame(x=c(-250,250),y=c(0,0)))+
             ###halfcourt semicircle:
             geom_path(data=data.frame(x=c(-60000:(-1)/10000,1:60000/10000),y=c(sqrt(60^2-c(-60000:(-1)/10000,1:60000/10000)^2))),aes(x=x,y=y))+
             geom_path(data=data.frame(x=c(-60000:(-1)/10000,1:60000/10000),y=-c(sqrt(60^2-c(-60000:(-1)/10000,1:60000/10000)^2))),aes(x=x,y=y))+
             ###solid FT semicircle above FT line:
             geom_path(data=data.frame(x=c(-60000:(-1)/10000,1:60000/10000),y=c(280-sqrt(60^2-c(-60000:(-1)/10000,1:60000/10000)^2))),aes(x=x,y=y))+
             geom_path(data=data.frame(x=c(-60000:(-1)/10000,1:60000/10000),y=-c(280-sqrt(60^2-c(-60000:(-1)/10000,1:60000/10000)^2))),aes(x=x,y=y))+
             ###dashed FT semicircle below FT line:
             geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
             geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
             ###key:
             geom_path(data=data.frame(x=c(-80,-80,80,80,-80),y=c(470,280,280,470,470)))+
             geom_path(data=data.frame(x=-c(-80,-80,80,80,-80),y=-c(470,280,280,470,470)))+
             ###box inside the key:
             geom_path(data=data.frame(x=c(-60,-60,60,60,-60),y=c(470,280,280,470,470)))+
             geom_path(data=data.frame(x=c(-60,-60,60,60,-60),y=-c(470,280,280,470,470)))+
             ###restricted area semicircle:
             geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
             geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
             ###rim:
             geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000)*10,y=c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))*10),aes(x=x,y=y))+
             geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000)*10,y=-c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))*10),aes(x=x,y=y))+
             ###backboard:
             geom_path(data=data.frame(x=c(-30,30),y=c(430,430)),lineend='butt')+
             geom_path(data=data.frame(x=c(-30,30),y=-c(430,430)),lineend='butt')+
             ###three-point line:
             geom_path(data=data.frame(x=c(-220,-220,-220000:(-1)/1000,1:220000/1000,220,220),y=c(470,470-1690/12,417.5-sqrt(237.5^2-c(-220000:(-1)/1000,1:220000/1000)^2),470-1690/12,470)),aes(x=x,y=y))+
             geom_path(data=data.frame(x=c(-220,-220,-220000:(-1)/1000,1:220000/1000,220,220),y=-c(470,470-1690/12,417.5-sqrt(237.5^2-c(-220000:(-1)/1000,1:220000/1000)^2),470-1690/12,470)),aes(x=x,y=y))
    })
    output$plot<-renderPlot({
      
      if(input$onlyMade){
        shots=shots[shots$made==1,]
        shots<-shots[1:100,]
      }
     truePlot<- bigPlot()
       if(input$arrowsAdd ){
         if(input$bySize){
           defSize<-(shots$defender_velocity_ft_sec)/10
           offSize<-(shots$shooter_velocity_ft_sec)/10
         }
         else{
           defSize<-1
           offSize<-1
         }
         if(input$addDef){
      truePlot<- truePlot+geom_segment(data=shots, mapping=aes(x=defX*10, y=defY*10-470, xend=shot_x*10, yend=shot_y*10-422.5), arrow=arrow(), size=.1, color="blue")
      truePlot<- truePlot+geom_segment(data=shots, mapping=aes(x=defX*10, y=defY*10-470,xend=defXMov*10, yend=defYMov*10-470 ), arrow=arrow(), size=defSize, color="red")
      
      }
         else{
           truePlot<- truePlot+geom_segment(data=shots, mapping=aes(x=shot_x*10, y=shot_y*10-422.5,xend=offX*10, yend=offY*10-470 ), arrow=arrow(), size=offSize, color="purple")
           
         }
       }
     
     truePlot<-truePlot+
      geom_point(data = shots, aes(shot_x*10, shot_y*10-422.5,colour="shooter"))+
      geom_point(data = shots, aes(defX*10, defY*10-470,colour="defender"))+
       coord_fixed()
    truePlot<-truePlot
    print(truePlot)
    },height=1000,width = 1000) 
  output$plots<-renderPlot({
       pat()
      
      if(input$ShotsOn){
        courtsa<- courter + geom_point(data = shots, aes(shot_x, shot_y+4.75,colour="red"))
      }
    else{
      courtsa<-courter
    }
       courtsar<-courtsa
       print(courtsar)
  })
  shotSign<-reactive({
    shotsign(test_data)
  })
  output$widgets<-renderShotsign({
    test<-read.csv("hello.csv")
    print(shotsign(test))
    shotsign(test)
  })
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste("plot",".png", sep='')
    },
    content = function(file) {
      #      src <- normalizePath('report.Rmd')
      if(input$tabs=="shotSigns"){
        here<-shotSign()
        
      }
      else{
        here<-courted
        
      }
      here<-here
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(here, file="temp.html", selfcontained = F) 
      
      webshot("temp.html", file = file,
              cliprect = "viewport")
    }
  )
}
)