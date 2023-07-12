#install.packages("leaflet")
#install.packages("dplyr")
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)


# A function that returns a subset dataframe created with all the unique Sites and their average latitude longitude values.
plot_points_data <- function(dataset){
  
  average_location <- dataset %>% 
    group_by(dataset$Site) %>%
    summarise(across(c("Lat", "Lon"), mean))
  
  average_location <- setNames(average_location, c("Site","Lat","Lon"))
  
  #average_location
  
  all_butterfly_count <- dataset %>% 
    group_by(dataset$Site) %>%
    summarise(across(ButterflyCount , sum))
  
  all_butterfly_count <- setNames(all_butterfly_count, c("Site","ButterflyCount"))
  
  locations <- merge(average_location, all_butterfly_count)
  
}


# A function that returns a subset dataframe created with the top five sites with maximum butterfly count
top_five_locations <- function(dataset){
  
  butterfly_count <- dataset %>% 
    group_by(dataset$Site) %>%
    summarise(across(ButterflyCount , sum))
  
  sorted_butterfly_count <- butterfly_count[order(-butterfly_count$ButterflyCount),]
  
  final_butterfly_count <- sorted_butterfly_count[1:5, ]
  
  final_butterfly_count <- setNames(final_butterfly_count, c("Site","ButterflyCount"))
  
}

# A function that returns a subset dataframe created with the top five sites with maximum butterfly count spread across all the days of sighting.
day_wise_top_five_locations <- function(top_five_locations_dataframe, dataset){
  
  dataset$Datetime<-strftime(strptime(dataset$Datetime,"%m/%d/%y"),"%d/%m/%Y")
  
  day_butterfly_count <- dataset %>% 
    group_by(dataset$Datetime, dataset$Site) %>%
    filter(Site %in% top_five_locations_dataframe$Site) %>%
    summarise(across(ButterflyCount , sum))
  
  day_butterfly_count <- setNames(day_butterfly_count, c("Date", "Site", "ButterflyCount"))
  
  day_butterfly_count <- day_butterfly_count[order(as.Date(day_butterfly_count$Date, format="%d/%m/%Y")),]
  
  
}



ui <- fluidPage(
  
  
  fluidRow(
    
    tags$style('.container-fluid{background-color: #2b3e4f;}'),
    
    h3(column(12, div(style = "background-color: #4c9be8; padding: 5px;",
                      HTML(paste("<p style=\"color: #e9ebec\"><u><b>SURVEY OF BUTTERFLIES IN MELBOURNE 2017</b></u></p>", 
                                 "<font color=\"#cacbcd\">")), 
    ), align = "center"
    )),
    
    h4(column(12, align = "center", div(style = "padding: 5px;",
                                        HTML(paste("<p style=\"color: #e9ebec\">The data used for this visualization project is Our City's Little Gems 
    study which observed butterfly biodiversity and flower-butterfly interactions in the City of Melbourne between 
               January - March 2017. This visualization project visualizes the spread of butterflies spotted across a
               set of sites and its trends.</p>")), 
    ))),
    
    column(12,
           
           column(4,
                  
                  fluidRow(
                    
                    h4(column(12, align = "center", HTML(paste("<p style=\"color: #e9ebec\"><u><b>Location of the survey</b></u></p>")))),
                    h4(column(12, align = "center", HTML(paste("<p style=\"color: #e9ebec\">It is observed that there are
                                                               60 butterflies spotted at Royal Park and Womens Peace Gardens
                                                               which is the highest number of butterflies spotted in a 
                                                               site. It is also observed that there are no butterflies spotted at
                                                               Canning/Neill St Reserve, State Library of Victoria and
                                                               University Square. Another observation is that the size of the 
                                                               site does not affect the number of butterflies spotted. 
                                                               Royal park is a huge site and it has the maximum number of 
                                                               butterflies spotted but at the same time, 
                                                               Womens Peace Garden isnt as big but still has the same butterfly count.</p>"))))
                    
                  )),
           
           column(8,
                  
                  fluidRow(
                    
                    div(style = "background-color: #4d5c6d; padding: 5px;",
                        #column(12, sliderInput("range", "", 0, 60, value = c(0,60), step = 5), align = "center"),
                        leafletOutput("map", height = "45vh"),
                        absolutePanel(top = 10, right = 120,
                                      sliderInput("range", "", 0, 60, value = c(0,60), step = 5)
                        )
                    )
                    
                  )
                  
           )
           
    ),
    
    column(12,
           
           column(4,
                  plotOutput("top_five_locations_plot")
           ),
           
           column(2,
                  fluidRow(
                    
                    h4(column(12, align = "center", HTML(paste("<p style=\"color: #e9ebec\"><u><b>Top Sites for Butterflies</b></u></p>")))),
                    h4(column(12, align = "center", HTML(paste("<p style=\"color: #e9ebec\">Being a top butterfly count(60) site, at
                                                               Womens Peace Gardens all the butterflies are spotted on 2 days meanwhile
                                                              the butterflies spotted at other sites are spread across several days.</p>"))))
                    
                  )
           ),
           
           column(6,
                  plotOutput("day_wise_top_five_locations_plot")
           )
           
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- read.csv("Butterfly_biodiversity_survey_2017_PE2.csv")
  
  plot_points_dataframe <- plot_points_data(dataset)
  
  #leaflet map output
  output$map <- renderLeaflet({
    leaflet(data = plot_points_dataframe) %>% addTiles() %>%
      addCircleMarkers(lng=~Lon, lat=~Lat, popup = paste(plot_points_dataframe$Site, "(", plot_points_dataframe$ButterflyCount, ")"),
                       radius = (plot_points_dataframe$ButterflyCount + 10), 
                       color = "Purple", stroke = FALSE, fillOpacity = 0.6)
    
  })
  
  #left slider input value change
  left_slider <- reactive({
    plot_points_dataframe[plot_points_dataframe$ButterflyCount >= input$range[1] & 
                            plot_points_dataframe$ButterflyCount <= input$range[2],]
  })
  
  #right slider input value change
  right_slider <- reactive({
    plot_points_dataframe[plot_points_dataframe$ButterflyCount >= input$range[1] & 
                            plot_points_dataframe$ButterflyCount <= input$range[2] ,]
  })
  
  #this method changes the values on the map based on the input obtained from the left and right slider changes in the range slider
  observe({
    leafletProxy("map", data = left_slider()) %>%
      clearMarkers() %>%
      addCircleMarkers(data = left_slider(), lng=~Lon, lat=~Lat, popup = paste(left_slider()$Site, "(", left_slider()$ButterflyCount, ")"),
                       radius = (left_slider()$ButterflyCount + 10), 
                       color = "Purple", stroke = FALSE, fillOpacity = 0.6)
    
  }) 
  
  top_five_locations_dataframe <- top_five_locations(dataset)
  
  #bar chart with top 5 sites
  output$top_five_locations_plot <- renderPlot({
    
    ggplot(top_five_locations_dataframe, aes(x = Site, y = ButterflyCount, fill = Site), show.legend = FALSE) +
      geom_bar(stat = "identity")+ theme(legend.position = "none", plot.background = element_rect(fill = "#d9534f", colour="#d9534f"),
                                         panel.background = element_rect(fill = "white", colour="white"),
                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, colour = "white"),
                                         axis.text.y = element_text(colour = "white")) +
      geom_text(aes(label=ButterflyCount), position=position_dodge(width=0.9), vjust=-0.25)
    
  })
  
  day_wise_top_five_locations_dataframe <- day_wise_top_five_locations(top_five_locations_dataframe, dataset)
  
  #bar chart with top 5 sites spread across the days of sighting.
  output$day_wise_top_five_locations_plot <- renderPlot({
    
    ggplot(day_wise_top_five_locations_dataframe, aes(x = Date, y = ButterflyCount)) +
      geom_col(aes(fill = Site))+ theme(plot.background = element_rect(fill = "#68bd68", colour="#68bd68"), legend.position='bottom',
                                        panel.background = element_rect(fill = "white", colour="white"),
                                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, colour = "white"),
                                        axis.text.y = element_text(colour = "white")) +
      guides(fill=guide_legend(ncol=3,nrow=2,byrow=TRUE)) +
      geom_text(aes(label=ButterflyCount), position=position_dodge(width=0.9), vjust=-0.25)
    
  })
  
}

shinyApp(ui, server)