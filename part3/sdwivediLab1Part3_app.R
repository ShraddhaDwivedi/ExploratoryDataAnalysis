#################################################################################
#                           sdwivedi@buffalo.edu                                #
#                           yshikhar@buffalo.edu                                #
#################################################################################

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Lab#1:Part#3"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Selector for variable to plot against mpg ----
    selectInput("variable", "Charts:", 
                c("Full_Data" =1, "Keywords: Flu" =2, "Keywords: Influenza"=3))
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    h3(textOutput("caption")),
    
    # Output: Plot of the requested variable against mpg ----
    plotOutput("Plot1"),
    plotOutput("Plot2")
  )
)
## install devtools package if it's not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
#install.packages('mapproj')
library(mapproj)
library(usmap)
library(urbnmapr)
library(ggplot2)
library(ggmap)
library(openintro)
library(stringr)


my_data1 <- read.csv("geo.csv")
my_data2 <- read.csv("geo_flu.csv")
my_data3 <- read.csv("geo_influenza.csv")
my_data4 <- read.csv("StateDatabyWeekforMap_2018-19week40-4.csv")
my_data4$Activity_Level<-substr(my_data4$ACTIVITY.LEVEL, 7, 7)
my_data4$Activity_Level<-as.numeric(my_data4$Activity_Level)
new_df <- my_data4[!is.na(my_data4$Activity_Level),]
state<-aggregate( Activity_Level ~ STATENAME, new_df, mean)
map.df <- merge(statedata,state, by.x="state_name",by.y='STATENAME', all.x=T)
#map.df <- map.df[order(map.df$fips),]
map.df$fips <- map.df$state_fips

plotting <- function(my_data){
  result<-data.frame(my_data)
  result$abr<-str_sub(result$V1, -13, -12)
  result$state<-abbr2state(result$abr)
  st_c <- as.data.frame(table(unlist(result$state)))
  
  library(maps)
  
  states<-statedata
  map.df <- merge(states,st_c, by.x="state_name",by.y='Var1', all.x=T)
  map.df$fips <- map.df$state_fips
  map.df <- map.df[order(map.df$fips),]
  map.df$Activity_Level <- map.df$Freq
  return (map.df)
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  formulaText <- reactive({
    paste("Twitter_Data vs CDC_Data - The twitter data was not supposed to be exactly same as CDC data as twitter is unreliable. We observe this for state of CA and TX. However, twitter gives satisfactory result for NY and MN. For Flu Keyword: The map returns same result as the full data graph. Ohio seems to be on higher Activity level. For Influenza Keyword: The map returns same result as the full data graph. Ohio seems to be on less Activity level compared to Flu keyword.")
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  
  output$Plot1 <- renderPlot({
    if (as.numeric(input$variable)==1)
    {
      data<-plotting(my_data1)
    }
    if (as.numeric(input$variable)==2)
    {
      
      data<-plotting(my_data2)
    }
    if (as.numeric(input$variable)==3)
    {
      data<-plotting(my_data3)
    }
    plot_usmap(data = data, values = "Activity_Level", lines = "black") + 
      scale_fill_gradientn(colours=c('green3','yellow','darkorange1','red','red3', 'darkred'),na.value="green3",name = "Activity_Level", label = scales::comma)+
      theme(legend.position = "right")+
      ggtitle("US Map obtained from Twitter data")
  })
  output$Plot2 <- renderPlot({
    plot_usmap(data = map.df, values = "Activity_Level", lines = "black") + 
      scale_fill_gradientn(colours=c('green3','yellow','darkorange1','red','darkred'),na.value="green3",name = "Activity_Level", label = scales::comma)+
      theme(legend.position = "right")+
      ggtitle("US Map obtained from CDC site data")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

