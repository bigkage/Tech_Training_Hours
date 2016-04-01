# shiny app to view certificated staff training stats

library(shiny)
library(dplyr)
library(ggplot2)

hours <- read.csv("hours.csv")

hours <- mutate(hours, Date.of.Session = as.POSIXct(Date.of.Session, format = "%m/%d/%Y"))

recent_date <- max(hours$Date.of.Session)

hours$Building <- factor(hours$Building, levels = c("ESC", "FHS", "CJH", "SLMS", "Hedden", "Endeavour", "Discovery" ))

subtotals <- hours %>% 
  group_by(Last.Name, First.Name, Email.Address, Building) %>% 
  mutate(total_hours = sum(X..of.Hours)) %>% 
  distinct()

total_hours <- sum(subtotals$total_hours)

participants <- nrow(subtotals)

##### session info #####

sessions <- hours %>% 
  distinct(Date.of.Session, Name.of.Session, Possible.Hours) %>% 
  select(Date.of.Session, Name.of.Session, Possible.Hours) %>% 
  arrange(Date.of.Session)

num_of_sessions <- nrow(sessions)
hours_possible <- sum(sessions$Possible.Hours)

##### ui #####
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Certificated Tech Training Hours 2015-16"),

   textOutput("asOfDate"),
   textOutput("totalHours"),
   textOutput("numParticipants"),
   textOutput("sessions"),
   textOutput("possibleHours"),
   sidebarLayout(sidebarPanel(radioButtons(
     "building_input",
     "Building",
     c(
       "All",
       "Discovery",
       "Endeavour",
       "Hedden",
       "SLMS",
       "CJH",
       "FHS",
       "ESC"
     )
   ), width = 2),
   mainPanel(plotOutput("distPlot")))
   )
)

##### server side #####
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2] 
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
     
      if(input$building_input != "All") {
subtotals_filtered <- filter(subtotals, Building == input$building_input)
      } else {
subtotals_filtered <- subtotals
}
      
      ggplot(subtotals_filtered, aes(x = total_hours, fill = Building)) +
        # geom_histogram() +
        geom_dotplot(binwidth = .2, stackgroups = TRUE, binpositions="all") +
        ggtitle("2015-16 Certificated Staff Tech Training Hours Usage") +
        ylab("Count of Staff") +
        xlab("Number of Hours Earned So Far") +
        scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8), limits = c(0,9)) +
        scale_y_continuous(breaks = NULL) +
        # scale_fill_brewer(palette="Accent") + 
        scale_fill_discrete(limits = c("Discovery", "Endeavour", "Hedden", "SLMS", "CJH", "FHS", "ESC")) 
   }, height = 500, width = 600)
   
   output$totalHours <- renderText({
     paste("Total hours of training earned by all certificated staff:", total_hours)
     })
   
   output$numParticipants <- renderText({
     paste("Number of participants:", participants)
   })
   
   output$asOfDate <- renderText({
     paste("Data from 2015-7-16 through", recent_date)
   })
   
   output$sessions <- renderText({
     paste("Number of sessions offered:", num_of_sessions)
   })
   
   output$possibleHours <- renderText({
     paste("Total training hours offered:", hours_possible)
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

