#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
# Run analysis
source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AC World Championship Fantasy Competition"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("affiliation",
                    "Affiliations",
                    choices = c("DfE", "TAD", "Imperial", "Croquet community", "Goodgym", "Rebels", "Eugene"),
                    multiple = T,
                    selectize = T
                    ),
        tableOutput("scoring")
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # h4("Experimental dashboard, containing fake results"),
        h5(a("Final results here", href="https://croquetscores.com/2020/ac/wcf-world-championship/")),
        h5("Created by Eugene Chang, Last updated 23/2/2020"),
        tabsetPanel(
          # plotOutput("distPlot"),
          tabPanel("Leaderboard", dataTableOutput("leaderboard")),
          tabPanel("Top team choices", dataTableOutput("topTeam")),
          tabPanel("Player scores", "Aiken Hakes scractched from the block; all Block games involving him have been removed.",dataTableOutput("playerScores")),
          tabPanel("Results", dataTableOutput("results"))
          
        )
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   newData <- reactive({
     if(is.null(input$affiliation)){
       tbl_leaderboard
     } else{
       tbl_leaderboard %>% 
         filter(str_detect(Affiliation, paste(input$affiliation, collapse = "|")))
     }
   })
   
   output$leaderboard <- renderDataTable(
       newData() %>% 
         select(-Affiliation)
   )

   output$topTeam <- renderDataTable({
    topPlayers <- head(tbl_leaderboard, 10) %>% 
      pull(Name)
    
    tbl_selections %>% 
      filter(Name %in% topPlayers) %>% 
      select(-TeamName)
   })
   output$playerScores <- renderDataTable({
     df_player_points %>% 
       select(-Key) %>% 
       mutate(Value = round(Points*100/Cost,1))
   })
   output$scoring <- renderTable(tbl_scoring)
   
   output$results <- renderDataTable({results})
}

# Run the application 
shinyApp(ui = ui, server = server)

