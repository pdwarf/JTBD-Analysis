#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("JTBD Cluster Analysis"),
   
   # Sidebar with a file upload for user CSV files
   sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        tags$p("The application expects a file with stories as rows, pushes and pulls as columns, and if a given push/pull was causal in any given story, it's coded as 1 (or otherwise 0). The first column is expected to contain the names of the stories."),
        tags$a(href="https://twitter.com/rjs/status/1196994350414061569", "See an example by Ryan Singer on Twitter here."),
        tags$a(href="https://github.com/pdwarf/JTBD-Analysis", "View code on GitHub"),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     'Comma'),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     'Double Quote')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("dendrogram")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$dendrogram <- renderPlot({
     # input$file1 will be NULL initially. After the user selects and uploads a 
     # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
     # columns. The 'datapath' column will contain the local filenames where the 
     # data can be found.
     
     inFile <- input$file1
     
     if (is.null(inFile))
       return(NULL)
     
     data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
     
     # Retrieve the number of colums
     no.of.columns <- ncol(data)
     
     # Standardize the data
     scaled.data <- scale(data[,2:no.of.columns])
     
     # Create disctance matrix
     d <- dist(scaled.data, method = "euclidian")
     
     # Hierarchical clustering using Ward's method
     clusters <- hclust(d, method = "ward.D" )
     
     # Plot the obtained dendrogram
     plot(clusters, cex = 0.6, hang = -1)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

