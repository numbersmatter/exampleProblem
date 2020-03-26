#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)

x1 <- c(1,2,3,3,3,3)
x2 <- c('red', 'blue', 'green', 'green','green','blue')
x3 <- c('small', 'medium', 'large', 'large', 'large', 'small')


df <-data.frame(x1,x2,x3)


ui <- dashboardPage(
    dashboardHeader(title = "Resource Finder"),
    
    # Sidebar for inputs 
    dashboardSidebar(
            selectizeInput("number",
                        "Number:",
                        choices = c(1,2,3),
                        multiple = TRUE
                    ),
            selectizeInput("color",
                        "color:",
                        choices = c('red', 'blue', 'green'),
                        multiple = TRUE
            ),
            selectizeInput("size",
                        "size:",
                        choices = c('small', 'medium', 'large'),
                        multiple = TRUE
            )
        ),

        # Show a plot of the generated distribution
    dashboardBody(    
    
    fluidRow(
        box(
            DT::dataTableOutput("table")
        )
    ),
    
    fluidRow(
            
          uiOutput("programinfo")

            
            
        )
    )

)


server <- function(input, output, session) {
    appdata <-reactive({
        df %>%
            filter(
                is.null(input$number) | x1 %in% input$number,
                is.null(input$color) | x2 %in% input$color,
                is.null(input$size)  | x3 %in% input$size
            )
    })
    
    output$table <- DT::renderDataTable({
        df <- appdata()
        
        action <-
            DT::dataTableAjax(session, df, outputId = "table")
        
        DT::datatable(df, options = list(ajax = list(url = action), lengthMenu =c(5,10,15), pageLength = 5), escape = FALSE)
    })
    
    output$programinfo<- renderUI({
        lapply(1:nrow(appdata()), function(i) {
            box(
            
                h2(appdata()[i,'x2']),
                p(paste0("A Program of: ", appdata()[i,'x2'])),
                h3(appdata()[i,'x3']),
                p(paste( "Hours: ",appdata()[i,3], sep = " "))
            )
 
                # withTags({
                #     div(
                #         h2(appdata()[i,1]),
                #         h3(appdata()[i,1]),
                #         p(appdata()[i,1]),
                #         body(
                #             b("Monday: "), appdata()[i,1], br(),
                #             b("Sunday: "), appdata()[i,1], br()
                #         )
                #     )
                # })
            

            })
    })
    
 
}

# Run the application 
shinyApp(ui = ui, server = server)
