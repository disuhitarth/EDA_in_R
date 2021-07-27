#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#install.packages("shinydashboard")

library(AmesHousing)
library(modelsummary)
library(modeldata)
library(dplyr)
library(ggThemeAssist)
library(esquisse)
library(tidyverse)
library(corrplot)
library(skimr)
library(broom)
library(rmarkdown)
library(equatiomatic)
library(here)


library(shiny)
library(shinydashboard)


# Loading Dataset ---------------------------------------------------------

data(ames, package = "modeldata")
data(ames)
ames_n <- ames %>% 
    select(where(is.numeric))








ui <- dashboardPage(
    
    skin = "purple",
    
    dashboardHeader(title = "Final Project"),
    
    
    
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home Page", tabName = "dashboard3", icon = icon("th")),
            menuItem("Explore Models", tabName = "dashboard2", icon = icon("th"),badgeLabel = "new", badgeColor = "green"),
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Source code", icon = icon("file-code-o"), 
                     href = "https://github.com/rstudio/shinydashboard/")
        )
    ),
    
    
    
    
    ## Body content
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls",
                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "dashboard2",
                    h2("Explore Models"),
                    fluidRow(
                        box(selectInput(inputId = "xcol", 
                                    label = "Select an explanatory variable", 
                                    choices = colnames(ames_n), 
                                    selected = "Lot_Area"),
                        
                        selectInput(inputId = "ycol", 
                                    label = "Select a response variable", 
                                    choices = colnames(ames_n), 
                                    selected = "Sale_Price")),
                        
                        box(plotOutput('plot')),
                        br(),
                        box(uiOutput('eq1')),
                        br(),
                        box(tableOutput('table1')),
                        br(),
                        box(uiOutput('eq2')),
                        br(),
                        box(tableOutput('table2'))
                    )
            ),

# Dashboard 3 -------------------------------------------------------------

            
            tabItem(tabName = "dashboard3",
                    h2("EDA"),
                    
                    fluidRow(
                        box(
                            title = "Storyboard", status = "primary", solidHeader = TRUE, width = 12,
                            imageOutput("picture", height = "auto")
                        )
                        # selectInput(inputId = "xcol", 
                        #             label = "Select an explanatory variable", 
                        #             choices = colnames(ames_n), 
                        #             selected = "Lot_Area"),
                        # 
                        # selectInput(inputId = "ycol", 
                        #             label = "Select a response variable", 
                        #             choices = colnames(ames_n), 
                        #             selected = "Sale_Price"),
                        # box(plotOutput('plot')),
                        # br(),
                        # box(uiOutput('eq1')),
                        # br(),
                        # box(tableOutput('table1')),
                        # br(),
                        # box(uiOutput('eq2')),
                        # br(),
                        # box(tableOutput('table2'))
                        
                    )
            )
        )
    )
    
)

# Server ------------------------------------------------------------------


# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }


server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)

    

# Dashboard 1 -------------------------------------------------------------

        
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    

# Dashboard2 --------------------------------------------------------------

    data <- reactive({
        ames_n  %>% 
                 select(col1 = input$xcol, col2 = input$ycol)       
        # mpg2 %>% 
        #     select(col1 = input$xcol, col2 = input$ycol)
        
    })
    
    
        
    output$plot <- renderPlot({
        
        
        
        ggplot(data(),
               aes(col1, col2)) +
            geom_point(shape = 21, size = 4, stroke = 1,
                       color = "black", fill = "skyblue") +
            geom_smooth(method = "lm", se=FALSE) +
            labs(x = input$xcol, y = input$ycol) +
            theme_light() 
        
        
    })
    
    
    output$table1 <- renderTable({
        
        m1 <- lm(reformulate(input$xcol, input$ycol), data = ames_n)
        
        broom::tidy(m1)
        
    })
    
    
    output$table2 <- renderTable({
        
        m1 <- lm(reformulate(input$xcol, input$ycol), data = ames_n)
        
        broom::glance(m1)[, 1:2]
        
    })
    
    
    
    output$eq1 <- renderUI({
        
        m1 <- lm(reformulate(input$xcol, input$ycol), data = ames_n)
        
        withMathJax( paste0("Model: ", "$$", extract_eq(m1), "$$") )
    })
    
    
    output$eq2 <- renderUI({
        
        m1 <- lm(reformulate(input$xcol, input$ycol), data = ames_n)
        
        withMathJax( paste0("Model Coefs.: ", "$$", extract_eq(m1, use_coefs = TRUE), "$$") )
    })
    
    
    
    
    output$picture <- renderImage({
        return(list(src = here("E:/Rproject/RWebProj/Script/Final Project Group 36/Dashboard/www", "sb.PNG"),contentType = "image/png",alt = "Alignment"))
    }, deleteFile = FALSE) #where the src is wherever you have the picture
}



# Run the application  ----------------------------------------------------


shinyApp(ui = ui, server = server)
