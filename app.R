
# Load relevant libraries. 

library(shiny)
library(shinythemes)
library(tidyverse)

CPI_shapefile <- readRDS("CPI_shapefile.RDS")
final_data <- readRDS("final_data.RDS")

# Define UI for application that draws a histogram

ui <- shinyUI(
    navbarPage(
        theme = shinytheme("readable"),
        "Predicting Corruption in Latin America",
        
        tabPanel("The Question",
                 titlePanel("How can we predict corruption in Latin America?"),
                 p("Latin America is generally regarded as one of the most corrupt regions 
                   in the world. However, there is a lot of variation within the region: 
                   Uruguay, Chile, and Costa Rica, for instance, are regarded as some of the 
                   least corrupt countries in the world. Why is there so much variation
                   within the region?"),
                 plotOutput("cpi_shapefile")),
        
        tabPanel("The Data",
            fluidPage(
                selectInput("x", 
                            "Select explanatory variable",
                            choices = names(final_data)),
                selectInput("geom", "geom", c("point", "linear", "quadratic")), 
                plotOutput("plot"))),
        
        tabPanel("The Model"),
        
        tabPanel("About",
                 titlePanel("About"),
                 h3("The Data"),
                 p(tags$ol(
                     tags$li("CPI data"),
                     tags$li("GDP per capita, inequality, government spending, 
                              and poverty all came from the", a("The World Bank", href = "https://data.worldbank.org"), 
                             "dataset. The inequality data is a lot spottier than the other three indicators."),
                     tags$li("Bureaucratic remuneration and public campaign finance 
                              come from the vdem dataset, accessed through the vdem 
                              package."),
                     tags$li("Economic freedom"),
                     tags$li("Spending on infrastructure")
                 )
                   ))
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$cpi_shapefile <- renderPlot({
        CPI_shapefile %>%
        select(country, CPI, geometry) %>%
            ggplot(aes(geometry = geometry)) +
            geom_sf(aes(fill = CPI)) +
            scale_fill_distiller(palette = "Reds")
    }, height = 400, width = 600)
    
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               linear = geom_smooth(method = lm, formula = y ~ x),
               quadratic = geom_smooth(method = lm, formula = y ~ poly(x, 2))
        )
    })
    
    output$plot <- renderPlot({
        ggplot(final_data, aes(.data[[input$x]], CPI)) +
            plot_geom()})
}

shinyApp(ui = ui, server = server)
