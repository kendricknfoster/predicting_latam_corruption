
# Load relevant libraries. 

library(shiny)
library(shinythemes)
library(tidyverse)

CPI_shapefile <- readRDS("CPI_shapefile.RDS")
final_data <- readRDS("final_data.RDS")
rf <- readRDS("rf.RDS")
final_test <- readRDS("final_test.RDS") %>%
    select(-CPI, -country, -year, -log_gdp)

# Define UI for application that draws a histogram

ui <- shinyUI(
    navbarPage(
        theme = shinytheme("cosmo"),
        "Predicting Corruption in Latin America",
        
        tabPanel("The Question",
                 titlePanel("How can we predict corruption in Latin America?"),
                 fluidPage(
                     fluidRow(column(6,
                                     plotOutput("cpi_shapefile")),
                              column(6, 
                                     p("Latin America is generally regarded as one of the most corrupt regions in the world. However, there is a lot of variation within the region: 
                   Uruguay, Chile, and Costa Rica, for instance, are regarded as some of the 
                   least corrupt countries in the world. Why is there so much variation
                   within the region?"),
                                     p("Political science has posited several major explanations for corruption: ",
                                       tags$ul(
                                           tags$li("GDP Per Capita: Richer countries
                                                   have lower levels of corruption due to more
                                                   developed political and economic institutions (Serra 2006)."),
                                           tags$li("Poverty"),
                                           tags$li("Inequality"))),
                                     p("I also hope to test several less common explanations for corruption using datasets beyond traditional World Bank datasets: ",
                                       tags$ul(
                                           tags$li("Bureaucratic Remuneration: 
                                                   If bureaucrats get paid more, they have less incentive for petty corruption."),
                                           tags$li("Government Spending: 
                                                   If a government spends less overall, there is less of a chance for grand corruption to occur."),
                                           tags$li("Infrastructure Spending: 
                                                   Infrastructure projects are fertile breeding grounds for corruption, so a we would expect a higher level of corruption with more spending on infrastructure."),
                                           tags$li("Public Campaign Finance: ")),
                                     p("I do not include two common indicators 
                                       for corruption since the CPI calculation 
                                       already includes them to derive the CPI score: ")))
                     )
                 )), 
        
        tabPanel("The Data",
            fluidPage(
                selectInput("x", 
                            "Select explanatory variable",
                            choices = c("GDP per Capita" = "gdp_pc",
                                        "Gini Coefficient" = "gini",
                                        "Poverty Rate" = "poverty_rate",
                                        "Government Spending" = "govt_spending",
                                        "Poverty Rate" = "poverty_rate",
                                        "Bureaucratic Remuneration" = "bur_rem",
                                        "Public Campaign Finance" = "pcf",
                                        "Infrastructure Spending" = "infra_spend")),
                selectInput("geom", 
                            "Select plot type", 
                            c("Scatterplot" = "point", 
                              "Linear Regression" = "linear", 
                              "Quadratic Regression" = "quadratic")), 
                plotOutput("plot"))),
        
        tabPanel("The Model",
                 fluidPage(
                     fluidRow(
                         column(6, 
                                numericInput( 'gdp_pc', 'GDP Per Capita', 4707.79),
                                numericInput( 'gini', 'Gini Coefficient', 49.20),
                                numericInput( 'govt_spending', 'Government Spending', 21.46),
                                numericInput( 'infra_spend', 'Infrastructure Spending', 1.92)),
                         column(6, 
                                sliderInput( 'bur_rem', 'Bureaucratic Remuneration', min = 0, max = 4, value = 3.75, step = 0.25),
                                sliderInput( 'pcf', 'Public Campaign Finance', min = 0, max = 4, value = 2.75, step = 0.25),
                                numericInput( 'poverty_rate', 'Poverty Rate', 4.6))),
                     fluidRow(h3("Calculate Predicted CPI"), 
                              column(2, actionButton('cal','Calculate', icon = icon('calculator'))),
                              column(2, verbatimTextOutput("value", placeholder = TRUE))))), 
                 
        
        tabPanel("About",
                 titlePanel("About"),
                 h3("Project Background"),
                 p(),
                 p("The GitHub repo for this project lives", a("here", href = "https://github.com/kendricknfoster/predicting_latam_corruption", ".")),
                 h3("The Data"),
                 p(tags$ul(
                     tags$li("CPI data comes from the", a("Transparency International", href = "https://www.transparency.org/en/cpi"),
                             "which compiles the CPI Index."),
                     tags$li("GDP per capita, inequality, government spending, 
                              and poverty all come from the", a("The World Bank", href = "https://data.worldbank.org"), 
                             "dataset. The inequality data is a lot spottier than the other three indicators."),
                     tags$li("Bureaucratic remuneration and public campaign finance 
                              come from the", a("vdem", href = "https://www.v-dem.net/en/"), 
                              "dataset, accessed through the vdem package."),
                     tags$li("Infrastructure spending comes from the", a("Infralatam", href = "http://home.infralatam.info/"), 
                             "dataset."))),
                 h3("About Me"),
                 p("I am a junior at Harvard College studying History and Government, with a focus on public policy and diplomacy in Latin America. You can reach me at", 
                   a("kfoster@college.harvard.edu", href = "mailto:kfoster@college.harvard.edu"),"."
                 ))))

server <- function(input, output) {

    output$cpi_shapefile <- renderPlot({
        CPI_shapefile %>%
        select(country, CPI, geometry) %>%
            ggplot(aes(geometry = geometry)) +
            geom_sf(aes(fill = CPI)) +
            scale_fill_distiller(palette = "Reds") +
            theme(legend.position = "left") +
            theme_void()
        })
    
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               linear = geom_smooth(method = lm, formula = y ~ x),
               quadratic = geom_smooth(method = lm, formula = y ~ poly(x, 2))
        )
    })
    
    output$plot <- renderPlot({
        final_data %>%
            drop_na(.data[[input$x]]) %>%
            ggplot(aes(.data[[input$x]], CPI)) +
            plot_geom()
        })
    
    a <- reactiveValues(result = NULL)
    
    observeEvent(input$cal, {
        
        #Dataframe for the single prediction
        values = data.frame(gdp_pc = input$gdp_pc, 
                            gini = input$gini,
                            govt_spending = input$govt_spending, 
                            poverty_rate = input$poverty_rate,
                            bur_rem = as.integer(input$bur_rem),
                            pcf = as.integer(input$pcf), 
                            infra_spend = input$infra_spend)
        
        #Include the values into the new data
        final_test <- rbind(final_test, values)
        
        #Single preiction using the randomforest model
        a$result <-  round(predict(rf, 
                                   newdata = final_test[nrow(final_test),]), 
                           digits = 3)
    })
    
    output$value <- renderText({
        #Display the prediction value
        paste(a$result)
    })
}

shinyApp(ui = ui, server = server)
