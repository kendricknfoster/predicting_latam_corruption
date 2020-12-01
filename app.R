# Load relevant libraries. 

library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)

# Read in the different RDSs that I set up in the other files. 

CPI_shapefile <- readRDS("www/CPI_shapefile.RDS")
final_data <- readRDS("final_data.RDS")
model <- readRDS("model.RDS")

# I select the qualitative variables and the Log of GDP out of the dataset so
# that I don't end up with a lot of errors later on.

final_test <- readRDS("final_test.RDS") %>%
    select(-CPI, -country, -year, -log_gdp)

ui <- shinyUI(
    navbarPage(
        theme = shinytheme("cosmo"),
        "Predicting Corruption in Latin America",
        
        ### FIRST PAGE ###
        
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
                                     p("Political science has posited several major variables that are highly correlated with corruption: ",
                                       tags$ul(
                                           tags$li("GDP Per Capita: Richer countries
                                                   have lower levels of corruption due to more
                                                   developed political and economic institutions (Serra 2006)."),
                                           tags$li("Poverty Rate: Poor individuals need access to government services, 
                                                   so a higher level of poverty means more citizens might be willing
                                                   to pay bribes to access government services (Juresten and Bjornskov 2014)."),
                                           tags$li("Inequality: Relatedly, with higher inequality, the rich have more to lose from fair economic
                                                   systems, so they are more likely to engage in corrupt behavior to maintain their
                                                   current positions. As a result, we would expect higher inequality to lead to 
                                                   higher levels of corruption (You and Khagram 2005)."))),
                                     p("I also hope to test several less common explanations for corruption using datasets beyond 
                                       traditional economic factors:",
                                       tags$ul(
                                           tags$li("Bureaucratic Remuneration: 
                                                   If bureaucrats get paid more, they have less incentive for petty corruption. 
                                                   As a result, we would expect higher levels of bureaucratic remuneration to
                                                   produce less corruption. "),
                                           tags$li("Government Spending: 
                                                   If a government is larger overall, there are fewer opportunities for inequality (Goel and Nelson 1998). 
                                                   As a result, we would expect higher government spending to breed more corruption."),
                                           tags$li("Infrastructure Spending: 
                                                   Infrastructure projects are fertile breeding grounds for corruption, so 
                                                   we would expect a higher level of corruption with more spending on infrastructure."),
                                           tags$li("Public Campaign Finance:
                                                   Financing for campaigns is a common opportunity for corruption, so we would expect
                                                   countries with higher levels of public campaign financing to have lower levels of corruption.")),
                                       p("I do not include two common indicators for corruption since the CPI calculation 
                                       already includes them to derive the CPI score: democracy and economic freedom.")))
                     )
                 )), 
        
        ### SECOND PAGE ###
        
        tabPanel("The Data",
                 fluidPage(
                     p("In this panel, look over the various indicators to see how strongly they are
                  correlated with Corruption Perceptions Index (CPI) scores. Transparency International's
                  Corruption Perceptions Index seeks to measure how the business world and academia
                  perceive public sector corruption. On their scale, a 0 is an extremely corrupt country
                       and a 100 is a very clean country.",
                       tags$ul(
                           tags$li(
                               tags$b("GDP per capita"), "is measured in 2011 US dollars."),
                           tags$li("The", tags$b("Gini coefficient"), "measures inequality, with 0 representing
                              a perfectly unequal country and 1 representing a perfectly equal country."),
                           tags$li("For this project, the", tags$b("poverty rate"), "is defined as the proportion of people
                              living on less than $1.90 per day."),
                           tags$li(
                               tags$b("Government spending"), "and", tags$b("infrastructure spending"), "are both measured as
                              percentage of GDP."),
                           tags$li(
                               tags$b("Property rights"), "are measured on a scale of 0 to 1, where 0 is a country
                              with absolutely no property right protections and 1 is a country with
                              absolute property right protections."),
                           tags$li(
                               tags$b("Bureaucratic remuneration"), "is measured on a scale of 0 to 4, where 0 is a
                              country where no bureaucrats are salaried and state-employed and 
                              4 is a country where all or almost all bureaucrats are salaried and
                              state-employed."),
                           tags$li(
                               tags$b("Public campaign finance"), "is measured on a scale of 0 to 4, where 0 is a country
                              that has no public campaign financing and 4 is a country where public campaign
                              finance contributes significantly to all or nearly all political parties.")
                       )),
                     
                     # Define the choices for the interactive plot. 
                     
                     selectInput("x", 
                                 "Select explanatory variable",
                                 choices = c("GDP per Capita" = "gdp_pc",
                                             "Gini Coefficient" = "gini",
                                             "Poverty Rate" = "poverty_rate",
                                             "Government Spending" = "govt_spending",
                                             "Infrastructure Spending" = "infra_spend",
                                             "Property Rights" = "prop_rights",
                                             "Bureaucratic Remuneration" = "bur_rem",
                                             "Public Campaign Finance" = "pcf")), 
                     
                     # Define the choices for the different plot types. 
                     
                     selectInput("geom", 
                                 "Select plot type", 
                                 c("Scatterplot" = "point", 
                                   "Linear Regression" = "linear", 
                                   "Quadratic Regression" = "quadratic")), 
                     
                     plotOutput("aggregate"))),
        
        
        navbarMenu("The Model", 
                   tabPanel("Interpreting the Model",
                            titlePanel("Interpreting the Model"),
                            fluidRow(
                                column(6,
                                       tableOutput("regression_table")),
                                column(6,
                                       p("I end up using a multivariate linear model with an interaction between
                                         between government spending and infrastructure spending (since a country 
                                         with high infrastructure spending likely also has high government spending).
                                         This model had the lowest RMSE of all the models I ran."),
                                       p("Our most significant predictors are GDP per capita, property rights, and bureaucratic 
                                          remuneration, which are negatively correlated with corruption, and the Gini coefficient and
                                          poverty rate, which are positively correlated with corruption.",
                                       p("On average, every $1000 increase in GDP per capita increases the predicted CPI
                                         by 1 point. On average, every 0.01 point increase in a country's property rights
                                         index increases the predicted CPI by 0.337 points. On average, every 1 point increase
                                         in a country's bureaucratic remuneration score increases the predicted CPI by 7.343 points. 
                                         On the other hand, every 1 point increase in a country's Gini coefficient decreases the
                                         predicted CPI by 0.547 points and every 1 point increase in a country's poverty rate
                                         decreases the predicted CPI by 0.670 points."),
                                       p("Interpret the interaction term here"),
                                       p("One surprising thing about this regression was the impact of Public Campaign Finance:
                                         We would expect that more public campaign finance increases a country's CPI score, but
                                         the model shows that it in fact decreases a country's predicted CPI. (reasons why)"),
                                       p("Implications"),
                                       p("Limitations"))))),
                   
                   tabPanel("Make Your Own Predictions", 
                            titlePanel("Make Your Own Predictions"),
                            fluidPage(
                                fluidRow(
                                    p("The default values are the median values of each indicator: "),
                                    column(6, 
                                           numericInput( 'gdp_pc', 'GDP Per Capita', 4707.79),
                                           numericInput( 'gini', 'Gini Coefficient', 49.20),
                                           numericInput( 'poverty_rate', 'Poverty Rate', 4.6),
                                           numericInput( 'prop_rights', 'Property Rights', 0.785)), 
                                    column(6, 
                                           sliderInput( 'bur_rem', 'Bureaucratic Remuneration', 
                                                        min = 0, max = 4, value = 3.75, step = 0.25),
                                           sliderInput( 'pcf', 'Public Campaign Finance', 
                                                        min = 0, max = 4, value = 2.75, step = 0.25),
                                           numericInput( 'govt_spending', 'Government Spending', 21.46),
                                           numericInput( 'infra_spend', 'Infrastructure Spending', 1.92))),
                                fluidRow(h3("Calculate Predicted CPI"), 
                                         column(2, actionButton('cal','Calculate', icon = icon('calculator'))),
                                         column(2, verbatimTextOutput("value", placeholder = TRUE)))))), 
        
        
        tabPanel("About",
                 titlePanel("About"),
                 h3("Project Background"),
                 p("This project originated from research I conducted for Prof. Alisha Holland on the determinants
                   for corruption in infrastructure projects in Latin America. In particular, that project made me 
                   wonder what indicators could be used to predict more corrupt countries in the region and what
                   public policy investments Latin American countries can make to reduce corruption."),
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
    
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               linear = geom_smooth(method = lm, formula = y ~ x),
               quadratic = geom_smooth(method = lm, formula = y ~ poly(x, 2))
        )
    })
    
    output$aggregate <- renderPlot({
        plot <- final_data %>%
            drop_na(.data[[input$x]]) %>%
            ggplot(aes(.data[[input$x]], CPI)) +
            plot_geom() + 
            ylab("CPI")
        
        if (input$x == "gdp_pc")
            plot <- plot + xlab("GDP Per Capita ($)")
        
        if(input$x == "gini")
            plot <- plot + xlab("Gini Coefficient")
        
        if(input$x == "poverty_rate")
            plot <- plot + xlab("Poverty Rate")
        
        if(input$x == "govt_spending")
            plot <- plot + xlab("Government Spending (% of GDP)")
        
        if(input$x == "infra_spend")
            plot <- plot + xlab("Infrastructure Spending (% of GDP)")
        
        if(input$x == "prop_rights")
            plot <- plot + xlab("Property Rights (0 to 1)")
        
        if(input$x == "bur_rem")
            plot <- plot + xlab("Bureaucratic Remuneration (0 to 4)")
        
        if(input$x == "pcf")
            plot <- plot + xlab("Public Campaign Finance (0 to 4)")
        
        plot
        
    })
    
    a <- reactiveValues(result = NULL)
    
    observeEvent(input$cal, {
        
        values = data.frame(gdp_pc = input$gdp_pc, 
                            gini = input$gini,
                            govt_spending = input$govt_spending, 
                            poverty_rate = input$poverty_rate,
                            bur_rem = as.integer(input$bur_rem),
                            pcf = as.integer(input$pcf), 
                            infra_spend = input$infra_spend,
                            prop_rights = input$prop_rights)
        
        final_test <- rbind(final_test, values)
        
        a$result <-  round(predict(model, 
                                   newdata = final_test[nrow(final_test),]), 
                           digits = 3)
    })
    
    output$value <- renderText({
        
        paste(a$result)
    })
    
    output$cpi_shapefile <- renderPlot({
        CPI_shapefile %>%
            select(country, CPI, geometry) %>%
            ggplot(aes(geometry = geometry)) +
            geom_sf(aes(fill = CPI)) +
            scale_fill_distiller(palette = "Reds") +
            theme(legend.position = "left") +
            theme_void() +
            labs(title = "Corruption Perceptions Index in Latin America, 2019",
                 subtitle = "Higher score means less corruption.")
    })
    
    output$regression_table <- render_gt({
        
        tbl_regression(model,
                       estimate_fun = ~style_number(.x, digits = 4)) %>%
            as_gt()
        
            })
}

shinyApp(ui = ui, server = server)