# Load relevant libraries. 

library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(gt)
library(gtsummary)

# Read in the different RDSs that I set up in the other files. 

CPI_shapefile <- readRDS("www/CPI_shapefile.RDS")
final_data <- readRDS("final_data.RDS")
model <- readRDS("model.RDS")

# I select the qualitative variables and the Log of GDP out of the dataset so
# that I don't end up with a lot of errors later on.

final_test <- readRDS("final_test.RDS") %>%
    select(-CPI, -country, -year, -log_gdp)

# As a broad note, I didn't religiously check the lines of code with paragraphs
# in them to make sure they were less than 80 characters, since those lines
# aren't essential to understanding how the code works and it's a pain in the
# butt to check each line of paragraph code and then check it again every time I
# reword the paragraph slightly.

ui <- shinyUI(
    navbarPage(
        theme = shinytheme("cosmo"),
        "Predicting Corruption in Latin America",
        
        ### FIRST PAGE ###
        
        tabPanel("The Question",
                 titlePanel("How can we predict corruption in Latin America?"),
                 fluidPage(
                     
                     # I attempt to make the map bigger here, but I couldn't
                     # find a way to do that without making it non-fluid or
                     # without making it WAY too big.
                     
                     fluidRow(column(6, 
                                     plotOutput("cpi_shapefile")),
                              column(6, 
                                     p("Latin America is generally regarded as one of the most corrupt regions in the world. 
                                        However, there is a lot of variation within the region: 
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
                                                   higher levels of corruption (You and Khagram 2005)."),
                                           tags$li("Property Rights: Poor state enforcement of contracts and property rights prompt
                                                   individuals in the private sector to engage in corrupt behavior to preserve their
                                                   property rights (Dimant and Tosato 2018: 341)."))),
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
                     
                     # Output the plot from the server. 
                     
                     plotOutput("aggregate"))),
        
        # I initially had some problems getting the regression table to output,
        # but I realized I needed to use the gt_output function (and load the gt
        # package in Shiny as well).
        
        navbarMenu("The Model", 
                   
                   ### THIRD PAGE ###
                   
                   tabPanel("Interpreting the Model",
                            titlePanel("Interpreting the Model"),
                            fluidRow(
                                column(6,
                                       gt_output("regression_table")),
                                column(6,
                                       p("I end up using a multivariate linear model with an interaction between
                                         between government spending and infrastructure spending (since a country 
                                         with high infrastructure spending likely also has high government spending).
                                         I choose this model because it has the lowest RMSE in the tests I ran."),
                                       p("Our most significant predictors are GDP per capita, property rights, and bureaucratic 
                                          remuneration, which are negatively correlated with corruption, and Gini coefficient
                                          and poverty rate, which are positively correlated with corruption.",
                                       p("On average, every $1000 increase in GDP per capita increases the predicted CPI
                                         by 1.5 points. On average, every 0.01 point increase in a country's property rights
                                         index increases the predicted CPI by 0.355 points. On average, every 1 point increase
                                         in a country's bureaucratic remuneration score increases the predicted CPI by 8.618 points. 
                                         On the other hand, every 1 point increase in a country's Gini coefficient decreases the
                                         predicted CPI by 0.491 points and every 1 point increase in a country's poverty rate
                                         decreases the predicted CPI by 0.600 points."),
                                       p("Our interaction term is statistically significant, which means that there is indeed an
                                         interplay between government spending and infrastructure spending."),
                                       p("One surprising thing about this regression was the impact of Public Campaign Finance:
                                         We would expect that more public campaign finance increases a country's CPI score, but
                                         the model shows that it in fact decreases a country's predicted CPI. This could have to do
                                         with the way that the Public Campaign Finance data is set with discrete values; more 
                                         research on potential explanations for this is needed."),
                                       p("There are several limitations to this model. First, I did not add a quadratic term
                                         into the model for the poverty variable; this could improve the model's reliability.
                                         Second, since all the data comes from a time series, it could suffer from autocorrelation,
                                         and I did not control for this possibility. Third, this model only has 92 observations 
                                         since there was so much missing data, so it could be wildly incorrect. Finally, 
                                         As a side note, be careful when extrapolating results of this projects to different regions, 
                                         as different regions have different distributions of variables."), 
                                       p("Given the number of statistically significant variables in the analysis, reducing corruption
                                         by acting on only one variable would likely not do much to reduce corruption. Addition, there
                                         is a significant chicken-and-egg problem in reducing corruption. For example, countries may try 
                                         to improve economic development to reduce corruption, but corruption itself drives foreign investors 
                                         away and reduces economic development as well. As a result, governments should probably start on
                                         factors that are more within their control, like bureaucratic remuneration or enforcement of
                                         property rights."))))),
                   
                   ### FOURTH PAGE ###
                   
                   # I set numeric inputs for the continuous variables and
                   # slider inputs for the variables with discrete scales.
                   
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
                                
                                # I now create a "calculate" button and give it
                                # an object to that the server below will
                                # recognize it. I then set the text output of
                                # the reactive value to the output of the new
                                # prediction.
                                
                                fluidRow(h3("Calculate Predicted CPI"), 
                                         column(2, actionButton('cal','Calculate', icon = icon('calculator'))),
                                         column(2, verbatimTextOutput("value", placeholder = TRUE)))))), 
        
        
        ### FIFTH PAGE ###
        
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
                             "dataset."),
                     tags$li("Shapefile data comes from", a("replication data", 
                                                            href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OSNGFE"),
                             "for Calil et al. (2017)."))),
                 h3("About Me"),
                 p("I am a junior at Harvard College studying History and Government, with a focus on public policy and diplomacy in Latin America. You can reach me at", 
                   a("kfoster@college.harvard.edu", href = "mailto:kfoster@college.harvard.edu"),"."
                 ))))

server <- function(input, output) {
    
    # Define server output to switch the plot type in the main data tab. 
    
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               linear = geom_smooth(method = lm, 
                                    formula = y ~ x),
               quadratic = geom_smooth(method = lm, 
                                       formula = y ~ poly(x, 2))
        )
    })
    
    # Define server output to input the final data and create the plot defined
    # in the above server step.
    
    output$aggregate <- renderPlot({
        plot <- final_data %>%
            drop_na(.data[[input$x]]) %>%
            ggplot(aes(.data[[input$x]], CPI)) +
            plot_geom() + 
            ylab("CPI")
        
        # Define labels for the reactive plot. 
        
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
    
    # Set server logic required to set up the prediction tab. 
    
    a <- reactiveValues(result = NULL)
    
    observeEvent(input$cal, {
        
        # Set the values equal to the user-selected input. 
        
        values = data.frame(gdp_pc = input$gdp_pc, 
                            gini = input$gini,
                            govt_spending = input$govt_spending, 
                            poverty_rate = input$poverty_rate,
                            bur_rem = as.integer(input$bur_rem),
                            pcf = as.integer(input$pcf), 
                            infra_spend = input$infra_spend,
                            prop_rights = input$prop_rights)
        
        # Bind the new values to the test dataset. 
        
        final_test <- rbind(final_test, values)
        
        # Predict the new CPI based on the results that the user inputs. 
        
        a$result <-  round(predict(model, 
                                   newdata = final_test[nrow(final_test),]), 
                           digits = 3)
    })
    
    # Put the result in an output slot. 
    
    output$value <- renderText({
        
        paste(a$result)
    })
    
    # Define the shapefile plot on the first page. 
    
    output$cpi_shapefile <- renderPlot({
        
        # Since I was having the hardest time fixing an error with the
        # shapefile, I found this hack online to set the operating version of
        # Shapefile to the operating version that Ubuntu has (since apparently R
        # has a more updated version than Ubuntu).
        
        st_crs(CPI_shapefile) <- 4326
        
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
    
    # Output the regression table on the model tab. As discussed above, I
    # realized that I would have to use the render_gt function in order to get
    # the app to work.
    
    output$regression_table <- render_gt({
        
        # I look online to see how to add more digits into the table. 
        
        tbl_regression(model,
                       estimate_fun = ~style_number(.x, digits = 4)) %>%
            as_gt()
        
            })
}

# Run the Shiny App. 

shinyApp(ui = ui, server = server)