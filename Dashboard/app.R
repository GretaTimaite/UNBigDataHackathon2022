#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(dashboardthemes)
library(tmaptools)
library(ggplot2)
library(shiny)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)
library(fields)
library(ggsci)
library(sf)
library(geojsonio)
library(tmap)
# load data
sfdf <- geojson_read("data/climate_action_data.geojson",what="sp") %>% st_as_sf()
sfdf20 <- sfdf[c(sample(1:nrow(sfdf),20)),]
# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title =tags$a("UNBigDataHackathon2022", href="https://github.com/Kika0/ResCompLeedsCon2022.RShiny",target="_blank"),
                    titleWidth=250),
    dashboardSidebar(
      width=250,
      sidebarMenu(
        
        menuItem("EDA",tabName = "selectInput"),
        menuItem("Linear regression models",tabName = "widgets_together"),
        menuItem("Some map",tabName = "radioButtons_tmap"))
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "blue_gradient"),
      tabItems(
        tabItem(tabName = "selectInput",
                
                #mod_selectInput_ui("selectInput_1")
                ),
        tabItem(tabName = "widgets_together",
              h1("Regression models for a sample of 20 countries"),
              h3("World countries are randomly sampled each time app is run. Reload the page for another sample of countries."),
               #mod_widgets_together_ui("widgets_together_1")
               fluidRow (
               selectInput("res_var",
                            "Select x-axis variable",
                            choices=(colnames(sfdf %>% st_drop_geometry())),selected = "name_long"),
                selectInput(inputId="lot_var",
                            label = "Select y-axis variable",
                            choices=(colnames(sfdf %>% st_drop_geometry())),selected = "ren_2014")
        ,
        fluidRow(plotOutput(outputId = "regression_model")))
                ),
        tabItem(tabName = "radioButtons_tmap",
                h2("Map of origin and destination for the selected mode of transport"),
                tags$p("The choropleth maps show counts of origins and destinations from each Hanoi commune."),
                
                radioButtons(inputId="vars",label= NULL,inline=TRUE,
                             choices = colnames(sfdf %>% as.data.frame())),
                fluidRow(
                 h3("Choropleth map of variables"),tmapOutput(outputId = "map")),
                  
            
                #mod_radioButtons_tmap_ui("radioButtons_tmap_1")
                )
      )))
# Define server logic required to draw a histogram
server <- function(input, output) {
  # mod_selectInput_server("selectInput_1")
  # mod_widgets_together_server("widgets_together_1")
  # mod_radioButtons_tmap_server("radioButtons_tmap_1")
# regression models
  output$regression_model <- renderPlot({
    
    # t.c <- df_leeds %>% pull(sym!!(input$res_var))
    # t.t <- df_leeds %>% pull(sym!!(input$lot_var))
    #modeldensity_leeds <- data.frame(sfdf %>% pull(!!sym(input$res_var)),sfdf %>% pull(!!sym(input$lot_var)))
    # fit regressions
    
    sfdf20 %>% st_drop_geometry() %>% ggplot(aes(x=sfdf20 %>% pull(!!sym(input$res_var)),y=sfdf20 %>% pull(!!sym(input$lot_var)))) +
      geom_smooth(method='lm') +
      geom_point(alpha=0.5) +
      xlab(input$res_var) +
      ylab(input$lot_var) +
      labs(title=paste0("Regression of ",input$res_var," and ",input$lot_var))
  })
  
  output$map <- renderTmap({
    tmap_options(basemaps = "OpenStreetMap")
  
    tm_shape(sfdf) +
      tm_polygons(col= "ren_2018",palette=viridis(n=7),alpha = 0.5)
  }) # end of renderTmap
}

# Run the application 
shinyApp(ui = ui, server = server)
