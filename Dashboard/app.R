#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(geojsonio)
library(sf)
library(dashboardthemes)
library(tmaptools)
library(ggplot2)
library(shiny)
library(tidyverse)
library(shinythemes)
library(viridis)
#library(RColorBrewer)
#library(fields)
#library(ggsci)
library(paletteer)
library(RColorBrewer)
library(fields)
library(ggsci)
library(sf)
library(geojsonio)
library(tmap)
# load data
sfdf <- geojson_read("climate_action_data.geojson",what="sp") %>% st_as_sf()
# read excel file for OSM data
OSM = readxl::read_excel("osm.xlsx") 
world = spData::world
OSM$timestamp <- substr(OSM$timestamp,0,4)
#%>% rename("count_ren"='0')
# pivot_wider()
OSM1 <- OSM %>% pivot_wider(names_from = timestamp)
# join with world data 
OSM_sf <- world %>% select(name_long) %>%  left_join(OSM1,by=c("name_long"="Country")) 
# load data for WVS
wvs <- read_csv("wvs.csv")
wv <- wvs %>% select(country_5) %>% left_join((sfdf %>% st_drop_geometry() %>% select(name_long,dis_ISO3)),by=(c("country_5"="name_long")))
#sfdf20 <- sfdf[c(sample(1:nrow(sfdf),20)),]
# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title =tags$a("UNBigDataHackathon2022", href="https://gretatimaite.github.io/campr/",target="_blank"),
                    titleWidth=250),
    dashboardSidebar(
      width=250,
      sidebarMenu(
        
        
        menuItem("EDA by country",tabName = "widgets_together"),
        menuItem("EDA world map",tabName = "radioButtons_tmap"),
        menuItem("EDA WVS",tabName = "selectInput"),
        menuItem("EDA world renewables",tabName = "OSM")
    )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "blue_gradient"),
      tabItems(
        tabItem(tabName = "selectInput",
                h1("World View Survey (WVS) data plotted per country"),
                h3("Is environmental protection more important than economic growth?"),
                fluidRow (
                  selectInput("name1",
                              "Select Country",
                              choices=(unique(wvs %>% select(country_4))),selected = "ARG"),
                  
                  
                  fluidRow(plotOutput(outputId = "wvs")))
                #mod_selectInput_ui("selectInput_1")
                ),
        tabItem(tabName = "widgets_together",
              h1("Plotted values for selected country of selected variables"),
              h3("Data is plotted for the 2000-2019 period (minus NA values that are country specific)"),
              p("Legend: ren (% of renewable energy), temp (average yearly temperature),
                gdp(Total Gross Domestic product), dis (number of disasters), co2 (total CO2 emissions)."),
               #mod_widgets_together_ui("widgets_together_1")
               fluidRow (
               selectInput("name",
                            "Select Country",
                            choices=(unique(sfdf %>% st_drop_geometry() %>% select(name_long))),selected = "Kenya"),
                selectInput(inputId="var",
                            label = "Select y-axis variable",
                            choices=c("ren","co2","temp","dis","gdp"),selected = "ren")
        ,
        fluidRow(plotOutput(outputId = "regression_model")))
                ),
        tabItem(tabName = "radioButtons_tmap",
                h2("World map of selected variable"),
                tags$p("Legend: ren (% of renewable energy), temp (average yearly temperature),
                gdp(Gross Domestic Product per person), dis (number of disasters), co2 (total CO2 emissions)."),
                
                radioButtons(inputId="vars",label= NULL,inline=TRUE,
                             choices = colnames(sfdf %>% as.data.frame() %>% select(starts_with(c("ren","co2","gdp","temp","dis"))))),
                fluidRow(
                 h3("Choropleth map of variables"),tmapOutput(outputId = "map")),
                  
            
                #mod_radioButtons_tmap_ui("radioButtons_tmap_1")
                ),
        tabItem(tabName = "OSM",
                h2("World map of count of renewable energy generators"),
                tags$p("Count of generators per country per year."),
                
                radioButtons(inputId="year",label= NULL,inline=TRUE,
                             choices = colnames(OSM_sf %>% as.data.frame() %>% select(-c(name_long,geom)))),
                fluidRow(
                  h3("Choropleth map for selected year"),
                  tmapOutput(outputId = "mapOSM")),
                
                
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
    
    # sfdf20 %>% st_drop_geometry() %>% ggplot(aes(x=sfdf20 %>% pull(!!sym(input$res_var)),y=sfdf20 %>% pull(!!sym(input$lot_var)))) +
    #   geom_smooth(method='lm') +
    #   geom_point(alpha=0.5) +
    #   xlab(input$res_var) +
    #   ylab(input$lot_var) +
    #   labs(title=paste0("Regression of ",input$res_var," and ",input$lot_var))
    coln <- sfdf %>% filter(name_long==input$name) %>%  st_drop_geometry() %>% select(-c(dis_Country,dis_ISO3,co2_Country.Name,gdpPercap)) %>%
      select(starts_with(c(input$var))) %>% names()
    coln1 <- as.numeric(substr(coln,nchar(coln)-4+1,nchar(coln)))
    values <- sfdf %>% st_drop_geometry() %>%filter(name_long==input$name) %>%  select(-c(dis_Country,dis_ISO3,co2_Country.Name,gdpPercap)) %>%
      select(starts_with(c(input$var))) %>% unname() %>%  as_vector() 
    plot(x=coln1,y=values)
    # library(stringr)
    df <- data.frame(coln1,values)
    ggplot(df) + geom_line(aes(x=coln1,y=values))  + geom_point(aes(x=coln1,y=values),lwd=2) +
      xlab(input$var) +
      
      theme_minimal() 
  })
  
  output$map <- renderTmap({
    tmap_options(basemaps = "OpenStreetMap")
  
    tm_shape(sfdf) +
      tm_polygons(col= input$vars,palette=viridis(n=7),alpha = 0.5)
  }) # end of renderTmap
  
  output$mapOSM <- renderTmap({
    tmap_options(basemaps = "OpenStreetMap")
    
    tm_shape(OSM_sf) +
      tm_polygons(col= input$year,palette=viridis(n=7),alpha = 0.5)
  }) # end of renderTmap
  # make tmap
  
  output$wvs <- renderPlot({
    theme_set(theme_light())
    # Explore how responses have changed over time
    env_count <- wvs %>% 
      # Select evn columns
      mutate("country_5"=as_vector( wv[,2])) %>% 
      filter(country_5==input$name1 |country_4==input$name1 |country_6==input$name1 |country_7==input$name1  ) %>% 
      select(contains("env")) %>% 
      #filter(if_any(everything(), is.na)) %>%  
      # Reshape data for easy analysis
      pivot_longer(everything(), names_to = "env", values_to = "opinion") %>% 
      #mutate(across(everything()))
      # Drop missing values
      drop_na() %>% 
      mutate(opinion = factor(opinion)) %>% 
      # Count number of respondents in each category
      count(env, opinion) %>% 
      group_by(env) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(pct = n/total) %>% 
      # Rename rows
      mutate(env = case_when(
        env == "env_4_num" ~ "wave_4",
        env == "env_5_num" ~ "wave_5",
        env == "env_6_num" ~ "wave_6",
        env == "env_7_num" ~ "wave_7"
      ))
    
    # Visualize this
    env_count %>% 
      ggplot(mapping = aes(x = env, y = pct*100)) +
      geom_col(aes(fill = opinion), position = "dodge", alpha = 0.8) +
      paletteer::scale_fill_paletteer_d("ggthemes::Tableau_10",
                                        labels=c("protect environment", " Economic growth", "Other")) +
      ggtitle("Protecting environment vs Economic growth") +
      labs(x = "survey period",
           y = "% of respondents in survey") +
      theme(plot.title = element_text(hjust = 0.5))
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
