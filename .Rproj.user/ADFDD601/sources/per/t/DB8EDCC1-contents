#rm(list=lsf.str())
# Library ---------------------------------------------------------------------
library(shinyLP)
library(shiny)
library(shinydashboard)
library(shiny.pwa)
library(plotly)
library(tidyverse)
library(lubridate)
library(timetk)
library(shinycssloaders)
library(purrr)
library(leaflet)
library(weather)
library(icons)
#library(rmarkdown)
library(shinydashboardPlus)
#library(odbc) #Contains drivers to connect to a database
library(DBI) #Contains functions for interacting with the database
library(dbplyr)
#library(RODBC)
library(data.table)
#library(utils)
library(RMySQL)
library(dygraphs)
library(xts)



# SQL Connection - start session ---------------------------------------------------------------
# bcon <- dbConnect(RMySQL::MySQL(), user = user.name, password = pass,dbname = 'climate_data', host = 'climatedata.mysql.database.azure.com')

# UI ---------------------------------------------------------------------------
options(spinner.color="#63addb", spinner.color.background="#ffffff", spinner.size=2) # Options for Spinner (Loading Graphics)
  ## dashboard Page --------------------------------------
ui <- dashboardPage(skin = "black", 
                    header = dashboardHeader(title = span(img(src="CCI_lite.png",width="150",height="55"))),
                    #header = dbHeader,
                    footer = dashboardFooter(left = h1(strong( "Delivering Climate Intelegence amidst a Changing Climate")
                                                       , style = "font-size:12px;"),right =  span(img(src='climXdiad.png', height='40',width='220'))),
  ## Sidebar Page --------------------------------------
                    sidebar = dashboardSidebar(sidebarMenu(menuItem("Rain Gauge", tabName = "rain", icon =weather_icon("wi-rain"),
                                                                    menuItem("Monthly",
                                                                             tabName = "monthly"),
                                                                    menuItem("Weekly",
                                                                             tabName = "weekly"),
                                                                    menuItem("Daily",
                                                                             tabName = "daily"),
                                                                    menuItem("Water Deficit",
                                                                             tabName = "wd"),
                                                                    menuItem("Rainfall Extremes",
                                                                             tabName = "rain_ex")),
                                                           menuItem("Automatic Weather Station", tabName = "AWS", icon =weather_icon("day-cloudy-gusts"),
                                                                    menuItem("Historical Data",
                                                                             tabName = "aws_data"),
                                                                    menuItem("Real Time Telemetry",
                                                                             tabName = "telemetry")),
                                                           menuItem("Climate Driver Update", tabName = "climate_Driver", icon = icon("dashboard")),
                                                           menuItem("Secondary Resources", tabName = "secondary", icon =weather_icon("fire"),
                                                                    menuItem("Local Fire Danger Rating",
                                                                             tabName = "lfdr"),
                                                                    menuItem("Gobal Fire Danger Rating",
                                                                             tabName = "gfdr"),
                                                                    menuItem("Secondary Climate Data",
                                                                             tabName = "secondary_climate_data")),
                                                           menuItem("Forecast", tabName = "climateforecast", icon = icon("bar-chart-o"),
                                                                    menuItem("Short-term Forecast",
                                                                             tabName = "shortterm_forecast"),
                                                                    menuItem("Long-term Downscaled Forecast",
                                                                             tabName = "longterm_forecast")),
                                                           menuItem("About", tabName = "about", icon = icon("info"))
                                                           
                    )),
  ## dashboard Body --------------------------------------
                    body = dashboardBody(tags$head(tags$style(HTML(
                      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: red;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> For demonstration purposes only - Didi Adisaputro  </span>\');
      })
     ')),
    pwa("https://libzclim.shinyapps.io/CCI_Lite/", output = "www", icon ="www/climatology.png", title = "CCI Lite" ), 
                                         tabItems(tabItem(tabName = "monthly",
                                                          fluidRow(box( title = "Monthly Rain",
                                                                        collapsible = TRUE,
                                                                        selectInput(inputId = "selected_region_monthly",
                                                                                    label = "Select Region",
                                                                                    choices = sort(unique(sts_ombro$region))), 
                                                                        selectInput(inputId =  "selected_year_monthly", 
                                                                                    label = "Time Series Year:", 
                                                                                    choices = 2000:2022,
                                                                                    selected = 2022),
                                                                        sliderInput("slider_monthly", "Boxplot Range:",
                                                                                    min = 2000, 
                                                                                    max = as.numeric(format(Sys.Date(), "%Y")), 
                                                                                    value = c(2000, as.numeric(format(Sys.Date(), "%Y"))),
                                                                                    sep = "")),
                                                                   box(title = textOutput("text_monthly_plot"), 
                                                                       withSpinner(plotlyOutput("monthlyplot")), width=11))),
                                                  tabItem(tabName = "weekly", 
                                                          fluidRow(box( title = "Weekly Rain",
                                                                        collapsible = TRUE,
                                                                        selectInput(inputId = "selected_region_weekly",
                                                                                    label = "Select Region",
                                                                                    choices = sort(unique(sts_ombro$region))),
                                                                        selectInput(inputId =  "selected_year_weekly", 
                                                                                    label = "Time Series Year:", 
                                                                                    choices = 2000:2022,
                                                                                    selected = 2022),
                                                                        sliderInput("slider_weekly", "Boxplot Range:",
                                                                                    min = 2000, 
                                                                                    max = as.numeric(format(Sys.Date(), "%Y")), 
                                                                                    value = c(2000, as.numeric(format(Sys.Date(), "%Y"))),
                                                                                    sep = "")),
                                                                   box(title = textOutput("text_weekly_plot"), 
                                                                       withSpinner(plotlyOutput("weeklyplot")), width = 11))),
                                                  tabItem(tabName = "daily", 
                                                          fluidRow(box(collapsible = TRUE,
                                                                       title = "Daily Rain",
                                                                       selectInput(inputId = "selected_id_daily",
                                                                                   label = "Select id",
                                                                                   choices = sort(unique(sts_bmkg$id)))),
                                                                   box(withSpinner(dygraphOutput("dailyplot")), width = 11))),
                                                  tabItem(tabName = "wd", 
                                                          fluidRow(box(collapsible = TRUE,
                                                                       title = "Water Deficit",
                                                                       selectInput(inputId = "selected_id_wd",
                                                                                   label = "Select ID",
                                                                                   choices = sort(unique(sts_ombro_aws$id)))),
                                                                   box(withSpinner(plotlyOutput("wdplot")), width = 11))),
                                                  tabItem(tabName = "rain_ex", 
                                                          fluidRow(box(collapsible = TRUE,
                                                                       title = "Extreme Rainfall Indices",
                                                                       selectInput(inputId = "selected_id_rain_ex",
                                                                                   label = "Select ID",
                                                                                   choices = sort(unique(sts_ombro_aws$id)))),
                                                                   box(withSpinner(plotlyOutput("rexplot")), width = 11))),
                                                  tabItem(tabName = "aws_data", 
                                                          fluidRow(div(collapsible = TRUE,
                                                                       id = "aws",
                                                                       title = "Historical AWS Data",
                                                                       box(title = "Historical Data"),
                                                                       tabBox(id = "aws",
                                                                              tabPanel("Distribution",
                                                                                       leafletOutput("map", width = "100%", height = 550)),
                                                                              tabPanel("Timeseries",
                                                                                       withSpinner(htmlOutput("aws_plot")), width = 12),
                                                                              width = 12,
                                                                              selected = "Distribution"),
                                                                       h1 = "Blue Circle : BMKG's Weather Stations"))),
                                                  tabItem(tabName = "telemetry",
                                                          fluidRow(box(title = "Real-time Measurement",
                                                                       selectizeInput(inputId = "telemetry_selected",label = "Select Station", choices = iframe_weatherlink$name)),
                                                                   box(htmlOutput("iframe_weatherlink"), width=12))),
                                                  tabItem(tabName = "climate_Driver", 
                                                          fluidRow(div(collapsible = TRUE,
                                                                       id = "driver",
                                                                       title = "Climate Driver Update",
                                                                       tabBox(tabPanel("ENSO",
                                                                                       includeMarkdown('./user_guide/enso.rmd'),
                                                                                       imageOutput("image_enso", width = "auto"), width=12),
                                                                              tabPanel("IOD",
                                                                                       includeMarkdown('./user_guide/iod.rmd'),
                                                                                       imageOutput("image_iod", width = "auto"), width=12),
                                                                              tabPanel("SOI",
                                                                                       includeMarkdown('./user_guide/soi.rmd'),
                                                                                       imageOutput("image_soi", width = "auto"), width=12),
                                                                              tabPanel("MJO",
                                                                                       includeMarkdown('./user_guide/mjo.rmd'),
                                                                                       imageOutput("image_mjo", width = "auto"), width=12),
                                                                       ), 
                                                                       width = 12,
                                                                       selected = "ENSO")
                                                          )),
                                                  tabItem(tabName = "shortterm_forecast",
                                                          fluidRow(box(iframe(width = "100%", height = "700px",
                                                                              url_link = "https://embed.windy.com/embed2.html?lat=-1.801&lon=121.113&detailLat=-6.173&detailLon=106.827&width=
                                            1400&height=450&zoom=5&level=surface&overlay=rain&product=ecmwf&menu=&message=&marker=true&calendar=now&pressure=
                                            &type=map&location=coordinates&detail=&metricWind=km%2Fh&metricTemp=%C2%B0C&radarRange=-1"),
                                            h5(strong("Forecast Model : ECWMF (The European Centre for Medium-Range Weather Forecasts)")), width=12)))
                                            #,tabItem(tabName = "about", includeMarkdown('./user_guide/stations.rmd'))
                                            ,tabItem(tabName ="gfdr",
                                                     iframe(width = "100%", height = "700px",
                                                            url_link = "https://worldview.earthdata.nasa.gov/?v=93.90862303172369,-13.5196545272304,141.11364485142798,
                                                           9.763239047384161&l=VIIRS_NOAA20_Thermal_Anomalies_375m_All,Coastlines_15m,Reference_Labels_15m,
                                                           Reference_Features_15m,MODIS_Terra_Thermal_Anomalies_Day(hidden),Land_Water_Map&lg=false&t=2022-03-28-T23%3A39%3A59Z"))
                                            ,tabItem(tabName = "secondary_climate_data", includeMarkdown('./user_guide/climate_data_sources.rmd'))
                                            ,tabItem(tabName = "about", includeMarkdown('./user_guide/aws_profile.rmd'))
                                         ))
                    
)

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  ## Image of climate driver ----------------------
  output$image_enso <- renderImage({
    filename <- normalizePath(file.path(paste0('www/', 'enso', '.png')))
    list(
      src = filename, 
      height = 200
    )
  }, deleteFile = FALSE)
  output$image_iod <- renderImage({
    filename <- normalizePath(file.path(paste0('www/', 'iod', '.png')))
    list(
      src = filename, 
      height = 200
    )
  }, deleteFile = FALSE)
  output$image_soi <- renderImage({
    filename <- normalizePath(file.path(paste0('www/', 'soi', '.png')))
    list(
      src = filename, 
      height = 200
    )
  }, deleteFile = FALSE)
  output$image_mjo <- renderImage({
    filename <- normalizePath(file.path(paste0('www/', 'mjo', '.png')))
    list(
      src = filename, 
      height = 200
    )
  }, deleteFile = FALSE)
  
  ## Maps -----------------------------------------------------------------
  output$map <- renderLeaflet({
    
    pal <- colorFactor(palette = 'Set1', domain = sts_clim1005_map$`AWS Status`)
    pal2 <- colorFactor(palette = 'Dark2', domain = sts_clim1005_map$Manufacturer)
    
    leaflet(sts_clim1005_map, options = leafletOptions(zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'bottomleft' }).addTo(this)}") %>% 
      addTiles(group = "Open Street Map") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>% 
      addScaleBar(position = "bottomleft") %>%
      #addCircleMarkers(lng = ~lon, lat = ~lat, radius=4, label=~id,  layerId = ~id, color=~pal(`AWS Status`), group = "AWS Status")%>%
      addCircleMarkers(lng = sts_bmkg$lon, lat = sts_bmkg$lat, radius=1, label=sts_bmkg$id,  layerId = sts_bmkg$id, group = sts_bmkg$id) %>% 
      #addCircleMarkers(lng = ~lon, lat = ~lat, radius=4, label=~id,  color=~pal2(`Manufacturer`), group = "Manufacturer") %>%
      #leaflet::addLegend(pal = pal, values = ~`AWS Status`,position = "topright",  group = "AWS Status") %>% 
      leaflet::addLegend(pal = pal2, values = ~Manufacturer,position = "topright",  group = "Manufacturer") %>% 
      addLayersControl(#overlayGroups = c("AWS Status", "Manufacturer"), 
                       baseGroups = c("Open StreetMap", "Esri World Imagery"),options = layersControlOptions(collapsed = TRUE), position = "bottomright") %>% 
      addMeasure(primaryLengthUnit = "meters",
                 primaryAreaUnit = "sqmeters",
                 completedColor = "#ff0000",
                 activeColor = "#ad3b3b", position = "bottomleft")
  })

  ## Data from SQL that has been filtered  using select, slider, etc from UI --------------------------------------
  filtered_df_monthly.react <-  reactive({ 
    
    regional <- input$selected_region_monthly
    
    filtered_df_monthly <- filtered_df_monthly %>% 
      dplyr::filter(region==regional) %>% 
      dplyr::collect() %>% 
      as.data.frame() %>% 
      mutate(date=ymd(date)) %>% 
      mutate(month=lubridate::month(date, label = TRUE)) %>% 
      mutate(month=as.factor(month))

  })
  filtered_df_weekly.react <-  reactive({ 
    
    regional <- input$selected_region_weekly
    
    filtered_df_weekly <- filtered_df_weekly %>%
      dplyr::filter(region==regional) %>% 
      dplyr::collect() %>% 
      as.data.frame() %>% 
      mutate(date=ymd(date))
    
    
  })
  df_ombro_aws.react <-  reactive({ 
    
    selected_id <- input$selected_id_daily
    
    df_ombro_aws <-df_ombro_aws%>%
      dplyr::filter(id==selected_id) %>% 
      #filter(date >= input$daterange_ombro[1] & date <= input$daterange_ombro[2]) %>% 
      dplyr::collect() %>% 
      as.data.frame() %>% 
      mutate(date=ymd(date))
    
  })
  df_ombro_wd.react <- reactive({
    
    selected_id <- input$selected_id_wd
    
    df_ombro_wd <- df_ombro_aws %>%
      dplyr::filter(id==selected_id) %>% 
      #filter(date >= input$daterange_ombro[1] & date <= input$daterange_ombro[2]) %>% 
      dplyr::collect() %>% 
      as.data.frame() %>% 
      mutate(date=ymd(date))
  })
  df_ombro_rex.react <- reactive({
    
    selected_id <- input$selected_id_rain_ex
    
    df_ombro_rex <- df_ombro_aws %>%
      dplyr::filter(id==selected_id) %>% 
      #filter(date >= input$daterange_ombro[1] & date <= input$daterange_ombro[2]) %>% 
      dplyr::collect() %>% 
      as.data.frame() %>% 
      mutate(date=ymd(date))
  })
  df_aws.react <- reactive({
    
    id_map <- input$region_id_map_selected
    
    df_aws <-df_aws %>%
      filter(id==id_map) %>% 
      dplyr::collect() %>% 
      #as.data.frame() %>% 
      select(-contains("wind"), -contains("row_names")) %>% 
      mutate(date=ymd(date)) 
    
    
  })
  
  ## Observe - df_ombro_aws 
 
  ## observe input - update timeseries input year of monthly/ weekly plot based on filtered region ----------------------------  
  
  observe({    
    
    start_year <- input$slider_monthly[1]
    end_year <- input$slider_monthly[2]
    
    filtered_df_monthly <-filtered_df_monthly.react() %>% 
      filter_by_time(date, .start_date = paste0(start_year,"-01-01"), .end_date = paste0(end_year,"-12-31"))
    
    min_year <- as.numeric(min(filtered_df_monthly$year))
    max_year <- as.numeric(max(filtered_df_monthly$year))
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd
    
    updateSelectInput(session, "selected_year_monthly", choices = min_year:max_year,
                      selected = max_year)
    
  }) # Observe input monthly
  observe({    
    
    start_year <- input$slider_weekly[1]
    end_year <- input$slider_weekly[2]
    regional <- input$selected_region_weekly
    
    filtered_df_weekly <- filtered_df_weekly.react() %>% 
      filter_by_time(date, .start_date = paste0(start_year,"-01-01"), .end_date = paste0(end_year,"-12-31"))
    
    min_year <- as.numeric(min(filtered_df_weekly$year))
    max_year <- as.numeric(max(filtered_df_weekly$year))
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd
    
    updateSelectInput(session, "selected_year_weekly", choices = min_year:max_year,
                      selected = max_year)
    
  }) # Observe input weekly
  
  ## observe input - update boxplot range year of monthly/ weekly plot based on filtered region ----------------------------------------------------------------------
  observe({    
    
    filtered_df_monthly <- filtered_df_monthly.react()
    
    min_year <- as.numeric(min(filtered_df_monthly$year))
    max_year <- as.numeric(max(filtered_df_monthly$year))
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "slider_monthly", 
                      min = min_year, 
                      max = max_year, 
                      value = c(min_year,max_year))
  })
  observe({    
    filtered_df_weekly <- filtered_df_weekly.react()
    
    min_year <- as.numeric(min(filtered_df_weekly$year))
    max_year <- as.numeric(max(filtered_df_weekly$year))
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "slider_weekly", 
                      min = min_year, 
                      max = max_year, 
                      value = c(min_year,max_year))
  })
  ## text Output for monthly and weekly plot --------------------------------------------------------
  output$text_monthly_plot <- renderText({
    
    start_year <- input$slider_monthly[1]
    end_year <- input$slider_monthly[2]
    regional <- input$selected_region_monthly
    
    filtered_df_monthly <- filtered_df_monthly.react()  %>% 
      filter_by_time(date, .start_date = paste0(start_year,"-01-01"), .end_date = paste0(end_year,"-12-31"))
    
    start_year <- as.character(min(filtered_df_monthly$year)) 
    end_year <- as.character(max(filtered_df_monthly$year)) 
    
    paste0(regional, " - Boxplot (", start_year, "-" , end_year, ") - Timeseries (",input$selected_year_monthly, ")" )
    
  })
  output$text_weekly_plot <- renderText({
    
    start_year <- input$slider_weekly[1]
    end_year <- input$slider_weekly[2]
    regional <- input$selected_region_weekly
    
    filtered_df_weekly <- filtered_df_weekly.react() %>% 
      filter_by_time(date, .start_date = paste0(start_year,"-01-01"), .end_date = paste0(end_year,"-12-31"))
    
    start_year <- as.character(min(filtered_df_weekly$year)) 
    end_year <- as.character(max(filtered_df_weekly$year)) 
    
    paste0(regional, " - Boxplot (", start_year, "-" , end_year, ") - Timeseries (", input$selected_year_weekly, ")" )
    
  })
  
  ## plot - Monthly Rain  ----------------------------------------------------------------
  output$monthlyplot <- renderPlotly({ 
    
    plot.monthly.fun <- function(start_year, end_year, regional) {
      
      filtered_df_monthly <- filtered_df_monthly.react()  %>% 
        filter_by_time(date, .start_date = paste0(start_year,"-01-01"), .end_date = paste0(end_year,"-12-31"))
      
      #   ts <- as.numeric(end_year)
      
      ggplotly(ggplot(filtered_df_monthly, aes(x = month, y = rain_mm.sum, group = month)) + 
                 geom_boxplot(fill="lightblue", colour="Gray", outlier.size = .5,  alpha = 0.9) +
                 geom_point(data = subset(filtered_df_monthly, year %in% c(input$selected_year_monthly)), aes(color=estate)) +
                 geom_line(data = subset(filtered_df_monthly, year %in% c(input$selected_year_monthly)), aes(color=estate, group =estate)) +
                 #           scale_color_brewer(palette = "Set1", direction = 1) +
                 labs( x= element_blank(),
                       y = "Rain (mm/month)",
                       subtitle = "Data : Subtitle",
                       caption = "Data : Caption"
                 ) +
                 theme_bw()) %>% 
        layout(legend = list(orientation = 'h'))
      
      
    }
    
    start_year <- as.character(input$slider_monthly[1])
    end_year <- as.character(input$slider_monthly[2])
    regional <- as.character(input$selected_region_monthly)
    
    plot.monthly.fun(start_year, end_year, regional)
    
  })
  ## plot - weekly Rain  -----------------------------------------------------------------
  output$weeklyplot <- renderPlotly({
    
    plot.weekly.fun <- function(start_year, end_year, regional) {
      
      
      filtered_df_weekly <- filtered_df_weekly.react() %>% 
        filter_by_time(date, .start_date = paste0(start_year,"-01-01"), .end_date = paste0(end_year,"-12-31"))
      
      #ts <- max(filtered_df_weekly$year) 
      
      ggplotly(ggplot(filtered_df_weekly, aes(x = week, y = rain_mm.sum, group = week)) + 
                 geom_boxplot(fill="lightblue", colour="Gray", outlier.size = .1,  alpha = 0.4) +
                 geom_point(data = subset(filtered_df_weekly, year %in% c(input$selected_year_weekly)), aes(color=estate)) +
                 geom_line(data = subset(filtered_df_weekly, year %in% c(input$selected_year_weekly)), aes(color=estate, group =estate)) +
                 #       scale_color_brewer(palette = "Set1", direction = 1) +
                 labs(
                   x =element_blank(),
                   y = "Rain (mm/week)",
                   subtitle = "Data : Subtitle",
                   caption = "Data : Caption"
                 ) +
                 theme_bw()) %>% 
        layout(legend = list(orientation = 'h'),
               xaxis = list(dtick = 4, tick0 = 1, tickmode = "linear")) %>% 
        add_trace(hovertext = paste("Week :", filtered_df_weekly$week,
                                    "Month :", filtered_df_weekly$month))
      
      
      
    }
    
    start_year <- as.character(input$slider_weekly[1])
    end_year <- as.character(input$slider_weekly[2])
    regional <- as.character(input$selected_region_weekly)
    
    plot.weekly.fun(start_year, end_year, regional)
    
    
  })
  ## plot - Daily Rain  ------------------------------------------------------------------
  
  output$dailyplot <- renderDygraph({
    
    df <- df_ombro_aws.react() %>% 
      select(date, rain_mm.sum)
    
    max_date <- tail(df$date,n=1)
    min_date <- max_date-360 
    
    
    # Then you can create the xts necessary to use dygraph
    df.xts <- xts(x = df$rain_mm.sum, order.by = df$date)
    
    dygraph(df.xts) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = F, colors="#0e1af0") %>%
      dyAxis("y", label = "Rain (mm)")  %>% 
      dyRangeSelector(dateWindow = c(min_date, max_date)) %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
    
  })
  

  ## plot - WD (Water Deficit ) ------------------
  output$wdplot <- renderPlotly({
    
    
    
    df.ombro.wd <- df_ombro_wd.react() %>%
      agg.monthly.daily.ombro() %>%
      #   filter(date >= input$daterange_ombro[1] & date <= input$daterange_ombro[2]) %>% 
      select(date, wd_mm, drainage_mm) %>%
      mutate(wd_mm = wd_mm*-1) %>%
      reshape2::melt(id.var = c('date')) %>% 
      filter(variable!="id") %>% 
      mutate(value=as.numeric(value)) 
    
    df.ombro.wd.plot  <- ggplot(df.ombro.wd) +
      aes(x = date,  y = value, fill = variable, colour = variable, group = variable) +
      geom_area(size = 0.5) +
      scale_fill_manual(values = c(wd_mm = "#FF1100", drainage_mm = "#0024FF")) +
      scale_color_manual(values = c(wd_mm = "#FF1100",drainage_mm = "#0024FF")) +
      labs(y = "(mm)", x=element_blank()) +
      theme_bw() +
      theme(text = element_text(size = 12), axis.title.y = element_text(size = 12L, face = "bold"), 
            axis.title.x = element_text(size = 14L, face = "bold"))
    
    ggplotly(df.ombro.wd.plot) %>% 
      layout(legend = list(orientation = 'h'))
    
    
  })
  ## plot - REX (Rainfall Extreme) ----------------------
  output$rexplot <- renderPlotly({
    
    df <- df_ombro_rex.react() %>%
      #    filter(date >= input$daterange_ombro[1] & date <= input$daterange_ombro[2]) %>% 
      agg.monthly.daily() %>% 
      mutate(rain_mm= as.numeric(rain_mm.sum)) %>% 
      mutate(raindays = as.numeric(raindays))
    
    # create plot
    # axis properties
    minyaxis1 <- floor(min(0.0,min(df$rain_mm.sum)))
    maxyaxis1 <- ceiling(max(800.0,max(df$rain_mm.sum)))
    minyaxis2 <- 0.0
    maxyaxis2 <- 40
    
    dateRange <- c(min(df$date), max(df$date))
    plotColors <- c("blue", 'rgb(205, 12, 24)')
    pAxisSpacer <- 0.07
    
    plot <- plot_ly(df, x = ~date) %>%
      add_lines(y = ~rain_mm.sum, 
                type="scatter",
                mode="lines",
                name='Rain (mm)',
                fill = 'tozeroy',
                line = list(color = plotColors[1], width = 1.5)
      ) %>%
      add_lines(y = ~raindays, 
                type="scatter",
                mode="lines",
                name='Raindays (Days)',
                yaxis='y2', 
                line = list(color = plotColors[2], width = 1.5, dash = 'dot')
      ) %>%
      add_lines(y = ~r90, 
                type="scatter",
                mode="lines",
                name='R90 (Days)',
                yaxis='y2', 
                line = list(color = 'red', width = 1, dash = 'lines')
      ) %>%
      add_lines(y = ~r10, 
                type="scatter",
                mode="lines",
                name='R10 (Days)',
                yaxis='y2', 
                line = list(color = plotColors[2], width = 2, dash = 'lines+markers')
      ) %>% 
      layout(
        xaxis = list(title = "Date & Time", 
                     domain = c(pAxisSpacer*3, 1), 
                     type = "date",
                     range = dateRange, 
                     ticks='outside', 
                     zeroline=TRUE, 
                     showline = T),
        yaxis = list(title = 'mm', 
                     side = "left", 
                     color = plotColors[1], 
                     range = c(minyaxis1,maxyaxis1), 
                     ticks='outside', 
                     dtick = 100, 
                     tick0 = minyaxis1, 
                     tickmode = "linear",
                     position = 0,
                     anchor = 'free', 
                     zeroline = F, 
                     showline = T),
        yaxis2 = list(title = 'days', 
                      side = "right",
                      color = plotColors[2],
                      range = c(minyaxis2,maxyaxis2), 
                      ticks='outside', 
                      dtick = 5, 
                      tick0 = minyaxis2, 
                      tickmode = "linear", 
                      position = pAxisSpacer, 
                      overlaying = "y",
                      anchor = 'free',
                      zeroline=F, 
                      showline = T),
        legend = list(x=pAxisSpacer*3.5, y= 1), 
        showlegend = T,
        title = list(text = " ")
      )
    return(plot)
    
  }) # Plot for rainfall extreme
  ## plot - AWS Daily  --------------------------
  output$awsplot <- renderPlotly({
    
    df_aws.react()  %>% 
      plot.facet()
    
  })
  ## Iframe weatherlink  
  output$iframe_weatherlink <- renderUI({
    
    df <- iframe_weatherlink %>% 
      filter(name==input$telemetry_selected)
    
    iframe(width = "100%", 
           height = "700px",
           url_link = df$link)
    
  })
  ## SQL Connection -  end session ------------------------------------------------------
  

}
# Shiny app ---------------------------------
shinyApp(ui, server)