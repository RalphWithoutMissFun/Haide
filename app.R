#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(rvest)
library(plyr)
library(shinyAce)
library(shiny)
library(ggplot2)
library(ggthemr)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(shinyjs)
library(weatherr)
library(RMySQL)
Logged = FALSE
my_username <- "test"
my_password <- "test"

fetchData <- function(city, date){
  
  baseUrl <- 'http://lishi.tianqi.com/'
  Url <- paste(baseUrl, city, '/', date, '.html', sep = '')
  
  content <- Url %>%
    read_html(encoding='GBK') %>%
    html_nodes('div.tqtongji2') %>%
    html_nodes("ul") %>%
    html_text() %>%
    strsplit("\\s{4,}")
  
  content <- ldply(content[-1])
  names(content) <- c('date', 'highDegree', 'lowDegree', 'weather', 'windDirection', 'windForce')
  return(content) 
}
fetchData2 <- function(city, date){
  date <- paste(substring(date,first=0, last=4) , substring(date,first = 6,last = 7), sep ="")
  baseUrl <- 'http://lishi.tianqi.com/'
  Url <- paste(baseUrl, city, '/', date, '.html', sep = '')
  
  content <- Url %>%
    read_html(encoding='GBK') %>%
    html_nodes('div.tqtongji2') %>%
    html_nodes("ul") %>%
    html_text() %>%
    strsplit("\\s{4,}")
  
  content <- ldply(content[-1])
  names(content) <- c('date', 'highDegree', 'lowDegree', 'weather', 'windDirection', 'windForce')
  return(content) 
}
fetchPredict <- function(mode)
{
  lng <- c(113.3244465713, 115.89, 116.46)
  lat <- c(23.1064679044, 28.68, 39.92)
  mode <- as.numeric(mode)
  print(mode)
  print(lng[mode])
  data <- locationforecast(lat[mode], lng[mode])
  return(data)
}



ui2 <- dashboardPage(
  
  dashboardHeader(title="Haide"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("graphics", tabName = "graphics", icon = icon("dashboard")),
      menuItem("table", icon = icon("th"), tabName = "table"),
      menuItem("map", tabName = "map", icon = icon("map-marker",lib = "glyphicon")),
      menuItem("predict", icon = icon("flag",lib = "glyphicon"), tabName = "predict"),
      menuItem("feedback", tabName = "feedback", icon = icon("pencil",lib = "glyphicon")),
      menuItem("info", icon = icon("header",lib = "glyphicon"), tabName = "info")
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "graphics",
                sidebarLayout(
                  position = "right",
                  sidebarPanel(
                    h4("select what you want here"),
                    selectInput("select", h3("Select city"), choices = 
                                  list("广州" = 'guangzhou', "南昌" = 'nanchang',
                                       "北京" = 'beijing'), selected = 2),
                    dateInput("date", h3("Date input"), value = "2018-03-03"),
                    selectInput("select_theme", h3("Select theme"), choices = list("dust" = "dust", "earth" = "earth","light" = "light", "pale" ="pale" , "fresh" = "fresh","chalk"="chalk","light"="light","sky"="sky","grass"="grass","grape"="grape"), selected = "fresh")
                    ,bookmarkButton(id = "bookmark")
                    
                  ),
                  mainPanel(
                    plotOutput("plot_point")%>% withSpinner(type=4),
                    plotOutput("plot_line")%>% withSpinner(type=4),
                    plotOutput("plot_bar")%>% withSpinner(type=4)
                  )
                )
        ),
        
        tabItem(tabName = "table",
                sidebarLayout(
                  position = "right",
                  sidebarPanel(
                    h4("select what you want here"),
                    selectInput("select2", h3("Select city"), choices = 
                                  list("广州" = 'guangzhou', "南昌" = 'nanchang',
                                       "北京" = 'beijing'), selected = 2),
                    dateInput("date2", h3("Date input"), value = "2018-03-03")
                    ,downloadButton("downloadData", "Download"),
                    
                    checkboxGroupInput("show_vars", "Columns in tables to show:",
                                       c("date"="date","highDegree"="highDegree","lowDegree"="lowDegree", "weather"="weather","windDirection"="windDirection"), selected = c("date"="date","highDegree"="highDegree","lowDegree"="lowDegree", "weather"="weather","windDirection"="windDirection")
                                       
                    )),
                  mainPanel(
                    DT::dataTableOutput('table')%>% withSpinner(type=4)
                  )
                )
                
        ),
        tabItem(tabName = "map",
                sidebarPanel(
                  h5("basic geo information"),
                  textOutput("geo")
                  ,h5("temparature now")
                  ,textOutput("wea")
                ),
                mainPanel(
                  leafletOutput("map")%>% withSpinner(type=4)
                )
        ),
        
        tabItem(tabName = "predict",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("select4", h3("Select city"), choices = 
                                  list("广州" = 1, "南昌" = 2,
                                       "北京" = 3), selected = 2),
                    checkboxGroupInput("show_vars2", "Columns in tables to show:",
                                       c("time"="time","temperature"="temperature","humidity"="humidity"),selected = c("time"="time","temperature"="temperature","humidity"="humidity"))
                    
                  ),
                  mainPanel(
                    DT::dataTableOutput('table4')%>% withSpinner(type=4)
                  )
                )
        ),
        tabItem(tabName = "feedback",
                sidebarLayout(
                  sidebarPanel(
                    h5("Thanks for giving us valuable feedback"),
                    actionButton("reset", "Reset text"),
                    actionButton("submit", "submit")
                  ),
                  
                  mainPanel(
                    aceEditor("ace",fontSize = 20,autoComplete = "enabled",theme="ambiance")
                  )
                )
        ),
        
        tabItem(tabName = "info",
                sidebarLayout(
                  sidebarPanel("information",
                               h5("maintainer: Ralph Haide"),
                               h5("version: 1.3.0"),
                               h6("contact me:"),
                               a("ralphshaides@gmail.com")
                               
                  ),
                  mainPanel(
                    
                    img(src="https://s1.ax1x.com/2018/04/15/CeieW6.png",width=200,height=200),
                    br(),
                    h2("Haide",aign="center"),
                    h3("a simple weather application powered by shiny",align="left"),
                    h5("technology used: ",",",a("R",href="https://www.r-project.org/"),",", a("shiny",href="http://shiny.rstudio.com/") ,",", "Crawler" ,",", a("ggplot2",href="http://ggplot2.org/")),
                    h5("library used:", a("shiny",href="http://shiny.rstudio.com/") , a("DT"), a("ggplot2",href="http://ggplot2.org/"), a("ggthemr",href="https://cran.r-project.org/web/packages/ggthemes/index.html"), a("plyr",href="https://cran.r-project.org/web/packages/plyr/index.html"), a("rvest",href="https://cran.r-project.org/web/packages/rvest/index.html"),a("weatherR",href="https://cran.r-project.org/web/packages/weatherr/index.html"))
                    ,includeMarkdown("log.md")
                  )                    
                )
        )
      ))
  )
)
server <- function(input, output,session) {
  
  output$selected_var <- renderText({ 
    paste("city selected: " , input$select, " ")
  })
  output$plot_point <- renderPlot({
    ggthemr(input$select_theme)
    name <- as.character(input$date)
    city <- input$select
    name <- paste(substring(name,first=0, last=4) , substring(name,first = 6,last = 7), sep ="")
    weatherData <- fetchData(city,name)
    len <- length(weatherData$date)
    ggplot(weatherData, aes(x=lowDegree, y=highDegree)) + geom_point()
  })
  output$plot_bar <- renderPlot({
    ggthemr(input$select_theme)
    name <- as.character(input$date)
    city <- input$select
    name <- paste(substring(name,first=0, last=4) , substring(name,first = 6,last = 7), sep ="")
    weatherData <- fetchData(city,name)
    len <- length(weatherData$date)
    day <- rep(seq(1, len),2)
    day <- day + 1
    degrees <- as.numeric(c(weatherData$lowDegree, weatherData$highDegre))
    tags <- rep(c("low","high"),each=len)
    data <- data.frame(day, degrees,tags)
    option = input$select_shape
    option = as.numeric(option)
    ggplot(data, aes(x = day, y = degrees , fill=tags)) +geom_bar(stat= 'identity', width = 0.5, position =  position_dodge(0.7))
  })
  output$plot_line <- renderPlot({
    ggthemr(input$select_theme)
    name <- as.character(input$date)
    city <- input$select
    name <- paste(substring(name,first=0, last=4) , substring(name,first = 6,last = 7), sep ="")
    weatherData <- fetchData(city,name)
    len <- length(weatherData$date)
    day <- rep(seq(1, len),2)
    day <- day + 1
    degrees <- as.numeric(c(weatherData$lowDegree, weatherData$highDegre))
    tags <- rep(c("low","high"),each=len)
    data <- data.frame(day, degrees,tags)
    option = input$select_shape
    option = as.numeric(option)
    ggplot(data, aes(x = day, y = degrees , color=tags)) + geom_point() + geom_line()
  })
  output$table <- DT::renderDataTable({
    fetchData2(input$select2,as.character(input$date2))[,input$show_vars, drop=FALSE]
  }
  )
  output$table4 <- DT::renderDataTable({
    fetchPredict(input$select4)[,input$show_vars2, drop=FALSE]
  }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data <- fetchData2(input$select2,as.character(input$date2))
      write.csv(data[,-1], con,row.names = TRUE)
    }
  )
  observeEvent(input$bookmark, {
    session$doBookmark()
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  
      addMarkers(lng=115.89, lat=28.68,
                 popup="南昌",label = "南昌") %>%
      addMarkers(lng=113.3244465713,lat=23.1064679044,popup="广州",label = "广州") %>%
      addMarkers(lng=116.46,lat=39.92,popup="北京",label = "北京") %>%
      addMarkers(lng=114.30, lat=30.60,popup="武汉",label = "武汉")%>%addMiniMap()%>%addMeasure()
    
    
  })
  observeEvent(input$map_marker_click,{
    print("observed map_marker_click")
    p <- input$map_marker_click
    print(p)
    str <- paste(p$lat," , ",p$lng)
    print(str)
    output$geo <- renderText({ 
      str
    })
    output$wea <- renderText({
      data <- locationforecast(p$lat, p$lng)
      as.character(data$temperature[1])
    })
  })
  observeEvent(input$map_click,{
    print("observed map_marker_click")
    p <- input$map_click
    print(p)
    str <- paste(p$lat," , ",p$lng)
    print(str)
    output$geo <- renderText({ 
      str
    })
    output$wea <- renderText({
      data <- locationforecast(p$lat, p$lng)
      as.character(data$temperature[1])
    })
  }
  )
  observeEvent(input$reset, {
    updateAceEditor(session, "ace", value = "")
    
  })
  observeEvent(input$submit,{
    str = as.character(input$ace)
    print(str)
  })
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }     
    }
  })
  output$dataInfo <- renderPrint({
    if (values$authenticated) "OK!!!!!"
    else "You are NOT authenticated"
  })
}

enableBookmarking(store = "url")
app <- shinyApp(ui = ui2, server = server)
