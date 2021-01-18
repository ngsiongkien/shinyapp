#Load package required.
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plyr)
library(leaflet)
library(ggplot2)
library(plotly)

#Data cleaning
one_star <- read.csv("one-star-michelin-restaurant.csv")
one_star <- one_star %>%
    mutate("Star" = "*") %>%
    select("Name", "Year", "Latitude", "Longitude", "City", "Country", "Cuisine", "Price", "Star", "URL")

two_stars <- read.csv("two-stars-michelin-restaurant.csv")
two_stars <- two_stars %>%
    mutate("Star" = "**") %>%
    select("Name", "Year", "Latitude", "Longitude", "City", "Country", "Cuisine", "Price", "Star", "URL")

three_stars <- read.csv("three-stars-michelin-restaurant.csv")
three_stars <- three_stars %>%
    mutate("Star" = "***") %>%
    select("Name", "Year", "Latitude", "Longitude", "City", "Country", "Cuisine", "Price", "Star", "URL")

restaurant <- rbind(one_star, two_stars)
restaurant <- rbind(restaurant, three_stars)

restaurant[c("Cuisine","Country","City")] <- map(restaurant[c("Cuisine","Country","City")], as.factor)

ui <- dashboardPage(
    title = "Michelin Restaurant Recommender",
    skin = "blue", 
    dashboardHeader(title = "Michelin Restaurant"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Restaurant Overview", tabName = "overview", icon = icon("eye")),
            menuItem("Restaurant Recommender", tabName = "table", icon = icon("utensils")),
            menuItem("Restaurant Location", tabName = "map", icon = icon("map-marker-alt")),
            menuItem("Restaurant Analysis", tabName = "analysis", icon = icon("chart-bar")))),
    dashboardBody(
        tabItems(
            
            #For Home tab
            tabItem(tabName = "home", 
                    fluidPage(
                        titlePanel(h1(strong("Michelin Restaurant Recommender"), align = "center")),
                        helpText(h3(strong("Satisfy All your Cravings !"), align = "center"))),
                    fluidRow(
                        img(src = "michelin.png"))),

            #For Restaurant Overview tab
            tabItem(tabName = "overview", 
                    fluidPage(
                        titlePanel(h1(strong("Michelin Restaurant Overview Dashboard"), align = "center")),
                        helpText(h3(strong("An Overview of Michelin Restaurant available !"), align = "center"))),
                    fluidRow(
                        box(
                            title = "Overview",
                            status = "primary",
                            width = 12,
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            fluidRow(
                                box(
                                    status = "primary",
                                    width = 12,
                                    solidHeader = FALSE,
                                    collapsible = TRUE,
                                    valueBoxOutput("total_restaurant", width = 3),
                                    valueBoxOutput("total_cuisine", width = 3),
                                    valueBoxOutput("total_country", width = 3),
                                    valueBoxOutput("total_city", width = 3)),
                                box(
                                    title = "Top 10 Cuisine",
                                    status = "primary",
                                    width = 4,
                                    solidHeader = FALSE,
                                    collapsible = TRUE,
                                    dataTableOutput("cuisinelist")),
                                box(
                                    title = "Top 10 Country",
                                    status = "primary",
                                    width = 4,
                                    solidHeader = FALSE,
                                    collapsible = TRUE,
                                    dataTableOutput("countrylist")),
                                box(
                                    title = "Top 10 City",
                                    status = "primary",
                                    width = 4,
                                    solidHeader = FALSE,
                                    collapsible = TRUE,
                                    dataTableOutput("citylist")))))),

            #For Restaurant Recommender tab
            tabItem(tabName = "table", 
                    fluidPage(
                        titlePanel(h1(strong("Michelin Restaurant Recommender"), align = "center")),
                        helpText(h3(strong("Select your Michelin Restaurant !"), align ="center"))),
                    fluidRow(
                        column(width = 4, selectInput("Cuisine", "Cuisine: ", c("All",as.character(restaurant$Cuisine)))),
                        column(width = 4, selectInput("Price", "Price: ", c("All",as.character(restaurant$Price)))),
                        column(width = 4, selectInput("Star", "Star: ", c("All",as.character(restaurant$Star)))),
                        column(width = 4, selectInput("Country", "Country: ", c("All",as.character(restaurant$Country)))),
                        column(width = 4, selectInput("City", "City: ", c("All",as.character(restaurant$City)))),
                        DT::dataTableOutput("table"))),

            #For Restaurant LOcation tab
            tabItem(tabName = "map", 
                    fluidPage(
                        titlePanel(h1(strong("Michelin Restaurant Location"), align = "center")),
                        helpText(h3(strong("Locate your Michelin Restaurant !"), align ="center"))),
                    fluidRow(
                        column(width = 5,numericInput("long", label = h5(strong("Restaurant Longitude:")),value = 0)),
                        column(width = 5,numericInput("lat", label = h5(strong("Restaurant Latitude:")),value = 0)),
                        actionButton("recalc", "Show point"),
                        leafletOutput("mymap"))),

            #For Restaurant Analysis tab
            tabItem(tabName = "analysis", 
                    fluidPage(
                        titlePanel(h1(strong("Michelin Restaurant Analysis"), align = "center")),
                        helpText(h3(strong("An Analysis of Michelin Restaurant available !"), align = "center")),
                        ),
                    fluidRow(
                        column(width = 5, selectInput("features","Features:",choices = c("Cuisine","Price","Star"))),
                        box(plotlyOutput("barplot"))))
        )
    )
)

server <- function(input, output, session) {
    
    #Output of Restaurant Overview tab
    output$total_restaurant <- renderValueBox({
        valueBox(
            value = length(restaurant$Name),
            subtitle = "Total Restaurant",
            icon = icon("utensils"),
            color = "maroon"
        )
    })
    
    output$total_cuisine <- renderValueBox({
        valueBox(
            value = nrow(count(restaurant$Cuisine)),
            subtitle = "Total Cuisine",
            icon = icon("pizza-slice"),
            color = "orange"
        )
    })
    
    output$total_country <- renderValueBox({
        valueBox(
            value = nrow(count(restaurant$Country)),
            subtitle = "Total Country",
            icon = icon("map-marker-alt"),
            color = "green"
        )
    })
    
    output$total_city <- renderValueBox({
        valueBox(
            value = nrow(count(restaurant$City)),
            subtitle = "Total City",
            icon = icon("flag"),
            color = "lime"
        )
    })
    
    output$cuisinelist <- renderDataTable({
        cuilist <- restaurant %>%
            dplyr::group_by(Cuisine) %>%
            dplyr::summarise(n=n()) %>% 
            dplyr::arrange(desc(n))
        colnames(cuilist) <- c("Cuisine", "Number of Restaurant")
        
        dat1 <- cuilist[1:10,]
        return(dat1)
    })
    
    output$countrylist <- renderDataTable({
        counlist <- restaurant %>%
            dplyr::group_by(Country) %>%
            dplyr::summarise(n=n()) %>% 
            dplyr::arrange(desc(n))
        colnames(counlist) <- c("Country", "Number of Restaurant")
        
        dat2 <- counlist[1:10,]
        return(dat2)
    })
    
    output$citylist <- renderDataTable({
        citylist <- restaurant %>%
            dplyr::group_by(City) %>%
            dplyr::summarise(n=n()) %>% 
            dplyr::arrange(desc(n))
        colnames(citylist) <- c("City", "Number of Restaurant")
        
        dat3 <- citylist[1:10,]
        return(dat3)
    })
    
    #Output of Restaurant Recommender tab
    output$table <- DT::renderDataTable(DT::datatable({
        data <- restaurant
        if(input$Cuisine != "All"){
            data <- data[data$Cuisine == input$Cuisine,]
        }
        if (input$Price != "All") {
            data <- data[data$Price == input$Price,]
        }
        if (input$Star != "All") {
            data <- data[data$Star == input$Star,]
        }
        if (input$Country != "All") {
            data <- data[data$Country == input$Country,]
        }
        if (input$City != "All") {
            data <- data[data$City == input$City,]
        }
        data
    })
    )
    
    #Output of Restaurant Location tab
    points <- eventReactive(input$recalc, {
        cbind(input$long, input$lat)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            setView(lat = 0, lng = 0, zoom = 2) %>%
            addProviderTiles("Stamen.TonerLite",
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = points())
    })
    
    
    #Output of Restaurant Analysis tab
    barplottest <- reactive({
        if( "Star" %in% input$features) return(bar1)
        if( "Price" %in% input$features) return(bar2)
        if( "Cuisine" %in% input$features) return(bar3) 
    })
    
    starcount <- count(restaurant$Star)
    bar1 <- ggplot(starcount,aes(x = x, y = freq, fill = x)) +
        geom_bar(stat = "identity") +
        ggtitle("Michelin Star Distribution Analysis") +
        xlab("Star Level") +
        ylab("No. of Restaurant") +
        coord_flip()
    
    pricecount <- count(restaurant$Price)
    bar2 <- ggplot(pricecount,aes(x = x, y = freq, fill = x))+
        geom_bar(stat = "identity") +
        ggtitle("Price Distribution Analysis") +
        xlab("Price Level") +
        ylab("No. of Restaurant") +
        coord_flip()
    
    cuisinecount <- count(restaurant$Cuisine)
    bar3 <- ggplot(cuisinecount,aes(x = x, y = freq, fill = x))+
        geom_bar(stat = "identity") +
        ggtitle("Cuisine Distribution Analysis") +
        xlab("Cuisine Type") +
        ylab("No. of Restaurant") +
        coord_flip()
    
    output$barplot <- renderPlotly({
        dataplots = barplottest()
        print(dataplots)
    })
}

shinyApp(ui = ui, server = server)


