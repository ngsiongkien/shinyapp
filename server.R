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

