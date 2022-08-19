#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(wordcloud2)
library(tm)
library(tidytext)
library(stopwords)
library(leaflet)
library(ggpubr)
library(ggthemes)


# Dataset
listings <- read_csv("data/listings.csv")
listings$price = as.numeric(gsub("\\$", "", listings$price ))
col_values <- c("#FF9900","#000000","#4267B2","#DB4437","#7FBA00","#1DA1F2")
listings <-  listings %>% 
    mutate(
        income_monthly = round(price*(365-availability_365)/12),
        highly_available = (availability_365 >=60)
    )

listing_map <- listings %>% 
    select(id, longitude, latitude, room_type, price, 
           number_of_reviews, availability_365, latitude, longitude ,income_monthly,host_is_superhost,highly_available
           ) %>% 
    group_by(room_type) %>% 
    dplyr::summarise(
        nb_bnb = n(),
        avg_price = mean(price, na.rm = T),
        avg_reviews = mean(number_of_reviews,na.rm = T),
        avg_monthly_income = mean(income_monthly, na.rm = T)
    )
listing_map_hood <- listings %>% 
    select(id, neighbourhood_cleansed, longitude, latitude, room_type, price, 
           number_of_reviews, availability_365,latitude, longitude, income_monthly,host_is_superhost,highly_available
           ) %>% 
    group_by(neighbourhood_cleansed,room_type) %>% 
    dplyr::summarise(
        nb_bnb = n(),
        avg_price = mean(price, na.rm = T),
        avg_reviews = mean(number_of_reviews,na.rm = T),
        avg_monthly_income = mean(income_monthly, na.rm = T)
    )


n_hood <- c("All","East Boston","Roxbury","Beacon Hill","Downtown" ,
            "Back Bay","North End","Dorchester","South End","Charlestown",
            "Jamaica Plain","South Boston","Bay Village","Brighton",
            "West Roxbury","Roslindale","Mission Hill","Fenway","Allston",
            "Hyde Park","West End","Mattapan","South Boston Waterfront",
            "Chinatown")

# Functions
vibe_word_df <- function(df = listings,n_words){
    # word cloud answers the vibe of each neighbourhood
    # word_col can be description or neighborhood_overview
    
        
    tidy_txt <- df %>% select(id,neighborhood_overview) %>% 
        unnest_tokens(word,neighborhood_overview) %>% 
        anti_join(rbind((stop_words),c("br"),c("boston"),c("neighborhood"),c("city")))
    tidy_txt_df <- tidy_txt %>%
        filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%  # remove punctuation and digits 
        dplyr::count(word, sort = T) %>% 
        na.omit(word) %>% 
        filter(n > n_words) %>% 
        mutate(word = reorder(word,n))
    return (tidy_txt_df)
    
}



# Define UI for application that draws a histogram
ui <- navbarPage(
    
    # Application title
    title = "Analysis of Airbnb Listings in Boston",
    


            selectInput(
                "n_hood",
                "Neighborhood",
                n_hood,
                selected = "All",width = 'auto'
            ),
    tabPanel("Map",leafletOutput("map",width = "75%", height = "700px"),
            absolutePanel(id='controls',class = "",
            fixed = T,draggable = F, top=130,left = "76%",right="auto",
            bottom = "auto",width = "auto", height = "700px",
            #checkboxGroupInput(inputId = "r_type",label = h4("Room Type:"),
                               #choices = unique(listings$room_type),
                              # selected =unique(listings$room_type) ),
            verbatimTextOutput(h1("nlist")),
            sliderInput("price_slide",
                       "Price",
                       min = 0,
                       max = 1000,
                       value = c(0,1000),
                       step = 1),
            sliderInput("reviews",
                        "Ratings",
                        min = 0,
                        max = 5,
                        value = c(0,5),
                        step = 0.1),
            plotOutput("no_lists",height = "150px"),
            plotOutput("price_d",height = "200px"),
            plotOutput("monthly_income",height = "200px")
            
             )),
                        tabPanel("Neighbourhood Vibe",sliderInput("no_words","No. of times a word appears",min = 10, max = 100,value = 50,step = 5),
                                 wordcloud2Output("word_cloud",width = "100%", height = "600px")),
    tabPanel("Analysis",splitLayout(plotOutput("rtype_plot",width = "100%"), 
             plotOutput("dens_plot2",width = "100%"),cellWidths = c("30%","70%")),
             splitLayout(plotOutput("no_bnb"),
                         cellWidths = c("100%")
                         )
             )
                        )

        
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    n_df <- reactive({if(input$n_hood != 'All'){
        n_df <- listings %>% filter(neighbourhood_cleansed == input$n_hood
                                    )
    }
    else {
        n_df <- listings 
    }})

    df <- reactive({
        if(input$n_hood != 'All'){
            df <- listing_map_hood %>% filter(neighbourhood_cleansed == input$n_hood)
        }
        else {
            df <- listing_map 
        }
        df
    })
    
    
   
    
    
    
    
    l_map <- reactive({
       
        lmap <- n_df() %>% filter(
        review_scores_rating <= input$reviews[2],
        review_scores_rating >= input$reviews[1],
        price <= input$price_slide[2],
        price >= input$price_slide[1]) %>% 
            group_by(room_type) %>% 
            dplyr::summarise("nb_bnb" = n())
        lmap 
            
    })
    
    dense_plot <- function(dat, cont_col,fill_col){
        mean_dat <- mean(dat[[cont_col]],na.rm = T)
        g <- ggplot(data=dat) +
            geom_density(mapping = aes(x=dat[[cont_col]],fill = dat[[fill_col]]),alpha = 0.6,color = NA) +
            theme_pubclean() + theme(legend.title = element_blank())+ scale_fill_brewer(palette="Set1")
        return (g)
    }
    
    dense_plot_mean <- function(dat, cont_col){
        mean_dat <- mean(dat[[cont_col]],na.rm = T)
        g <- ggplot(data = dat) +
            geom_density(mapping = aes(x=dat[[cont_col]])) + 
            theme_pubclean() + theme(legend.position = "none") + 
            geom_vline(mapping = aes(xintercept = mean_dat),linetype = 'dashed')  + scale_fill_brewer(palette="Set1")
    return (g)
    }
    
    output$dens_plot2 <- renderPlot({
        ndf <- n_df() 
        g <- dense_plot(ndf,"price",fill_col =   "room_type") +
            labs(x="Price/night",  
                 title = "Price Distribution"
            ) 
        g
    })
    
    output$word_cloud <- renderWordcloud2({
        set.seed(1234)
        wordcloud_df <- vibe_word_df(n_df(),n_words = input$no_words)
        wordcloud2(wordcloud_df,fontFamily = 'helvitica nue',font = 1,color = 'random-dark',size = 2)
    })
    
    output$price_d <- renderPlot({
        ndf <- n_df() %>% filter(
            review_scores_rating <= input$reviews[2],
            review_scores_rating >= input$reviews[1],
            price <= input$price_slide[2],
            price >= input$price_slide[1])
        g <-dense_plot_mean(dat = ndf,'price')       +
            labs(x="Price/night",  
                 title = "Price Distribution"
            ) 
        g
        
    })
    
    output$no_lists <- renderPlot({
        
        g <- ggplot(data = l_map()) +
            geom_col(mapping = aes(x=reorder(room_type,-nb_bnb),y=nb_bnb, fill = factor(room_type))) + 
            theme_pubclean() + theme(legend.position = "none") + 
            labs(x="Room Type", y = "Number of listings", 
                 title = "Number of listings per room type"
            )+ scale_fill_brewer(palette="Set1")
        g
        
    })
       
    output$rtype_plot <- renderPlot({
        g <- ggplot(data = df()) +
            geom_col(mapping = aes(x=reorder(room_type,-avg_price),y=avg_price, fill = factor(room_type))) + 
            theme_pubclean() + theme(legend.position = "none") + 
            labs(x="Room Type", y = "Average Price per day", 
                 title = "Avg. Price of listings according to Room Type"
                     ) + scale_fill_brewer(palette="Set1")
    g
    })

    
    output$map <- renderLeaflet({
        # Filtering according to review scores 
        n_df <- n_df() %>% filter(review_scores_rating <= input$reviews[2],
                       review_scores_rating >= input$reviews[1],
                       price <= input$price_slide[2],
                       price >= input$price_slide[1])
        
        leaflet(n_df) %>% addTiles() %>% 
            fitBounds(~min(longitude),~min(latitude),~max(longitude),~max(latitude)) %>% 
            addMarkers(
                clusterOptions = markerClusterOptions(), 
                popup = ~paste(
                    "<b>",name,"</b><br/>",
                    "Type: ",room_type,"<br/>",
                    "Price: $",round(price),sep = "","<br/>",
                    "Availability per year: ",round(availability_365)," days","<br/>"
                )
            ) %>% 
            addProviderTiles(providers$CartoDB.Positron)})
        
    output$no_bnb <- renderPlot({
        if (input$n_hood == 'All'){
        n_df <- n_df() %>% filter(
            `neighbourhood_cleansed` %in% n_hood
        ) %>% 
            group_by(`neighbourhood_cleansed`) %>% 
            dplyr::summarise(
                "nb_bnb" = n()
            )
        g <- ggplot(data = n_df) +
            geom_col(mapping = aes(x=reorder(`neighbourhood_cleansed`,-nb_bnb),y=`nb_bnb`, fill = factor(`neighbourhood_cleansed`))) + 
            theme_pubclean() + theme(legend.position = "none") + 
            labs(x="Neighbourhood", y = "No. of listings", 
                 title = "No. of listings in each neighbourhood") 
        g
        }
        
    })
    
    output$monthly_income <- renderPlot({
        ndf <- n_df() %>% filter(
            review_scores_rating <= input$reviews[2],
            review_scores_rating >= input$reviews[1],
            price <= input$price_slide[2],
            price >= input$price_slide[1]
             )
    
        g <-  dense_plot_mean(dat = ndf,cont_col = "income_monthly")  +
            labs(x="Monthly Income",  
                 title = "Monthly Income Distribution"
            ) 
        g
        
    })
    
    output$nlist <- renderPrint({
       n_df <-  n_df() %>% filter(
            review_scores_rating <= input$reviews[2],
            review_scores_rating >= input$reviews[1],
            price <= input$price_slide[2],
            price >= input$price_slide[1])
        n <-  paste("No. of listings: ",as.character((nrow(n_df))),sep = "")
        print (n)
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)


