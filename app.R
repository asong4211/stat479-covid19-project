
library(shiny)
library(readr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(plotly)
library(rsconnect)
library(ggplot2)
library(ggthemes)

library(maps)
library(usmap)
library(mapproj)


# THIS APP is deployed to Shinyapps.io
# Link: https://aiden-song.shinyapps.io/covid-19/

# reading data and processing it, data cleaning

dat1 = read.csv("https://covidtracking.com/api/states/daily.csv")

stay.at.home = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTMb4QgSodTBkJW9o8t1MNbhamgSHJIIL8z2HIFaBoUHIx5gp4Avx4_4RoUR4X1KICtzX_PsVusvt_b/pub?gid=0&single=true&output=csv")
## Select the 5 most hit states: NY,NJ, CA, MI, MA
stay.at.home = stay.at.home %>% 
    mutate(stayHomeDate=parse_date_time(date, "mdy")) %>% 
    select(state, stayHomeDate, state.full)

data = dat1 %>% as_tibble() %>% mutate(date= parse_date_time(date, "ymd")) %>% 
    mutate(totalTest= total) %>%
    mutate(state=as.factor(state)) %>%
    select(date,state, positive, negative,death, hospitalized,totalTest) 

data = left_join(data,stay.at.home,by = c("state", "state")) 


# get a list of all the states available
state_list = unique(stay.at.home$state.full)


dateRange = data %>% summarise(min_date = min(date) , max_date = max(date))
min_date = dateRange$min_date; 
max_date = dateRange$max_date;

summary_data = data %>% filter(date == max_date)
    




simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}


########################################################################
################### UI #################################################
########################################################################
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title

    titlePanel(
        h1("COVID-19 Cases by State (STAT 479)",  
           h3(em("Authors: Aiden Song, Shane McIntyre"),align="center"), 
           h3(" "),
           align="center")
    ),
    fluidRow(
        column(6,align="center",plotlyOutput("infectionMap")),
        column(6,align="center", plotlyOutput("testsMap"))
    ),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "State:", state_list),
            sliderInput("dateSlider", "Date Range:", 
                        min=min_date, max=max_date, value=c(min_date,max_date),
                        timeFormat="%m-%d"
                        ),
            checkboxInput("confirmedCases", "Display Total Confirmed Cases", TRUE),
            checkboxInput("numTestings", "Display Total Number of Tests Given", FALSE),
            checkboxInput("dailyConfirmed", "Display Daily New Confirmed Cases", FALSE),
            checkboxInput("dailyTests", "Display Daily Tests Given", FALSE),
            checkboxInput("stayhome", "Display date of stay-at-home order", FALSE),
            checkboxInput("log","Log Scale",FALSE)
            # Show a plot of the generated distribution
        ),
        mainPanel( 
                 tableOutput('statistics'),
                 plotlyOutput("ConfirmedCases")
        )
    ),
    
)

########################################################################
################## SERVER ##############################################
########################################################################

server <- function(input, output) {
    

    
    output$statistics <- renderTable({
        display_data = summary_data %>% 
            filter(state.full == input$state)  %>%
            mutate(State = state.full, Confirmed.Cases = positive, Death.Counts=death, 
                   Hospitalized = hospitalized, Total.Test.Given = totalTest)  %>%
            select(State,Confirmed.Cases, Death.Counts, Hospitalized,Total.Test.Given) 
        display_data
        
        
    })
    
    output$ConfirmedCases <- renderPlotly({
        # generate bins based on input$bins from ui.R
        
   
        
        new_data = data %>% 
            filter(state.full==input$state) %>% 
            arrange(date) %>% 
            mutate(new_positive=c(0,diff(positive)), new_tests = c(0, diff(totalTest))) %>%
            filter(new_positive >= 0,new_tests >= 0) %>%
            filter(date>=input$dateSlider[1], date <= input$dateSlider[2])
            

        stay_home = stay.at.home %>% filter(state.full==input$state)
        
        if(input$log == TRUE){
            new_data = new_data %>% mutate(positive=log(positive), new_positive=log(new_positive),
                                           negative=log(negative), death = log(death),
                                           hospitalized=log(hospitalized), totalTest=log(totalTest),
                                           new_positive=log(new_positive), new_tests=log(new_tests)
                                           )
        }

       
        # start of a plot
        g = ggplot() + labs(x="Date", y="Cases")

        
        if(input$numTestings == TRUE){
            g = g + geom_line(data=new_data, aes(x=date, y=totalTest, color="Total Tests Given"),size=1)
        }
        if(input$confirmedCases == TRUE){
            g = g + geom_line(data=new_data, aes(x=date, y = positive,color="Confirmed Cases"),size=1)
        }
       
        
        if(input$dailyTests == TRUE){
            g = g + geom_area(data = new_data, aes(x=date, y=new_tests), fill="lightblue")
        }
        if(input$dailyConfirmed == TRUE){
            g = g  + 
                geom_area(data=new_data,aes(x=date,y=new_positive),fill="yellow")
        }
        if(input$stayhome == TRUE){
            g = g + geom_vline(data=stay_home, mapping=aes(xintercept=as.numeric(stayHomeDate),color="Date of Stay-at-home order"),size=1)
        }
       
        # return g at the end
        fig = ggplotly(g)
        fig

        
    })
    
    # infection map per 100k
    output$infectionMap = renderPlotly({
        my_map = map_data("state")
        summary = summary_data %>% mutate(region=tolower(state.full))
        state_population = statepop %>% mutate(region=tolower(full)) %>% select(region, pop_2015)
        
        summary_map = left_join(my_map, summary, by="region")
        summary_map = left_join(summary_map, state_population, by="region")
        
        summary_map = summary_map %>% 
            mutate(infection.per.100k = (positive/pop_2015)*1000000) %>%
            mutate(infection.per.100k = cut(infection.per.100k, c(0, 500,1000,2500,10000,1000000)))
           
        state_centroids <- summarize(group_by( my_map, region),
                                     x = mean(range(long)), y = mean(range(lat)))
        names(state_centroids)[1] <- "state"
        
        g = ggplot(summary_map)+ labs(title="Number of Confirmed Cases per hundred thoudsands people") +
            geom_polygon(aes(long, lat, group = group, fill = infection.per.100k), col="#ecf0f1") +
            geom_text(aes(x,y,label=sapply(state, simpleCap)), data=state_centroids,size=2) +
            ggthemes::theme_map() +
            scale_fill_brewer(palette = "Reds", na.value = "gray")
            
        ggplotly(g)
    })
    
    
    
    
    # render tests per million people
    output$testsMap = renderPlotly({
        my_map = map_data("state")
        summary = summary_data %>% mutate(region=tolower(state.full))
        state_population = statepop %>% mutate(region=tolower(full)) %>% select(region, pop_2015)
        
        summary_map = left_join(my_map, summary, by="region")
        summary_map = left_join(summary_map, state_population, by="region")
        
        summary_map = summary_map %>% 
            mutate(tests.per.100k = (totalTest/pop_2015)*1000000) %>%
            mutate(tests.per.100k = cut(tests.per.100k, c(0,10000,15000,20000,30000,1000000)))
        
        state_centroids <- summarize(group_by( my_map, region),
                                     x = mean(range(long)), y = mean(range(lat)))
        names(state_centroids)[1] <- "state"
        

        g = ggplot(summary_map)+
            labs(title="Total Tests given per hundred thoudsands people") + 
            geom_polygon(aes(long, lat, group = group, fill =tests.per.100k ), col="#ecf0f1") +
            geom_text(aes(x,y,label=sapply(state, simpleCap)), data=state_centroids,size=2) +
            ggthemes::theme_map() +
            scale_fill_brewer(palette = "Reds", na.value = "gray")
        
        ggplotly(g)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
