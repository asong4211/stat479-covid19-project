
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
        title=h1("COVID-19 Cases by State",  
           h3(em("Authors: Aiden Song, Shane McIntyre"),align="center"), 
           h3(" "),
           align="center"),
        windowTitle="COVID-19 Cases by State"
    ),
    fluidRow(
   
        HTML(
            paste(
                h2("Project Description:", padding=20), "<br/>",
                p("In this project, we are interested in how COVID-19 affects each states. We first looked at how the number of confirmed
                  cases per hundred thousands people across different state to see how server covid is in each state adjusted to the state's
                  population. Then we looked at how aggresive testings have done in that state, again adjusted to the state's population. From
                  what we can see in the graph, testing is very positively correlated with the number of confirmed cases, some of the most 
                  hit states have performed some of the most aggresive testings. However some of the state, even though it might seem like aggresive
                  testings have been performed, the population adjusted data indicated otherwise (e.g. see California) "), "<br />",
                p("Then we plot all the confirmed cases, total number of testings in the plot to see how the graph is growing. We also looked
                  at daily new testings and daily new confirmed cases across different state. From the area under the curve graph we can see that
                  some states like Wisconsin have done pretty aggresive testings because the daily new cases vs the daily new testings area is big
                  whereas states like New Jersey, the daily new cases are almost half of the area of the daily new testings. We also looked at if state
                  at home is help flatting the curve. Since most of the states are still climbing the curve and have not seen the curve flattening, it's
                  hard to see if or when stay-at-home order would become effective.")
            )
            
        ),
        style="padding:20px"
        
        
       
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
            checkboxInput("infections","Display infections rate", FALSE),
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
        
        if(input$infections == T){
            g = g + geom_line(data = new_data, aes(x = date, y = new_positive/totalTest, color="Infection Rate"), size = 1)
        }
        

        if(input$dailyTests == TRUE){
            g =  g + 
                stat_smooth(data=new_data, mapping=aes(x=date,y=new_tests),geom = 'area', method = 'loess', 
                            span = .2, alpha = 1/2, fill = 'blue', fullrange = F)
        }
        if(input$dailyConfirmed == TRUE){
            g = g+
                stat_smooth(mapping=aes(x=date,y=new_positive), data=new_data,geom = 'area', method = 'loess', 
                            span = .2, alpha = 1/2, fill = 'red', fullrange = F)
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
            mutate(infection.per.100k.stats = (positive/pop_2015)*1000000) %>%
            mutate(infection.per.100k = cut(infection.per.100k.stats, c(0, 500,1000,2500,10000,1000000)))
           
        state_centroids <- summarize(group_by( my_map, region),
                                     x = mean(range(long)), y = mean(range(lat))) 
        names(state_centroids)[1] <- "state"
        state_centroids$state = sapply(state_centroids$state,simpleCap)
        
        
        confirmedPer100 = summary_map %>% select(state.full, infection.per.100k.stats) %>% distinct()
        state_centroids = left_join(state_centroids, confirmedPer100, by=c("state" = "state.full")) %>% 
            mutate(infection.per.100k.stats = round(infection.per.100k.stats,0))
        
        
        g = ggplot(summary_map)+ labs(title="Number of Confirmed Cases per hundred thoudsands people") +
            geom_polygon(aes(long, lat, group = group, fill = infection.per.100k), col="#ecf0f1") +
            geom_text(aes(x,y,label=state), data=state_centroids,size=2) +
            geom_text(aes(x,y,label=paste("\n\n\n", infection.per.100k.stats)), data=state_centroids, size=1.7)+
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
            mutate(tests.per.1000.stats = (totalTest/pop_2015)*1000) %>%
            mutate(tests.per.1000 = cut(tests.per.1000.stats, c(0,5,10,20,45,60,300)))
        
        state_centroids <- summarize(group_by( my_map, region),
                                     x = mean(range(long)), y = mean(range(lat)))
        names(state_centroids)[1] <- "state"
        state_centroids$state = sapply(state_centroids$state,simpleCap)
        
        testsPer100 = summary_map %>% select(state.full, tests.per.1000.stats) %>% distinct()
        state_centroids = left_join(state_centroids, testsPer100, by=c("state" = "state.full")) %>% 
            mutate(tests.per.1000.stats = round(tests.per.1000.stats,0))
        
        
        
        
        
        g = ggplot(summary_map)+
            labs(title="Total Tests given per one thoudsand people") + 
            geom_polygon(aes(long, lat, group = group, fill =tests.per.1000 ), col="#ecf0f1") +
            geom_text(aes(x,y,label=state), data=state_centroids,size=2) +
            geom_text(aes(x,y,label=paste("\n\n\n", tests.per.1000.stats)), data=state_centroids, size=1.7)+
            ggthemes::theme_map() +
            scale_fill_brewer(palette = "Reds", na.value = "gray")
        
        ggplotly(g)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
