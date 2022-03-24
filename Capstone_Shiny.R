library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggpubr)
library(utf8)

#Importing dataset movie gross with over 1000 theatre showing
data_movie <- read_delim("MovieGross1k.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
names(data_movie) <- make.names(names(data_movie))

#Importing dataset covid cases US
data_covid <- read_csv("Data-Covid-US.csv")
names(data_covid) <- make.names(names(data_covid))

vaccinations_df <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv')

data_movie <- data_movie %>% 
  mutate_if(is.character, ~gsub('[^ -~]', '', .))

df_movie <- data_movie %>% 
  mutate(Year_Month = paste(Year, Month, sep="-")) %>% 
  mutate_if(is.character, as_utf8)
         
head(df_movie)
head(data_covid)


df_movie_max_month <- df_movie %>%
  select(Movie, Opening_Gross, Year_Month, Year) %>% 
  drop_na(Opening_Gross) %>% 
  group_by(Year_Month) %>% 
  filter(Opening_Gross == max(Opening_Gross))



#Create dataframe for avg daily new cases per month
df_US_AVG <- data_covid %>% 
  group_by(Year_Month) %>% 
  summarise(avg_monthly_new_cases = mean(New_cases)) %>%
  arrange(Year_Month) %>% 
  mutate_if(is.numeric, round, 3)

#create dataframe movie with respective monthly average daily cases based on released month
df_movie_cases <- df_movie %>% 
  left_join(df_US_AVG, by="Year_Month")

#simplifying dataset movie for correlation testing
df_movie_cases_simp <- df_movie_cases %>% 
  mutate(OpeningPerMill = Opening_Gross/1000000) %>% 
  mutate(AvgMonthlyPer100 = avg_monthly_new_cases/10000) %>% 
  mutate_if(is.numeric, round, 3)

#taking data from 2019 onward and remove outliers
df_movie_cases_simp_corr <- df_movie_cases_simp %>% 
  filter(Year>2019) %>%
  filter(OpeningPerMill<100, AvgMonthlyPer100<50) %>% 
  mutate_if(is.numeric, round, 3)

#correlation test
cor.test(df_movie_cases_simp_corr$OpeningPerMill, df_movie_cases_simp_corr$AvgMonthlyPer100)
#result are r = -0,15 p = 0,09

#preparing for top movie gross Data
df_movie_t2018 <- data_movie %>% 
  select(Movie, Gross, Year, Month) %>%
  arrange(desc(Gross)) %>% 
  filter(Year == 2018) %>%
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>% 
  head(7)


df_movie_t2019 <- data_movie %>% 
  select(Movie, Gross, Year, Month) %>%
  arrange(desc(Gross)) %>% 
  filter(Year == 2019) %>%
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>% 
  head(7)

df_movie_t2020 <- data_movie %>% 
  select(Movie, Gross, Year, Month) %>%
  arrange(desc(Gross)) %>% 
  filter(Year == 2020) %>%
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>% 
  head(7)

df_movie_t2021 <- data_movie %>% 
  select(Movie, Gross, Year, Month) %>%
  arrange(desc(Gross)) %>% 
  filter(Year == 2021) %>%
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>% 
  head(7)

df_movie_t2022 <- data_movie %>% 
  select(Movie, Gross, Year, Month) %>%
  arrange(desc(Gross)) %>% 
  filter(Year == 2022) %>%
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>% 
  head(7)




ui <- dashboardPage( skin = "purple",
  dashboardHeader(title = "Movie Gross Related to Covid"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "dashboard-tab", icon=icon("home", lib = "glyphicon")),
      menuItem("Movies", tabName = "movies-tab", icon = icon("film", lib = "glyphicon")),
      menuItem("Covid-19 Cases", tabName = "covid-tab", icon = icon("list-alt", lib = "glyphicon")),
      menuItem("Movies pt.2", tabName = "movies2-tab", icon = icon("question-sign", lib="glyphicon")),
      menuItem("Correlation", tabName = "correlation-tab", icon = icon("asterisk", lib = "glyphicon"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        "dashboard-tab",
        fluidRow(
          h1("Article Background"),
          box(
            h2("The Batman Raup Rp 4,3 Triliun dalam Tiga Pekan Penayangan"),
            br(),
            h4("The Batman menjadi film nomor satu di box office domestik Amerika
               selama tiga minggi berturut-turut"),
            h4("Film yang dibintangin Robert Pattinson ini berhasil menembus pendapatan
               sebesar 300 juta dolar AS dalam kurun waktu tinga minggu"),
            h4("Hal ini membuat film The Batman menjadi film kedua yang rilis di era pandemi yang mampu mencapai pendapatan
               tersebut setelah Spider-Man: No Way Home"),
            h4("Dikutip dari Variety, selain AS, china menyumbang 12,1 juta dolar walau
               penayangan hanya di 43% bioskop karena masih pandemi Covid-19"),
            h6("source : https://www.kompas.com/hype/read/2022/03/21/103501266/the-batman-raup-rp-43-triliun-dalam-tiga-pekan-penayangan"),
            width = 12
          )
          
        )
        
      ),
      
      tabItem(
        "movies-tab",
        fluidRow(
          h3("Movies Related Data"),
          box(
            plotlyOutput("plotGross2018"),
            width = 4
          ),
          box(
            plotlyOutput("plotGross2019"),
            width = 4
          ),
          box(
            plotlyOutput("plotGross2020"),
            width = 4
            )
        ),
        fluidRow(
          box(
            plotlyOutput("plotGross2021"),
            width = 4
          ),
          box(
            plotlyOutput("plotGross2022"),
            width = 4
          )
        )
      ),
      
      tabItem(
        "covid-tab",
        fluidRow(
          h3("Covid Supporting Data"),
          box(
            plotlyOutput("plotCovidCases")
          ),
          box(
            checkboxInput("vaccination", "Data Vaccination", FALSE),
            plotlyOutput("plotCovidVacc")
          )
        )
      ),
      tabItem(
        "movies2-tab",
        fluidRow(
          box(
            plotlyOutput("plotMaxOpGross")
          ),
          box(
            plotlyOutput("plotCovidCases2")
          )
        )
      ),
      tabItem(
        "correlation-tab",
        fluidRow(
          h3("Graph of correlation of the Data"),
          box(plotlyOutput("plotCorr"), width = 12))
        )
      )
    )
    
  )
  



server <- function (input, output, session) {
  
  output$plotGross2018 <- renderPlotly({
    plot_ly(df_movie_t2018, x = ~Movie, y = ~Gross, type='bar', hovertext = ~paste('Released Month : ', Month)) %>% 
      layout(
        title = "Highest Gross Movie 2018",
        xaxis = list(title = "Movie Title"),
        yaxis = list(title = "Total Gross", range = c(0,800000000))
      )
  })
  
  output$plotGross2019 <- renderPlotly({
    plot_ly(df_movie_t2019, x = ~Movie, y = ~Gross, type='bar', hovertext = ~paste('Released Month : ', Month)) %>% 
      layout(
        title = "Highest Gross Movie 2019",
        xaxis = list(title = "Movie Title"),
        yaxis = list(title = "Total Gross", range = c(0,800000000))
      )
  })
  
  output$plotGross2020 <- renderPlotly({
    plot_ly(df_movie_t2020, x = ~Movie, y = ~Gross, type='bar', hovertext = ~paste('Released Month : ', Month)) %>% 
      layout(
        title = "Highest Gross Movie 2020",
        xaxis = list(title = "Movie Title"),
        yaxis = list(title = "Total Gross", range = c(0,800000000))
      )
  })
  
  output$plotGross2021 <- renderPlotly({
    plot_ly(df_movie_t2021, x = ~Movie, y = ~Gross, type='bar', hovertext = ~paste('Released Month : ', Month)) %>% 
      layout(
        title = "Highest Gross Movie 2021",
        xaxis = list(title = "Movie Title"),
        yaxis = list(title = "Total Gross", range = c(0,800000000))
      )
  })
  
  output$plotGross2022 <- renderPlotly({
    plot_ly(df_movie_t2022, x = ~Movie, y = ~Gross, type='bar', hovertext = ~paste('Released Month : ', Month)) %>% 
      layout(
        title = "Highest Gross Movie 2022",
        xaxis = list(title = "Movie Title"),
        yaxis = list(title = "Total Gross", range = c(0,800000000))
      )
  })
  
  output$plotMaxOpGross <- renderPlotly({
    plot_ly(df_movie_max_month, x = ~Year_Month, y = ~Opening_Gross, type='bar', hovertext = ~paste('Movie Title : ', Movie), color = ~Year) %>% 
      layout(
        title = "Movies Maximum Opening Gross each Month (2018-2022)",
        xaxis = list(title = "Released Data"),
        yaxis = list(title = "Opening Gross")
      )
  })
  
  output$plotCovidCases <- renderPlotly({
    vizCovidCases <- data_covid %>% 
      drop_na(New_cases) %>% 
      group_by(Date) %>% 
      summarize(sum_new_cases = sum(New_cases)) %>% 
      highlight_key(~Date) %>% 
      plot_ly(
        x = ~Date,
        y = ~sum_new_cases,
        mode = "lines+markers"
      ) %>% 
      layout(
        title = "Daily Covid New Cases",
        xaxis = list(title = "Date"),
        yaxis = list(title = "New Cases")
      ) %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
    vizCovidCases
  })
  
  output$plotCovidCases2 <- renderPlotly({
    data_covid %>% 
      drop_na(New_cases) %>% 
      group_by(Date) %>% 
      summarize(sum_new_cases = sum(New_cases)) %>% 
      highlight_key(~Date) %>% 
      plot_ly(
        x = ~Date,
        y = ~sum_new_cases,
        mode = "lines+markers"
      ) %>% 
      layout(
        title = "Daily Covid New Cases",
        xaxis = list(title = "Date"),
        yaxis = list(title = "New Cases")
      ) %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  
  observe({
    if(input$vaccination == TRUE){
      output$plotCovidVacc <- renderPlotly({
        vizCovidVacc <- vaccinations_df %>%
          drop_na(people_vaccinated) %>%
          group_by(date) %>%
          summarize(sum_people_vaccinated = sum(people_vaccinated)) %>%
          highlight_key(~date) %>%
          plot_ly(
            x = ~date, 
            y = ~sum_people_vaccinated,
            mode = "lines+markers"
          ) %>% 
          layout(
            title = "Covid Vaccination",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Cumulative People Vaccinated")
          ) %>% 
          # add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')  %>% 
          highlight(on = "plotly_click", off = "plotly_doubleclick")
        vizCovidVacc
      })
    }
  })
  
  output$plotCorr <- renderPlotly({
    vizCorr <- ggplot(data=df_movie_cases_simp_corr, aes(y=OpeningPerMill, x=AvgMonthlyPer100))+
      geom_point(size=2, colour='darkblue')+
      geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95, color='#006000')+
      labs(x="Avg Daily Cases", y="Opening Gross", title="Korelasi Jumlah Kasus Covid dengan Opening Gross Movie")+
      annotate("text", label = "r = -0.15, p = 0.09", x = 15, y = 80)+
      theme(axis.text.x = element_text(hjust = 0.5, size=10), axis.text.y = element_text(hjust = 0.5, size=10), plot.title = element_text(hjust = 0.5, vjust = 1))
    plotvizCorr <- ggplotly(vizCorr)
    plotvizCorr
  })
  
  
  output$output1 <- renderText({
    input$input1
  })
}

shinyApp(ui, server)
