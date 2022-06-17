"    .............................  NOTE......................................

DOWNLOAD THE SAMPLE DATA FROM THE FOLDER (Sample Data) AND SAVE IT IN YOUR WORKING DIRECTORY"



# load the required packages

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library (tidyr)
library(shinythemes) 
library("stringr")
library (reshape2)
library(kableExtra)
library(knitr)
library(tidyverse)
library(lubridate)
library(readr)


#MAKING THE DATA READY FOR ANALYSIS
Healthroom_data1 <- read.csv('sampledata.csv', stringsAsFactors = FALSE)
Healthroom_data1$dov <- dmy(Healthroom_data1$dov)


df11 <- gather(Healthroom_data1, complain, val, complain_1:complain_4) %>%
  count(user_id,dov,gr_no,category,class, section, age, val) %>%
  spread(val, n, fill = 0) 

#lubridate dov into seperate year, month, date column
df11 <- df11 %>% 
  mutate(dov = ymd(dov)) %>% 
  mutate_at(vars(dov), funs(year, month, day))

df22 <- melt(df11, id.vars = c("user_id", "dov","category","class", "section","age","gr_no","year","month","day"), variable.name = "complaint", value.name = "reported")
df22 <- df22[!(df22$reported=="0" |df22$complaint=="<NA>"|df22$complaint=="V1"),]




#head(df1)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Healthroom Dashboard")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    
  ),
  selectInput('slct2',"Select Year", choices = list("2022", "2021", "2020", "2019", "2018", "2017"),selected = "2022"),
  selectInput('slct3',"Select School", choices= list("School 1", "School 2"), selected = "School 1"),
  selectInput('slct4',"Select Category", choices = list("Student", "Teacher", "Other"), selected = "Student"),
  
  sidebarMenu(
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "www.you_can_enter_your_organisation's_website_link_here.com")
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "Top reported complaints"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,width = 12
    ,align = "center"
    ,plotOutput("complaintsbyAge", height = "400px")
  )
)

frow3 <- fluidRow(
  
  box(
    title = "Monthly Trends"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("complaintTrends", height = "428px")
  ) 
  
  ,box(
    title = "Top Reported Complaints in Healthroom"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,tableOutput("datahead")
  ) 
  
)

frow4 <- fluidRow(
  
  box(
    title = "Trends in Cases"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,width = 12
    ,align = "center"
    ,plotOutput("casesTrends", height = "400px")
  )
  
)

# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3,frow4)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  
  
  
  #VALUE BOX ONE, TWO, AND THREE
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    a <- filter(df11,year==input$slct2)
    #some data manipulation to derive the values of KPI boxes
    b <- dplyr::count(a, category)
    total.studentvisits <- b$n[b$category=="Student"][1]
    
    valueBox(
      formatC(total.studentvisits, format="d", big.mark=',')
      ,paste('Total Visits (Students)')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
  })
  
  output$value2 <- renderValueBox({
    a <- filter(df11,year==input$slct2)
    b <- dplyr::count(a, category)
    # Please insert OR function here; Teacher OR teacher
    total.teachervisits <- b$n[b$category=="Teacher"][1]
    valueBox(
      formatC(total.teachervisits, format="d", big.mark=',')
      ,'Total Visits (Teachers)'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
  })
  
  output$value3 <- renderValueBox({
    a <- filter(df11,year==input$slct2)
    b <- dplyr::count(a, category)
    #Total visits by Others
    c <-  b %>% filter(category != "Teacher" & category != "Student")
    total.othervisits <- sum(c$n)
    valueBox(
      formatC(total.othervisits, format="d", big.mark=',')
      ,paste('Total Visits (Others)')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  
  #PLOT CONTENT
  #creating the plotOutput content
  
  output$complaintsbyAge <- renderPlot({
    df_plot <- df22 %>%
      filter(year == input$slct2, category ==input$slct4) %>%
      group_by(complaint,age) %>%
      select(complaint, reported) %>%
      summarise_all(funs(sum)) %>%
      mutate_if(is.numeric, round, 0) %>%
      arrange(desc(reported))
    ggplot(data = head(df_plot,40), 
           aes(x=age, y=reported, fill=factor(complaint))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Reported Cases)") + 
      xlab("Age") + theme(legend.position="bottom" 
                          ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Reported Cases by Age") + labs(fill = "Complaint")
    
    
  })
  
  
  output$complaintTrends <- renderPlot({
    target <- c("Fever", "Vomitting","Missed Breakfast","Headache", "Periods","Stomach Pain","Flu","Cough","Asthama","Blood Pressure Assessment","Back Pain")
    df_plot1 <- df22 %>%
      filter(year == input$slct2, category ==input$slct4) %>%
      select(complaint,month,reported) %>%
      group_by(month,complaint) %>% 
      summarise(reported = sum(reported))%>%
      filter(complaint %in% target)
    
    ggplot(data = df_plot1, aes(x = month, y = reported)) +
      geom_line()+
      facet_wrap(facets = vars(complaint))
   
    
    
  })
  
  output$datahead <- function(){
    df3 <- df22 %>%
      filter(year == input$slct2, category ==input$slct4) %>%
      group_by(complaint) %>%
      select(complaint, reported) %>%
      summarise_all(funs(sum)) %>%
      mutate_if(is.numeric, round, 0) %>%
      arrange(desc(reported))
    head(df3,10) %>%
      kable("html") %>%
      kable_styling(c("striped", "hover"), full_width = T)}
  
  output$casesTrends <- renderPlot({
    target <- c("Fever", "Vomitting","Missed Breakfast","Headache", "Periods","Stomach Pain","Flu","Cough","Asthama","Blood Pressure Assessment","Back Pain")
    df_plot1 <- df22 %>%
      filter(year == input$slct2, category ==input$slct4) %>%
      select(complaint,month,reported) %>%
      group_by(month,complaint) %>% 
      summarise(reported = sum(reported))%>%
      filter(complaint %in% target)
    
    ggplot(data = df_plot1, aes(x = month, y = reported, color=complaint)) +
      geom_line()
    
    
    
  })
  
}



shinyApp(ui, server)
