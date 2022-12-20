library(tidyverse)
library(caret)
library(kernlab)
library(ggplot2)
library(rpart)
library(dplyr)
library(caret)
library(imputeTS)

df <- "https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv"
File1 <- read_csv(df)
str(File1)

File1$bmi <- na_interpolation(File1$bmi)
File1$hypertension <- na_interpolation(File1$hypertension)

File1$highcost <- as.factor(File1$cost >= 4775)

set.seed(111)

rpartTrainList <- createDataPartition(y= File1$highcost,p=.70, list=FALSE)
train_Data <- File1[rpartTrainList,]
test_Data <- File1[-rpartTrainList,]
trctrl <- trainControl(method="repeatedcv", number=10)
model.rpart <- train(highcost ~ smoker+exercise+married+education_level+location_type+married+yearly_physical, data = train_Data,method = "rpart",trControl=trctrl,tuneLength = 50)

RpartOut <- predict(model.rpart,newdata=test_Data)
varImp(model.rpart)


File1$region <- tolower(File1$location)
file2 <- File1 %>% filter(File1$cost<30000)
us <- map_data("state")
mapandfile <- merge(file2,us,by="region") 
mapandfile <- mapandfile %>%  arrange(order)

library(shiny)
library(shinydashboard)

ui <-dashboardPage(
        dashboardHeader(title = "Medical Expenses"),
        dashboardSidebar(
          sidebarMenu(id = "sidebar",
                      menuItem("Dataset", tabName = "data", icon = icon("file-csv")),
                      menuItem("Result", tabName = "res", icon = icon("chart-simple")),
                      menuItem("Confusion Matrix", tabName = "conmat", icon = icon("chart-simple")),
                      menuItem("Sensitivity", tabName = "sen", icon = icon("chart-simple")),
                      menuItem("Map", tabName = "map", icon = icon("chart-column")),
                      menuItem("Boxplot", tabName = "bp", icon = icon("chart-column")),
                      menuItem("Histograms", tabName = "hist", icon = icon("chart-column")),
                      menuItem("Scatter Plots", tabName = "scatterplot", icon = icon("chart-column"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "data", 
                    tabBox(id="t1", width = 12,
                           tabPanel("About", icon=icon("address-card"),
                                    fluidRow(
                                      column(width = 12, tags$p("Insert your test file down and open result section to view results")),
                                      column(width = 12, fileInput("file1", "Choose CSV File", accept=c('text/csv', 
                                                                                                        'text/comma-separated-values,text/plain', 
                                                                                                        '.csv'))
                                      )
                                            )
                                    ), 
                           tabPanel("Data", tableOutput("dataT"), icon = icon("table")), 
                           tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted"))
                           )
                    
            ),
            
            tabItem(tabName = "res",
                    tabBox(id="t2", width= 12,
                           tabPanel("Result",verbatimTextOutput("result"), icon=icon("uncharted"),
                                    fluidRow(
                                      column(width = 12, tags$p("Here True means high expensive and vice versa")),
                                      column(width = 12, tags$p("People who smoke and not exercise having more expenses")))
                           
                    ))),
            
            tabItem(tabName = "conmat",
                    tabBox(id="t3", width= 12,
                           tabPanel("Matrix",verbatimTextOutput("mat"), icon=icon("uncharted"))
                           
                    )),
            tabItem(tabName = "sen",
                    tabBox(id="t4", width= 12,
                           tabPanel("Sensitivity",verbatimTextOutput("Sens"), icon=icon("uncharted"),
                                    fluidRow(
                                      column(width = 12, tags$p("Insert solution file to get sensitivity")),
                                      column(width = 12, fileInput("file2", "Choose CSV File", accept=c('text/csv', 
                                                                                                        'text/comma-separated-values,text/plain', 
                                                                                                        '.csv')))
                                             
                                            )
                                     )
                           )
                   ),
            
            tabItem(tabName = "map",
                    tabBox(id="t5", width= 12,
                           tabPanel("Map",plotOutput("map"), icon=icon("chart-pie"),
                                    fluidRow(
                                      column(width = 12, tags$p("NOTE - wait for 6 seconds to load map :)")))
                           
                    ))),
            
            tabItem(tabName = "bp",
                    tabBox(id="t6", width= 12,
                           tabPanel("state", plotOutput("st"),icon=icon("chart-column")),
                           tabPanel("exercise",plotOutput("ex"), icon=icon("chart-column")),
                           tabPanel("gender",plotOutput("ge"), icon=icon("chart-column")),
                           tabPanel("married",plotOutput("ma"),icon=icon("chart-column")),
                           tabPanel("education",plotOutput("ed"), icon=icon("chart-column")),
                           tabPanel("physical",plotOutput("ph"), icon=icon("chart-column")),
                           tabPanel("smoker",plotOutput("sm"), icon=icon("chart-column"))
                           
                    )),
            tabItem(tabName = "hist",
                    tabBox(id="t7", width= 12,
                           tabPanel("education",plotOutput("edh"), icon=icon("chart-pie")),
                           tabPanel("state", plotOutput("sth"),icon=icon("chart-pie")),
                           tabPanel("married", plotOutput("mrh"),icon=icon("chart-pie"))
                           
                           
                    )),
            tabItem(tabName = "scatterplot",
                    tabBox(id="t8", width= 12,
                           tabPanel("age",plotOutput("spab"), icon=icon("chart-pie"))
                    ))
          )
        )
)
        




server <- function(input, output){

  mydata <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath)
  })
  
  mydata2 <- reactive({
    file <- input$file2
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath)
  })
  
  
        output$dataT <- renderTable({
        req(mydata)
        mydata()
      })
      output$result <- renderPrint({
        rpart_t <- predict(model.rpart ,mydata(), type="raw")
        rpart_t
      })
      output$structure <- renderPrint({
        str(mydata())
      })
      output$st <- renderPlot({
        box_state <- ggplot(File1)+aes(x=cost,y=location)+geom_boxplot()
        box_state
        })
      output$ex <- renderPlot({
        box_exercise <- ggplot(File1) + aes(x=cost,y=exercise)+geom_boxplot()
        box_exercise
      })
      output$sm <- renderPlot({
        box_smoker <- ggplot(File1) + aes(x=cost,y=smoker)+geom_boxplot()
        box_smoker
      })
      output$ge <- renderPlot({
        box_gender <- ggplot(File1) + aes(x=cost,y=gender)+geom_boxplot()
        box_gender
      })
      output$ma <- renderPlot({
        box_married <- ggplot(File1) + aes(x=cost,y=married)+geom_boxplot()
        box_married
      })
      output$ed <- renderPlot({
        box_education_level <- ggplot(File1) + aes(x=cost,y=education_level)+geom_boxplot()
        box_education_level
      })
      output$ph <- renderPlot({
        box_physical <- ggplot(File1) + aes(x=cost,y=yearly_physical)+geom_boxplot()
        box_physical
      })
      output$mat <- renderPrint({
       confusionMatrix(RpartOut,test_Data$highcost)
      })
      output$edh <- renderPlot({
        bar_education_level <- ggplot(File1) + aes(x=education_level,y=cost, fill = education_level)+ geom_bar(stat="identity")
        bar_education_level
      })
      output$sth <- renderPlot({
        bar_state <- ggplot(File1) + aes(x=location,y=cost, fill = location)+ geom_bar(stat="identity")
        bar_state
      })
      output$mrh <- renderPlot({
        bar_mar <- ggplot(File1,aes(x=married,y=cost,fill = married))+ geom_bar(stat="identity")
        bar_mar
      })
      output$map <- renderPlot({
        map2 <- ggplot(mapandfile, aes(map_id=location)) + aes(x=long, y=lat, group=group)+
          geom_polygon(aes(fill=cost))+scale_fill_viridis_c(option="D")+
          coord_fixed(ratio = 1.5) +
          scale_y_continuous(expand = c(0,0))
        map2
      })
      output$Sens <- renderPrint({
        expensive_predicted <- predict(model.rpart ,mydata(), type="raw")
        actual_expensive <- mydata2()$expensive 
        conf_Mar <- table(expensive_predicted, actual_expensive)
        sensitivity(conf_Mar)
      })
      output$spab <- renderPlot({
        scatterplot_age <- ggplot(File1,aes(x=age,y=cost,color=age))+geom_jitter()
        scatterplot_age
      })
  }

shinyApp(ui, server)
