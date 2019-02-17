#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#    ----
#    title: "Drug Review Analzer"
#    author:  Deepa Chhetri
#    Date: 02/16/2019
#    ---
#    This project is inspired by Yelp. It is web application to find effective medicines for a physical condition one
#    might be suffering from. The dataset consists of over 160k+ rows of drug review data like drug name, condition name,
#    review comments, ratings, upvotes on comments.
#    This applications is aimed at performing analytics on reviews available which could be used to
#    get insights like effectiveness of medicine, side effects, ratings etc. Due to huge dataset, we could not
#    finish training models on time. Future work involves sentiment analysis of comments.

library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(data.table)
library(DT)
# Define UI for application that draws a histogram
#

#data gathering and cleaning
#
data <- read.csv("data/drugsComTrain_raw.csv",
                 stringsAsFactors = FALSE,
                 header = TRUE,
                 na.strings = c("", " ", "NA"))
data <- data[!grepl("users found this comment helpful", data$condition),]

data$drugName <- as.factor(data$drugName)
data$condition <- as.factor(data$condition)
data <-
  data %>% separate(col = date, into = c("Date", "Month", "Year"), sep = "-", remove = FALSE, extra = "drop")
data <- within(data, rm(Date))
data$Year <- as.factor(data$Year)
data$Month <- as.factor(data$Month)

#UI interface
ui <- fluidPage(
  tags$head(
    tags$style(
      ".title {margin: auto; width: 400px}"
    )
  ),
  tags$div(class = "title", titlePanel(HTML("Drug Data Review Analyzer")),
           a(href = "https://archive.ics.uci.edu/ml/datasets/Drug+Review+Dataset+%28Druglib.com%29","Link to Data Source")),
  hr(),
  br(),
  sidebarPanel(
    selectInput("cond","Select the Condition: ",levels(data$condition),selected = "Headache", width = 200),
    uiOutput("select_drug_ui"),
    br(),
    #selectInput("drug","Choose Drug",levels(data$drugName), width = 200),
    width = 3

    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Detail Review",dataTableOutput("table")),
      tabPanel("Visualizations",plotOutput("plot"),
               br(),
               dataTableOutput("plot2")),
      tabPanel("Analysis",dataTableOutput("analysis"))
      )
    )
  )

#Server side
server <- function(input,output){
  output$select_drug_ui <- renderUI({
    selectInput("drug", "Select the Drug: ",
                choices = data[data$condition %in% input$cond,"drugName"],selected = "None", width = 200
    )
  })
  temp_data <- reactive({
    data <- data[data$condition == input$cond ,]
    data <- data[data$drugName == input$drug ,]
    data <- na.omit(data)
    colnames(data) <- c("ID","drugName","Condition","Review","Rating","Date","Month","Year","Upvotes")
    data
  })

  temp_data2 <- reactive({
    data <- data[data$condition == input$cond ,]
    data <- na.omit(data)
    colnames(data) <- c("ID","drugName","Condition","Review","Rating","Date","Month","Year","Upvotes")
    data
  })

  output$table <- DT::renderDataTable(DT::datatable({

    data_tmp <- temp_data()
    data_tmp <- data_tmp[,c("Upvotes","Review","Rating","Date")]
    data_tmp

  }))

  output$plot <- renderPlot({
    dat <- temp_data()
    #count = sort(table(dat$rating), decreasing = TRUE)
    dat_df <-  as.data.frame(table(dat$Rating))
    names(dat_df) <- c("Rating","count")
    #print(head(dat_df))
    dat_df %>%  ggplot() + aes(x = dat_df$Rating, y = count) + geom_bar(fill = "blue",colour = "blue",stat = "identity") +
      theme(axis.text.x = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 7)) +
     scale_x_discrete(name = "Rating") + coord_flip() +  geom_text(aes(label = dat_df$count), vjust = 0) + labs(title = "Rating Distribution")
  })

  output$plot2 <- renderDataTable({
    #average rating of a medicine
    dat2 <- temp_data2()
#    dat2 %>% group_by(drugName) %>% summarize(Average = mean(Rating))
    summ <- as.data.frame(summary(dat2[,c("drugName","Rating")]))
    summ$Var1 <- NULL
    colnames(summ) <- c("Attribute","Count")
    summ
  })

  output$analysis <- renderDataTable({
    dat3 <- temp_data()
    total <- nrow(dat3)
    ppl_recommend <- round((sum(dat3$Rating > 5)*100)/total,0)
    top <- dat3[dat3$Upvotes == max(dat3$Upvotes),c("Review")]
    out <- as.data.frame(rbind(total,ppl_recommend, top))

    rownames(out) = c("Total Ratings","Recommendation in %","Top Comment")
    colnames(out) = c("Insights")
    out
  })

}

# Run the application
shinyApp(ui = ui, server = server)

