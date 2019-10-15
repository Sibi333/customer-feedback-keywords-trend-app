
# NPS trend monitoring - RShiny application 

# Import libraries 

library(stringi)
library(glue)
library(quanteda)
library(tidyr)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(RJDBC)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Establish connection with database

jdbcDriver = JDBC("oracle.jdbc.OracleDriver",classPath="C:/tmp/ojdbc6.jar")
jdbcConnection = dbConnect(jdbcDriver, "jdbc:oracle:thin:@//hostname:port/servicename", "username", "password")

# Import data

full_data = dbGetQuery(jdbcConnection,'select * from "TABLE_NAME"')
View(full_data)
raw_data_keyword_filter <- full_data[,c(3, 4, 5)]
colnames(raw_data_keyword_filter) <- c("DATE_SUBMITTED","RATING","TEXT_CONTENT")

# Classify dataframe into promoters, detractors, passives based on rating

raw_data_keyword_classify <- raw_data_keyword_filter
raw_data_keyword_classify$CATEGORY <- NA
raw_data_keyword_classify$CATEGORY <- ifelse(raw_data_keyword_classify$RATING >= 0 & raw_data_keyword_classify$RATING <= 6, 'Detractors',
                                             ifelse(raw_data_keyword_classify$RATING >=7 & raw_data_keyword_classify$RATING <=8, 'Passives',
                                                    ifelse(raw_data_keyword_classify$RATING >=9 & raw_data_keyword_classify$RATING <=10, 'Promoters', 'NotRated')))
View(raw_data_keyword_classify)

# Data processing 

raw_data_keyword_filter_split <- separate(raw_data_keyword_classify, DATE_SUBMITTED, 
                                          into = c("Date", "Time"), sep = " ")
raw_data_keyword_filter_split <- raw_data_keyword_filter_split[,c(1,4,5)]
raw_data_keyword_filter_split$docname <- seq.int(nrow(raw_data_keyword_filter_split))
raw_data_keyword_filter_split$docname <- paste("text", 
                                               raw_data_keyword_filter_split$docname,
                                               sep="")
raw_data_keyword_filter_split$month <- months(as.Date(raw_data_keyword_filter_split$Date))
View(raw_data_keyword_filter_split)
str(raw_data_keyword_filter_split)
raw_data_keyword_filter_split$TEXT_CONTENT <- as.character(
  as.factor(raw_data_keyword_filter_split$TEXT_CONTENT))

# Initialize columns for each of the keywords to be tracked 

raw_data_keyword_filter_split$delivery_time <- NA
raw_data_keyword_filter_split$shipping_cost <- NA
raw_data_keyword_filter_split$payment <- NA
raw_data_keyword_filter_split$others <- NA

keywords_df <- raw_data_keyword_filter_split
View(keywords_df)

# Remove rows with no text content

keywords_df <- keywords_df[!is.na(keywords_df$TEXT_CONTENT),]

# Delivery time - keyword 10

multi_keywords10 <- c('lieferzeit*', 'versandzeit*', 'schnellere lieferung', 'fr*here lieferung',
                      'schnellerer versand', 'k*rzere Lieferzeit*', 'k*rzere Versandzeiten',
                      'lange Lieferzeit', 'schneller liefern', 'schnellere Produktlieferung')
keywords_df$delivery_time <- grepl(paste(multi_keywords10, collapse = "|"), 
                                   keywords_df$TEXT_CONTENT, ignore.case = TRUE)

# Shipping Cost - keyword 12

multi_keywords12 <- c('teure Lieferkosten', 'Versandkosten', 'Liefergeb*hr*', 'Versandgeg*hr*', 
                      'g*nstigere Lieferung', 'geringere Lieferkosten')
keywords_df$shipping_cost <- grepl(paste(multi_keywords12, collapse = "|"), 
                                   keywords_df$TEXT_CONTENT, ignore.case = TRUE)

# Payment - keyword 20

multi_keywords20 <- c('Zahlungsmodalit*ten', 'Zahlungsm*glichkeiten', 'Zahlungsmethoden', 'Zahlunssystem', 
                      'Bezahlm*glichkeiten', 'Bezahlungsarten', 'Zahlung', 'Bezahlen')
keywords_df$payment <- grepl(paste(multi_keywords20, collapse = "|"), 
                             keywords_df$TEXT_CONTENT, ignore.case = TRUE)

keywords_df$others <- NA
keywords_df$others <- ifelse(((keywords_df$delivery_time) == TRUE |
                                (keywords_df$shipping_cost) == TRUE  |(keywords_df$payment)  == TRUE), 'FALSE', 'TRUE')

# Creating the dataframe for first plot - actual count
# Removing unrequired columns

keywords_df_first <- keywords_df[,c(3,5:12)]
View(keywords_df_first)

keywords_df_first$others <- as.logical(
  as.character(keywords_df_first$others))
keywords_df_first <- keywords_df_first[,c(2,1,3:9)] 
cols <- sapply(keywords_df_first, is.logical)
keywords_df_first[,cols] <- lapply(keywords_df_first[,cols], as.numeric)

# Grouping and summing up

keywords_df_try <- keywords_df_first
View(keywords_df_try2)

keywords_df_try2 <- keywords_df_try %>%
  group_by(month, CATEGORY) %>%
  summarise(delivery_time = sum(delivery_time), 
            shipping_cost = sum(shipping_cost), payment = sum(payment), 
            others = sum(others)) %>%
  select(month, CATEGORY, delivery_time, shipping_cost, payment,
         others)

# Transposing data frame to treat keywords as columns

keywords_df_try3 <- melt(keywords_df_try2, id.vars = c('month', 'CATEGORY'))
View(keywords_df_try3)

# Creating dataframe for second plot - percentage count

keywords_df_try4 <- keywords_df_try3

keywords_df_try4$percentCount <- NA
keywords_df_try4 <- keywords_df_try4 %>%
  group_by(month) %>%
  mutate(percentCount = (round(value/ sum(value), 3))*100)
View(keywords_df_try4)

# Application part - Creating RShiny app

ui <- fluidPage(
  
  titlePanel("NPS Dasboard 2018"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("plotinput1", "Select month:", 
                  c(unique(as.character(keywords_df_try3$month))), 
                  selected = "August"), 
      
      checkboxGroupInput("plotinput2", "Select category:", 
                         c(unique(as.character(keywords_df_try3$CATEGORY))), 
                         selected = "Detractors")
      
    ),
    
    mainPanel(
      plotlyOutput("plotoutput1")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("plotinput3", "Select month:", 
                  c(unique(as.character(keywords_df_try4$month))), 
                  selected = "August"), 
      
      checkboxGroupInput("plotinput4", "Select category:", 
                         c(unique(as.character(keywords_df_try4$CATEGORY))), 
                         selected = "Detractors")    
    ),
    
    mainPanel(
      plotlyOutput("plotoutput2")
    )
  ) 
)

server <- shinyServer( function(input, output){
  
  chartInput1 <- reactive({
    input$plotinput1
  })
  
  chartInput2 <- reactive({
    input$plotinput2
  })
  
  output$plotoutput1 <- renderPlotly({
    
    keywords_df_try3 <- keywords_df_try3 %>%
      filter(month==chartInput1())
    
    keywords_df_try3 <- keywords_df_try3 %>%
      filter(CATEGORY==chartInput2())
    
    ggplot(keywords_df_try3, aes(x=variable, y=value, fill=CATEGORY)) +
      geom_bar(stat="identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1))   
  }
  )
  
  chartInput3 <- reactive({
    input$plotinput3
  })
  
  chartInput4 <- reactive({
    input$plotinput4
  })
  
  output$plotoutput2 <- renderPlotly({
    
    keywords_df_try4 <- keywords_df_try4 %>%
      filter(month==chartInput3())
    
    keywords_df_try4 <- keywords_df_try4 %>%
      filter(CATEGORY==chartInput4())
    
    ggplot(keywords_df_try4, aes(x=variable, y=percentCount, fill=CATEGORY)) +
      geom_bar(stat="identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) 
    #  + coord_cartesian(ylim = c(0, 50))
    
  }
  )
})

shinyApp(ui, server)
