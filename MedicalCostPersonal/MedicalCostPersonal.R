library(shiny)
library(dplyr)
library(corrplot)
library(ggplot2)

data <- read.csv(file="insurance.csv", header=TRUE, sep=",")
(colnames(data))
head(data)
ui<- fluidPage(
  titlePanel("analiza danych ze zbioru: Medical Cost Personal"),
  
  sidebarLayout(
    sidebarPanel (
      
      
      selectInput("varboxplot",
                  label = "wybierz zmienna ilosciowa do scharakteryzowania ",
                  choices = c(
                             "age",
                             "bmi",
                             "children",
                             "charges")),
                              
                  
      selectInput("varplot",
                label = "wybierz zmienna jakosciowa do scharakteryzowania",
                choices = c(
                            "sex",
                            "smoker",
                            "region")
    
                ),
      
      selectInput("varhistogram",
                  label = "wybierz zmienna dla ktorej chcesz zobaczyc histogram",
                  choices = c(
                    "age",
                    "bmi",
                    "children",
                    "charges")
                  ),
      
      sliderInput("range", 
                  label = "wiek:",
                  min = 18,
                  max = 64,
                  value = c(18,64))
    ),
    mainPanel (
      tabsetPanel(
        tabPanel(
          p("charakterystyka zmiennych ilosciowy"), 
          plotOutput("boxplot"),
          verbatimTextOutput("boxsummary")),
        
        tabPanel(
          p("charakterystyka zmiennych jakosciowych"),
          plotOutput("plot"),
          verbatimTextOutput("plotsummary")),
      
        tabPanel(
          p("histogram"),
          plotOutput("histogram")),
        
        tabPanel(
          p("zaleznosci miêdzy zmiennymi"),
          plotOutput("zalezoœci")),
        
        tabPanel(
          p("korelacje miêdzy zmiennymi"),
          plotOutput("korelacje")),
        
        tabPanel(
          p("wykres 3 zmiennych"),
          plotOutput("zmienne"))
          
        
          
      )
    )
  )
)


# Define server logic ----
server <- function(input, output) {
 
 
  output$boxplot <- renderPlot({
    dat <- switch(
           input$varboxplot,
           "age" = data$age,
           
           "bmi" = data$bmi,
           "children" = data$children,
          
           "charges" = data$charges
      
            )
            boxplot(dat)
  
  
        output$boxsummary = renderPrint({
        summary(dat)
    })
  })
  
  
  
  output$plot <- renderPlot({
      dat2<-switch(input$varplot,
                 "sex" = data$sex,
                 "smoker" = data$smoker,
                 "region" = data$region
                 )
      output$plotsummary <-renderPrint({
      (summary(dat2))
      })  
    
      plot(dat2)
  })
  
  output$histogram <- renderPlot({
    dat2<-switch(input$varhistogram,
                 
                 "age" = data$age,
                 
                 "bmi" = data$bmi,
                 "children" = data$children,
                 
                 "charges" = data$charges
    )
    
    
    hist(dat2)
  })
  
  output$zalezoœci <- renderPlot({
    
    par(mfrow=c(2, 3), mar=c(4, 4, 1, 1))
    plot(data$age, data$charges )
    plot(data$sex,data$charges )
    plot(data$bmi, data$charges )
    plot(data$children,data$charges )
    plot(data$smoker,data$charges )
    plot(data$region,data$charges )
  })
  
  
  
  
  output$korelacje <- renderPlot({
    a<-data.frame(data$age, data$bmi, data$children,  data$charges)
    b<-cor(a)
    corrplot(b)  
  })
  
  
  
  
  output$zmienne <- renderPlot({
    
    
    #barplot(data)
    tmp <- filter(data,
                  data$age <= input$range[2] & data$age >= input$range[1])
    ggplot(tmp, aes(x=bmi, y=charges, size = age),guide=FALSE) +
      geom_point(colour="white", aes(fill=age), shape=21) +theme_bw()
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)




