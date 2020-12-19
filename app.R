#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(arules)
library(arulesViz)

# Mean/ Median for Visualization
# 
employee <- read.csv("employee.csv", header = T, stringsAsFactors = T)
employee_num <- read.csv("employee1.csv", header = T, stringsAsFactors = T)
employee_num <- employee_num[complete.cases(employee_num) == T,]

# Define UI for application that draws a histogram
ui <- navbarPage(
  tabPanel("Welcome Page",
           h4("IST707 - Data Analytics Project")),
  tabPanel("Dataset",
           sidebarLayout(
             sidebarPanel(
               radioButtons("dataset", "Datasets:"
                            , c("Original" = "employee_num",
                                "Discretized" = "employee"))
             ),
             mainPanel(
               dataTableOutput("data") 
             )
          )),
  tabPanel("Summary",
           sidebarLayout(
             sidebarPanel(
               radioButtons("summary_data", "Datasets:"
                            , c("Original" = "employee_num",
                                "Discretized" = "employee"))
             ),
             mainPanel(
               verbatimTextOutput("summary") 
             )
           )),
  navbarMenu("EDA",
             tabPanel("Binning",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("discret", "Discretized Distribution:", 
                                    choices=colnames(employee)[2:length(colnames(employee))])
                        ),
                        mainPanel(
                          plotOutput("barPlot")
                        )
                      )
                      ),
             tabPanel("Visualizing Distributions",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("field1", "Select the field for Distribution",
                                      choices = colnames(employee_num)[2:dim(employee_num)[2]]),
                          radioButtons("plottype", "Select Distribution Type : ",
                                       c("Histogram" = "histogram",
                                         "Boxplot" = "boxplot",
                                         "Density" ="density")),
                          sliderInput("quantile", "Value at selected percentile is",
                                      min = 0, max = 100, value = 5),
                          h4("Range of Data"),
                          verbatimTextOutput("range"),
                          h4("Percetile"),
                          verbatimTextOutput("percentile")
                        ),
                        mainPanel(
                          plotOutput("distributionPlot"),
                          h4("Mean"),
                          verbatimTextOutput("mean"),
                          h4("Median"),
                          verbatimTextOutput("median"),
                          h4("Standard Deviation"),
                          verbatimTextOutput("sd")
                        )
                      )
                      ),
             tabPanel("Relation between Attributes",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("scatter_x", "X Axis", 
                                      choices=colnames(employee_num)[2:length(colnames(employee_num))]),
                          selectInput("scatter_y", "Y Axis", 
                                      choices=colnames(employee_num)[2:length(colnames(employee_num))])
                        ),
                        mainPanel(
                          plotOutput("scatterPlot")
                        )
                      )
                      )
             ),
  tabPanel("Association Rules",
           sidebarLayout(
             sidebarPanel(
               radioButtons("yesorno", "Select Attrition Options:"
                            , c("Yes" = "Yes",
                                "No" = "No"), inline = T),
               sliderInput("supp", "Slide to pick the support",
                           min = 0.01, max = 1, value = 0.1, step = 0.005),
               sliderInput("conf", "Slide to pick the Confidence",
                           min = 0.01, max = 1, value = 0.1, step = 0.005),
               sliderInput("mlen", "Minimum length of a rule :",
                           min = 1, max = 20, value = 2, step = 1),
               sliderInput("Mlen", "Maximum length of a rule :",
                           min = 1, max = 20, value = 5, step = 1),
               checkboxInput("all", "Show all rules ? ", T),
               conditionalPanel(
                 condition = "input.all == false",
                 sliderInput("head", "Minimum Number of Rules :",
                             min = 1, max = 100, value = 2, step = 1)
                 ),
               plotOutput("rulesPlot")
             ),
             mainPanel(
               verbatimTextOutput("rules")
             )
           ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$val, {
    if(input$all == T){
      shinyjs::disable("head")
    }else{
      shinyjs::enable("head")
    }
  })
   output$data <- renderDataTable(
     if (input$dataset == "employee") {
       employee[,2:dim(employee)[2]]
     }else {
       employee_num[,2:dim(employee_num)[2]]
     }
   )
   
   output$summary <- renderPrint(
     if (input$summary_data == "employee") {
       summary(employee[,2:dim(employee)[2]])
     }else {
       summary(employee_num[,2:dim(employee_num)[2]])
     }
   )
   
   output$barPlot <- renderPlot({
     par(bty = "n")
      x <- table(employee[,input$discret])
      barplot(x[], col = "darkcyan", horiz = T)
   })
   
   output$distributionPlot <- renderPlot({
     par(bty = "n")
     x <- employee_num[,input$field1]
     
    if (input$plottype == "histogram") {
      hist(x, col = "darkcyan", main = paste("Distribution : Histogram for ",input$field1), xlab = input$field1)
      abline(v = quantile(employee_num[,input$field1], input$quantile/100), col = "red", lty = 3, lwd = 2)
    }else{
      if (input$plottype == "boxplot"){
        boxplot(x, col = "darkcyan", horizontal = T, main = paste("Distribution : Boxplot for ",input$field1), xlab = input$field1)
        abline(v = quantile(employee_num[,input$field1], input$quantile/100), col = "red", lty = 3, lwd = 2)
        }else{
          plot(density(x),  col = "darkcyan",main = paste("Distribution : Boxplot for ",input$field1), xlab = input$field1)
          abline(v = quantile(employee_num[,input$field1], input$quantile/100), col = "red", lty = 3, lwd = 2)
      }
    }
   })
   
   output$mean <- renderPrint(
     mean(employee_num[,input$field1])
   )
   
   output$median <- renderPrint(
     median(employee_num[,input$field1])
   )
   
   output$sd <- renderPrint(
     sd(employee_num[,input$field1])
   )
   
   output$percentile <- renderText(
     quantile(employee_num[,input$field1], input$quantile/100)
   )
   
   output$range <- renderText({
     x <- employee_num[,input$field1]
     paste("from",min(x), "to", max(x))
   })
   
   output$scatterPlot <- renderPlot({
     x <- employee_num[,input$scatter_x]
     y <- employee_num[,input$scatter_y]
     plot(x,y, pch = 16, col = "grey")
     abline(lm(employee_num, formula = y ~ x), lty = 3, lwd = 2, col = "red")
   })
   
   output$rules <- renderPrint({
     if (input$mlen > input$Mlen) {
       print("Maximum length of Rules should be greater than Minimum length of Rules")
     }
     else{
       if (input$all == T) {
         rules_all <- apriori(employee, parameter = list(supp = input$supp
                                                         , conf = input$conf
                                                         , minlen = input$mlen
                                                         , maxlen = input$Mlen
                                                         , maxtime = 0
                                                         , target = "rules")
                              , appearance = list(rhs = paste0("Attrition=",input$yesorno)))
         inspect(sort(rules_all, decreasing = T, by = "confidence"))
       }else{
         rules_all <- apriori(employee, parameter = list(supp = input$supp
                                                         , conf = input$conf
                                                         , minlen = input$mlen
                                                         , maxlen = input$Mlen
                                                         , maxtime = 0
                                                         , target = "rules")
                              , appearance = list(rhs = paste0("Attrition=",input$yesorno, sep = "")))
         #inspect(rules_all[1:input$head])
         if (input$head <= length(rules_all)) {
           inspect(sort(rules_all, decreasing = T, by = "confidence")[1:input$head])
         }else{
           inspect(sort(rules_all, decreasing = T, by = "confidence"))
         }
       }
     }
     
    })
   
   output$rulesPlot <- renderPlot({
     if (input$mlen > input$Mlen) {
       print("Maximum length of Rules should be greater than Minimum length of Rules")
     }
     else{
       if (input$all == T) {
         rules_all <- apriori(employee, parameter = list(supp = input$supp
                                                         , conf = input$conf
                                                         , minlen = input$mlen
                                                         , maxlen = input$Mlen
                                                         , maxtime = 0
                                                         , target = "rules")
                              , appearance = list(rhs = paste0("Attrition=",input$yesorno)))
         if (length(rules_all) != 0) {
           plot(sort(rules_all, decreasing = T, by = "confidence"), main = "Distribution of rules w.r.t. Support & Confidence")
         }
      }else{
         rules_all <- apriori(employee, parameter = list(supp = input$supp
                                                         , conf = input$conf
                                                         , minlen = input$mlen
                                                         , maxlen = input$Mlen
                                                         , maxtime = 0
                                                         , target = "rules")
                              , appearance = list(rhs = paste0("Attrition=",input$yesorno, sep = "")))
         #inspect(rules_all[1:input$head])
         if (input$head <= length(rules_all)) {
           if (length(rules_all) != 0) {
            plot(sort(rules_all, decreasing = T, by = "confidence")[1:input$head], main = "Distribution of rules w.r.t. Support & Confidence")
           }
         }else{
           if (length(rules_all) != 0) {
             plot(sort(rules_all, decreasing = T, by = "confidence"), main = "Distribution of rules w.r.t. Support & Confidence")
           }
         }
       }
     }
     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

