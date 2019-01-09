library(shiny)
library(data.table)
library(DT)

rm(list = ls())
setwd("~/Documents/betting")
md <- fread("prediction.csv")
md$BTTS_PREDICT <- ifelse(md$BTTS_PREDICT > 0.5,1,0)

#Build confidence feature
md$Confidence1 <- md$PROB - md$A
md$Confidence2 <- md$PROB - md$D
md$Confidence3 <- md$PROB - md$H

md$Confidence1 <- ifelse(md$Confidence1 == 0, 2, md$Confidence1)
md$Confidence2 <- ifelse(md$Confidence2 == 0, 2, md$Confidence2)
md$Confidence3 <- ifelse(md$Confidence3 == 0, 2, md$Confidence3)

md$Confidence <- round((apply(md[,13:15], 1, FUN=min)) * 100, 2)
md$Confidence1 <- NULL
md$Confidence2 <- NULL
md$Confidence3 <- NULL

#Tidy
md$BTTS_0 <- NULL

md$A <- NULL
md$D <- NULL
md$H <- NULL
md$BTTS_1 <- round(md$BTTS_1 * 100,2)
md$PROB <- round(md$PROB * 100,2)

md <- md[,c(1,2,3,5,8,9,6,7,4)]

colnames(md)[1] <- "Home Team"
colnames(md)[2] <- "Away Team"
colnames(md)[4] <- "Prediction"
colnames(md)[5] <- "Probability"
colnames(md)[6] <- "Confidence"
colnames(md)[7] <- "BTTS Prediction"
colnames(md)[8] <- "BTTS Probability"

ch <- subset(md, Div == "E1")
l1 <- subset(md, Div == "E2")
l2 <- subset(md, Div == "E3")

ch$Div <- NULL
l1$Div <- NULL
l2$Div <- NULL

ch <- datatable(ch,options = list(pageLength = 15))
l1 <- datatable(l1,options = list(pageLength = 15))
l2 <- datatable(l2,options = list(pageLength = 15))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$CH <- DT::renderDataTable({ch})
  output$L1 <- DT::renderDataTable({l1})
  output$L2 <- DT::renderDataTable({l2})
  
})
