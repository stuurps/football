#https://www.football-data.org/documentation/quickstart for live scores
#http://www.football-data.co.uk/englandm.php for historic

library(h2o)
library(data.table)
library(caret)

setwd("~/Documents/betting")

rm(list = ls())

#Championship
e <- fread("http://www.football-data.co.uk/mmz4281/1819/E1.csv")
e1 <- fread("http://www.football-data.co.uk/mmz4281/1718/E1.csv")
e2 <- fread("http://www.football-data.co.uk/mmz4281/1617/E1.csv")
e3 <- fread("http://www.football-data.co.uk/mmz4281/1516/E1.csv")
e4 <- fread("http://www.football-data.co.uk/mmz4281/1415/E1.csv")
e5 <- fread("http://www.football-data.co.uk/mmz4281/1314/E1.csv")
e6 <- fread("http://www.football-data.co.uk/mmz4281/1213/E1.csv")
e7 <- fread("http://www.football-data.co.uk/mmz4281/1112/E1.csv")

md <- rbind(e,e1,e2,e3,e4,e5,e6,e7, fill = T)

#League1
e <- fread("http://www.football-data.co.uk/mmz4281/1819/E2.csv")
e1 <- fread("http://www.football-data.co.uk/mmz4281/1718/E2.csv")
e2 <- fread("http://www.football-data.co.uk/mmz4281/1617/E2.csv")
e3 <- fread("http://www.football-data.co.uk/mmz4281/1516/E2.csv")
e4 <- fread("http://www.football-data.co.uk/mmz4281/1415/E2.csv")
e5 <- fread("http://www.football-data.co.uk/mmz4281/1314/E2.csv")
e6 <- fread("http://www.football-data.co.uk/mmz4281/1213/E2.csv")
e7 <- fread("http://www.football-data.co.uk/mmz4281/1112/E2.csv")

md <- rbind(md,e,e1,e2,e3,e4,e5,e6,e7, fill = T)

#League2
e <- fread("http://www.football-data.co.uk/mmz4281/1819/E3.csv")
e1 <- fread("http://www.football-data.co.uk/mmz4281/1718/E3.csv")
e2 <- fread("http://www.football-data.co.uk/mmz4281/1617/E3.csv")
e3 <- fread("http://www.football-data.co.uk/mmz4281/1516/E3.csv")
e4 <- fread("http://www.football-data.co.uk/mmz4281/1415/E3.csv")
e5 <- fread("http://www.football-data.co.uk/mmz4281/1314/E3.csv")
e6 <- fread("http://www.football-data.co.uk/mmz4281/1213/E3.csv")
e7 <- fread("http://www.football-data.co.uk/mmz4281/1112/E3.csv")

md <- rbind(md,e,e1,e2,e3,e4,e5,e6,e7, fill = T)
md$Date <- as.Date(md$Date, format = "%d/%m/%Y")
head(md)

md <- md[order(-md$Date),]
md <- subset(md, FTR != "")

tmp_f <- md
md <- subset(md, select = c(Div, Date, HomeTeam,AwayTeam, FTHG,FTAG,FTR))

md$RECENT <- ifelse(lubridate::year(md$Date) == 2018 | lubridate::year(md$Date) == 2019,"1","0")

pb <- txtProgressBar(min = 1, max = nrow(md), initial = 1, style = 3) 

hme_form <- NULL
for(i in 1:nrow(md)){
  setTxtProgressBar(pb,i)
  tmp <- md[i]
  
  hme <- subset(md, HomeTeam == tmp$HomeTeam | AwayTeam == tmp$HomeTeam & Date < tmp$Date)
  hme <- hme[1:5,]
  hme$GOAL <- ifelse(hme$HomeTeam == tmp$HomeTeam, hme$FTHG, hme$FTAG)
  hme$CONCEEDED <- ifelse(hme$HomeTeam == tmp$HomeTeam, hme$FTAG, hme$FTHG)
  #hme <- hme[order(-hme$Date),]
  
  hme <- subset(hme, select = c(GOAL, CONCEEDED))
  hme_sc <- as.data.frame(1)
  for(z in 1:nrow(hme)){
    hme_sub <- hme[z]
    names(hme_sub) <- paste0("HOME_",names(hme_sub),"X",z)
    hme_sc <- cbind(hme_sc,hme_sub)
  }
  
  hme_form <- rbind(hme_form,hme_sc)
}

awa_form <- NULL
for(i in 1:nrow(md)){
  setTxtProgressBar(pb,i)
  tmp <- md[i]
  
  awa <- subset(md, HomeTeam == tmp$AwayTeam | AwayTeam == tmp$AwayTeam & Date < tmp$Date)
  awa <- awa[1:5,]
  awa$GOAL <- ifelse(awa$HomeTeam == tmp$AwayTeam, awa$FTHG, awa$FTAG)
  awa$CONCEEDED <- ifelse(awa$HomeTeam == awa$HomeTeam, awa$FTAG, awa$FTHG)
  #hme <- hme[order(-hme$Date),]
  
  awa <- subset(awa, select = c(GOAL, CONCEEDED))

  awa_sc <- as.data.frame(1)
  for(z in 1:nrow(awa)){
    awa_sub <- awa[z]
    names(awa_sub) <- paste0("AWAY_",names(awa_sub),"X",z)
    awa_sc <- cbind(awa_sc,awa_sub)
  }
  
  awa_form <- rbind(awa_form,awa_sc)
}

#all form


df <- cbind(md,hme_form,awa_form)
df$`1` <- NULL
df$`1` <- NULL

df$HOME_SCORED <- rowSums(df[,9:13] > 0)
df$AWAY_SCORED <- rowSums(df[,14:18] > 0)

head(df)

df$BTTS <- as.factor(ifelse(df$FTHG > 0 & df$FTAG > 0 , "1", "0"))

##Bring in odds
df$RN <- 1:nrow(df)
df$ID <- paste0(df$HomeTeam, df$AwayTeam, df$RN)
df$RN <- NULL
fixture <- fread("http://www.football-data.co.uk/fixtures.csv")
tmp_f <- as.data.frame(tmp_f)
fixture <- as.data.frame(fixture)
tmp_f <- tmp_f[,names(tmp_f) %in% names(fixture)]
tmp_f$RN <- 1:nrow(tmp_f)
tmp_f$ID <- paste0(tmp_f$HomeTeam, tmp_f$AwayTeam, tmp_f$RN)
tmp_f <- subset(tmp_f, select = -c(Div,Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR,HTHG,HTAG,HTR, RN))

nrow(tmp_f)
nrow(df)

df <- merge(df,tmp_f, by = "ID" )

#Build Model
y <- "BTTS"
x <- setdiff(names(df), y)
x <- setdiff(x, c("Date","FTHG","FTAG", "FTR", "ID"))
print(x)

table(df$BTTS)
split <- createDataPartition(df$BTTS, p = 0.7, list = F)

md_train <- df[split, ]
md_test <- df[-split, ]

h2o.init(max_mem_size = "200G")
train_h2o <- as.h2o(md_train)
test_h2o <- as.h2o(md_test)

btts <- h2o.automl(x = x, y = y, training_frame = train_h2o,
                  nfolds = 5, stopping_metric = "logloss", max_runtime_secs = ((60*60)*7),
                  seed = 1235)

btts@leaderboard
btts@leader
h2o.varimp_plot(btts@leader)
h2o.download_mojo(btts@leader, get_genmodel_jar = TRUE)

h2o.shutdown(prompt = F)

#Features to include
#League position
#Form
#Points
#Month
#Game Num
#Player ratings previous game?
#Home form and away form and both. And previous number of goals scored
#Last 5 and last 10