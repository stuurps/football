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
  
  hme$TEAM_WIN <- ifelse(hme$HomeTeam == tmp$HomeTeam & hme$FTR == "H",1,
                         ifelse(hme$AwayTeam == tmp$HomeTeam & hme$FTR == "A",1,0))
  hme$HOME_WIN <- ifelse(hme$HomeTeam == tmp$HomeTeam & hme$FTR == "H",1,0)
  hme$AWAY_WIN <- ifelse(hme$AwayTeam == tmp$HomeTeam & hme$FTR == "A",1,0)
  
  hme$TEAM_DRAW <- ifelse(hme$HomeTeam == tmp$HomeTeam & hme$FTR == "D",1,
                          ifelse(hme$AwayTeam == tmp$HomeTeam & hme$FTR == "D",1,0))
  hme$HOME_DRAW <- ifelse(hme$HomeTeam == tmp$HomeTeam & hme$FTR == "D",1,0)
  hme$AWAY_DRAW <- ifelse(hme$AwayTeam == tmp$HomeTeam & hme$FTR == "D",1,0)
  
  hme$TEAM_LOSE <- ifelse(hme$HomeTeam == tmp$HomeTeam & hme$FTR == "A",1,
                          ifelse(hme$AwayTeam == tmp$HomeTeam & hme$FTR == "H",1,0))
  hme$HOME_LOSE <- ifelse(hme$HomeTeam == tmp$HomeTeam & hme$FTR == "A",1,0)
  hme$AWAY_LOSE <- ifelse(hme$AwayTeam == tmp$HomeTeam & hme$FTR == "H",1,0)
  
  hme <- hme[,9:ncol(hme)]
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
  
  awa <- subset(md, HomeTeam == tmp$AwayTeam  | AwayTeam == tmp$AwayTeam & Date < tmp$Date)
  awa <- awa[1:5,]
  
  awa$TEAM_WIN <- ifelse(awa$HomeTeam == tmp$AwayTeam & awa$FTR == "H",1,
                         ifelse(awa$AwayTeam == tmp$AwayTeam & awa$FTR == "A",1,0))
  awa$HOME_WIN <- ifelse(awa$HomeTeam == tmp$AwayTeam & awa$FTR == "H",1,0)
  awa$AWAY_WIN <- ifelse(awa$AwayTeam == tmp$AwayTeam & awa$FTR == "A",1,0)
  
  awa$TEAM_DRAW <- ifelse(awa$HomeTeam == tmp$AwayTeam & awa$FTR == "D",1,
                          ifelse(awa$AwayTeam == tmp$AwayTeam & awa$FTR == "D",1,0))
  awa$HOME_DRAW <- ifelse(awa$HomeTeam == tmp$AwayTeam & awa$FTR == "D",1,0)
  awa$AWAY_DRAW <- ifelse(awa$AwayTeam == tmp$AwayTeam & awa$FTR == "D",1,0)
  
  awa$TEAM_LOSE <- ifelse(awa$HomeTeam == tmp$AwayTeam & awa$FTR == "A",1,
                          ifelse(awa$AwayTeam == tmp$AwayTeam & awa$FTR == "H",1,0))
  awa$HOME_LOSE <- ifelse(awa$HomeTeam == tmp$AwayTeam & awa$FTR == "A",1,0)
  awa$AWAY_LOSE <- ifelse(awa$AwayTeam == tmp$AwayTeam & awa$FTR == "H",1,0)
  
  awa <- awa[,9:ncol(awa)]
  awa_sc <- as.data.frame(1)
  for(z in 1:nrow(awa)){
    awa_sub <- awa[z]
    names(awa_sub) <- paste0("AWAY_",names(awa_sub),"X",z)
    awa_sc <- cbind(awa_sc,awa_sub)
  }
  
  awa_form <- rbind(awa_form,awa_sc)
}

df <- cbind(md,hme_form,awa_form)
df$`1` <- NULL
df$`1` <- NULL

head(df)

df$FIVE_HOME_FORM_W <- df$HOME_TEAM_WINX1 + df$HOME_TEAM_WINX2 + df$HOME_TEAM_WINX3 + df$HOME_TEAM_WINX4 + df$HOME_TEAM_WINX5
df$FIVE_HOME_FORM_D <- df$HOME_TEAM_DRAWX1 + df$HOME_TEAM_DRAWX2 + df$HOME_TEAM_DRAWX3 + df$HOME_TEAM_DRAWX4 + df$HOME_TEAM_DRAWX5
df$FIVE_HOME_FORM_L <- df$HOME_TEAM_LOSEX1 + df$HOME_TEAM_LOSEX2 + df$HOME_TEAM_LOSEX3 + df$HOME_TEAM_LOSEX4 + df$HOME_TEAM_LOSEX5
df$FIVE_AWAY_FORM_W <- df$AWAY_TEAM_WINX1 + df$AWAY_TEAM_WINX2 + df$AWAY_TEAM_WINX3 + df$AWAY_TEAM_WINX4 + df$AWAY_TEAM_WINX5
df$FIVE_AWAY_FORM_D <- df$AWAY_TEAM_DRAWX1 + df$AWAY_TEAM_DRAWX2 + df$AWAY_TEAM_DRAWX3 + df$AWAY_TEAM_DRAWX4 + df$AWAY_TEAM_DRAWX5
df$FIVE_AWAY_FORM_L <- df$AWAY_TEAM_LOSEX1 + df$AWAY_TEAM_LOSEX2 + df$AWAY_TEAM_LOSEX3 + df$AWAY_TEAM_LOSEX4 + df$AWAY_TEAM_LOSEX5
  
#Convert all to factors
for(i in names(df)){
  df[[i]] <- as.factor(as.character(df[[i]]))
}

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
y <- "FTR"
x <- setdiff(names(df), y)
x <- setdiff(x, c("Date","FTHG","FTAG", "ID"))
print(x)

table(df$FTR)
split <- createDataPartition(df$FTR, p = 0.7, list = F)

md_train <- df[split, ]
md_test <- df[-split, ]

h2o.init(max_mem_size = "500G", nthreads = -1)
train_h2o <- as.h2o(md_train)
test_h2o <- as.h2o(md_test)

aml <- h2o.automl(x = x, y = y, training_frame = train_h2o,
                  nfolds = 5, max_runtime_secs = ((60*60)*9),
                  seed = 1234)


aml@leaderboard
aml@leader
h2o.varimp_plot(aml@leader)
h2o.download_mojo(aml@leader, get_genmodel_jar = TRUE)

h2o.shutdown(prompt = F)
