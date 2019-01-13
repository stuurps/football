library(h2o)
library(data.table)
library(caret)

setwd("~/Documents/betting")

rm(list = ls())

######
###### ACCA
######

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

fixture <- fread("http://www.football-data.co.uk/fixtures.csv")

md <- rbind(md,e,e1,e2,e3,e4,e5,e6,e7, fill = T)
md$Date <- as.Date(md$Date, format = "%d/%m/%Y")
head(md)

md <- subset(md, select = c(Div, Date, HomeTeam,AwayTeam, FTHG,FTAG,FTR))
md <- md[order(-md$Date),]
md <- subset(md, FTR != "")
md$RECENT <- ifelse(lubridate::year(md$Date) == 2018 | lubridate::year(md$Date) == 2019,1,0)

fix <- fixture
fix <- subset(fix, Div == "E1" |  Div == "E2" | Div == "E3")
fix <- fix[order(-fix$Date),]
temp_f <- fix

fix <- subset(fix, select = c(Div, Date, HomeTeam,AwayTeam, FTHG,FTAG,FTR))


pb <- txtProgressBar(min = 1, max = nrow(fix), initial = 1, style = 3) 

hme_form <- NULL
for(i in 1:nrow(fix)){
  setTxtProgressBar(pb,i)
  tmp <- fix[i]
  
  hme <- subset(md, HomeTeam == tmp$HomeTeam | AwayTeam == tmp$HomeTeam)
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
  
  hme <- hme[,8:ncol(hme)]
  hme_sc <- as.data.frame(1)
  for(z in 1:nrow(hme)){
    hme_sub <- hme[z]
    names(hme_sub) <- paste0("HOME_",names(hme_sub),"X",z)
    hme_sc <- cbind(hme_sc,hme_sub)
  }
  
  hme_form <- rbind(hme_form,hme_sc)
}

awa_form <- NULL
for(i in 1:nrow(fix)){
  setTxtProgressBar(pb,i)
  tmp <- fix[i]
  
  awa <- subset(md, HomeTeam == tmp$AwayTeam  | AwayTeam == tmp$AwayTeam)
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
  
  awa <- awa[,8:ncol(awa)]
  awa_sc <- as.data.frame(1)
  for(z in 1:nrow(awa)){
    awa_sub <- awa[z]
    names(awa_sub) <- paste0("AWAY_",names(awa_sub),"X",z)
    awa_sc <- cbind(awa_sc,awa_sub)
  }
  
  awa_form <- rbind(awa_form,awa_sc)
}

fix <- cbind(fix,hme_form,awa_form)
fix$`1` <- NULL
fix$`1` <- NULL

head(fix)
fix$RECENT <- "1"

fix$FIVE_HOME_FORM_W <- fix$HOME_TEAM_WINX1 + fix$HOME_TEAM_WINX2 + fix$HOME_TEAM_WINX3 + fix$HOME_TEAM_WINX4 + fix$HOME_TEAM_WINX5
fix$FIVE_HOME_FORM_D <- fix$HOME_TEAM_DRAWX1 + fix$HOME_TEAM_DRAWX2 + fix$HOME_TEAM_DRAWX3 + fix$HOME_TEAM_DRAWX4 + fix$HOME_TEAM_DRAWX5
fix$FIVE_HOME_FORM_L <- fix$HOME_TEAM_LOSEX1 + fix$HOME_TEAM_LOSEX2 + fix$HOME_TEAM_LOSEX3 + fix$HOME_TEAM_LOSEX4 + fix$HOME_TEAM_LOSEX5
fix$FIVE_AWAY_FORM_W <- fix$AWAY_TEAM_WINX1 + fix$AWAY_TEAM_WINX2 + fix$AWAY_TEAM_WINX3 + fix$AWAY_TEAM_WINX4 + fix$AWAY_TEAM_WINX5
fix$FIVE_AWAY_FORM_D <- fix$AWAY_TEAM_DRAWX1 + fix$AWAY_TEAM_DRAWX2 + fix$AWAY_TEAM_DRAWX3 + fix$AWAY_TEAM_DRAWX4 + fix$AWAY_TEAM_DRAWX5
fix$FIVE_AWAY_FORM_L <- fix$AWAY_TEAM_LOSEX1 + fix$AWAY_TEAM_LOSEX2 + fix$AWAY_TEAM_LOSEX3 + fix$AWAY_TEAM_LOSEX4 + fix$AWAY_TEAM_LOSEX5

#Convert all to factors
for(i in names(fix)){
  fix[[i]] <- as.factor(as.character(fix[[i]]))
}

##Bring in odds
fix$RN <- 1:nrow(fix)
fix$ID <- paste0(fix$HomeTeam, fix$AwayTeam, fix$RN)
fix$RN <- NULL

temp_f$RN <- 1:nrow(temp_f)
temp_f$ID <- paste0(temp_f$HomeTeam, temp_f$AwayTeam, temp_f$RN)
temp_f <- subset(temp_f, select = -c(Div,Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR,HTHG,HTAG,HTR, RN))

fix <- merge(fix,temp_f, by = "ID" )

pred <- h2o.mojo_predict_df(fix,"/Users/stuartbarker/Documents/betting/DeepLearning_grid_1_AutoML_20190111_073728_model_29.zip")

table(pred$predict)

fixv2 <- subset(fix, select = c(HomeTeam,AwayTeam, Date, Div))
pred2 <- cbind(fixv2,pred)
pred2 <- pred2[order(pred2$HomeTeam),]
acca_pred <- pred2

rm(list=setdiff(ls(), c("acca_pred","md", "fixture", "temp_f")))

######
###### BTTS
######

fix <- fixture
fix <- subset(fix, Div == "E1" |  Div == "E2" | Div == "E3")

fix <- subset(fix, select = c(Div, Date, HomeTeam,AwayTeam, FTHG,FTAG,FTR))
fix <- fix[order(-fix$Date),]

pb <- txtProgressBar(min = 1, max = nrow(fix), initial = 1, style = 3) 

hme_form <- NULL
for(i in 1:nrow(fix)){
  setTxtProgressBar(pb,i)
  tmp <- fix[i]
  
  hme <- subset(md, HomeTeam == tmp$HomeTeam | AwayTeam == tmp$HomeTeam & Date < tmp$Date)
  hme <- hme[1:5,]
  hme$GOAL <- ifelse(hme$HomeTeam == tmp$HomeTeam, hme$FTHG, hme$FTAG)
  hme$CONCEEDED <- ifelse(hme$HomeTeam == tmp$HomeTeam, hme$FTAG, hme$FTHG)
  
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
for(i in 1:nrow(fix)){
  setTxtProgressBar(pb,i)
  tmp <- fix[i]
  
  awa <- subset(md, HomeTeam == tmp$AwayTeam | AwayTeam == tmp$AwayTeam & Date < tmp$Date)
  awa <- awa[1:5,]
  awa$GOAL <- ifelse(awa$HomeTeam == tmp$AwayTeam, awa$FTHG, awa$FTAG)
  awa$CONCEEDED <- ifelse(awa$HomeTeam == awa$HomeTeam, awa$FTAG, awa$FTHG)
  
  awa <- subset(awa, select = c(GOAL, CONCEEDED))
  awa_sc <- as.data.frame(1)
  for(z in 1:nrow(awa)){
    awa_sub <- awa[z]
    names(awa_sub) <- paste0("AWAY_",names(awa_sub),"X",z)
    awa_sc <- cbind(awa_sc,awa_sub)
  }
  
  awa_form <- rbind(awa_form,awa_sc)
}

fix <- cbind(fix,hme_form,awa_form)
fix$`1` <- NULL
fix$`1` <- NULL

head(fix)
fix$RECENT <- "1"

fix$HOME_SCORED <- rowSums(fix[,8:12] > 0)
fix$AWAY_SCORED <- rowSums(fix[,13:17] > 0)

fix$RN <- 1:nrow(fix)
fix$ID <- paste0(fix$HomeTeam, fix$AwayTeam, fix$RN)
fix$RN <- NULL
fix <- merge(fix,temp_f, by = "ID" )


pred <- h2o.mojo_predict_df(fix,"/Users/stuartbarker/Documents/betting/GLM_grid_1_AutoML_20190111_204454_model_1.zip")
table(pred$predict)
get
fixv2 <- subset(fix, select = c(HomeTeam,AwayTeam, Date, Div))
pred2 <- cbind(fixv2,pred)
pred2 <- pred2[order(pred2$HomeTeam),]
print(pred2)

#Optimise
rm(list=setdiff(ls(), c("acca_pred","md", "pred2")))

pred2 <- subset(pred2, select = c(HomeTeam, predict, X0, X1))
colnames(pred2)[2] <- "BTTS_PREDICT"
colnames(pred2)[3] <- "BTTS_0"
colnames(pred2)[4] <- "BTTS_1"

pred2 <- merge(acca_pred, pred2, by = "HomeTeam")

pred2$PROB <- ifelse(pred2$predict == "H", pred2$H,
                     ifelse(pred2$predict == "D", pred2$D,
                            ifelse(pred2$predict == "A", pred2$A,NA)))

pred2 <- pred2[order(pred2$Div),]
pred2
write.csv(pred2, "prediction.csv", row.names = F)


