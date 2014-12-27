#download.file("http://rbidocs.rbi.org.in/rdocs/ATM/DOCs/ATMCS181214_SEP14.xls",
#dest = "ATMCS181214_SEP14.csv" )
library(ggplot2)
library(dplyr)
library(reshape2)
atm_card <- read.table(file = "C:\\Users\\Ravi\\Documents\\R_Data\\ATMCS310714.csv",
                       header = FALSE, sep= ",", skip = 5)
atm_card_tbldf <-tbl_df(atm_card)
atm_card_tbldf<- select(atm_card_tbldf, -c(1, 17:24))
ru<-which(atm_card_tbldf$V2=="Grand Total")
rl<-max(dim(atm_card_tbldf))
atm_card_tbldf <- atm_card_tbldf[-(ru:rl), ]

colnames(atm_card_tbldf) <- c("Bank", "ATM_ONSITE", "ATM_OFFSITE", "POS_ONLINE",
  "POS_OFFLINE", "CC_OUTSTANDING_CARD_COUNT",
  "CC_TRANS_ATM_COUNT","CC_TRANS_POS_COUNT",
  "CC_TRANS_ATM_AMOUNT","CC_TRANS_POS_AMOUNT",
  "DC_OUTSTANDING_CARD_COUNT",
  "DC_TRANS_ATM_COUNT","DC_TRANS_POS_COUNT",
  "DC_TRANS_ATM_AMOUNT","DC_TRANS_POS_AMOUNT")

atm_card_tbldf
x<-melt(atm_card_tbldf)


x$dev<-rep("NA", max(dim(x)))
x$card<-rep("NA", max(dim(x)))
x$device_location<-rep("NA", max(dim(x)))
x$metric <- rep("NA", max(dim(x)))

#tagging device type ATM or POS
x$dev[which(x$variable == "ATM_ONSITE"
            | x$variable == "ATM_OFFSITE" 
            | x$variable == "CC_TRANS_ATM_COUNT"
            | x$variable == "CC_TRANS_ATM_AMOUNT"
            | x$variable == "DC_TRANS_ATM_COUNT"
            | x$variable == "DC_TRANS_ATM_AMOUNT")]<- "ATM"

x$dev[which(x$variable == "POS_ONLINE"
            | x$variable == "POS_OFFLINE" 
            | x$variable == "CC_TRANS_POS_COUNT"
            | x$variable == "CC_TRANS_POS_AMOUNT"
            | x$variable == "DC_TRANS_POS_COUNT"
            | x$variable == "DC_TRANS_POS_AMOUNT")]<- "POS"

#tagging card type Debit or Credit
x$card[which(x$variable == "CC_OUTSTANDING_CARD_COUNT"
            | x$variable == "CC_TRANS_ATM_COUNT"
            | x$variable == "CC_TRANS_POS_COUNT"
            | x$variable == "CC_TRANS_ATM_AMOUNT"
            | x$variable == "CC_TRANS_POS_AMOUNT")] <- "Credit"

x$card[which(x$variable == "DC_OUTSTANDING_CARD_COUNT"
             | x$variable == "DC_TRANS_ATM_COUNT"
             | x$variable == "DC_TRANS_POS_COUNT"
             | x$variable == "DC_TRANS_ATM_AMOUNT"
             | x$variable == "DC_TRANS_POS_AMOUNT")] <- "Debit"

#tagging location of devices(online/offline/onsite/offsite)
x$device_location[which(x$variable == "ATM_ONSITE")] <- "ONSITE"
x$device_location[which(x$variable == "ATM_OFFSITE")] <- "OFFSITE"
x$device_location[which(x$variable == "POS_ONLINE")] <- "ONLINE"
x$device_location[which(x$variable == "POS_OFFLINE")] <- "OFFLINE"

x$metric[which(x$variable == "DC_OUTSTANDING_CARD_COUNT"
                          | x$variable == "CC_OUTSTANDING_CARD_COUNT")] <- "OUTSTANDING_CARD_COUNT"

x$metric[which(x$variable == "DC_TRANS_ATM_COUNT"
               | x$variable == "DC_TRANS_POS_COUNT"
               | x$variable == "CC_TRANS_ATM_COUNT"
               | x$variable == "CC_TRANS_POS_COUNT")] <- "transaction_count"

x$metric[which(x$variable == "DC_TRANS_ATM_AMOUNT"
               | x$variable == "DC_TRANS_POS_AMOUNT"
               | x$variable == "CC_TRANS_ATM_AMOUNT"
               | x$variable == "CC_TRANS_POS_AMOUNT")] <- "transaction_amount"


x$metric[which(x$variable == "ATM_ONSITE"
               | x$variable == "ATM_OFFSITE"
               | x$variable == "POS_ONLINE"
               | x$variable == "POS_OFFLINE")] <- "device_count"

x$dev_location <-NULL
x$variable <- NULL
View(x)

x<-tbl_df(x)

dev_count_tbl <- x %>%
                filter(metric == "device_count") %>% 
                mutate(device_count=value, month="2014-07-31") %>%
                select(-c(card, value, metric))
card_count_tbl <- x %>%
                  filter(metric == "OUTSTANDING_CARD_COUNT") %>% 
                  mutate(OUTSTANDING_CARD_COUNT=value, month="2014-07-31") %>%
                  select(-c(dev,device_location, value, metric))

transaction_count <- x %>%
                    filter(metric == "transaction_count") %>% 
                    mutate(transaction_count=value, month="2014-07-31") %>%
                    select(-c(device_location,metric, value))


transaction_amount <- x %>%
  filter(metric == "transaction_amount") %>% 
  mutate(transaction_amount=value, month="2014-07-31") %>%
  select(-c(device_location,metric, value))

library(RMySQL)

con <- dbConnect(MySQL(), user="root", password="123@morgan123",
                  dbname="card_stats")


Bank<-unique(x$Bank)
#dbWriteTable(con, "bank", data.frame(Bank), append = F, overwrite = T)
dbWriteTable(con, "dev_count_tbl", data.frame(dev_count_tbl), append = T, overwrite = F)
dbWriteTable(con, "card_count_tbl", data.frame(card_count_tbl), append = T, overwrite = F)
dbWriteTable(con, "transaction_count", data.frame(transaction_count), append = T, overwrite = F)
dbWriteTable(con, "transaction_amount", data.frame(transaction_amount),append = T, overwrite = F)

# dbListFields(con, "dev_count_tbl")
# 
# 
# dbDisconnect(con)
# 
# 
# dbListTables(con)







 
                








             

