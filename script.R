link <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(link, destfile = "dataset.csv.bz2")
StormDB <- read.csv("dataset.csv.bz2")
library(ggplot2)

colnames(StormDB)

event_types <- as.data.frame(table(StormDB$EVTYPE))
event_types <- event_types[order(event_types$Var1), ]

StormDB$EVTYPE <- as.character(StormDB$EVTYPE)
StormDB$EVTYPE[grepl("/|&|and", StormDB$EVTYPE,ignore.case = TRUE)] <- "Multiple Event"
StormDB$EVTYPE[grepl("volc", StormDB$EVTYPE,ignore.case = TRUE)] <- "Volcano"
StormDB$EVTYPE[grepl("wind|wnd", StormDB$EVTYPE,ignore.case = TRUE)] <- "WIND"
StormDB$EVTYPE[grepl("funnel|tornado", StormDB$EVTYPE,ignore.case = TRUE)] <- "Tornado"
StormDB$EVTYPE[grepl("glaze", StormDB$EVTYPE,ignore.case = TRUE)] <- "Glaze"
StormDB$EVTYPE[grepl("hail", StormDB$EVTYPE,ignore.case = TRUE)] <- "Hail"
StormDB$EVTYPE[grepl("dust", StormDB$EVTYPE,ignore.case = TRUE)]  <- "DUST"
StormDB$EVTYPE[grepl("flood", StormDB$EVTYPE,ignore.case = TRUE)] <- "FLOOD"
StormDB$EVTYPE[grepl("ic(e|y)", StormDB$EVTYPE,ignore.case = TRUE)] <- "Ice"
StormDB$EVTYPE[grepl("fire|smoke", StormDB$EVTYPE,ignore.case = TRUE)] <- "FIRE"
StormDB$EVTYPE[grepl("thunder", StormDB$EVTYPE,ignore.case = TRUE)] <- "Thunder Storm"
StormDB$EVTYPE[grepl("slide|eros", StormDB$EVTYPE,ignore.case = TRUE)] <- "Erosion"
StormDB$EVTYPE[grepl("rain", StormDB$EVTYPE,ignore.case = TRUE)] <- "Rain"
StormDB$EVTYPE[grepl("freez|cold|snow|chill|winter", StormDB$EVTYPE,ignore.case = TRUE)] <- "Cold Weather"
StormDB$EVTYPE[grepl("TROPICAL.STORM", StormDB$EVTYPE,ignore.case = TRUE)] <- "TROPICAL STORM"
StormDB$EVTYPE[grepl("heat", StormDB$EVTYPE,ignore.case = TRUE)] <- "Heat"
StormDB$EVTYPE[grepl("(hurri|opal)", StormDB$EVTYPE,ignore.case = TRUE)] <- "Hurricane"

health <- StormDB[,(c(8,23:24))]
property<-StormDB[,c(8,25:28)]

table(property$PROPDMGEXP)
table(property$CROPDMGEXP)

property$PROPDMGEXP<-factor(property$PROPDMGEXP,levels=c("H","K","M","B","h","m","O"))
property$PROPDMGEXP[is.na(property$PROPDMGEXP)] <- "O"

property$CROPDMGEXP<-factor(property$CROPDMGEXP,levels=c("K","M","B","k","m","O"))
property$CROPDMGEXP[is.na(property$CROPDMGEXP)] <- "O"

property$PROPDMGEXP <- as.character(property$PROPDMGEXP)
property$CROPDMGEXP <- as.character(property$CROPDMGEXP)

property$PROPDMGMLT <- 0
property$CROPDMGMLT <- 0

property$PROPDMGMLT[grepl("h", property$PROPDMGEXP,ignore.case = TRUE)]<-100
property$PROPDMGMLT[grepl("k", property$PROPDMGEXP,ignore.case = TRUE)]<-1000
property$PROPDMGMLT[grepl("m", property$PROPDMGEXP,ignore.case = TRUE)]<-1000000
property$PROPDMGMLT[grepl("b", property$PROPDMGEXP,ignore.case = TRUE)]<-1000000000
property$PROPDMGMLT[grepl("o", property$PROPDMGEXP,ignore.case = TRUE)]<-1

property$CROPDMGMLT[grepl("k", property$CROPDMGEXP,ignore.case = TRUE)]<-1000
property$CROPDMGMLT[grepl("m", property$CROPDMGEXP,ignore.case = TRUE)]<-1000000
property$CROPDMGMLT[grepl("b", property$CROPDMGEXP,ignore.case = TRUE)]<-1000000000
property$CROPDMGMLT[grepl("o", property$CROPDMGEXP,ignore.case = TRUE)]<-1

property$PROPDMG <- property$PROPDMG * property$PROPDMGMLT
property$CROPDMG <- property$CROPDMG * property$CROPDMGMLT
property$total <- property$PROPDMG + property$CROPDMG

health.totals <- aggregate(cbind(FATALITIES,INJURIES) ~ EVTYPE, data = health, sum, na.rm=TRUE)
health.totals$TOTAL <- health.totals$FATALITIES + health.totals$INJURIES
health.totals <- health.totals[order(-health.totals$TOTAL), ]
health.totals <- health.totals[1:25,]

ggplot(health.totals, aes(EVTYPE, FATALITIES)) + geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle=45, hjust=1)) 
ggplot(health.totals, aes(EVTYPE, INJURIES )) + geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(health.totals, aes(EVTYPE, TOTAL )) + geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle=45, hjust=1))

economic.total <- aggregate(cbind(PROPDMG,CROPDMG, total) ~ EVTYPE, data = property, sum, na.rm=TRUE)
economic.crop <- economic.total[order(-economic.total$CROPDMG), ]
economic.crop <- economic.crop[1:25,]

economic.prop <- economic.total[order(-economic.total$PROPDMG), ]
economic.prop <- economic.prop[1:25,]

ggplot(economic.prop, aes(EVTYPE, PROPDMG)) + geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle=45, hjust=1)) 
ggplot(economic.prop, aes(EVTYPE, CROPDMG)) + geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(economic.prop, aes(EVTYPE, total)) + geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle=45, hjust=1)) + ylab("TOTAL")




