#################################################################################
#                           sdwivedi@buffalo.edu                                #
#                           yshikhar@buffalo.edu                                #
#                                                                               #
#################################################################################


#################################################################################
###     Problem 2.4 Influenza postivee test reoported by US Clinical Labs    ####
#################################################################################

#set the working directory from where we read the csv files

nationalClinicalData <- read.csv("WHO_NREVSS_Clinical_Labs.csv")
clinicalData<-nationalClinicalData[c(53:70),]
totalPositiveSpecimens <- NULL
PositiveA <- NULL
PositiveB <- NULL
for (i in 1:nrow(clinicalData)) {
  totalPositiveSpecimens <- ((clinicalData$PERCENT.POSITIVE*clinicalData$TOTAL.SPECIMENS)/100)
  PositiveA <- ((clinicalData$PERCENT.A*clinicalData$TOTAL.SPECIMENS)/100)
  PositiveB <- ((clinicalData$PERCENT.B*clinicalData$TOTAL.SPECIMENS)/100)
  
}

clinicalData$PositiveA <- (paste(as.character(PositiveA)))
clinicalData$PositiveB <- (paste(as.character(PositiveB)))
#totalPositiveSpecimens <- as.factor(totalPositiveSpecimens)
clinicalData$PERCENT.A <- as.factor(clinicalData$PERCENT.A)
clinicalData$PERCENT.B <- as.factor(clinicalData$PERCENT.B)
#to plot graph
library(ggplot2)
clinicalData$new.group <- as.numeric(paste(as.numeric(as.character(clinicalData$YEAR)),clinicalData$WEEK, sep = ""))

#c combine value into the vector list
par(mar=c(5,5,1,2))
barplot(t(clinicalData[c('PositiveA')]),
        names.arg = c(as.character(clinicalData$new.group)),
        main = "Influenza Positive Tests reported to CDC by U.S. Clinical Laboratories", 
        las=2,
        xlab = "Week",
        ylab = "Number of Positive Specimens", 
        ylim=c(0,14000),
        srt = 45,
        xpd = TRUE, 
        
        col="yellow"
)
par(new=TRUE)
barplot(t(clinicalData[c('PositiveB')]),
        ylim=c(0,14000),
        yaxt = "n",
        xaxt = "n",
        col="green"
)
par(new=TRUE)

plot(clinicalData$PERCENT.POSITIVE,
     col="black", 
     ylim=c(0,30),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=1,
     ylab = "", xlab = "", lwd=2)


par(new=TRUE)
plot(as.numeric(as.character(clinicalData$PERCENT.A)),
     col="yellow", 
     ylim=c(0,30),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=5,
     ylab = "", xlab = "", lwd=2)
axis(4)
par(new=TRUE)
plot(as.numeric(as.character(clinicalData$PERCENT.B)),
     col="green", 
     ylim=c(0,30),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=2,
     ylab = "", xlab = "", lwd=2)
legend("topleft", legend = c("A","B", "Percent Positive", "% Positive Flu A", "% Positive Flu B"), 
       col = c("yellow","green","black", "yellow", "green"),
       bty = "n",
       border = c("black","black",NA,NA,NA),
       fill = c("yellow","green",NA,NA,NA),
       lwd =c(NA,NA,2, 2, 2),
       lty = c(5,5,1),
       inset=.05)






##################################################################
# Load Influenza Positive Test Reported by U.S Public Health     #
##################################################################

#Load Influenza Positive Test Reported by U.S Public Health file

nationalPublicHealthData <- read.csv("WHO_NREVSS_Public_Health_Labs.csv")

#public_Health<-read.csv(file.choose(), header=T)

publicHealthData<-nationalPublicHealthData[c(54:70),]


#to know the data type of the data
str(publicHealthData)

publicHealthData$A..Subtyping.not.Performed. <- as.factor(publicHealthData$A..Subtyping.not.Performed.)
publicHealthData$A..2009.H1N1. <- as.factor(publicHealthData$A..2009.H1N1.)
publicHealthData$A..H3. <- as.factor(publicHealthData$A..H3.)
publicHealthData$H3N2v <- as.factor(publicHealthData$H3N2v)
publicHealthData$B <- as.factor(publicHealthData$B)
publicHealthData$BVic <- as.factor(publicHealthData$BVic)
publicHealthData$BYam <- as.factor(publicHealthData$BYam)

publicHealthData$combiningYearWeek <- as.numeric(paste(as.numeric(as.character(publicHealthData$YEAR)),publicHealthData$WEEK, sep = ""))
xlabel<-c(publicHealthData$combiningYearWeek)
type <-c('A(subtyping not performed)','A(H1N1)pdm09', "A(H3N2)", "H3N2v", "B(lineage not performed)","B(Victoria Lineage)","B(Yamagata Lineage)")
colors  <- c("yellow","orange","red", "violet","darkgreen","lightgreen", "green")
k=barplot(t(publicHealthData[c("A(subtyping not performed)","A(H1N1)pdm09", "A(H3N2)", "H3N2v", "B(lineage not performed)","B(Victoria Lineage)","B(Yamagata Lineage)")]),main = "Influenza Positive Tests reported to CDC by U.S. Public Health Laboratories \n National Summary, 2018-2019 Season",xlab = "Week",ylab = "Number of Positive Specimens",
          xaxt = "n", ylim = c(0,4000),col = colors)
legend("topleft", inset=.05, type, fill=colors)
text(k, labels = xlabel,par("usr")[3], srt = 45, adj = c(1,1), xpd = TRUE, cex=1)

par(mar=c(6,7,2,2))
barplot(t(publicHealthData[c('A..Subtyping.not.Performed.')]),
        names.arg = c(as.character(publicHealthData$combiningYearWeek)),
        main = "Influenza Positive Tests reported to CDC by U.S. Public Health Laboratories", 
        las=2,
        xlab = "Week",
        ylab = "Number of Positive Specimens", 
        
        ylim=c(0,2500),
        
        col="yellow"
)
par(new=TRUE)
barplot(t(publicHealthData[c('A..2009.H1N1.')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="orange"
)
par(new=TRUE)

barplot(t(publicHealthData[c('A..H3.')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="red"
)

par(new=TRUE)
barplot(t(publicHealthData[c('H3N2v')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="violet"
)
par(new=TRUE)
barplot(t(publicHealthData[c('B')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="darkgreen"
)
par(new=TRUE)
barplot(t(publicHealthData[c('BVic')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="lightgreen"
)
par(new=TRUE)
barplot(t(publicHealthData[c('BYam')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="green"
)

legend("topright", legend = c("A(subtyping not performed)","A(H1N1)pdm09", "A(H3N2)", "H3N2v", "B(lineage not performed)","B(Victoria Lineage)","B(Yamagata Lineage)"),
       col = c("yellow","orange","red", "violet","darkgreen","lightgreen", "green"),
       bty = "n",
       border = c("black","black","black","black","black","black","black"),
       fill = c("yellow","orange","red", "violet","darkgreen","lightgreen", "green"))



########################################################################
###  Problem 2b : Influenza Virus Characterization Main Pie chart   ####
#######################################################################
nationalPublicHealthData <- read.csv("WHO_NREVSS_Public_Health_Labs.csv")

publicHealthData<-nationalPublicHealthData[c(54:70),]



value<-c(sum(publicHealthData$A..H3.),sum(publicHealthData$B),sum(publicHealthData$BYam), sum(publicHealthData$BVic),sum(publicHealthData$A..Subtyping.not.Performed.),sum(publicHealthData$A..2009.H1N1.))
color<-c("red","darkolivegreen2","green","blue","yellow","orange")
pie(value,labels=value,col=color,main="Influenza positive specimens Reported by US Public \nhealth laboratories, Cumulative,2018-2019 season",init.angle = 105, cex=0.6)

legend("bottomright",
       c("Influenza A(H3N2)","Influenza B(Lineage not determined)","Influenza B Yamagata","Influenza B Victoria","Influenza A(subtype unkown)","Influenza A(H1N1)pdm09"), cex=0.75, fill=color)



#######################################################################
###   Problem 2b: Influenza Virus Characterization Sub Pie charts  ####
##########################################################################
library(cowplot)
library(ggplot2)
library(scales)

value1<-c(126,64,235)
value2<-c(690)
value3<-c(16,4,37)
value4<-c(86)
IF1 <- data.frame(  Genetic_Group1<-c("3C.2a","3C.2a1","3C.3a"), Percentage_value1<-c(30,15,55))
IF2 <- data.frame(  Genetic_Group2<-c("6B.1"), Percentage_value2<-c(100))
IF3 <- data.frame(  Genetic_Group3<-c("V1A","V1A-3Del","V1A.1"), Percentage_value3<-c(28,7,65))
IF4 <- data.frame(  Genetic_Group4<-c("Y3"), Percentage_value4<-c(100))


plot1<-ggplot(IF1, aes(x="", y=Percentage_value1, fill=Genetic_Group1))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=5.2)+
  ggtitle("Influenza A(H3N2)")+  theme(axis.text = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),line = element_blank())+
  scale_fill_manual(values = c("3C.2a"="salmon","3C.2a1"="bisque","3C.3a"="red"))+geom_text(aes( label = paste0((percent(Percentage_value1/100))," (",value1,") " )), size=3,position = position_stack(vjust = 0.5))

plot2<-ggplot(IF2, aes(x="", y=Percentage_value2, fill=Genetic_Group2))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+
  ggtitle("Influenza A(H1N1)pdm09")+theme(axis.text = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),line = element_blank())+
  scale_fill_manual(values = c("6B.1"="burlywood1"))+geom_text(aes( label = paste0((percent(Percentage_value2/100))," (",value2,") " )), size=3,position = position_stack(vjust = 0.5))

plot3<-ggplot(IF3, aes(x="", y=Percentage_value3, fill=Genetic_Group3))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=3.4)+
  ggtitle("Influenza B Victoria")+theme(axis.text = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),line = element_blank())+
  scale_fill_manual(values = c("V1A"="darkolivegreen2","V1A-3Del"="darkseagreen1","V1A.1"="lightgreen"))+geom_text(aes(label = paste0((percent(Percentage_value3/100))," (",value3,") " )), size=3,position = position_stack(vjust = 0.5))

plot4<-ggplot(IF4, aes(x="", y=Percentage_value4, fill=Genetic_Group4))+geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+
  ggtitle("Influenza B Yamagata")+theme(axis.text = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),line = element_blank())+
  scale_fill_manual(values = c("Y3"="forestgreen"))+geom_text(aes(label = paste0((percent(Percentage_value4/100))," (",value4,") " )), size=3,position = position_stack(vjust = 0.5))

plot_grid(plot1,plot2,plot3,plot4,nrow = 2,ncol=2)



##############################################################################
##  #Problem 3 : Pneumonia and Influenza mortality                           #
##############################################################################


library(ggplot2)

#Select the file "Pneumonia and Influenza mortality"
mortality<-read.csv(file.choose(), header=T)
mortality=mortality[436:487,]
str(mortality)
mortality$Week = factor(mortality$Week,levels=unique(mortality$Week))

ggplot() + 
  geom_smooth(data = mortality,aes(x = mortality$Week , y = mortality$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza,group = 1),stat = "identity",color="red")  +
  geom_smooth(data = mortality,aes(x = mortality$Week , y = mortality$Expected,group = 1),stat = "identity",color="black")+
  geom_smooth(data = mortality,aes(x = mortality$Week , y = mortality$Threshold,group = 1),stat = "identity",color="black")+theme_bw()+
  annotate("text", x = 72, y = 6.5, label = "Seasonal Baseline",angle = 90)+
  annotate("text", x = 50, y = 7.7, label = "",angle = 90)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("MMWR Week")+ylab("% of All Deaths Due to P&I")+ggtitle("Pneumonia & Influenza Mortality from \nthe National Centre for Health Statistics Mortality Surveillance System \n Data through the week ending January 28,2019,  as of Febrauary 1,2019  ")+scale_x_discrete(breaks = mortality$Week[c(T,F,F,F,F,F,F,F,F,F)])+
  theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#########################################################################
##  Problem 5: Plot the heat map ILINet State Activity Indicator Map:  ##
#########################################################################
my_data <- read.csv(file.choose(), header=T)
my_data$Activity_Level<-substr(my_data$ACTIVITY.LEVEL, 7, 7)
my_data$Activity_Level<-as.numeric(my_data$Activity_Level)
new_df <- my_data[!is.na(my_data$Activity_Level),]
state<-aggregate( Activity_Level ~ STATENAME, new_df, mean)
#state$STATENAME<- tolower(state$STATENAME)
library(ggplot2)
library(maps)
library(maptools)
library(urbnmapr)
library(mapproj)
library(usmap)
#states <- map_data("state")
map.df <- merge(statedata,state, by.x="state_name",by.y='STATENAME', all.x=T)
map.df$fips <- map.df$state_fips
map.df <- map.df[order(map.df$fips),]

plot_usmap(data = map.df, values = "Activity_Level", lines = "black") + 
  scale_fill_gradientn(colours=c('green3','yellow','darkorange1','red','darkred'),na.value="green3",name = "Activity_Level", label = scales::comma)+
  theme(legend.position = "right")

############################################################################
##                          PART 2 TASK 5                                 ##
############################################################################

# Load data 
nationalClinicalData <- read.csv("WHO_NREVSS_Clinical_Labs.csv")
nationalPublicHealthData <- read.csv("WHO_NREVSS_Public_Health_Labs.csv")

# to load 52 weeks recent data
clinicalData <- tail(nationalClinicalData,-19)
publicHealthData <- tail(nationalPublicHealthData,-19)

#to know the data type of the data
str(clinicalData)


#2.5 Influenza Positive Tests reported by U.S. Clinical Laboratories

totalPositiveSpecimens <- NULL
PositiveA <- NULL
PositiveB <- NULL
for (i in 1:nrow(clinicalData)) {
  totalPositiveSpecimens <- ((clinicalData$PERCENT.POSITIVE*clinicalData$TOTAL.SPECIMENS)/100)
  #titles <- c(titles,extractTitle(clinicalData[i,"PositiveSpecimens"]))
  PositiveA <- ((clinicalData$PERCENT.A*clinicalData$TOTAL.SPECIMENS)/100)
  #posiA <- c(PositiveA,extractPositiveA(clinicalData[i,"PositiveSpecimens"]))
  PositiveB <- ((clinicalData$PERCENT.B*clinicalData$TOTAL.SPECIMENS)/100)
  
}

clinicalData$PositiveA <- (paste(as.character(PositiveA)))
clinicalData$PositiveB <- (paste(as.character(PositiveB)))
#totalPositiveSpecimens <- as.factor(totalPositiveSpecimens)
clinicalData$PERCENT.A <- as.factor(clinicalData$PERCENT.A)
clinicalData$PERCENT.B <- as.factor(clinicalData$PERCENT.B)
#to plot graph
library(ggplot2)
#nationalClinicalData$Pclass <- as.factor(train$Pclass)
clinicalData$new.group <- as.numeric(paste(as.numeric(as.character(clinicalData$YEAR)),clinicalData$WEEK, sep = ""))
#ggplot(clinicalData, aes(x= clinicalData$new.group , fill= clinicalData$PERCENT.A,clinicalData$PERCENT.B)) +
 # geom_bar(stat = "identity")+
  #xlab("Week")+
  #ylab("Number of Positive Specimens")+
  #geom_boxplot() +
  #labs(fill="clinicalData$PERCENT.POSITIVE","clinicalData$PERCENT.B")+
  #theme(axis.text.x = element_text(angle=60, hjust=1))


#c combine value into the vector list
par(mar=c(6,7,2,2))
barplot(t(clinicalData[c('PositiveA')]),
        names.arg = c(as.character(clinicalData$new.group)),
        main = "Influenza Positive Tests reported to CDC by U.S. Clinical Laboratories", 
        las=2,
        xlab = "Week",
        ylab = "Number of Positive Specimens", 
        ylim=c(0,14000),
        
        col="yellow"
        )
par(new=TRUE)
barplot(t(clinicalData[c('PositiveB')]),
        ylim=c(0,14000),
        yaxt = "n",
        xaxt = "n",
        col="green"
        )
par(new=TRUE)

plot(clinicalData$PERCENT.POSITIVE,
     col="black", 
     ylim=c(0,30),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=1,
     ylab = "", xlab = "", lwd=2)


par(new=TRUE)
plot(as.numeric(as.character(clinicalData$PERCENT.A)),
     col="yellow", 
     ylim=c(0,30),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=5,
     ylab = "", xlab = "", lwd=2)
axis(4)
par(new=TRUE)
plot(as.numeric(as.character(clinicalData$PERCENT.B)),
     col="green", 
     ylim=c(0,30),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=2,
     ylab = "", xlab = "", lwd=2)
legend("center", legend = c("A","B", "Percent Positive", "% Positive Flu A", "% Positive Flu B"), 
       col = c("yellow","green","black", "yellow", "green"),
       bty = "n",
       border = c("black","black",NA,NA,NA),
       fill = c("yellow","green",NA,NA,NA),
       lwd =c(NA,NA,2, 2, 2),
       lty = c(5,5,1))


#2.5 Influenza Positive Tests reported by U.S. Public Health Laboratories

#to know the data type of the data
str(publicHealthData)
publicHealthData$A..Subtyping.not.Performed. <- as.factor(publicHealthData$A..Subtyping.not.Performed.)
publicHealthData$A..2009.H1N1. <- as.factor(publicHealthData$A..2009.H1N1.)
publicHealthData$A..H3. <- as.factor(publicHealthData$A..H3.)
publicHealthData$H3N2v <- as.factor(publicHealthData$H3N2v)
publicHealthData$B <- as.factor(publicHealthData$B)
publicHealthData$BVic <- as.factor(publicHealthData$BVic)
publicHealthData$BYam <- as.factor(publicHealthData$BYam)

publicHealthData$combiningYearWeek <- as.numeric(paste(as.numeric(as.character(publicHealthData$YEAR)),publicHealthData$WEEK, sep = ""))
par(mar=c(6,7,2,2))
barplot(t(publicHealthData[c('A..Subtyping.not.Performed.')]),
        names.arg = c(as.character(publicHealthData$combiningYearWeek)),
        main = "Influenza Positive Tests reported to CDC by U.S. Public Health Laboratories", 
        las=2,
        xlab = "Week",
        ylab = "Number of Positive Specimens", 
        
        ylim=c(0,2500),
        
        col="yellow"
)
par(new=TRUE)
barplot(t(publicHealthData[c('A..2009.H1N1.')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="brown"
)
par(new=TRUE)

barplot(t(publicHealthData[c('A..H3.')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="red"
)

par(new=TRUE)
barplot(t(publicHealthData[c('H3N2v')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="violet"
)
par(new=TRUE)
barplot(t(publicHealthData[c('B')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="darkgreen"
)
par(new=TRUE)
barplot(t(publicHealthData[c('BVic')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="lightgreen"
)
par(new=TRUE)
barplot(t(publicHealthData[c('BYam')]),
        las=2,
        ylim=c(0,2500),
        xaxt = "n", 
        yaxt = "n", 
        col="green"
)

legend("topright", legend = c("A(subtyping not performed)","A(H1N1)pdm09", "A(H3N2)", "H3N2v", "B(lineage not performed)","B(Victoria Lineage)","B(Yamagata Lineage)"),
       col = c("yellow","brown","red", "violet","darkgreen","lightgreen", "green"),
       bty = "n",
       border = c("black","black","black","black","black","black","black"),
       fill = c("yellow","brown","red", "violet","darkgreen","lightgreen", "green"))


############################################################################
##                          PART 2 TASK 6                                 ##
############################################################################

# Load data 
stateClinicalData <-read.csv(file.choose(), header = TRUE)

# to load 52 weeks recent data
clinicalDataNewYork <- tail(stateClinicalData,-19)


#to know the data type of the data
str(clinicalDataNewYork)

totalPositiveSpecimens <- NULL
PositiveA <- NULL
PositiveB <- NULL
for (i in 1:nrow(clinicalDataNewYork)) {
  totalPositiveSpecimens <- ((clinicalDataNewYork$PERCENT.POSITIVE*clinicalDataNewYork$TOTAL.SPECIMENS)/100)
  PositiveA <- ((clinicalDataNewYork$PERCENT.A*clinicalDataNewYork$TOTAL.SPECIMENS)/100)
  PositiveB <- ((clinicalDataNewYork$PERCENT.B*clinicalDataNewYork$TOTAL.SPECIMENS)/100)
  
}

clinicalDataNewYork$PositiveA <- (paste(as.character(PositiveA)))
clinicalDataNewYork$PositiveB <- (paste(as.character(PositiveB)))
#totalPositiveSpecimens <- as.factor(totalPositiveSpecimens)
clinicalDataNewYork$PERCENT.A <- as.factor(clinicalDataNewYork$PERCENT.A)
clinicalDataNewYork$PERCENT.B <- as.factor(clinicalDataNewYork$PERCENT.B)
#to plot graph
library(ggplot2)
#nationalClinicalData$Pclass <- as.factor(train$Pclass)
clinicalDataNewYork$new.group <- as.numeric(paste(as.numeric(as.character(clinicalDataNewYork$YEAR)),clinicalDataNewYork$WEEK, sep = ""))

#c combine value into the vector list
par(mar=c(6,7,2,2))
barplot(t(clinicalDataNewYork[c('PositiveA')]),
        names.arg = c(as.character(clinicalData$new.group)),
        main = "Influenza Positive Tests reported to CDC by U.S. Clinical Laboratories", 
        las=2,
        xlab = "Week",
        ylab = "Number of Positive Specimens", 
        ylim=c(0,1600),
        
        col="yellow"
)
par(new=TRUE)
barplot(t(clinicalDataNewYork[c('PositiveB')]),
        ylim=c(0,1600),
        yaxt = "n",
        xaxt = "n",
        col="green"
)
par(new=TRUE)

plot(clinicalDataNewYork$PERCENT.POSITIVE,
     col="black", 
     ylim=c(0,35),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=1,
     ylab = "", xlab = "", lwd=2)


par(new=TRUE)
plot(as.numeric(as.character(clinicalDataNewYork$PERCENT.A)),
     col="yellow", 
     ylim=c(0,35),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=5,
     ylab = "", xlab = "", lwd=2)
axis(4)
par(new=TRUE)
plot(as.numeric(as.character(clinicalDataNewYork$PERCENT.B)),
     col="green", 
     ylim=c(0,35),
     type="l", 
     xaxt = "n", 
     yaxt = "n", 
     lty=2,
     ylab = "", xlab = "", lwd=2)
legend("center", legend = c("A","B", "Percent Positive", "% Positive Flu A", "% Positive Flu B"), 
       col = c("yellow","green","black", "yellow", "green"),
       bty = "n",
       border = c("black","black",NA,NA,NA),
       fill = c("yellow","green",NA,NA,NA),
       lwd =c(NA,NA,2, 2, 2),
       lty = c(5,5,1))


