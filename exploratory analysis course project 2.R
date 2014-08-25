NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Question 1
totalbyyear <- aggregate(NEI[ ,4], by=list(NEI$year), FUN=sum)
png(filename="question1.png")
totalbyyear$Group.1 <- factor(totalbyyear$Group.1)
barplot(totalbyyear[ ,2], main=expression("Total PM"[2.5]*" Emission"),names.arg=c("1999","2002","2005","2008"))
dev.off()

##Question 2
total_baltimore <- tapply(NEI$Emissions[NEI$fips=="24510"], NEI$year[NEI$fips=="24510"], FUN=sum)
png(filename="question2.png")
barplot(total_baltimore, main=expression("Total PM"[2.5]*" Emission in Baltimore City"))
dev.off()

##Question 3
png("question3.png")
Baltimore <- NEI[NEI$fips=="24510", ]
GroupBaltimore <- aggregate(Baltimore[ ,4], by=list(Baltimore$type, Baltimore$year), FUN=sum)
names(GroupBaltimore) <- c("type","year","Emission")
GroupBaltimore$year <- factor(GroupBaltimore$year)
GroupBaltimore$type <- factor(GroupBaltimore$type)
library(ggplot2)
p <- qplot(x=year,y=Emission,data=GroupBaltimore,geom="bar", stat="identity", width=0.5)
p + facet_grid(type~., scales="free")
dev.off()

#Question 4
png("question4.png")
comb <- grep("Comb", SCC$Short.Name,ignore.case=TRUE)
coal <- grep("Coal", SCC$Short.Name,ignore.case=TRUE)
combcoal <- intersect(comb,coal)
scc.combcoal <- SCC$SCC[combcoal]
NEI_coal <- NEI[NEI$SCC %in% scc.combcoal, ]
barplot(tapply(NEI_coal$Emission, NEI_coal$year, FUN=sum),main=expression("Total PM"[2.5]*" Emission for Coal Combustion"))
dev.off()

#Question 5
png("question5.png")
motor <- grep("motor", SCC$Short.Name,ignore.case=TRUE)
scc.motor <- SCC$SCC[motor]
NEI_motor <- NEI[NEI$SCC %in% scc.motor, ]
aaa <- tapply(NEI_motor$Emission[NEI_motor$fips=="24510"], NEI_motor$year[NEI_motor$fips=="24510"],FUN=sum)
barplot(aaa,main=expression("Total PM"[2.5]*" Emission for Motor Vehicle in Baltimore"))
dev.off()

#Question 6
png("question6.png")
motor_B <- NEI_motor[NEI_motor$fips=="24510", ]
motor_L <- NEI_motor[NEI_motor$fips=="06037", ]
agg_B <- aggregate(motor_B[ ,4], by=list(motor_B$year), FUN=sum)
agg_L <- aggregate(motor_L[ ,4], by=list(motor_L$year), FUN=sum)
agg_BL <- rbind(agg_B,agg_L)
City <- c(rep("Baltimore",4),rep("Los Angles",4))
agg_BL <- cbind(agg_BL,City)
names(agg_BL)[1:2] <- c("Year","Emission")
qplot(x=Year,y=Emission,fill=City,geom="bar",stat="identity",position="dodge",data=agg_BL,main=expression("Comparison of PM"[2.5]*" Emission for motor vehicle between Los Angles and Baltimore"))
