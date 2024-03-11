# I relieved heavily on the following sources in learning about and coding my final project, each proved indispensable in their own way:

# for the workflow
# btd. (2024, January 7). arules: A Practical Guide to Data Mining in R. Medium. https://baotramduong.medium.com/data-mining-in-r-with-arules-9be1e373f9ac

# for the support, confidence, and lift definitions
# Garg, A. (2018, September 3). Complete guide to Association Rules (1/2). Medium. https://towardsdatascience.com/association-rules-2-aa9a77241654

# for the item matrix plot and especially for making lhs and rhs coding finally make sense
# Kirenz, J. (2022, July 1). Introduction to Association Rule Mining in R. https://www.kirenz.com/blog/posts/2020-05-14-r-association-rule-mining/

# for the excellent support and confidence level tests and graphical validations
# Ogbebor, A. O. (2022, January 24). Association rules mining in R. Medium. https://medium.com/@Ah_Yoh/association-rules-mining-in-r-863ebe7c5055

# for the detailed step-by-step walkthrough
# Wang, J. (2018, October 10). A Guide to Association Rules in R - Part 1 The Transactions Class in arules. https://www.jdatalab.com/data_science_and_data_mining/2018/10/10/association-rule-transactions-class.html

# -------------------------------------------------------------------------------------------------

# load necessary R libraries 
library(arules)
library(arulesViz)
library(tidyverse)
library(knitr)
library(gridExtra)

# load data set
civil_disorder_dataset <- read.csv("~/temp/civil_disorder_dataset.csv", header=TRUE,sep=",",na.strings="",strip.white=TRUE,colClasses="factor")

# view data set records and layout
View(civil_disorder_dataset)

# view first several records in data file
head(civil_disorder_dataset)

# view last several records in data file
tail(civil_disorder_dataset)

# view summary statistics
summary(civil_disorder_dataset)
 
# str displays the structure of the object
str(civil_disorder_dataset)

# create array of support levels to test rules at various levels 
supportLevels <- c(0.1,0.03,0.05,0.06,0.07,0.08)

# create array of confidence levels to test rules at various levels
confidenceLevels <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)

# create empty arrays to store number of rules
rules_sup10 <- integer(length=9)
rules_sup3 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup6 <- integer(length=9)
rules_sup7 <- integer(length=9)
rules_sup8 <- integer(length=9)

# store the number of rules generated at each support and confidence level in each of the 6 arrays
for(i in 1:length(confidenceLevels)) {rules_sup10[i] <- length(apriori(civil_disorder_dataset,parameter=list(sup=supportLevels[1],conf=confidenceLevels[i],target="rules"),appearance=list(rhs=c("Civil.Disorder.Category=Riot","Civil.Disorder.Category=Incident","Civil.Disorder.Category=Protest"),default="lhs")))}
for(i in 1:length(confidenceLevels)) {rules_sup3[i] <- length(apriori(civil_disorder_dataset,parameter=list(sup=supportLevels[2],conf=confidenceLevels[i],target="rules"),appearance=list(rhs=c("Civil.Disorder.Category=Riot","Civil.Disorder.Category=Incident","Civil.Disorder.Category=Protest"),default="lhs")))}
for(i in 1:length(confidenceLevels)) {rules_sup5[i] <- length(apriori(civil_disorder_dataset,parameter=list(sup=supportLevels[3],conf=confidenceLevels[i],target="rules"),appearance=list(rhs=c("Civil.Disorder.Category=Riot","Civil.Disorder.Category=Incident","Civil.Disorder.Category=Protest"),default="lhs")))}
for(i in 1:length(confidenceLevels)) {rules_sup6[i] <- length(apriori(civil_disorder_dataset,parameter=list(sup=supportLevels[4],conf=confidenceLevels[i],target="rules"),appearance=list(rhs=c("Civil.Disorder.Category=Riot","Civil.Disorder.Category=Incident","Civil.Disorder.Category=Protest"),default="lhs")))}
for(i in 1:length(confidenceLevels)) {rules_sup7[i] <- length(apriori(civil_disorder_dataset,parameter=list(sup=supportLevels[5],conf=confidenceLevels[i],target="rules"),appearance=list(rhs=c("Civil.Disorder.Category=Riot","Civil.Disorder.Category=Incident","Civil.Disorder.Category=Protest"),default="lhs")))}
for(i in 1:length(confidenceLevels)) {rules_sup8[i] <- length(apriori(civil_disorder_dataset,parameter=list(sup=supportLevels[6],conf=confidenceLevels[i],target="rules"),appearance=list(rhs=c("Civil.Disorder.Category=Riot","Civil.Disorder.Category=Incident","Civil.Disorder.Category=Protest"),default="lhs")))}

# create and store plots of rules by confidence levels
plot_1 <- qplot(confidenceLevels,rules_sup10,geom=c("point","line"),xlab="Confidence level",ylab="Number of rules found",main="Apriori with a support level of 10%") + theme_bw()
plot_2 <- qplot(confidenceLevels,rules_sup3,geom=c("point","line"),xlab="Confidence level",ylab="Number of rules found",main="Apriori with a support level of 3%") + scale_y_continuous(breaks=seq(0,10,2)) + theme_bw()
plot_3 <- qplot(confidenceLevels,rules_sup5,geom=c("point","line"),xlab="Confidence level",ylab="Number of rules found",main="Apriori with a support level of 5%") + scale_y_continuous(breaks=seq(0,10,2)) + theme_bw()
plot_4 <- qplot(confidenceLevels,rules_sup6,geom=c("point","line"),xlab="Confidence level",ylab="Number of rules found",main="Apriori with a support level of 6%") + scale_y_continuous(breaks=seq(0,10,2)) + theme_bw()
plot_5 <- qplot(confidenceLevels,rules_sup7,geom=c("point","line"),xlab="Confidence level",ylab="Number of rules found",main="Apriori with a support level of 7%") + scale_y_continuous(breaks=seq(0,10,2)) + theme_bw()
plot_6 <- qplot(confidenceLevels,rules_sup8,geom=c("point","line"),xlab="Confidence level",ylab="Number of rules found",main="Apriori with a support level of 8%") + scale_y_continuous(breaks=seq(0,10,2)) + theme_bw()

# display each plot arranged into 2 columns
grid.arrange(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,ncol=2)

# generate apriori rules using support level = 0.08 and confidence level = 0.5
rules <- apriori(civil_disorder_dataset,parameter=list(sup=supportLevels[6],conf=confidenceLevels[5],minlen=3,target="rules"),appearance=list(rhs=c("Civil.Disorder.Category=Riot","Civil.Disorder.Category=Incident","Civil.Disorder.Category=Protest"),default="lhs"))

# view summary statistics for generated rules
summary(rules)

# view rules in descending order by lift value
inspect(head(sort(rules,by="lift",decreasing=TRUE),12))

# generate scatter plot for rules validation    
plot(rules,measure=c("support","lift"),shading="confidence")

# generate circle plot for rules validation
plot(rules,method="graph")

# generate matrix plot for fules validation
plot(rules,method="grouped")

    