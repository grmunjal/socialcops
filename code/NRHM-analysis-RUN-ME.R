#------------------------------------------
# Code sample for NRHM (Maharashtra) Data 
# Author: Gitanshu Munjal
# Date: 3/3/18
#
# Requirements: 
# "tidy-NRHM-compiledset.csv" (produced elsewhere from the raw files)
# packages (checks/installation included below):
# ggplot2, viridis, lattice, gridExtra
#------------------------------------------


#Clear working space
rm(list=ls())



#Set working directory, attach helper functions
setwd("~/Desktop/socialcops/data/")
source("../code/NRHM-HelperFunctions.R")


#Read in data and load packages
ipak(packages)
nrhm.data <- read.table("tidy-NRHM-compiledset.csv", 
                        header = T, sep = ",")



#Aggregate values for the entire year
nrhm.data$Total.Year <- rowSums(nrhm.data[,10:21], na.rm = TRUE)

# In the decentralized healthcare scenario, Sub centre makes the lowest division, followed by PHC and then block level hospitals (or RH). Tell us if the numbers follow this vertical hierarchy.
#Answer: YES!; #SCs > #PHCs > #RHs

#How many SCs?
length(grep("SC", unique(nrhm.data$FacilityName)))

#How many PHCs?
length(grep("PHC", unique(nrhm.data$FacilityName)))

#How many RHs?
length(grep("RH", unique(nrhm.data$FacilityName)))





# The data set has a large number of missing entries (almost 50%). Most these entries were from the "hidden" sections (stock, inventory, etc..) in the excel files. 

# Analysis strategy for this sample
# The raw data were categorized into broad subject areas (e.g.: "REPRODUCTIVE AND CHILD HEALTH") which in turn had more specific Focus areas (e.g.: "Ante Natal Care Services ANC") which in turn had specific survey questions (e.g.: "Number of pregnant women received 3 ANC check ups").

#To conduct exploratory analyses and look for trends my plan is to look at survey questions from a topic, make plots, think of inferences that can be made with the data at hand and think of data needed to make more inferences. As a lot of the steps are recursive I will use some custom helper functions like:


#------------------------------------------
# What are possible causes of an increasing Maternal Mortality Rate in Maharashtra? 
#http://niti.gov.in/content/maternal-mortality-ratio-mmr-100000-live-births#
#------------------------------------------

#What is our focus area?
#E is "Mortality Details"
subject.area("E")



#What survey responses do we have for disease prevlance in children ?
#How are those data grouped?
#(M17 from above)
focus.topics(17)



#Isolate relevant data: (numbers from above)
iso.data <- focus.data(c(477:512))
iso.data <- iso.data[grep("*Total*", 
                          as.character(iso.data$Grouping), 
                          invert = TRUE),]

head(iso.data[,1:5])


#Aggregate, data across months and places
t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping),sum)

#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100



#Visualize (Color by age grouping)
ggplot(t.iso.data, aes(reorder_size(Description), proportion), group=Grouping) +
  geom_col(alpha=0.7, color="black", aes(fill=Grouping)) + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + rotlabels + coord_flip() + brlegend 






iso.data <- iso.data[grep("*Total*", 
                          as.character(iso.data$Grouping), 
                          invert = TRUE),]

#------------------------------------------
# Indicators: IMR, MMR, SBR, AMR,
#------------------------------------------


#------MMR

#Extract total maternal mortalities 
#(exclude Accidents/Burns, Suicides, Other Causes, Animal bites)
#include only 15-55

subject.area("E")
focus.topics(17)
iso.data <- focus.data(c(477:512))
iso.data <- iso.data[grep("*Total*",
                          as.character(iso.data$Grouping),invert = TRUE),]

iso.data <- iso.data[grep("*Above 55*",
                          as.character(iso.data$Description),invert = TRUE),]
iso.data <- iso.data[grep("*14 yrs*",
                          as.character(iso.data$Description),invert = TRUE),]
iso.data <- iso.data[grep("*Burn*",
                          as.character(iso.data$Description),invert = TRUE),]
iso.data <- iso.data[grep("*Suicide*",
                          as.character(iso.data$Description),invert = TRUE),]
iso.data <- iso.data[grep("*Other*",
                          as.character(iso.data$Description),invert = TRUE),]
iso.data <- iso.data[grep("*Animal*",
                          as.character(iso.data$Description),invert = TRUE),]


#Aggregate, data across months and places
t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping),sum)

#Record Maternal Deaths
indicators[["MMR"]]['maternal.deaths'] <-  sum(t.iso.data$x,na.rm = T)



#Extract total live births
subject.area("A")
focus.topics(4)
iso.data <- focus.data(48:53)
head(iso.data)


iso.data <- focus.data(50)
t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping),sum)

indicators[["MMR"]]['live.births'] <-  t.iso.data$x

# ggplot(t.iso.data, aes(y=value,x=FacilityLevel)) +
#   geom_jitter(height = 0, alpha=0.5) + nolegend
# 
# ggplot(t.iso.data, aes(y=value,x=FacilityLevel)) +
#   geom_text(label=t.iso.data$FacilityName) +
#   geom_jitter(height = 0, alpha=0.5) + nolegend

#Calculate MMR
indicators[["MMR"]]['Maternal mortality ratio (per 100 000 live births)'] <-
  indicators[["MMR"]]['maternal.deaths']*100000/indicators[["MMR"]]['live.births']






#------IMR

#Extract total still births

subject.area("A")
focus.topics(4)
iso.data <- focus.data(48:52)
head(iso.data)


iso.data <- focus.data(51)
t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping),sum)

indicators[["IMR"]]['still.births'] <-  t.iso.data$x
indicators[["IMR"]]['live.births'] <-  indicators[["MMR"]]['live.births']
# ggplot(t.iso.data, aes(y=value,x=FacilityLevel)) + 
#   geom_jitter(height = 0, alpha=0.5) + nolegend
# 
# ggplot(t.iso.data, aes(y=value,x=FacilityLevel)) + 
#   geom_text(label=t.iso.data$variable) + 
#   geom_jitter(height = 0, alpha=0.5) + nolegend

#Calculate IMR
indicators[["IMR"]]['Infant mortality rate (per 1000 live births)'] <- 
  indicators[["IMR"]]['still.births']*1000/indicators[["IMR"]]['live.births']






#------SBR

#Extract total still births

subject.area("A")
focus.topics(4)
iso.data <- focus.data(48:52)
head(iso.data)


iso.data <- focus.data(51)
t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping),sum)

indicators[["SBR"]]['still.births'] <- t.iso.data$x
indicators[["SBR"]]['live.births'] <- indicators[["IMR"]]['live.births']

indicators[["SBR"]]['Stillbirth rate (per 1000 total births)'] <-
  indicators[["SBR"]][1]*1000/(indicators[["SBR"]][1]+indicators[["SBR"]][2])






#------ HIV prevalence (assuming samples representative of population)

#Extract HIV testing data

subject.area("C")
focus.topics(15)
iso.data <- focus.data(252:260)
iso.data <- iso.data[grep("*Total*",iso.data$Description),]

t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping),sum)

indicators[["HIV"]]['positive'] <- t.iso.data$x[1]
indicators[["HIV"]]['tested'] <- t.iso.data$x[2]

indicators[["HIV"]]["HIV prevalence (per 1000 population)"] <- 
  indicators[["HIV"]][1]*1000/indicators[["HIV"]][2]






#------ Malaria incidence rate (assuming samples representative of population)

#Extract Malaria testing data

subject.area("C")
focus.topics(15)
iso.data <- focus.data(267:269)


head(iso.data,n=10)


t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping),sum)

indicators[["Malaria"]]['positive'] <- t.iso.data$x[2] + t.iso.data$x[3]

indicators[["Malaria"]]['tested'] <- t.iso.data$x[1]

indicators[["Malaria"]]["Malaria incidence rate (per 1000 population)"] <- 
  indicators[["Malaria"]][1]*1000/indicators[["Malaria"]][2]
  
  
  
#Show 1 indicator and write all to file  
indicators[[1]]

ind.table <- c()
for(ind.num in names(indicators)){
  
  ind.data <- data.frame(t(indicators[[ind.num]]))
  colnames(ind.data) <- c("Num","Denom","Indicator")
  
  ind.data$Name <- colnames(t(indicators[[ind.num]]))[3]

  ind.table <- rbind(ind.table, ind.data)
  
}

write.table(ind.table, 
            "../output/indicators.csv", quote = F, sep = ",", row.names = F)  
  





#-------Some Health Trends


#Number of new RTI/STI for which treatment initiated 
subject.area("A")
focus.topics(8)
iso.data <- focus.data(c(82:85))
iso.data <- iso.data[grep("*ale",iso.data$Description),]

head(iso.data)
t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Level=iso.data$FacilityLevel),sum)

#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100


ggplot(t.iso.data, aes(reorder_size(Level), proportion, fill=Description)) +
  geom_col(alpha=0.7, color="black",position = "dodge") + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + rotlabels  + brlegend 


#Infant Deaths up to 4 weeks by cause

subject.area("E")
focus.topics(17)
iso.data <- focus.data(c(415:426))

iso.data <- iso.data[grep("*Total*",as.character(iso.data$Grouping),invert = TRUE),]


t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Level=iso.data$FacilityLevel),sum)

#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100


ggplot(t.iso.data, aes(reorder_size(Level), proportion, fill=Description)) +
  geom_col(alpha=0.7, color="black",position = "dodge") + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + rotlabels  + theme_bw() + coord_flip()







#Infant/Child Deaths up to 5 years by cause

subject.area("E")
focus.topics(17)
iso.data <- focus.data(c(428:442))
iso.data <- iso.data[grep("*Total*",as.character(iso.data$Grouping),invert = TRUE),]


t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Level=iso.data$FacilityLevel),sum)

#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100


ggplot(t.iso.data, aes(reorder_size(Level), proportion, fill=Description)) +
  geom_col(alpha=0.7, color="black",position = "dodge") + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + rotlabels  + theme_bw() + coord_flip()






#Adolescent/Adult deaths by cause

subject.area("E")
focus.topics(17)
iso.data <- focus.data(c(444:475))
iso.data <- iso.data[grep("*Total*", 
                          as.character(iso.data$Grouping), 
                          invert = TRUE),]


#Aggregate, data across months and places
t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping),sum)

#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100



#Visualize (Color by age grouping)
ggplot(t.iso.data, aes(reorder_size(Description), proportion), group=Grouping) +
  geom_col(alpha=0.7, color="black", aes(fill=Grouping)) + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + coord_flip() + brlegend 







#CHILD IMMUNIZATION: Number of Infants 0 to 11 months old who received the following
#multipled by 2

subject.area("A")
focus.topics(10)
iso.data <- focus.data(c(147:163))

#Aggregate, data across months and places
t.iso.data <- melt(iso.data,
                   id.vars = c("FacilityName", "FacilityLevel","Description","Grouping"))

t.iso.data <- aggregate(t.iso.data$value, 
                        list(Month=t.iso.data$variable,
                             Description=t.iso.data$Description),sum)
t.iso.data <- t.iso.data[t.iso.data$Month!="Total.Year",]


#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100



#Visualize (Color by age grouping)
ggplot(t.iso.data, aes(reorder_size(Month), proportion)) +
  geom_point(alpha=0.7, shape=21,color="black", aes(fill=Description)) + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + nolegend +facet_wrap(~Description)+ 
  geom_hline(aes(yintercept=0.5), linetype=4) + rotlabels








#CHILD IMMUNIZATION: Adverse Event Following Immunisation (AEFI)
subject.area("A")
focus.topics(10)
iso.data <- focus.data(c(182:184))

t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Level=iso.data$FacilityLevel),sum)

#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100


ggplot(t.iso.data, aes(reorder_size(Level), x, fill=Description)) +
  geom_col(alpha=0.7, color="black",position = "dodge") + 
  labs(y = "% of total AEFI", x ="") + 
  sfvt + rotlabels  + theme_bw()  + ylim(0,100)










#Number of cases of Childhood Diseases reported during the month0-5 years: 

subject.area("A")
focus.topics(12)
iso.data <- focus.data(c(195:203))

#Aggregate, data across months and places
t.iso.data <- melt(iso.data,
                   id.vars = c("FacilityName", "FacilityLevel","Description","Grouping"))

t.iso.data <- aggregate(t.iso.data$value, 
                        list(Month=t.iso.data$variable,
                             Description=t.iso.data$Description,
                             FacilityLevel=t.iso.data$FacilityLevel),sum)
t.iso.data <- t.iso.data[t.iso.data$Month=="Total.Year",]


#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100



#Visualize (Color by age grouping)
ggplot(t.iso.data, aes(reorder_size(FacilityLevel), proportion)) +
  geom_col(alpha=0.7,color="black", aes(fill=FacilityLevel)) + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + nolegend + facet_wrap(~Description) + rotlabels + coord_flip()







#A5

subject.area("A")
focus.topics(5)
iso.data <- focus.data(c(66:69))

#Aggregate, data across months and places
t.iso.data <- melt(iso.data,
                   id.vars = c("FacilityName", "FacilityLevel","Description","Grouping"))

t.iso.data <- aggregate(t.iso.data$value, 
                        list(Month=t.iso.data$variable,
                             Description=t.iso.data$Description,
                             FacilityLevel=t.iso.data$FacilityLevel),sum)
t.iso.data <- t.iso.data[t.iso.data$Month=="Total.Year",]


#Express aggregate as a proportion of total deaths
t.iso.data$proportion <- (t.iso.data$x/sum(t.iso.data$x,na.rm = T))*100



#Visualize (Color by age grouping)
ggplot(t.iso.data, aes(reorder_size(Description), proportion), group=FacilityLevel) +
  geom_col(alpha=0.7, color="black", aes(fill=FacilityLevel)) + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + rotlabels + coord_flip() + brlegend 




iso.data <- focus.data(50:51)
t.iso.data <- aggregate(iso.data$Total.Year, 
                        list(Description=iso.data$Description,
                             Grouping=iso.data$Grouping,
                             FacilityLevel=iso.data$FacilityLevel),sum)
#Express aggregate as a proportion of total deaths
t.iso.data$totals <- ifelse(t.iso.data$Description=="Still Birth",
                            indicators$SBR['still.births'],
                            indicators$SBR['live.births'])
t.iso.data$proportion <- (t.iso.data$x/t.iso.data$totals)*100


#Visualize (Color by age grouping)
ggplot(t.iso.data, aes(reorder_size(FacilityLevel), proportion)) +
  geom_col(alpha=0.7,color="black", aes(fill=FacilityLevel), position = "dodge") + 
  labs(y = "% of total Maternal Mortalities", x ="") + 
  sfvt + nolegend + facet_wrap(~Description) + rotlabels + coord_flip()

















