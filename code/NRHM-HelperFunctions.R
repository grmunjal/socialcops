#------------------------------------------
# Auxillary functions supporting the code sample for NRHM (Maharashtra) Data 
# Author: Gitanshu Munjal
# Date: 3/3/18
#------------------------------------------

#----------------------Load packages

#Function that checks for, installs, and loads packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#List of packages used
packages <- c("ggplot2","viridis","reshape2")

#Load packages
ipak(packages)



#----------------------Show Subject Area

subject.area <- function(topic.number){
  
  topic.string <- paste(topic.number, "-",  sep = "")
  topic.found <- nrhm.data[grep(topic.string,nrhm.data$id),2:4]
  topic.found <- topic.found[!duplicated(topic.found),]
  
  print(paste ("Subject Area", topic.number, ":", 
               topic.found$SubjectArea[1]), sep=" ")
  
  topic.found <- topic.found[,c(1,3)]
  return(topic.found)
}



#----------------------Show Focus Area Questions

focus.topics <- function(topic.number){
  
  topic.string <- paste("-M", topic.number, "-", sep = "")
  
  topic.found <- nrhm.data[grep(topic.string,nrhm.data$id),
                           
                           c("Description","Grouping")]
  
  topic.found <- topic.found[!duplicated(topic.found),]
  topic.end <- rownames(topic.found)[nrow(topic.found)]
  
  topic.found <- topic.found[topic.found$Grouping=="dummy",]
  
  for(topic.row in 1:nrow(topic.found)){
    grouping.r1 <- as.numeric(as.character(rownames(topic.found)[topic.row]))+1
    grouping.r2 <- as.numeric(as.character(rownames(topic.found)[topic.row+1]))-1
    grouping.r2 <- ifelse(topic.row==nrow(topic.found), 
                          as.numeric(as.character(topic.end)),
                          grouping.r2)
    
    groups.found <- nrhm.data$Grouping[grouping.r1:grouping.r2]
    groups.found <- as.character(unique(groups.found))
    
    descs.found <- nrhm.data$Description[grouping.r1:grouping.r2]
    descs.found <- as.character(unique(descs.found))
    
    topic.found$Desc.levels[topic.row] <- paste(unique(descs.found),collapse = ",")
    topic.found$GroupLevels[topic.row] <- paste(unique(groups.found),collapse = ",")
    
  }
  
  
  
  print(paste ("Topics Found For Focus Area M", topic.number,sep=""))
  print(paste ("Data For Focus Area M", 
               topic.number, " Ends at Row ",topic.end,sep=""))
  
  topic.found <- topic.found[,-2]
  return(topic.found)
  
}







#----------------------Get Focus Area Data

focus.data <- function(topic.number){
  
  
  name.string <- as.character(nrhm.data$Names[topic.number])
  name.found <- subset(nrhm.data, nrhm.data$Names %in% name.string)
  
  
  column.string <- c("FacilityName","FacilityLevel",
                     "Description","Grouping", "Total.Year",
                     "Apr.2015","May.2015","Jun.2015","Jul.2015",
                     "Aug.2015","Sep.2015","Oct.2015","Nov.2015",
                     "Dec.2015","Jan.2016","Feb.2016","Mar.2016")
  
  focused.data <- name.found[,column.string]
  
  
  return(focused.data)
}



#----------------------Plotting theme shortcuts and lists
nolegend <- theme_bw() + theme(legend.position = "none")
brlegend <- theme_bw() + theme(legend.position = c(0.75,0.25))
trlegend <- theme_bw() + theme(legend.position = c(0.85,0.75))
mrlegend <- theme_bw() + theme(legend.position = c(0.85,0.5))

rotlabels <- theme(axis.text.x=element_text(angle=60,hjust=1))
sfvt <- scale_fill_viridis(discrete = T)

indicators <- list()

reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}


#Aggregate values for the entire year
nrhm.data$Total.Year <- rowSums(nrhm.data[,10:21], na.rm = TRUE)