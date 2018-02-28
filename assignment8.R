#Module # 8 assignment
#Part1
install.packages("plyr")
library(plyr)
DSmean<-ddply(Dataset, "Sex", summarise, 
              mean.age=mean(Age), 
              mean.grade=mean(Grade))
write.table(DSmean,"data.txt", sep="\t", 
            row.name=FALSE, col.names = TRUE)
#Part2
i.names<-grep("[i]", Dataset$Name)
DSi.data<-Dataset[i.names,]
Mi.names<-matrix(Dataset[i.names,"Name"])

write.table(DSi.data,"dataset.csv", sep=",", 
            row.name=FALSE, col.names = TRUE)
write.table(Mi.names,"matrix.csv", sep=",", 
            row.name=FALSE, col.names = TRUE)
