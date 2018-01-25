Name<-c("Jeb","Donald","Ted","Marco","Carly","Hillary","Bernie")
ABC_ppr<-c(4,62,51,21,2,14,15)
CBS_ppr<-c(12,75,43,19,1,21,19)

pollresults<-cbind(Name,ABC_ppr,CBS_ppr)
pollresults.df<-data.frame(Name,ABC_ppr,CBS_ppr)

rowMeans(pollresults.df)
averagepoll<-rowMeans(pollresults.df[,2:3])

avg_poll<-data.frame(Name,averagepoll)
