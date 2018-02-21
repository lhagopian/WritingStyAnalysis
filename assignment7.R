a<-c(3,5,7,2)
b<-matrix(c(3,5,7,2), nrow=2)
c<-table(c(3,5,7,2))

d<-list(name="Jill", age=42, married=T)
setClass("citizen",
         representation(
           name="character",
           age="numeric",
           married="logical")
)
e<-new("citizen", name="Jill", age=42, married=T)
d$name
e@name
isS4(d)
isS4(e)

class(a)
class(b)
class(c)
class(d)
class(e)


f<-list(nane="Jack", age="cat", married=F)
g<-new("citizen", nane="Jack", age=32, married=F)
g2<-new("citizen", name=71, age=32, married=F)
