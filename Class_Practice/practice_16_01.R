library(dplyr)

WHdf=read.csv("womansHealth.csv")

WHdf=read.csv("womansHealth.csv", header=TRUE)

WHdf2=mutate(WHdf,TVINTAKE=A*C)
head(WHdf)
str(WHdf)
class(WHdf)
names(WHdf)
WHdf3<-mutate(WHdf,log_A=log(A))

# Creating categories
WHdf2<-mutate(WHdf2,IronCats=case_when(Iron<=10~1, 
                                       Iron>10 & Iron<=20~2,
                                       Iron>20 & Iron<=30~3,
                                       Iron>30 & Iron<=40~4,
                                       Iron>40 & Iron<=50~5,
                                       Iron>50 & Iron<=60~6,
                                       TRUE~7))
head(WHdf2)

#Replace values in a Dataset: Eliminate lowest values of Iron
WHdf2$IronCats[WHdf2$IronCats==1] <-77
head(WHdf2)

#Recheck Variable data types --change IronCats to factor
str(WHdf2)
WHdf2

WHdf3 <-WHdf2
WHdf3 <-mutate(WHdf3, Iron = replace(Iron, Iron>=0.000, NA))
head(WHdf3)
summary(WHdf3)

WHdf3 <-mutate(WHdf3, Protein = replace(Protein, Calcium<=350.0, NA))
head(WHdf3)
summary(WHdf3, n=1000)
summary(WHdf3)

#Rename varaiable : rename() in dplyr
WHdf3 <- rename(WHdf3, TVI = TVINTAKE, I_Cat = IronCats)
head(WHdf3)

