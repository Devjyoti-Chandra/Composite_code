library(readxl)

# Reading file

excavate=read_excel("excavate.xlsx")

# Removing the first column

excavate=excavate[,-1]

# Checking the number of missing values for each variable

# table(is.na(excavate$RI))
# table(is.na(excavate$Na))
# table(is.na(excavate$Mg))
# table(is.na(excavate$Al))
# table(is.na(excavate$Si))
# table(is.na(excavate$K))
# table(is.na(excavate$Ca))
# table(is.na(excavate$Fe))
# table(is.na(excavate$`I.D. of glass`))

# No missing values for K, Ba, Fe

# Using KNN for filling up the missing values 

library(class)

missing_RI=knn(excavate[!is.na(excavate$RI),c(6,8,9,10)],excavate[is.na(excavate$RI),c(6,8,9,10)],excavate$RI[!is.na(excavate$RI)],k=5)
missing_RI=as.numeric(paste(missing_RI))
excavate$RI[is.na(excavate$RI)]=missing_RI

missing_Na=knn(excavate[!is.na(excavate$Na),c(6,8,9,10)],excavate[is.na(excavate$Na),c(6,8,9,10)],excavate$Na[!is.na(excavate$Na)],k=5)
missing_Na=as.numeric(paste(missing_Na))
excavate$Na[is.na(excavate$Na)]=missing_Na

missing_Mg=knn(excavate[!is.na(excavate$Mg),c(6,8,9,10)],excavate[is.na(excavate$Mg),c(6,8,9,10)],excavate$Mg[!is.na(excavate$Mg)],k=5)
missing_Mg=as.numeric(paste(missing_Mg))
excavate$Mg[is.na(excavate$Mg)]=missing_Mg

missing_Al=knn(excavate[!is.na(excavate$Al),c(6,8,9,10)],excavate[is.na(excavate$Al),c(6,8,9,10)],excavate$Al[!is.na(excavate$Al)],k=5)
missing_Al=as.numeric(paste(missing_Al))
excavate$Al[is.na(excavate$Al)]=missing_Al

missing_Si=knn(excavate[!is.na(excavate$Si),c(6,8,9,10)],excavate[is.na(excavate$Si),c(6,8,9,10)],excavate$Si[!is.na(excavate$Si)],k=5)
missing_Si=as.numeric(paste(missing_Si))
excavate$Si[is.na(excavate$Si)]=missing_Si

missing_Ca=knn(excavate[!is.na(excavate$Ca),c(6,8,9,10)],excavate[is.na(excavate$Ca),c(6,8,9,10)],excavate$RI[!is.na(excavate$Ca)],k=5)
missing_Ca=as.numeric(paste(missing_Ca))
excavate$Ca[is.na(excavate$Ca)]=missing_Ca



# Dividing into training and testing data sets

excavate$`I.D. of glass`=as.factor(excavate$`I.D. of glass`)
train=excavate[1:150,]
validation=excavate[150:200,]
library(e1071)
model1=svm(`I.D. of glass`~., data=train) # Prelimanary model.
p=predict(model1,newdata = train)

x = validation[,-10] # Excluding last result column.
y = validation$`I.D. of glass`
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial",tunecontrol = tune.control(cross=7,nrepeat = 2), ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

# print(svm_tune)

c=svm_tune$best.parameters$cost
g=svm_tune$best.parameters$gamma

model2=svm(`I.D. of glass`~., data=excavate, cost=c, gamma=g, kernel="radial")
pre=predict(model2, newdata = excavate)
table=table(pre, excavate$`I.D. of glass`)

library(caret)
cm=confusionMatrix(table)
confusionMatrix(table)

# F1 Score of different types of glass

cm$byClass

