# Code for test dataset

test=read_excel("test.xlsx")

# Removing the first column

test=test[,-1]
# No missing values for K, Ba, Fe

# Using KNN for filling up the missing values 

library(class)

missing_RI=knn(test[!is.na(test$RI),c(6,8,9)],test[is.na(test$RI),c(6,8,9)],test$RI[!is.na(test$RI)],k=5)
missing_RI=as.numeric(paste(missing_RI))
test$RI[is.na(test$RI)]=missing_RI

missing_Na=knn(test[!is.na(test$Na),c(6,8,9)],test[is.na(test$Na),c(6,8,9)],test$Na[!is.na(test$Na)],k=5)
missing_Na=as.numeric(paste(missing_Na))
test$Na[is.na(test$Na)]=missing_Na

missing_Mg=knn(test[!is.na(test$Mg),c(6,8,9)],test[is.na(test$Mg),c(6,8,9)],test$Mg[!is.na(test$Mg)],k=5)
missing_Mg=as.numeric(paste(missing_Mg))
test$Mg[is.na(test$Mg)]=missing_Mg

missing_Al=knn(test[!is.na(test$Al),c(6,8,9)],test[is.na(test$Al),c(6,8,9)],test$Al[!is.na(test$Al)],k=5)
missing_Al=as.numeric(paste(missing_Al))
test$Al[is.na(test$Al)]=missing_Al

missing_Si=knn(test[!is.na(test$Si),c(6,8,9)],test[is.na(test$Si),c(6,8,9)],test$Si[!is.na(test$Si)],k=5)
missing_Si=as.numeric(paste(missing_Si))
test$Si[is.na(test$Si)]=missing_Si

missing_Ca=knn(test[!is.na(test$Ca),c(6,8,9)],test[is.na(test$Ca),c(6,8,9)],test$RI[!is.na(test$Ca)],k=5)
missing_Ca=as.numeric(paste(missing_Ca))
test$Ca[is.na(test$Ca)]=missing_Ca

# model2 already defined
test_predict=predict(model2, newdata = test)

