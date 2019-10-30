#Using NN to predict the vital signs of the patient
#"FSTAT" is the dependent variable describing the vital status
#of the patient at the time of their last follow up

d0=read.csv(file.choose())
str(d0)
#to see the structure of the dataset
#it is useful, we identified id column which is not useful and 
#can be removed

#removing the first column and dates
d0$admitdate= NULL
d0$disdate = NULL
d0$fdate = NULL
d0$id=NULL

#installing library neuralnet
library(neuralnet)

#checking if there are any missing datapoints
#also from str(d0) we found that all variables are int, numeric except dates which are factors

18*500 #number of data point sin the dataset
length(!is.na(d0)) #hence no missing values

#scaling the input variables
d0=scale(d0)

#checking if scaled

summary(d0)
#hence scaled (all means=0)

set.seed(1)

#splitting on testand training dataset
n=nrow(d0)
n
train=sample(1:n, 0.75*n)
dtrain=d0[train,]
dtest=d0[-train,]


m1= neuralnet(formula = fstat ~ age+gender+hr+sysbp+diasbp+bmi+cvd+afb+sho+chf+av3+miord+mitype+year+los+dstat+lenfol+fstat, data=dtrain, hidden = 3, linear.output = FALSE)
summary(m1)

m1$response
m1$result.matrix

#plotting the model
#i plotted 2 hidden layers with 5 and 3 number of nodes respec.
plot(m1)

#compute predicted
class(dtest)
dtest=as.data.frame(dtest)
pred= compute(m1, dtest)$net.result

class(pred)

#creating confusion matrix
table(pred, dtest$fstat, dnn = c("predicted", "actual"))

