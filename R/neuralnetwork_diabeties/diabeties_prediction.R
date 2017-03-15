###############################################################################################
#Load required package
###############################################################################################
#Uncomment the below line to install the packages if not installed
#install.packages("nnet")
#install.packages("gmodels")
library(nnet)
library(gmodels)

###############################################################################################
#Load input Data
###############################################################################################
df<-read.csv(
  file = "diabetes.csv",
  sep = ",",
  header = TRUE
)

###############################################################################################
#Neural network model creation
###############################################################################################
#Extract the field details from dataframe
str(df)

#Remove N/A values from df 
df<-na.omit(df)

#convert integer value into categorical value prediction column
df[,"Outcome"]<-as.factor(df[,"Outcome"])

#Shuffle the data
df<-df[sample(nrow(df)),]
df.train<-df[1:614,] # 80 % of data for training Model
df.test<-df[615:768,] # 20 % of data for evaluate the Model

model<-nnet(Outcome~.,data = df.train,size=15)

###############################################################################################
#Accuracy check using CrossTable and Confusion Matrix
###############################################################################################
prediction <-predict(model,df.test,type="class")

# Display predicted values
prediction

CrossTable(prediction,df.test$Outcome,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
###############################################################################################
#Make prediction on new input data
###############################################################################################
#Need to provide the following input values in list
#Pregnancies	
#Glucose	
#BloodPressure	
#SkinThickness	
#Insulin	
#BMI	
#DiabetesPedigreeFunction	
#Age	
#Outcome - response field

inputdata <-list(10,115,0,0,0,35.3,0.314,29,"NA")
dataframe<-data.frame(inputdata,stringsAsFactors = TRUE)
colnames(dataframe)<-colnames(df)
levels(dataframe$Outcome)<-levels(df$Outcome)
Outcome <- predict(model,dataframe,type="class")
Outcome