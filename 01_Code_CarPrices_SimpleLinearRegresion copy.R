#DESCRIPIPTION##################################################################

#Title: Simple Linear regression
#Version: 01
#Description: How to build a simple linear regression & applied case
#Data source: https://www.kaggle.com/datasets/hellbuoy/car-price-prediction?select=CarPrice_Assignment.csv
#Report: pending

# 00 THE CASE AND INTODUCTION  #################################################


TheCase<- "The strategic team of marketing, pricing, and financial planing know that the
bigues car manufactor competitr is going to releace a new line of car, they know
that the sice of the engienw wold be arrondu 200 units. They wold like to know
what prices to espct from the competitor, if the engine is 180, 200, or 220 units"


# 01 PREPARE THE OPERATION ####################################################

## PREPARE THE LIBRAIES ========================================================

library(dplyr)
library(readxl)
library(ggcorrplot)
library(RGraphics)
library(gridExtra)
library(ggplot2)
library(gt)

## PREPARE THE THEMES ==========================================================

###Define Colors----------------------------------------------------------------
mainColor="#FFB301"
SecondaryColor="#FBDA80"
dark1="#2A2E34"
titleTextColorTable="white"
titleTextColorTable2="black"
G1_High="#FFB301"
G1_Low="#2A2E34"
SoftGrey="#666666ff"
SoftGrey2="#E7E6E6"

### check your colors-----------------------------------------------------------

#Build a vector whit the collection of colors
colorColection<-cbind(
  mainColor,
  SecondaryColor,
  dark1,
  titleTextColorTable,
  titleTextColorTable2,
  G1_High,
  G1_Low,
  SoftGrey,
  SoftGrey2
  )


# Create the verification theme
themeColorcheck<-ttheme_minimal(
  core=list(
    bg_params=list(fill=colorColection)
  )
)

# Print the verification table
grid.table(colorColection,theme=themeColorcheck)


#Define Themes------------------------------------------------------------------

#Theme Table 1
themeTable1<-ttheme_default(
  colhead=list(
    bg_params=list(fill=mainColor),
    fg_params=list(col=titleTextColorTable)
    )
)


#Theme Text Box 1
themeTextBox1<-ttheme_default(
  colhead=list(
    bg_params=list(fill=SoftGrey),
    fg_params=list(col=titleTextColorTable)
  )
)

#Theme Text Box 2
themeTextBox2<-ttheme_minimal(
  colhead=list(
    bg_params=list(fill=SoftGrey),
    fg_params=list(col=titleTextColorTable)
  )
)


#Theme Text Box 3
themeTextBox3<-ttheme_minimal(
  colhead=list(
    bg_params=list(fill=SoftGrey2),
    fg_params=list(col=titleTextColorTable2)
  )
)


# 02 PREPARE THE  INFORMATION ##################################################

#Import data
main_data<-read.csv("CarPrice_Assignment.csv")
head(main_data)
fieldNames<-as.data.frame(colnames(main_data))
grid.table(fieldNames,theme=themeTable1)
grid.table(head(main_data[,1:5]),theme=themeTable1)

dataDesciption<-"The CarPrice_Assimnent file shows diferent characteristics of diferent
car brands and diferent models including the price"


#Know the class of the variables (Columns)
class_summary<-sapply(main_data,class)
class_summary
class_summary_table<-as.data.frame(table(class_summary))
grid.table(class_summary_table,theme=themeTable1, rows= NULL)

#Select the numeric variables
main_data_numeric <- select_if(main_data, is.numeric)
classcol<-sapply(main_data_numeric,class)
classcol
clascsolTable<-as.data.frame(classcol)
grid.table(clascsolTable,theme=themeTable1)
table(classcol) # Summary the variable class


## 02.1Data cleaning ===============================================================

# This is one of the most important step in any project, this will be illustrated in a future version

# 03 BULDING CORRELATIN MATRIX #################################################

#Helps to decide in this protect whit 

# Build and plot the correlation MATRIX
corr <- round(cor(main_data_numeric), 2)
ggcorrplot(corr,
           type = "lower", 
           lab=TRUE,
           outline.color = "white",
           colors = c(G1_Low, "white", G1_High)
           )


# 04 DEFINE VIRABLES ###########################################################


## Select the depending and independent variables ==============================
dependentCol<-"price"
independentCol<-"enginesize"

colnames(main_data_numeric)[colnames(main_data_numeric)==dependentCol]<- "dependent"
colnames(main_data_numeric)[colnames(main_data_numeric)==independentCol]<- "independent"

Simple_linear_DB<-select(main_data_numeric,c("dependent","independent"))
Simple_linear_DB

## Check lineal behavior ======================================================


ggplot(Simple_linear_DB, aes(x = independent, y = dependent)) + #define x and y axis variables
  geom_point(colour = mainColor)+ #ad scatter plot points
  xlab(independentCol) + ylab(dependentCol)

#this is just an quick test in future versions more test will be included.
#If there is reasons to believe that the relation is not linear, you should  select other variable.



# 05 BUILD THE LINEAR REGRESION ###############################################

Linear_regression_Model  <- lm(dependent ~ independent, data = Simple_linear_DB )
Linear_regression_Model 
summary(Linear_regression_Model)


## 05.1 Equation ================================================================


Linear_regression_Model 

summry_coefficients<-Linear_regression_Model$coefficients
interception<-round(data.frame(summry_coefficients)[1,1],2)
coeffient01<-round(data.frame(summry_coefficients)[2,1],2)

equation<-paste("y=",interception,if(coeffient01>0){"+"},coeffient01,"x")
equation
equationNames<-paste(dependentCol,"=",interception,if(coeffient01>0){"+"},coeffient01,independentCol)
equationNames

TITLE <- "THE EQUATION"
BODY<- c(equation,equationNames)
textBox<-data.frame(BODY)
colnames(textBox)[1]<- TITLE
textBox
grid.table(textBox,theme=themeTable1, rows = NULL) #print it




## 05.2 Model elements =========================================================


#  r square
r_squared<-summary(Linear_regression)$r.squared
r_squared<-round(r_squared,2)
r_squaredr_squared_Coment<-paste("In this model the dependent variable ("
                            ,dependentCol,
                            ") beahivior is explaned in "
                            ,r_squared, 
                            "by the independent variables")

TITLE <- "r square coment"
BODY<- r_squaredr_squared_Coment
textBox<-data.frame(BODY)
colnames(textBox)[1]<- TITLE
textBox
grid.table(textBox,theme=themeTable1, rows = NULL) #print it


# Summary of the coefficients.

sumarycoeficients<-summary(Linear_regression)$coefficients 
sumarycoeficients<-round(sumarycoeficients,2)
sumarycoeficients
grid.table(sumarycoeficients,theme=themeTable1)

#pending conclones! de los valores

## 05.3 TESTING THE MODEL ######################################################
#Pending: this would be include in a future version

# 06. THE PREDICTION INTERVAL FOR AN INDIVIDUAL RESPONSE########################

# CREATE THE 

#The model
Linear_regression_Model

#Create the preDICTINT
confidence<-predict(Linear_regression_Model, interval = 'confidence') #all the confidence model
head(confidence) #note that ggplot plot this with stat_smooth(method = lm)

predictions <- predict(Linear_regression_Model, interval = 'predict') #all the predictive model
head(predictions)

data_and_predictions <- cbind(Simple_linear_DB, predictions)
head(data_and_predictions)

equationNames

#create plot
ggplot(data_and_predictions, aes(x = independent, y = dependent)) + #define x and y axis variables
    geom_point(color="white") + #add scatterplot points
    stat_smooth(method = lm, color=dark1) + #linear model and confidence bands
    geom_line(aes(y = lwr), col = dark1, linetype = "dashed") + #lwr pred interval
    geom_line(aes(y = upr), col = dark1, linetype = "dashed") +#upr pred interval
    theme(panel.background = element_rect(fill = SecondaryColor))+
    xlab(independentCol) + ylab(dependentCol)
    

# PREDICT THE PARITCULAR DATA===================================================

predictValue1=150
predictValue2=180
predictValue3=200
  
predic_data <- data.frame(independent=c(predictValue1,predictValue2,predictValue3))
predict(data.lm, newdata = predic_data)
predict(data.lm, newdata = predic_data, interval = 'confidence')
summary_predic_data<-round(predict(data.lm, newdata = predic_data, interval = 'prediction'),2)
#pending- predictive cvalues indluirs en la tabla!!!!!!!!!
grid.table(summary_predic_data,theme=themeTable1, rows=NULL)

#The way to interpret these values is as follows:

R1<-paste("The 95% prediction interval of then", dependentCol, " for a car with a \n", independentCol, "of",predictValue1,"is between ",summary_predic_data[1,2],"and",summary_predic_data[1,3])
R2<-paste("The 95% prediction interval of the", dependentCol, " for a car with a \n", independentCol, "of",predictValue2,"is between ",summary_predic_data[2,2],"and",summary_predic_data[1,3])
R3<-paste("The 95% prediction interval of the", dependentCol, " for a car with a \n", independentCol, "of",predictValue3,"is between ",summary_predic_data[3,2],"and",summary_predic_data[1,3])

#Report it
TITLE <- "The way to interpret these values is as follows:"
BODY<- c(R1,R2,R3)
textBox<-data.frame(BODY)
colnames(textBox)[1]<- TITLE
textBox
grid.table(textBox,theme=themeTextBox1, rows = NULL) #print it


