---
title: "CIA LAB TEST-I"
author: "MANOJ KUMAR - 2048015"
date: "12/02/2021"
output: word_document
---



# 1-a. Create a character vector of 5 elements and assign a unique value to each element in the vector.
```{r}
# To create a character vector of 5 elements named Students_details.
character_details <- make.unique(c("character","character","character","character","character"),sep = "-")

# To check the type of the object.
typeof(character_details)

# Listing the character elements which created.
character_details
```
   
*Insights*   
   
    -We have created Character vector of 5 elements using make.unique() and c() function, namely *character_details*.
    -Type of Vector is identified using typeof() predefined function. Here our character_details is of *Character* type.
    -Listing the unique elements by directly calling *character_details*.
   
# 1-b.Create the matrix below
  
    [,1][,2][,3]
    [1,] 5 -5 5
    [2,] 5 -5 5
    [3,] 5 -5 5 and also display the transpose of the matrix

```{r}

# Create a matrix.

# Element list
matrix_element = c(5, -5, 5, 5, -5, 5, 5, -5, 5)

# Creating Matrix using matrix() predefined function. 
main_matrix = matrix( matrix_element, 
                      nrow = 3,         # No. of row
                      ncol = 3,         # No. of Column
                      byrow = TRUE      # Order type by Row
                      )

# To display our main matrix 
main_matrix
```
*Insights*   
   
    -There are different types of methods to define or create the Matrix, Here I have listed all the provided elements *matrix_element* and used *matrix()* to buuild our required matrix. 
    -nrow arg used to mention no. of row we needed
    -ncol arg used to mention no. of column we needed
    -byrow arg used to specify the filling type.
    
```{r}
# To display the transpose of the matrix

matrix_transpose = t(main_matrix)
matrix_transpose
```
*Insights* 

    -t() In-built function is used to transpose of the matrix

# 2.Study and interpret the results for the given data and fit the appropriate regression model      

      
#1. Import the income data and perform the exploratory data analysis
```{r}
# Importing the Income Data.
Income_dataframe <- read_csv("income.csv")

str(Income_dataframe)

# Exploring data
View(Income_dataframe)
```
*Insights* 

    -Here shared *income.csv* dataset having two numerical columns in it, namely Experience and Salary of an Employee we assume.
    -Experience is haven't having rounded values by years. Insteard of its having Actucal Decimal value, which may include months perios as well.
    -Salary is populated in normal form which actually makes sense to work.
    

```{r}
# Summary-Exploratory Data Analysis
summary(Income_dataframe)
```

*Insights* 

    -Minimum years of Experience is 1.1 in year 
    -Minimum Amount of Salary is 37731 (preferably currency is not mentioned) 
    -Maximum years of Experience is 10.5, which is 10 and half years. 
    -Maximum Amount of Salary provided is 122391 (preferably currency is not mentioned) 
    -Using *summary()* we come to all other basic stats features.
    
# 2. Use appropriate plot to visualize the relationship between the variables and outliers

```{r}
# Using scatter plot

x = Income_dataframe$Experience
y = Income_dataframe$Salary

scatter.smooth(x, 
               y, 
               xlab = "Experience", ylab = "Salary",  # X,Y-Lab
               main = "Relationship between the Experience & Salary",       # Title
               col.main = "Black",                 # Title lab color
               cex.lab = "1" ,                     # X,Y-axis Lab color
               pch = 19
)
```
*Insights* 

    -Scatter plot is used to visualize the relationship between the variables
    -The position of each dot on the horizontal and vertical axis indicates values for an individual data point.
    -Relationships between variables can be described and found *positive, strong, and linear*.
    

```{r}
boxplot(x, main = "Experience", border = "black" )
```
*Insights* 

    -boxplots are useful to detect potential outliers.
    -Clear no outliers to seem to be present in Experience, as our boxplot came out well.
    

```{r}
boxplot.default(y, main = "Salary", border = "orange")
# no outliers
```
*Insights* 

    -boxplots are useful to detect potential outliers.
    -Clear no outliers to seem to be present in Salary, as our boxplot came out well and good.
    
# 3. Use Density plot to check the normality and also check the correlation analysis.

```{r}
# A density plot shows the distribution of a numeric variable.

library(e1071)       # For skewness function
par(mfrow = c(1,2))  # Dividing graph area in 2 columns

# Density plot for 'Experience'
plot(density(x), 
     main = "Density : Experience",
     ylab = "Frequency",
     sub=paste("Skewness :",
               round(e1071::skewness(x),
                     2
                     )
               )
     ) 
polygon(density(x), col = "orange")

# Density plot for 'Salary'
plot(density(y), 
     main = "Density : Salary",
     ylab = "Frequency",
     sub=paste("Skewness :",
               round(e1071::skewness(y),
                     2
                     )
               )
     ) 

polygon(density(y), col = "blue")
```
```{r}
cor(x, y)
```
*Insights* 

    -A density plot is a representation of the distribution of a numeric variable.
    -Numeric variables namely *Experience* and *Salary*.
    -We estimated density to show the probability density function of the *Experience* and *Salary*
    -We can clearly see the hump in between the distribution of salary.
    
    -Correlation analysis studies the strength of relationship between two continuous variables.
    -Correlation values are colser to 1 so, *strong positive correlation*.

# 4. Calculate the model summary, model coefficients, estimate and error for the predictor variable.

```{r}
# Simple Linear Regression Model
LinearRegressionModel <- lm(formula = Experience ~ Salary, 
                            data = Income_dataframe)

# Summary
summary(LinearRegressionModel)
```


```{r}
# Coefficients
LinearRegressionModel
```

```{r}
t.test(x, y,
       paired = TRUE,
       alternative = "two.sided"
       )
```
*Insights* 

    -When p Value is less than significance level ( 2.44 > 0.05), we can *Accept the Null Hypothesis* that the co-efficient β of the predictor is zero.
    -Hence, there is no significant difference.

# 5. Fit the appropriate Regression model.
```{r}
# Create the training and test data (70:30)
set.seed(100)
rows = sample(nrow(Income_dataframe))

# Randomly order data:
data = Income_dataframe[rows, ]

# Identify row to split on: split
split = round(nrow(data) * .70)

# Create train
train = data[1:split,]
train

# Create test
test = data[(split + 1): nrow(data),]
test
```


```{r}
# Fit the model on training data and predict Salary on test data.

Model = lm(formula = Experience ~ Salary, data = train )
Model

Predict = predict(Model, test)
```
*Insights* 

    -Create the training and test data (70:30) and Randomly order data:
    -Fit the model on training data and predict Salary on test data.
    
# 6. Capture the summary of the model and review the diagnostic measures.
```{r}
summary(Model)
```

```{r}
plot(Model)
```
*Insights* 

    -The fitted model equation is given by 'Y = a (X) * b'. It has a Coefficient of Determination value of 0.9584 and an adjusted R2 value of 0.9562, which shows that the variability of employees' variability through their Experience is explained less than the full model.
    -The residual plot in the second model is slightly more linear, which indicates more variability. The Q-Q plot shows a higher digression of residuals from the complete model, which shows the residuals are less expected.
    -The Scale Location Graph shows its values concentrated even more towards the positive side, which indicates the residuals are not balanced. The Residuals v/s Leverage graph shows that though there are no visible outliers through the Cook's Distance method, there are more further apart in the fitted model.
