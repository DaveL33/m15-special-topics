# Introductory example using the housing data used here: 
# http://www.r2d3.us/visual-intro-to-machine-learning-part-1/
library(rpart)
library(rpart.plot)

# Read in data
homes <- read.csv('data/housing-data.csv')

# Function to compare values
AssessFit <- function(model, data = homes, outcome = 'in_sf') {
  predicted <- predict(model, data, type='class')
  accuracy <- length(which(data[,outcome] == predicted)) / length(predicted) * 100
  return(accuracy)
}

# Assess fit for different models

basic.fit <- rpart(in_sf ~ ., data = homes, method="class")

AssessFit(basic.fit)

basic.fits <- vector()
perfect.fits <- vector()

# Sample size for training dataset
sample.size <- floor(.75 * nrow(homes))
for(i in 1:100) {
 
  train.indicies <- sample(seq_len(nrow(homes)), size = sample.size)
  training.data <- homes[train.indicies,]
  test.data <- homes[-train.indicies,]
  
  
  # 2. Pass your **training data** to the `rpart` function to run a simple classification operation
  basic.fit <- rpart(in_sf ~ ., data = training.data, method="class")
  
  
  # 3. Pass your results to the `AssessFit` function to assess the fit
  assessment <- AssessFit(basic.fit, data=test.data)
  
  
  # 4. Store your assessment in the `basic.fits` vector
  basic.fits <- c(basic.fits, assessment)
  
}

# 5. Make a histogram of your `basic.fits` vector
hist(basic.fits)

# 6. Take the mean of your `basic.fits` vector
mean(basic.fits)


# 7. Pass your most recent model to the `rpart.plot` function to graph it
rpart.plot(basic.fits)
