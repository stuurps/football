library(keras)
library(data.table)

data <- df
data$Date <- NULL
data$FTHG <- NULL
data$FTAG <- NULL
data$ID <- NULL

use_condaenv("r-tensorflow", required = T)
use_python("/anaconda3/envs/r-tensorflow/bin/python", required = T)

data <- as.data.frame(data)
y <- (data[, "FTR"])
y <- as.factor(y)

x <- subset(data, select = -c(FTR))

for(i in names(x)){
  if(length(table(x[[i]])) == 1){
    print(i)
    x[[i]] <- NULL
  }
}

dmy <- dummyVars(" ~ .", data = x)
x1 <- data.frame(predict(dmy, newdata = x))

x1 <- data.matrix(x1)

# one hot encode classes / create DummyFeatures
levels(y) = 1:length(y)
y = to_categorical(as.integer(y)-1, num_classes = 3)

# create sequential model
model = keras_model_sequential()

# add layers, first layer needs input dimension
model %>%
  layer_dense(input_shape = ncol(x1), units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 200, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 200, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 200, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax")

# add a loss function and optimizer
model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "Adam",
    metrics = "accuracy"
  )

# fit model with our training data set, training will be done for 200 times data set
fit = model %>%
  fit(
    x = x1,
    y = y,
    shuffle = T,
    batch_size = 5,
    validation_split = 0.2,
    epochs = 200
  )
plot(fit)

model %>% evaluate(x, y,verbose = 0)
