library(keras)

data <- md_train
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
x <- data.matrix(x)
# one hot encode classes / create DummyFeatures
levels(y) = 1:length(y)
y = to_categorical(as.integer(y)-1, num_classes = 3)
x <- as.matrix(x)
# create sequential model
model = keras_model_sequential()

# add layers, first layer needs input dimension
model %>%
  layer_dense(input_shape = ncol(x), units = 10, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax")

# add a loss function and optimizer
model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

# fit model with our training data set, training will be done for 200 times data set
fit = model %>%
  fit(
    x = x,
    y = y,
    shuffle = T,
    batch_size = 5,
    validation_split = 0.3,
    epochs = 200
  )
plot(fit)