#install.packages("tensorflow")
#install.packages("keras")
library(tensorflow)
library(keras)

#read in dataframe
df <- read.csv(file = 'iris.csv')

#split data into 70% train and 30% test
set.seed(1)
sample <- sample.int(n = nrow(df), size = floor(.7*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

#create model with 2 layers for neural net
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 32, input_shape = ncol(df)) %>% 
  layer_activation('relu') %>% 
  layer_dense(units = 2) %>% 
  layer_activation('softmax')
 
#compile model for binary classification
 model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)


#fit train data to model
#cant get this to work
model %>% fit(
 train, 
 epochs = 100, 
 batch_size = 5,
 validation_split = 0.3
)

#summary(model)

#predict on test data using model
#predictions <- model %>% predict_classes(test)

