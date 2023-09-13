# Get libraries loaded
pacman::p_load(keras,
               neuralnet,
               mlbench,
               dplyr,
               magrittr)

# Get data
data("BostonHousing")
data <- BostonHousing
str(data) #View(data)

# Factors to numbers
data %<>% mutate(across(.cols = where(is.factor),
                 .fns = as.numeric))

# Define architecture
n <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
               data = data,
               hidden = c(12,7),
               linear.output = F,
               lifesign = 'full',
               rep=100)

# Plot network
# plot(n,col.hidden = 'darkgreen',     
#      col.hidden.synapse = 'lightgreen',
#      show.weights = F,
#      information = F,
#      fill = 'lightblue')
predict(n,data[1:10,-14])
# Data as matrix
data_m <- as.matrix(data)
# take out dimension names
dimnames(data_m) <- NULL

# Create Datasets
set.seed(123)
ind <- sample(2, nrow(data_m), replace = T, prob = c(.7, .3))
training <- data_m[ind==1,1:13]
test <- data_m[ind==2, 1:13]
trainingtarget <- data_m[ind==1, 14]
testtarget <- data_m[ind==2, 14]
str(trainingtarget)
str(testtarget)

# mean
m <- colMeans(training)
# sd
s <- apply(X = training, 2, sd)

# scaling
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

# Create model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>%
  layer_dense(units = 1)

# Compile
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop', 
                  metrics = 'mae') 
# Fit
mymodel <- model %>%          
  fit(training,trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Predict
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2) 

# Confusion plot
plot(testtarget, pred) 
abline(lm(pred~testtarget))
# Make changes to model
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape = c(13)) %>%
  layer_dropout(rate=0.4)  %>%
  layer_dense(units = 50, activation = 'relu')  %>%
  layer_dropout(rate=0.2)  %>%
  layer_dense(units = 1)

model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2) 

# Confusion plot
plot(testtarget, pred) 
abline(lm(pred~testtarget))

pacman::p_load(rpart, rpart.plot, wrappedtools)
traindata <- BostonHousing[ind==1,]
testdata <- BostonHousing[ind!=1,]

treeformula <- paste('medv~',
                     paste(colnames(BostonHousing)[-14], collapse='+')) |> 
  as.formula()
tree_out <- rpart(treeformula, traindata,
                  control = list(minsplit=2))
prp(tree_out)
predicted_val <- predict(tree_out,newdata = testdata)
testdata |> 
  mutate(predicted=predicted_val) |> 
  ggplot(aes(medv,predicted))+
  geom_point()+
  geom_smooth(method='lm')           
