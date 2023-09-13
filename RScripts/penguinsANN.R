pacman::p_load(tensorflow,
               keras,
               tidyverse,
               palmerpenguins
               )

# Big thank you to Sigrid Keydana, I used his talk at NairobiR Jan 2020
# to get this example

# !!! SAVE EVERYTHING BEFORE RUNNING THIS NEXT BIT!!!
# YOU WILL HAVE TO RESTART R MANUALLY BECAUSE IT MAY INSTALL,
# BUT CRASH AFTERWARDS

# installs the Python backend
# by default, will create a dedicated Miniconda environment
# named r-reticulate
# if you don't have Miniconda installed, you'll be prompted whether
# reticulate may install it for you
# install_tensorflow(restart_session=F)
# what is the TensorFlow (Python) version,
# and what environment does it run in?
# library(tensorflow)
tf_config()

penguins_df <- na.omit(penguins)
penguins_df <- penguins_df %>% 
  # need numerical input, preferredly starting from 0
  # conveniently, character data are already factors
  mutate_if(.predicate = is.factor, (function (x) as.numeric(x) - 1))

x <- penguins_df %>% 
  dplyr::select(-species) %>%
  map_at("island", ~ to_categorical(.x) %>% 
           array_reshape(c(length(.x),
                           length(levels(penguins$island))))) %>%
  map_at("sex", ~ to_categorical(.x) %>% 
           array_reshape(c(length(.x),
                           length(levels(penguins$sex))))) %>%
  abind::abind(along = 2)
y <- penguins_df %>%
  select(species) %>% 
  pull() %>%
  to_categorical()
train_indices <- sample(1:nrow(x), 200)
x_train <- x[train_indices, ]
x_val <- x[-train_indices, ]
y_train <- y[train_indices, ]
y_val <- y[-train_indices, ]


# Define architecture
model <- keras_model_sequential() %>%
  # a fully connected layer
  layer_dense(
    units = 32, # this layer has 32 "neurons"
    activation = "relu" # activation function is relu
  ) %>%
  # a stochastic ("noise") layer
  layer_dropout(0.2) %>%
  layer_dense(
    units = 32,
    activation = "relu"
  ) %>%
  layer_dropout(0.2) %>%
  # output layer has 3 units, for 3 classes
  layer_dense(units = 3, activation = "softmax")

# Loss function
# model %>% compile(
#   loss = "categorical_crossentropy",
#   optimizer = optimizer_rmsprop(),
#   metrics = "accuracy"
# )

# Optimiser
# model will be compiled AND overwrittenn!!! :-(
model %>% compile(
  loss = "categorical_crossentropy", 
  optimizer = optimizer_rmsprop(),
  metrics = "accuracy"
)

# Train it
history <- model %>% fit(
  x_train,
  y_train,
  validation_data = list(x_val, y_val),
  epochs = 100,
  batch_size = 8
)
 ##new
predictions <- predict(model, x_val)
predictions |> 
  as_tibble(.name_repair = 'unique') |> 
  mutate(species=factor(penguins_df$species[-train_indices])) |> 
  pivot_longer(contains('...')) |> 
  ggplot(aes(species,value, fill=name))+
  geom_boxplot()#+
  # facet_wrap(facets = vars(species))
