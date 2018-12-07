###########
# set-up  #
###########
# installing required packages
lib <- c("tidyverse", "ggthemes", "caret", "rpart", "rpart.plot", "modelr", "gridExtra",
         "lubridate", "broom","stringr")

lapply(lib, require, character.only = TRUE)

df <- read.csv("crime.csv")
# Exploratory analysis 1
# Color can handle a maximum of 9 values; analysis is limited to 9 major crime codes
offense <- as.data.frame(table(df$OFFENSE_CODE_GROUP)) # list all the counts
offense <- offense[order(offense$Freq),]
offense <- offense[59:67,] # Most frequent offense
names <- c("Verbal Disputes", "Vandalism", "Simple Assault",
           "Drug Violation", "Other", "Investigate Person",
           "Medical Assistance", "Larceny", "Motor Vehicle Accident Response")

only9 <- df[df$OFFENSE_CODE_GROUP %in% names,] # dataframe with only 9 crimes
factor(only9$OFFENSE_CODE_GROUP) # drop the factor names

train_control <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    allowParallel = TRUE,
    seeds = c(1:51)
  )

####################
# resuable theme   #
####################
# reusable theme for model predictions
theme_thinkr <- theme_economist() + theme(
  rect = element_rect(fill = "#f9f5f1"),
  plot.title = element_text(size = 12),
  plot.subtitle = element_text(size = 6),
  strip.text = element_text(size = 9),
  axis.text.x = element_text(size = 7),
  legend.text = element_text(size = 7),
  plot.background = element_rect(fill = "#f9f5f1")
)

# our wrangled final data frame
only9 <- df[df$OFFENSE_CODE_GROUP %in% names,]

crime <-
  only9 %>%
  select(YEAR, MONTH, DISTRICT, OFFENSE_CODE_GROUP) %>%
  mutate(
    year=factor(YEAR),
    month=factor(MONTH),
    maj_cat=factor(OFFENSE_CODE_GROUP),
    borough=factor(DISTRICT)
  ) %>%
  filter(!(borough == "")) %>%
  group_by(year, month, borough, maj_cat) %>%
  summarise(count = n())

##############################
# Generalized Linear Model   #
##############################

# GLM
model_GLM <-
  train(
    count ~ .,
    data = crime,
    method = "glm",
    metric = "Rsquared",
    family = "poisson",
    trControl = train_control
  )

gather_residuals(crime, model_GLM, .resid = "resid", .model = "model") %>%
  ggplot(aes(count, resid, colour = maj_cat)) +
  geom_point() +
  ggtitle("GLM residuals spread out at higher counts") +
  geom_hline(yintercept = 20, lty = 2, size = 1) +
  geom_abline(intercept = 80, slope = 0.15, colour = "grey80", size = 2, lty = 3) +
  geom_abline(intercept = -80, slope = -0.17, colour = "grey80", size = 2, lty = 3) +
  scale_colour_economist() +
  theme_thinkr


crime %>%
  spread_predictions("Generalized Linear Model" = model_GLM) %>%
  gather(key = model,
         value = pred,-year,-month,-borough,-maj_cat,-count) %>%
  rename(act = count) %>%
  ggplot(aes(pred, act, colour = maj_cat)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_abline(colour = "black", lty = 2) +
  geom_text(
    x = 250,
    y = 50,
    aes(
      label = paste0(
        "Method = glm","\n",
        "Type = regression","\n",
        "RMSE = ",
        model_GLM$results$RMSE,"\n",
        "R-Squared = ",
        model_GLM$results$Rsquared)
    )) +
  facet_wrap( ~ model) +
  scale_colour_economist(name = "Major Category") +
  scale_y_continuous(breaks = seq(0, 300, by = 50),
                     limits = c(0, 300)) +
  scale_x_continuous(breaks = seq(0, 300, by = 50),
                     limits = c(0, 300)) +
  ggtitle("Generalized Linear Model") +
  labs(x = "Predictions", y = "Actual") +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  theme_thinkr

##############################
# Recursive partitioning     #
##############################
## Recursive partitioning

tune_grid <-
  expand.grid(
    cp = 0.00001 
  )

model_RP <-
  train(
    count ~ .,  # the effect of interacting term is negligible in this model
    data = crime,
    method = "rpart",
    metric = "RMSE",
    parms = list(method = "poisson"),
    tuneGrid = tune_grid,
    trControl = train_control
  )

##############################
# Random Forest              #
##############################

tune_grid <-
  expand.grid(
    mtry = 2,
    splitrule = "variance",
    min.node.size = 5
  )

model_RF2 <-
  train(
    count ~ .,
    data = crime,
    method = "ranger",
    num.trees = 500,
    metric = "RMSE",
    respect.unordered.factors = TRUE,
    tuneGrid = tune_grid,
    trControl = train_control
  )


crime %>%
  spread_predictions("Random Forest | mtry = 02" = model_RF2) %>%
  gather(key = model,
         value = pred,-year,-month,-borough,-maj_cat,-count) %>%
  rename(act = count) %>%
  ggplot(aes(pred, act, colour = maj_cat)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_abline(colour = "black", lty = 2) +
  geom_text(
    x = 250,
    y = 50,
    aes(
      label = paste0(
        "Method = ranger","\n",
        "Type = regression","\n",
        "RMSE = ",
        model_RF2$results$RMSE,"\n",
        "R-Squared = ",
        model_RF2$results$Rsquared)
    )) +
  facet_wrap( ~ model) +
  scale_colour_economist(name = "Major Category") +
  scale_y_continuous(breaks = seq(0, 300, by = 50),
                     limits = c(0, 300)) +
  scale_x_continuous(breaks = seq(0, 300, by = 50),
                     limits = c(0, 300)) +
  ggtitle("Random Forest") +
  labs(x = "Predictions", y = "Actual") +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  theme_thinkr

###############################
# Stochastic Gradient Boosting#         
###############################

tune_grid <-
  expand.grid(
    interaction.depth = 10, 
    n.trees = 500,
    shrinkage = 0.1,
    n.minobsinnode = 5 
  )

model_SGB <-
  train(
    count ~ .,
    data = crime,
    distribution = "poisson",
    method = "gbm",
    metric = "RMSE",
    tuneGrid = tune_grid,
    verbose = FALSE,
    bag.fraction = 0.5,
    trControl = train_control
  )

crime %>%
  spread_predictions("Stochastic Gradient Boossting" = model_SGB) %>%
  gather(key = model,
         value = pred,-year,-month,-borough,-maj_cat,-count) %>%
  rename(act = count) %>%
  ggplot(aes(pred, act, colour = maj_cat)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_abline(colour = "black", lty = 2) +
  geom_text(
    x = 250,
    y = 50,
    aes(
      label = paste0(
        "Method = gbm","\n",
        "Type = regression","\n",
        "RMSE = ",
        model_SGB$results$RMSE,"\n",
        "R-Squared = ",
        model_SGB$results$Rsquared)
    )) +
  facet_wrap( ~ model) +
  scale_colour_economist(name = "Major Category") +
  scale_y_continuous(breaks = seq(0, 300, by = 50),
                     limits = c(0, 300)) +
  scale_x_continuous(breaks = seq(0, 300, by = 50),
                     limits = c(0, 300)) +
  ggtitle("Stochastic Gradient Boosting") +
  labs(x = "Predictions", y = "Actual") +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  theme_thinkr

##########
# cubist #         
##########

tune_grid <-
  expand.grid(
    committees = 80, 
    neighbors = 9 
  )

model_Cub <-
  train(count ~ .,
        data = crime,
        method = "cubist",
        metric = "RMSE",
        tuneGrid = tune_grid,
        trControl = train_control
  )

crime %>%
  spread_predictions("model_Cubist" = model_Cub) %>%
  gather(key = model,
         value = pred,-year,-month,-borough,-maj_cat,-count) %>%
  rename(act = count) %>%
  ggplot(aes(pred, act, colour = maj_cat)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_abline(colour = "black", lty = 2) +
  geom_text(x = 250, y = 50,
            aes(
              label = paste0(
                "Method = cubist","\n",
                "Type = regression","\n",
                "RMSE = ",
                model_Cub$results$RMSE,"\n",
                "R-Squared = ",
                model_Cub$results$Rsquared)
            )) +
  facet_wrap( ~ model) +
  scale_colour_economist(name = "Major Category") +
  scale_y_continuous(breaks = seq(0, 300, by = 50),
                     limits = c(0, 300)) +
  scale_x_continuous(breaks = seq(0, 300, by = 50),
                     limits = c(0, 300)) +
  ggtitle("Cubist") +
  labs(x = "Predictions", y = "Actual") +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  theme_thinkr