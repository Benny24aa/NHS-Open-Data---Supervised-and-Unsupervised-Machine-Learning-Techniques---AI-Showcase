library(data.table)
library(phsopendata) # For loading in PHS data and keeping it as close to real time as possible
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(xgboost)

dfa <- get_resource(res_id = "a203c8fc-c19d-451c-b637-781ea7c2066c") 

dfa$PrescriberLocation <- as.numeric(dfa$PrescriberLocation)


gp_list <- get_resource(res_id = "30b06220-17ad-44e8-b6c5-658d41ec1ea5") %>% 
  select(PracticeCode, HB, HSCP, DataZone, GPCluster, PracticeListSize) %>% 
  unique() %>% 
  rename(PrescriberLocation = PracticeCode)

dfa <- dfa %>% 
  filter(PrescriberLocationType == "GP PRACTICE")

dfa <- left_join(gp_list, dfa, by = "PrescriberLocation")

dfa <- dfa %>%
  select(PaidDateMonth, PrescriberLocation, PrescriberLocationType,
         DispenserLocation, DispenserLocationType, NumberOfPaidItems, HB, HSCP, DataZone, GPCluster, PracticeListSize)

dfa <- dfa %>% mutate(ratio = NumberOfPaidItems/PracticeListSize)

# %>%
#   mutate(
#     PrescriberFullLocation = paste(PrescriberLocation, PrescriberLocationType, sep = " - "),
#     DispenserFullLocation = paste(DispenserLocation, DispenserLocationType, sep = " - "),
#     Pathway = paste(PrescriberFullLocation, "to", DispenserFullLocation)
#   ) %>%
#   select(PaidDateMonth, ratio, Pathway, HB, HSCP, DataZone, GPCluster, PracticeListSize)


dfa <- dfa %>%
  mutate(
    PaidDateMonth = lubridate::ym(PaidDateMonth),
    MonthNum = lubridate::month(PaidDateMonth),
    Year = lubridate::year(PaidDateMonth)
  )

train_index <- createDataPartition(dfa$ratio, p = 0.8, list = FALSE)

train_dfa <- dfa[train_index, ]
test_dfa <- dfa[-train_index, ]


set.seed(123)
rf_model_a <- randomForest(
  ratio ~ PrescriberLocation + DispenserLocation + DispenserLocationType + MonthNum + HB + HSCP + DataZone + GPCluster,
  data = train_dfa,
  ntree = 500,
  importance = TRUE
)

test_dfa$Predicted <- predict(rf_model_a, test_dfa)
test_dfa$Residual <- test_dfa$ratio - test_dfa$Predicted
test_dfa$AbsResidual <- abs(test_dfa$Residual)


threshold <- quantile(test_dfa$AbsResidual, 0.99)
test_dfa <- test_dfa %>%
  mutate(Outlier = AbsResidual > threshold)

not_lose <- test_dfa

#### Preparing for running markdown

health_board_input <- "All"

hb_list <- test_dfa %>% 
  select(HB) %>% 
  unique()

#### Converts dataframe to values list to be used in loop
hb_codes <- list(hb_list)
hb_codes <- unlist(hb_codes)

if(health_board_input == "All") {
  for (health_board_loop in hb_codes) {
    source("C:/Users/benny/Documents/My Resps/NHS-Open-Data---Supervised-and-Unsupervised-Machine-Learning-Techniques---AI-Showcase/Markdown Prep.R")
  }
} else {
  health_board_loop = health_board_input
  source("C:/Users/benny/Documents/My Resps/NHS-Open-Data---Supervised-and-Unsupervised-Machine-Learning-Techniques---AI-Showcase/Markdown Prep.R")
}


