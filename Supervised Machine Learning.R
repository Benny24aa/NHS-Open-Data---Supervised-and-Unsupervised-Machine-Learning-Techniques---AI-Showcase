library(data.table)
library(phsopendata) # For loading in PHS data and keeping it as close to real time as possible
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(xgboost)

df <- get_resource(res_id = "fa5bbede-475a-4ca9-a71f-3d521657e7c6") # 2024 Prescribed and Dispensed Data (Used this as it's fullly complete)

df <- df %>% 
  select(PaidDateMonth, PrescriberLocation, PrescriberLocationType, DispenserLocation, DispenserLocationType, NumberOfPaidItems)

df <- df %>% 
  select(PaidDateMonth, PrescriberLocation, PrescriberLocationType, DispenserLocation, DispenserLocationType, NumberOfPaidItems) %>%
  mutate(
    PrescriberFullLocation = paste(PrescriberLocation, PrescriberLocationType, sep = " - "),
    DispenserFullLocation = paste(DispenserLocation, DispenserLocationType, sep = " - "),
    Pathway = paste(PrescriberFullLocation, "to", DispenserFullLocation)
  ) %>% 
  select(PaidDateMonth, NumberOfPaidItems, Pathway)