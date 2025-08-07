library(data.table)
library(phsopendata) # For loading in PHS data and keeping it as close to real time as possible
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(xgboost)

df <- get_resource(res_id = "a203c8fc-c19d-451c-b637-781ea7c2066c") 

df$PrescriberLocation <- as.numeric(df$PrescriberLocation)


gp_list <- get_resource(res_id = "30b06220-17ad-44e8-b6c5-658d41ec1ea5") %>% 
  select(PracticeCode, HB, HSCP, DataZone, GPCluster, PracticeListSize) %>% 
  unique() %>% 
  rename(PrescriberLocation = PracticeCode)

df <- df %>% 
  filter(PrescriberLocationType == "GP PRACTICE")

df <- left_join(gp_list, df, by = "PrescriberLocation")

df <- df %>%
  select(PaidDateMonth, PrescriberLocation, PrescriberLocationType,
         DispenserLocation, DispenserLocationType, NumberOfPaidItems, HB, HSCP, DataZone, GPCluster, PracticeListSize)

# %>%
#   mutate(
#     PrescriberFullLocation = paste(PrescriberLocation, PrescriberLocationType, sep = " - "),
#     DispenserFullLocation = paste(DispenserLocation, DispenserLocationType, sep = " - "),
#     Pathway = paste(PrescriberFullLocation, "to", DispenserFullLocation)
#   ) %>%
#   select(PaidDateMonth, NumberOfPaidItems, Pathway, HB, HSCP, DataZone, GPCluster, PracticeListSize)


df <- df %>%
  mutate(
    PaidDateMonth = lubridate::ym(PaidDateMonth),
    MonthNum = lubridate::month(PaidDateMonth),
    Year = lubridate::year(PaidDateMonth)
  )


set.seed(123)
rf_model <- randomForest(
  NumberOfPaidItems ~ PrescriberLocation + PrescriberLocationType + DispenserLocation + DispenserLocationType + MonthNum + HB + HSCP + DataZone + GPCluster + PracticeListSize,
  data = df,
  ntree = 500,
  importance = TRUE
)

df$Predicted <- predict(rf_model, df)
df$Residual <- df$NumberOfPaidItems - df$Predicted
df$AbsResidual <- abs(df$Residual)


threshold <- quantile(df$AbsResidual, 0.95)
df <- df %>%
  mutate(Outlier = AbsResidual > threshold)

library(plotly)

df_plot <- df %>%
  mutate(
    HoverText = paste0("Pescriber: ", PrescriberLocation,
                       "Dispenser: ", DispenserLocation,
                       "Prescriber Type: ", PrescriberLocationType,
                       "Dispenser Type: ", DispenserLocationType,
                       "<br>Actual: ", NumberOfPaidItems,
                       "<br>Predicted: ", round(Predicted),
                       "<br>Residual: ", round(Residual),
                       "<br>Health Board: ", HB,
                       "<br>Date: ", PaidDateMonth,
                       "<br>")
  )

a <- plot_ly(
  data = df_plot,
  x = ~Predicted,
  y = ~NumberOfPaidItems,
  type = 'scatter',
  mode = 'markers',
  color = ~Outlier,
  colors = c('FALSE' = 'blue', 'TRUE' = 'red'),
  text = ~HoverText,
  hoverinfo = 'text',
  marker = list(size = 10, opacity = 0.7)
) %>%
  layout(
    title = "Predicted vs Actual Number of Paid Items",
    xaxis = list(title = "Predicted NumberOfPaiItems"),
    yaxis = list(title = "Actual NumberOfPaidItems")
  )


a