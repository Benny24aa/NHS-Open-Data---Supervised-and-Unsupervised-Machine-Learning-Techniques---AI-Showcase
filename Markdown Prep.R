
health_board <- health_board_loop

library(flextable)


test_dfa_markdown <- test_dfa %>% 
  filter(HB == health_board)

test_dfa_markdown <- test_dfa_markdown %>% mutate(PotentialFraud = AbsResidual > threshold)



fraud_data <- test_dfa_markdown %>% 
  filter(PotentialFraud == TRUE) %>%
  group_by(PrescriberLocation, DispenserLocation, HB, DispenserLocationType, PrescriberLocationType) %>%
  summarise(
    AvgResidual = mean(Residual),
    MaxRatio = max(ratio),
    Count = n()
  ) %>%
  arrange(desc(AvgResidual))



flextable_format <- function(data) {
  data %>%
    flextable() |>
    bold(part = "header") %>%
    bg(bg = "#43358B", part = "header") %>%
    color(color = "white", part = "header") %>%
    align(align = "left", part = "header") %>%
    valign(valign = "center", part = "header") %>%
    valign(valign = "top", part = "body") %>%
    colformat_num(big.mark = ",") %>%
    fontsize(size = 8, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    border(border = fp_border_default(color = "#000000", width = 0.5), part = "all") |>
    autofit()
}

fraud_data_table <- fraud_data %>% 
  flextable_format() %>% 
  set_table_properties("autofit")


plot_graph <- test_dfa %>% 
  filter(HB == health_board)


library(ggplot2)

final_graph <- ggplot(plot_graph, aes(x = Predicted, y = ratio, color = Outlier)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(
    title = "Test Data - Actual Ratio vs Predicted Ratio",
    x = "Predicted Ratio",
    y = "Actual ratio"
  ) +
  theme_minimal()


#### Code run for report
# Produces the report
message("Producing report for ", health_board)
rmarkdown::render(
  "C:/Users/benny/Documents/My Resps/NHS-Open-Data---Supervised-and-Unsupervised-Machine-Learning-Techniques---AI-Showcase/Fraud_Reports.Rmd",
  params = list(
    health_board_rmd = health_board
  ),
  
  
  # Naming the output file with date and health board name
  output_file = paste0(
    "C:/Users/benny/Documents/My Resps/NHS-Open-Data---Supervised-and-Unsupervised-Machine-Learning-Techniques---AI-Showcase/", Sys.Date(), "_",
    gsub(" ", "_", health_board),
    ".docx"
  )
)



# plot_graph_report <- plot_graph %>%
#   mutate(
#     HoverText = paste0("Pescriber: ", PrescriberLocation,
#                        "<br>Dispenser: ", DispenserLocation,
#                        "<br>Dispenser Type: ", DispenserLocationType,
#                        "<br>Actual: ", ratio,
#                        "<br>Predicted: ", Predicted,
#                        "<br>Residual: ", Residual,
#                        "<br>Health Board: ", HB,
#                        "<br>Date: ", PaidDateMonth,
#                        "<br>")
#   )
# 





# 
# a <- plot_ly(
#   data = dfa_plot,
#   x = ~Predicted,
#   y = ~ratio,
#   type = 'scatter',
#   mode = 'markers',
#   color = ~Outlier,
#   colors = c('FALSE' = 'blue', 'TRUE' = 'red'),
#   text = ~HoverText,
#   hoverinfo = 'text',
#   marker = list(size = 10, opacity = 0.7)
# ) %>%
#   layout(
#     title = "Test Data - Actual Ratio vs Predicted Ratio",
#     xaxis = list(title = "Predicted Ratio"),
#     yaxis = list(title = "Actual ratio")
#   )
# 
# a
# 
# 
# 
# 
# 
# 
# write.csv(test_dfa, "Test_Data_Random_Forest.csv")
# write.csv(train_dfa, "Training_Data_Random_Forest.csv")
# write.csv(test_dfa_fraud, "Potential_Fraud_List.csv")


# train_dfa$Predicted <- predict(rf_model_a, train_dfa)
# train_dfa$Residual <- train_dfa$ratio - train_dfa$Predicted
# train_dfa$AbsResidual <- abs(train_dfa$Residual)
# 
# mae_train <- mean(train_dfa$AbsResidual, na.rm = TRUE)
# mae_test <- mean(test_dfa$AbsResidual, na.rm = TRUE)
# 
# # R-squared (optional)
# r2_train <- cor(train_dfa$ratio, train_dfa$Predicted, use = "complete.obs")^2
# r2_test <- cor(test_dfa$ratio, test_dfa$Predicted, use = "complete.obs")^2
# 
# cat("Train MAE:", mae_train, "\nTest MAE:", mae_test, "\n")
# cat("Train R²:", r2_train, "\nTest R²:", r2_test, "\n")