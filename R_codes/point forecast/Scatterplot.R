# SCATTERPLOT
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

selected_rows <- 1:12

process_data <- function(data, h_label) {

  df <- as.data.frame(data[selected_rows, ])
  df$Country <- rownames(df)
  df_long <- pivot_longer(df,cols = c("HUts", "HU", "HUrob", "wHU"),names_to = "Method",
                          values_to = "MSE")
  df_long$Method <- factor(df_long$Method, levels = c("HUts", "wHU", "HUrob", "HU"))
  df_long$h_value <- h_label
 return(df_long)
}

H1_long <- process_data(sqrt(H1), "h=1")
H5_long <- process_data(sqrt(H5), "h=5")
H10_long <- process_data(sqrt(H10), "h=10")

combined_data <- bind_rows(H1_long, H5_long, H10_long)
new_h_labels <- c("h=1" = "(a)","h=5" = "(b)","h=10" = "(c)")
combined_data$h_value <- factor(combined_data$h_value, 
                                levels = c("h=1", "h=5", "h=10"),
                                labels = new_h_labels)
combined_data$Method <- factor(combined_data$Method, levels = c("HUts", "wHU", "HUrob", "HU"))
custom_shapes <- c("HUts" = 16, "wHU" = 17, "HUrob" = 15, "HU" = 18)
custom_colors <- c("HUts" = "red", "wHU" = "orange", "HUrob" = "darkgreen", "HU" = "blue")

ggplot(combined_data, aes(x = Country, y = MSE)) +
  geom_point(data = subset(combined_data, Method == "wHU"),aes(color = Method, shape = Method),
             size = 4, alpha = 0.6, position = position_dodge(width = 0.7)) +
  geom_point(data = subset(combined_data, Method == "HUrob"),aes(color = Method, shape = Method),
             size = 4, alpha = 0.6, position = position_dodge(width = 0.7)) +
  geom_point(data = subset(combined_data, Method == "HU"),aes(color = Method, shape = Method),
             size = 4, alpha = 0.6, position = position_dodge(width = 0.7)) +
  geom_point(data = subset(combined_data, Method == "HUts"),aes(color = Method, shape = Method),
             size = 4, alpha = 0.6, position = position_dodge(width = 0.7)) +
  labs(x = "",y = "RMSE",title = "",color = "Model",shape = "Model") +
  scale_color_manual(values = custom_colors,breaks = c("HUts", "wHU", "HUrob", "HU")) +
  scale_shape_manual(values = custom_shapes,breaks = c("HUts", "wHU", "HUrob", "HU")) +
  facet_wrap(~ h_value, nrow = 1, scales = "free_y") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing = unit(2, "cm"))


