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

H1 <- H5 <- H10 <- matrix(NA, nrow=length(MSE_list_5), ncol=4)
rownames(H1) <- rownames(H5) <- rownames(H10) <- names(MSE_list_5)
colnames(H1) <- colnames(H5) <- colnames(H10) <- colnames(MSE_list_5[[1]])
for(i in 1:length(MSE_list_5)){
  H1[i,] <- MSE_list_5[[i]][1,]
  H5[i,] <- MSE_list_5[[i]][5,]
  H10[i,] <- MSE_list_5[[i]][10,]
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

# ggplot(combined_data, aes(x = Country, y = MSE)) +
#   geom_point(data = subset(combined_data, Method == "wHU"),aes(color = Method, shape = Method),
#              size = 4, alpha = 0.6, position = position_dodge(width = 0.7)) +
#   geom_point(data = subset(combined_data, Method == "HUrob"),aes(color = Method, shape = Method),
#              size = 4, alpha = 0.6, position = position_dodge(width = 0.7)) +
#   geom_point(data = subset(combined_data, Method == "HU"),aes(color = Method, shape = Method),
#              size = 4, alpha = 0.6, position = position_dodge(width = 0.7)) +
#   geom_point(data = subset(combined_data, Method == "HUts"),aes(color = Method, shape = Method),
#              size = 4, alpha = 0.6, position = position_dodge(width = 0.7)) +
#   labs(x = "",y = "RMSE",title = "",color = "Model",shape = "Model") +
#   scale_color_manual(values = custom_colors,breaks = c("HUts", "wHU", "HUrob", "HU")) +
#   scale_shape_manual(values = custom_shapes,breaks = c("HUts", "wHU", "HUrob", "HU")) +
#   facet_wrap(~ h_value, nrow = 1, scales = "free_y") +
#   theme_minimal() +
#   theme(legend.title = element_text(size = 12),
#         legend.text = element_text(size = 10),
#         legend.position = "bottom",
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
#         strip.text = element_text(size = 14, face = "bold"),
#         panel.spacing = unit(2, "cm"))

# Plot for h=1
# ggplot(H1_long, aes(x = Country, y = MSE)) +
#   geom_point(aes(color = Method, shape = Method),
#              size = 8, alpha = 0.6, position = position_dodge(width = 0)) +
#   labs(x = "", y = "RMSE", title = "", color = "Model", shape = "Model") +
#   scale_color_manual(values = custom_colors, breaks = c("HUts", "wHU", "HUrob", "HU")) +
#   scale_shape_manual(values = custom_shapes, breaks = c("HUts", "wHU", "HUrob", "HU")) +
#   theme_minimal() +
#   theme(
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 15),
#     legend.position = "none",
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
#     axis.title.y = element_text(size = 15),
#     plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
#     strip.text = element_text(size = 14, face = "bold"),
#     panel.spacing = unit(2, "cm")
#   )
# 
# # Plot for h=5
# ggplot(H5_long, aes(x = Country, y = MSE)) +
#   geom_point(aes(color = Method, shape = Method),
#              size = 8, alpha = 0.6, position = position_dodge(width = 0)) +
#   labs(x = "", y = "RMSE", title = "(b)", color = "Model", shape = "Model") +
#   scale_color_manual(values = custom_colors, breaks = c("HUts", "wHU", "HUrob", "HU")) +
#   scale_shape_manual(values = custom_shapes, breaks = c("HUts", "wHU", "HUrob", "HU")) +
#   theme_minimal() +
#   theme(
#     legend.title = element_text(size = 12),
#     legend.text = element_text(size = 10),
#     legend.position = "none",
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
#     strip.text = element_text(size = 14, face = "bold"),
#     panel.spacing = unit(2, "cm")
#   )
# 
# # Plot for h=10
# ggplot(H10_long, aes(x = Country, y = MSE)) +
#   geom_point(aes(color = Method, shape = Method),
#              size = 8, alpha = 0.6, position = position_dodge(width = 0)) +
#   labs(x = "", y = "RMSE", title = "(c)", color = "Model", shape = "Model") +
#   scale_color_manual(values = custom_colors, breaks = c("HUts", "wHU", "HUrob", "HU")) +
#   scale_shape_manual(values = custom_shapes, breaks = c("HUts", "wHU", "HUrob", "HU")) +
#   theme_minimal() +
  # theme(
  #   legend.title = element_text(size = 12),
  #   legend.text = element_text(size = 10),
  #   legend.position = "bottom",
  #   axis.text.x = element_text(angle = 45, hjust = 1),
  #   plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  #   strip.text = element_text(size = 14, face = "bold"),
  #   panel.spacing = unit(2, "cm")
  # )

# Plot for h=1
ggplot() +
  geom_point(
    data = subset(H1_long, Method == "wHU"),aes(x = Country, y = MSE),
    color = custom_colors["wHU"],shape = custom_shapes["wHU"],
    size = 8,alpha = 0.6,position = position_dodge(width = 0)) +
  geom_point(
    data = subset(H1_long, Method == "HUrob"),aes(x = Country, y = MSE),
    color = custom_colors["HUrob"],shape = custom_shapes["HUrob"],
    size = 8,alpha = 0.6,position = position_dodge(width = 0)) +
  geom_point(
    data = subset(H1_long, Method == "HU"),aes(x = Country, y = MSE),
    color = custom_colors["HU"],shape = custom_shapes["HU"],
    size = 8,alpha = 0.6,position = position_dodge(width = 0)) +
  geom_point(
    data = subset(H1_long, Method == "HUts"),aes(x = Country, y = MSE),
    color = custom_colors["HUts"],shape = custom_shapes["HUts"],
    size = 8,alpha = 0.6,position = position_dodge(width = 0)) +
  labs(
    x = "", y = "RMSE", title = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15+5),
    axis.text.y = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# Plot for h=5
ggplot() +
  geom_point(
    data = subset(H5_long, Method == "wHU"),aes(x = Country, y = MSE),
    color = custom_colors["wHU"],shape = custom_shapes["wHU"],
    size = 8,alpha = 0.6,position = position_dodge(width = 0)) +
  geom_point(
    data = subset(H5_long, Method == "HUrob"),aes(x = Country, y = MSE),
    color = custom_colors["HUrob"],shape = custom_shapes["HUrob"],
    size = 8,alpha = 0.6,position = position_dodge(width = 0)) +
  geom_point(
    data = subset(H5_long, Method == "HU"),aes(x = Country, y = MSE),
    color = custom_colors["HU"],shape = custom_shapes["HU"],
    size = 8,alpha = 0.6,position = position_dodge(width = 0)) +
  geom_point(
    data = subset(H5_long, Method == "HUts"),aes(x = Country, y = MSE),
    color = custom_colors["HUts"],shape = custom_shapes["HUts"],
    size = 8,alpha = 0.6,position = position_dodge(width = 0.7)) +
  labs(
    x = "", y = "RMSE", title = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15+5),
    axis.text.y = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

#H10
ggplot() +
  geom_point(
    data = subset(H10_long, Method == "wHU"),
    aes(x = Country, y = MSE, color = Method, shape = Method),
    size = 8, alpha = 0.6, position = position_dodge(width = 0)
  ) +
  geom_point(
    data = subset(H10_long, Method == "HUrob"),
    aes(x = Country, y = MSE, color = Method, shape = Method),
    size = 8, alpha = 0.6, position = position_dodge(width = 0)
  ) +
  geom_point(
    data = subset(H10_long, Method == "HU"),
    aes(x = Country, y = MSE, color = Method, shape = Method),
    size = 8, alpha = 0.6, position = position_dodge(width = 0)
  ) +
  geom_point(
    data = subset(H10_long, Method == "HUts"),
    aes(x = Country, y = MSE, color = Method, shape = Method),
    size = 8, alpha = 0.6, position = position_dodge(width = 0)
  ) +
  labs(
    x = "", y = "RMSE", title = "") +
  scale_color_manual(
    values = custom_colors,breaks = c("HUts","wHU", "HUrob", "HU")) +
  scale_shape_manual(values = custom_shapes,breaks = c("HUts","wHU", "HUrob", "HU")) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 25),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15+5),
    axis.text.y = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold"),
    #panel.spacing = unit(2, "cm")
  )
