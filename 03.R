library(ggplot2)
library(dplyr)
library(readxl)
library(cowplot)

mydata_clinical <- read_excel("03_clinical.xlsx") %>%
  mutate(adjusted_value = ifelse(group == "A", -value, value)) %>%
  mutate(type = factor(type, levels = c("Very Negative",
                                        "Negative", 
                                        "Neutral", 
                                        "Very Positive", 
                                        "Positive", 
                                        "Neutral'")),
         class = factor(class, levels = c("Clinical Writing p value: 0.0001","Not Use Clinical Writing", "Use Clinical Writing",
                                          "Clinical Report p value: 0.0104","Not Use Clinical Report", "Use Clinical Report",
                                          "Clinical Diagnosis p value: 0.0091","Not Use Clinical Diagnosis", "Use Clinical Diagnosis",
                                          "Clinical Treatment p value: 0.0196","Not Use Clinical Treatment", "Use Clinical Treatment")))

p_clinical <- ggplot(mydata_clinical, aes(y = class, x = adjusted_value, fill = type)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.25) * 100,
                     labels = paste0(seq(-100, 100, by = 25), "%"),
                     limits = c(-100, 100),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c(
    "Very Negative" = "#66BC98", 
    "Negative" = "#AAD09D", 
    "Neutral" = "#FCDC89", 
    "Neutral'" = "#FCDC89",
    "Positive" = "#E26844", 
    "Very Positive" = "#8A233F" 
  ),
  breaks = c("Very Negative", "Negative", "Neutral", "Neutral'", "Positive", "Very Positive"))  +
  geom_vline(xintercept = 0, linetype = 1) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 16),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(vjust = 0.5, angle = 90, margin = margin(r = 10)),
    axis.line.x = element_line(color = "black",arrow=arrow(length=unit(0.2,"cm"), type="closed",ends="both"),linewidth=0.8),
    axis.ticks.x = element_line(color = "black", size = 1),
    legend.title = element_text(size = 16, margin = margin(t = 5)),
    legend.text = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = unit(c(1, 2, 1, 2), units = "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )  +
  labs(y = "Clinical", fill = "Attitude")

mydata_research <- read_excel("03_research.xlsx") %>%
  mutate(adjusted_value = ifelse(group == "A", -value, value)) %>%
  mutate(type = factor(type, levels = c("Very Negative", "Negative", "Neutral", "Very Positive", "Positive", "Neutral'")),
         class = factor(class, levels = c("Academic Writing p value: 0.0008","Not Use Academic Writing", "Use Academic Writing",
                                          "Idea Generation p value: 0.0001","Not Use Idea Generation", "Use Idea Generation",
                                          "Research Analysis p value: 0.0180","Not Use Research Analysis", "Use Research Analysis",
                                          "Research Methods p value: 0.0001","Not Use Research Methods", "Use Research Methods")))

p_research <- ggplot(mydata_research, aes(y = class, x = adjusted_value, fill = type)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Very Negative" = "#66BC98", 
    "Negative" = "#AAD09D", 
    "Neutral" = "#FCDC89", 
    "Neutral'" = "#FCDC89", 
    "Positive" = "#E26844", 
    "Very Positive" = "#8A233F"
  ), breaks = c("Very Negative", "Negative", "Neutral", "Neutral'", "Positive", "Very Positive")) +
  geom_vline(xintercept = 0, linetype = 1) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.25) * 100,
                     labels = paste0(seq(-100, 100, by = 25), "%"),
                     limits = c(-100, 100),
                     expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 16, margin = margin(t = 8, r = 1)),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(vjust = 0.5, angle = 90, margin = margin(r = 10)),
    axis.line.x = element_line(color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both"), linewidth = 0.8),
    axis.ticks.x = element_line(color = "black", size = 1),
    legend.title = element_text(size = 16, margin = margin(t = 5)),
    legend.text = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = unit(c(1, 2, 1, 2), units = "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Research", fill = "Attitude")

mydata_teaching <- read_excel("03_teaching.xlsx") %>%
  mutate(adjusted_value = ifelse(group == "A", -value, value)) %>%
  mutate(type = factor(type, levels = c("Very Negative", "Negative", "Neutral", "Very Positive", "Positive", "Neutral'")),
         class = factor(class, levels = c("Exam Preparation p value: 0.0002", "Not Use Exam Preparation", "Use Exam Preparation",
                                          "Teaching Assistance p value: 0.0040","Not Use Teaching Assistance", "Use Teaching Assistance",
                                          "Research Knowledge p value: 0.0002","Not Use Research Knowledge", "Use Research Knowledge",
                                          "Medical Knowledge p value: 0.0010","Not Use Medical Knowledge", "Use Medical Knowledge")))

p_teaching <- ggplot(mydata_teaching, aes(y = class, x = adjusted_value, fill = type)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Very Negative" = "#66BC98", 
    "Negative" = "#AAD09D", 
    "Neutral" = "#FCDC89", 
    "Neutral'" = "#FCDC89", 
    "Positive" = "#E26844", 
    "Very Positive" = "#8A233F"
  ), breaks = c("Very Negative", "Negative", "Neutral", "Neutral'", "Positive", "Very Positive")) +
  geom_vline(xintercept = 0, linetype = 1) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.25) * 100,
                     labels = paste0(seq(-100, 100, by = 25), "%"),
                     limits = c(-100, 100),
                     expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 16, margin = margin(t = 8, r = 1)),
    axis.title = element_text(color = "black", size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(vjust = 0.5, angle = 90, margin = margin(r = 10)),
    axis.line.x = element_line(color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both"), linewidth = 0.8),
    axis.ticks.x = element_line(color = "black", size = 1),
    legend.title = element_text(size = 16, margin = margin(t = 5)),
    legend.text = element_text(size = 16, color = "black"),
    plot.title = element_blank(),
    plot.margin = unit(c(1, 2, 1, 2), units = "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Education", fill = "Attitude")

legend <- get_legend(p_clinical + theme_void())

plot_combined <- plot_grid(
  p_clinical, p_research, p_teaching,
  labels = "AUTO", ncol = 1, nrow = 3,
  rel_heights = c(2.5, 2.5, 2.5, 0.5),
  align = "vh" 
)

ggsave("03_combined_plots.pdf", plot = plot_combined, width = 13, height = 15, units = "in")
