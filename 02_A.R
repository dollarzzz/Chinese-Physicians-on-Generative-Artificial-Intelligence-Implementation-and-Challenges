library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(gridExtra)

data_sheet1 <- read_excel("02_百分比柱状图.xlsx", sheet = 1)

long_data_sheet1 <- data_sheet1 %>%
  pivot_longer(cols = -Name, names_to = "Category", values_to = "Count") %>%
  group_by(Name) %>%
  mutate(Percentage = Count / sum(Count) * 100)

long_data_sheet1 <- long_data_sheet1 %>%
  mutate(Label = paste0(Count, " (", scales::percent(Percentage/100, accuracy = 1), ")"))

p_A <- ggplot(long_data_sheet1, aes(x = Name, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "A", x = "Name", y = "Percentage", fill = "Which is better") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 13, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.title = element_text(size = 13),
    plot.margin = unit(c(1, 1, 1, 1), units = "cm"),
    axis.line.x = element_line(color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "last"), linewidth = 0.8),
    axis.line.y = element_line(color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "last"), linewidth = 0.8),
    axis.ticks.y = element_line(color = "black", size = 1),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 13, color = "black"),
    legend.title = element_text(size = 13, color = "black"),
    text = element_text(color = "black")
  ) +
  geom_text(aes(label = Label),
            position = position_fill(vjust = 0.5), hjust = 0.5, size = 4.5, color = "black") +
  scale_fill_manual(values = c(
    "Oversea" = "#EEA599",
    "Unsure" = "#FAC795",
    "Equal" = "#ABD3E1",
    "Chinese" = "#92B4C8"
  ))

data_sheet2 <- read_excel("02_百分比柱状图.xlsx", sheet = 2)

long_data_sheet2 <- data_sheet2 %>%
  pivot_longer(cols = -Category, names_to = "Region", values_to = "Count") %>%
  group_by(Category) %>%
  mutate(Percentage = Count / sum(Count) * 100)

long_data_sheet2 <- long_data_sheet2 %>%
  mutate(Label = paste0(Count, " (", scales::percent(Percentage/100, accuracy = 1), ")"))

p_B <- ggplot(long_data_sheet2, aes(x = Category, y = Percentage, fill = Region)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "B", x = "Category", y = "Percentage", fill = "Which is better") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 13, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.title = element_text(size = 13),
    plot.margin = unit(c(1, 1, 1, 1), units = "cm"),
    axis.line.x = element_line(color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "last"), linewidth = 0.8),
    axis.line.y = element_line(color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "last"), linewidth = 0.8),
    axis.ticks.y = element_line(color = "black", size = 1),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 13, color = "black"),
    legend.title = element_text(size = 13, color = "black"),
    text = element_text(color = "black")
  ) +
  geom_text(aes(label = Label),
            position = position_fill(vjust = 0.5), hjust = 0.5, size = 4.5, color = "black") +
  scale_fill_manual(values = c(
    "Oversea" = "#EEA599",
    "Unsure" = "#FAC795",
    "Equal" = "#ABD3E1",
    "Chinese" = "#92B4C8"
  ))

grid.arrange(p_A, p_B, ncol = 1, nrow = 2, heights = c(4, 4))

pdf("02_combined_percentage_bar_charts.pdf", width = 8, height = 12)
print(grid.arrange(p_A, p_B, ncol = 1, nrow = 2, heights = c(4, 4)))
dev.off()
