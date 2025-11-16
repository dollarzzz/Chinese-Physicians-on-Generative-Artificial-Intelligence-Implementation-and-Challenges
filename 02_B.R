library(readxl)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)

mydata <- read_excel("02_AI种类.xlsx")
mydata$AI <- factor(mydata$AI, levels = c("ChatGPT", "ERNIE-Bot", "Other","Mirror Site", "Bing Chat"))

mydata3 <- read_excel("02_better.xlsx")
mydata3$`Which is better` <- factor(mydata3$`Which is better`, levels = c("Oversea", "Unsure", "Equal", "Chinese"))

mydata2 <- read_excel("02_category.xlsx")
mydata2$Category <- factor(mydata2$Category, levels = c("Oversea AI chatbots", "Chinese AI chatbots", "Mirror site"))

custom_colors <- c(
  "ChatGPT" = "#EEA599", 
  "ERNIE-Bot" = "#FAC795", 
  "Other"= "#FFE9BE", 
  "Mirror Site"= "#ABD3E1", 
  "Bing Chat" = "#92B4C8",
  "Oversea AI chatbots" = "#EEA599",
  "Chinese AI chatbots" = "#FAC795",
  "Mirror site" = "#92B4C8",
  "Oversea" = "#EEA599",
  "Unsure" = "#FAC795",
  "Equal" = "#ABD3E1",
  "Chinese"="#92B4C8"
)

p <- ggplot(mydata, aes(x = "", y = Percentage1, fill = AI)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "AI Chatbots Used (by Name)") +
  geom_text(aes(label = Percentage),
            check_overlap = TRUE,
            position = position_stack(vjust = 0.5),
            hjust = 0.5,
            angle = 90,
            size = 5) +
  scale_fill_manual(values = custom_colors[names(custom_colors) %in% levels(mydata$AI)]) +
  theme(plot.margin = unit(c(0.5, 0.3, 0.5, 0.5), units = "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))

p2 <- ggplot(mydata2, aes(x = "", y = Percentage2_1, fill = Category)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "AI Chatbots Used (by Category)") +
  geom_text(aes(label = Percentage2),
            check_overlap = TRUE,
            position = position_stack(vjust = 0.5),
            hjust = 0.5,
            angle = 90,
            size = 5) +
  scale_fill_manual(values = custom_colors[names(custom_colors) %in% levels(mydata2$Category)]) +
  theme(plot.margin = unit(c(0.5, 0.3, 0.5, 0.5), units = "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))

p3 <- ggplot(mydata3, aes(x = "", y = Percentage3_1, fill = `Which is better`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "Which is better") +
  geom_text(aes(label = Percentage3),
            check_overlap = TRUE,
            position = position_stack(vjust = 0.5),
            hjust = 0.5,
            angle = 65,
            size = 5) +
  scale_fill_manual(values = custom_colors[names(custom_colors) %in% levels(mydata3$`Which is better`)]) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))

pdf("02_combined_pie_charts.pdf", width = 20, height = 13)

grid.arrange(arrangeGrob(p, p2, ncol = 2), p3, ncol = 1, heights = c(0.6, 0.6))

dev.off()
