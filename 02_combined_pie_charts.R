# 加载必要的包
library(readxl)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)

# 读取数据
mydata <- read_excel("D:/4生成AI/医生-AI-调查问卷/可视化/02_AI种类.xlsx")
mydata$AI <- factor(mydata$AI, levels = c("ChatGPT", "ERNIE-Bot", "Other","Mirror Site", "Bing Chat"))

mydata3 <- read_excel("D:/4生成AI/医生-AI-调查问卷/可视化/02_better.xlsx")
mydata3$`Which is better` <- factor(mydata3$`Which is better`, levels = c("Oversea", "Unsure", "Equal", "Chinese"))

mydata2 <- read_excel("D:/4生成AI/医生-AI-调查问卷/可视化/02_category.xlsx")
mydata2$Category <- factor(mydata2$Category, levels = c("Oversea AI chatbots", "Chinese AI chatbots", "Mirror site"))

# 自定义颜色
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
  # 假设还有其他AI名称，这里需要添加对应的颜色
  # 例如: "AI_Name1" = "#COLOR1", "AI_Name2" = "#COLOR2", ...
  # 根据mydata$AI中的levels来添加所有可能的AI名称及其颜色
  # 由于我们没有具体AI名称的完整列表，这里仅作为示例添加几个
  # "AI_Name1" = "#FFFFFF", # 白色作为示例
  # "AI_Name2" = "#000000"  # 黑色作为示例
)

# 绘制饼图，去除百分号
p <- ggplot(mydata, aes(x = "", y =Percentage1 , fill = AI)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "AI Chatbots Used (by Name)") +
  geom_text(aes(label = Percentage),  # 去除百分号，转换为小数（此处应预处理数据，将Percentage转换为不带%的数值）
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
  geom_text(aes(label = Percentage2),  # 去除百分号，转换为小数（此处应预处理数据，将Percentage2转换为不带%的数值）
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
  geom_text(aes(label = Percentage3),  # 去除百分号，转换为小数（此处应预处理数据，将Percentage3转换为不带%的数值）
            check_overlap = TRUE,
            position = position_stack(vjust = 0.5),
            hjust = 0.5,
            angle = 65,
            size = 5) +
  scale_fill_manual(values = custom_colors[names(custom_colors) %in% levels(mydata3$`Which is better`)]) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))

# 将饼图拼在一起
# 设置图形设备的宽度和高度以适应两张图的高度和一张图的宽度
pdf("02_combined_pie_charts.pdf", width = 20, height = 13)

# 使用grid.arrange将图形拼在一起
# ncol指定列数，nrow指定行数，heights指定每行的高度比例
grid.arrange(arrangeGrob(p, p2, ncol = 2), p3, ncol = 1, heights = c(0.6, 0.6)) # 调整了高度比例以更好地适应内容

# 关闭图形设备
dev.off()