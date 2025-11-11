# 加载必要的库
library(ggplot2)
library(dplyr)
library(readxl)
library(cowplot)  # 用于组合图表和图例

# 读取和预处理临床数据
mydata_clinical <- read_excel("D:/4生成AI/医生-AI-调查问卷/可视化/03_clinical.xlsx") %>%
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

# 绘制临床图表
p_clinical <- ggplot(mydata_clinical, aes(y = class, x = adjusted_value, fill = type)) + 
  # 条形的高度（或长度）由 x 轴上的值决定，而条形的类别则由 y 轴上的标签表示。
  geom_bar(stat = "identity", position = "stack") +
  # identity表示条形的高度是变量的值；
  # 当 position 设置为 "stack" 时，重叠的条形会被堆叠起来。
  scale_x_continuous(breaks = seq(-1, 1, by = 0.25) * 100,
                     labels = paste0(seq(-100, 100, by = 25), "%"),
                     limits = c(-100, 100),
                     expand = c(0, 0)) +  # 确保x轴从-100到100没有额外的空间
  scale_fill_manual(values = c(
    "Very Negative" = "#66BC98", 
    "Negative" = "#AAD09D", 
    "Neutral" = "#FCDC89", 
    "Neutral'" = "#FCDC89", # 注意这里可能是一个错误，因为通常不会有"Neutral'"这样的类别，除非数据中确实存在
    "Positive" = "#E26844", 
    "Very Positive" = "#8A233F" 
  ),
  breaks = c("Very Negative", "Negative", "Neutral", "Neutral'", "Positive", "Very Positive"))  +
  geom_vline(xintercept = 0, linetype = 1) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 16), # 统一设置x轴和y轴文本字体
    axis.title = element_text(color = "black", size = 16, face = "bold"), # 统一设置x轴和y轴标题字体
    axis.title.x = element_blank(), # 修正x轴标题的垂直对齐和边距
    axis.title.y = element_text(vjust = 0.5, angle = 90, margin = margin(r = 10)), # 修正y轴标题的垂直对齐、旋转角度和边距
    axis.line.x = element_line(color = "black",arrow=arrow(length=unit(0.2,"cm"), type="closed",ends="both"),linewidth=0.8),
    axis.ticks.x = element_line(color = "black", size = 1), # 添加x轴ticks
    
    legend.title = element_text(size = 16, margin = margin(t = 5)),  # 修改标题和字体大小
    legend.text = element_text(size = 16, color = "black"),  # 修改文本颜色和字体大小
    
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # 添加图名A并设置字体大小和居中
    plot.margin = unit(c(1, 2, 1, 2), units = "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )  +
  labs(y = "Clinical", fill = "Attitude")


# 读取和预处理研究数据
mydata_research <- read_excel("D:/4生成AI/医生-AI-调查问卷/可视化/03_research.xlsx") %>%
  mutate(adjusted_value = ifelse(group == "A", -value, value)) %>%
  mutate(type = factor(type, levels = c("Very Negative", "Negative", "Neutral", "Very Positive", "Positive", "Neutral'")),
         class = factor(class, levels = c("Academic Writing p value: 0.0008","Not Use Academic Writing", "Use Academic Writing",
                                          "Idea Generation p value: 0.0001","Not Use Idea Generation", "Use Idea Generation",
                                          "Research Analysis p value: 0.0180","Not Use Research Analysis", "Use Research Analysis",
                                          "Research Methods p value: 0.0001","Not Use Research Methods", "Use Research Methods")))

# 绘制研究图表
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
                     expand = c(0, 0)) + # 添加expand以匹配p_clinical
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 16, margin = margin(t = 8, r = 1)), # 合并x轴和y轴文本设置，但保留不同的边距
    axis.title = element_text(color = "black", size = 16, face = "bold"), # 统一设置x轴和y轴标题字体（x轴标题将被设置为blank）
    axis.title.x = element_blank(), # 隐藏x轴标题
    axis.title.y = element_text(vjust = 0.5, angle = 90, margin = margin(r = 10)), # 设置y轴标题及其垂直对齐、旋转角度和边距
    axis.line.x = element_line(color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both"), linewidth = 0.8),
    axis.ticks.x = element_line(color = "black", size = 1), # 添加x轴ticks
    
    legend.title = element_text(size = 16, margin = margin(t = 5)), # 修改标题和字体大小（不需要vjust，因为它是垂直对齐的设置）
    legend.text = element_text(size = 16, color = "black"), # 修改文本颜色和字体大小
    
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # 添加图名并设置字体大小和居中
    plot.margin = unit(c(1, 2, 1, 2), units = "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Research", fill = "Attitude")

# 读取和预处理教学数据
mydata_teaching <- read_excel("D:/4生成AI/医生-AI-调查问卷/可视化/03_teaching.xlsx") %>%
  mutate(adjusted_value = ifelse(group == "A", -value, value)) %>%
  mutate(type = factor(type, levels = c("Very Negative", "Negative", "Neutral", "Very Positive", "Positive", "Neutral'")),
         class = factor(class, levels = c("Exam Preparation p value: 0.0002", "Not Use Exam Preparation", "Use Exam Preparation",
                                          "Teaching Assistance p value: 0.0040","Not Use Teaching Assistance", "Use Teaching Assistance",
                                          "Research Knowledge p value: 0.0002","Not Use Research Knowledge", "Use Research Knowledge",
                                          "Medical Knowledge p value: 0.0010","Not Use Medical Knowledge", "Use Medical Knowledge")))

# 绘制教学图表
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
                     expand = c(0, 0)) + # 添加expand以匹配p_research
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 16, margin = margin(t = 8, r = 1)), # 合并x轴和y轴文本设置，但保留不同的边距
    axis.title = element_text(color = "black", size = 16, face = "bold"), # 统一设置x轴和y轴标题字体（x轴标题将被设置为blank）
    axis.title.x = element_blank(), # 隐藏x轴标题
    axis.title.y = element_text(vjust = 0.5, angle = 90, margin = margin(r = 10)), # 设置y轴标题及其垂直对齐、旋转角度和边距
    axis.line.x = element_line(color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both"), linewidth = 0.8),
    axis.ticks.x = element_line(color = "black", size = 1), # 添加x轴ticks
    
    legend.title = element_text(size = 16, margin = margin(t = 5)), # 修改标题和字体大小（不需要vjust，因为它是垂直对齐的设置）
    legend.text = element_text(size = 16, color = "black"), # 修改文本颜色和字体大小
    
    plot.title = element_blank(), # 如果不需要图名，则设置为blank；如果需要，可以添加并设置字体大小和居中
    plot.margin = unit(c(1, 2, 1, 2), units = "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Education", fill = "Attitude")


# 创建一个空的ggplot对象用于图例
# 注意：我们需要从其中一个图表中“借用”图例数据
legend <- get_legend(p_clinical + theme_void())  # 使用p_clinical的图例，因为它们都是相同的

# 使用cowplot的plot_grid函数来组合图表和图例
# 注意：我们将图例放在底部，并调整图表的高度以适应图例
plot_combined <- plot_grid(
  p_clinical, p_research, p_teaching,
  labels = "AUTO", ncol = 1, nrow = 3,
  rel_heights = c(2.5, 2.5, 2.5, 0.5),  # 最后一个高度是图例的高度，可以根据需要调整
  align = "vh" 
)

# 保存结果到PDF文件
ggsave("03_combined_plots.pdf", plot = plot_combined, width = 13, height = 15, units = "in")  # 调整高度以适应所有内容

