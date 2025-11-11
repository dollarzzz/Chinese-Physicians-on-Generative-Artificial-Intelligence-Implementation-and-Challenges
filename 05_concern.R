# 加载必要的库
library(ggplot2)
library(dplyr)
library(readxl)
library(cowplot)  # 用于组合图表和图例

# 读取和预处理临床数据
mydata_clinical <- read_excel("D:/4生成AI/医生-AI-调查问卷/可视化/05_concern.xlsx") %>%
  mutate(adjusted_value = ifelse(group == "A", -value, value)) %>%
  mutate(type = factor(type, levels = c("Not Concerned", "Less Concerned", "Neutral", "Concerned", "A Bit Concerned", "Neutral'")),
         class = factor(class, levels = c(
                                          "Critical thinking",
                                          "Education equality",
                                          "Self_study ability",
                                          "Cheating",
                                          "Fake/false information_3",
                                          "Teaching",
                                          "Independent creativity",
                                          "Academic abuse",
                                          "Authorship",
                                          "Academic fraud/misconduct",
                                          "Fake/false information_2",
                                          "Research",
                                          "Doctor's authority",
                                          "Medical dispute", 
                                          "Informed consent",
                                          "Fake/false information_1",
                                          "Responsibility",
                                          "Clinical")))

# 绘制临床图表
p_clinical <- ggplot(mydata_clinical, aes(y = class, x = adjusted_value, fill = type)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.25) * 100,
                     labels = paste0(seq(-100, 100, by = 25), "%"),
                     limits = c(-100, 100)) +
  scale_fill_manual(values = c(
    "Not Concerned" = "#66BC98", 
    "Less Concerned" = "#AAD09D", 
    "Neutral" = "#FCDC89", 
    "Neutral'" = "#FCDC89", 
    "A Bit Concerned" = "#E26844", 
    "Concerned" = "#8A233F" 
  ),
  breaks = c("Not Concerned", "Less Concerned", "Neutral", "Neutral'", "A Bit Concerned", "Concerned"))  +
  geom_vline(xintercept = 0, linetype = 1) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(color = "black", size = 16, margin = margin(r = 1)),
    axis.text.x = element_text(color = "black", size = 13, margin = margin(t = 8)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(vjust = 5,color = "black", size = 20, face = "bold", margin = margin(t = 10)), # 设置y轴标题及其字体大小和样式
    axis.line.x = element_line(color = "black",arrow=arrow(length=unit(0.2,"cm"), type="closed",ends="both"),linewidth=0.8),
    
    legend.title = element_text(vjust = 1,size = 15, face = "bold", margin = margin(t = 5)),  # 修改标题和字体大小
    legend.text = element_text(size = 13, color = "black"),  # 修改文本颜色和字体大小
    
    plot.margin = unit(c(1, 2, 1, 3), units = "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )  +
  labs(y = "Concern",fill = "Concern")




p_clinical
pdf("05_concern.pdf", width = 13, height = 5)  
print(p_clinical)
dev.off()