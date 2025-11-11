

# 加载必要的库
library(ggplot2)
library(readxl)
library(ComplexHeatmap)
library(circlize)

# library(tibble)  # 若未使用，可选择移除
# library(circlize)  # 若未使用circlize的功能，也可移除

# 设置工作目录（请根据您的实际情况调整路径）
setwd("D:/4生成AI/医生-AI-调查问卷/可视化")

# 读取CSV文件
df <- read.csv("04_spearman - reverse.csv", header = TRUE, row.names = 1)
group_data <- read.csv('04_group.csv', header = TRUE, row.names = 1)
group_data$group <- as.factor(group_data$group)

# 转换数据框为矩阵
data_matrix <- as.matrix(df)

# 定义颜色映射函数（针对热图数据）
col_fun_heatmap <- colorRamp2(
  c(-0.2, 0, 0.4),
  c("#55B7E6", "white", "#D1392B")
)

# # 定义单元格文本显示函数
# cell_fun_heatmap <- function(j, i, x, y, width, height, fill) {
#   grid.text(
#     sprintf("%.2f", data_matrix[i, j]),
#     x, y,
#     gp = gpar(fontsize = 12, col = "black")
#   )
# }


# 创建颜色函数，用于注释中的分组映射
col_fun_annotation <- function(x) {
  return(group_colors[as.character(x)])
}

# 创建热图，并添加右侧注释
ht <- Heatmap(
  data_matrix,
  name = "Correlation",
  col = col_fun_heatmap,
  # 单元格文本
  # cell_fun = cell_fun_heatmap,
  

  # 添加右侧注释
  right_annotation = rowAnnotation(
    group = group_data$group,  # 直接使用 group 列作为注释
    border = TRUE,  # 显示注释边框
    show_annotation_name = FALSE,  # 不显示注释列名
    
    # 设置颜色映射
    col = list(
      group = c(
        # Clinical = "#9392BE",
        # Research = "#D0E7ED",
        # Education = "#D5E4A8"
        Clinical = "#D5E4A8",
        Research = "#A695BD",
        Education = "#F9DAA6"
      )
    )
  ),
  # 其他热图设置
  column_names_rot = 90,
  row_names_rot = 0,
  clustering_method_rows = "ward.D2",
  clustering_method_columns = "ward.D2",
  row_dend_width = unit(0.5, "cm"),
  column_dend_height = unit(0.5, "cm"),
  border_gp = gpar(lty = 1, col = "black"),
  rect_gp = gpar(col = "white", lwd = 1),
  row_title_gp = gpar(fontsize = 12)
)

# 绘制热图
draw(ht, padding = unit(c(4, 20, 4, 4), "mm"))

pdf("04_heatmap_spearman_reverse.pdf", width = 8, height = 6)  # 调整宽度和高度以适应您的热图
draw(ht, padding = unit(c(4, 20, 4, 4), "mm"))
dev.off()  # 关闭图形设备