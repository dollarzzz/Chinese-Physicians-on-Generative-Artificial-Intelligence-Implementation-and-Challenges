library(ggplot2)
library(readxl)
library(ComplexHeatmap)
library(circlize)


df <- read.csv("04_spearman - reverse.csv", header = TRUE, row.names = 1)
group_data <- read.csv("04_group.csv", header = TRUE, row.names = 1)
group_data$group <- as.factor(group_data$group)

data_matrix <- as.matrix(df)

col_fun_heatmap <- colorRamp2(
  c(-0.2, 0, 0.4),
  c("#55B7E6", "white", "#D1392B")
)

col_fun_annotation <- function(x) {
  return(group_colors[as.character(x)])
}

ht <- Heatmap(
  data_matrix,
  name = "Correlation",
  col = col_fun_heatmap,
  right_annotation = rowAnnotation(
    group = group_data$group,
    border = TRUE,
    show_annotation_name = FALSE,
    col = list(
      group = c(
        Clinical = "#D5E4A8",
        Research = "#A695BD",
        Education = "#F9DAA6"
      )
    )
  ),
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

draw(ht, padding = unit(c(4, 20, 4, 4), "mm"))

pdf("04_heatmap_spearman_reverse.pdf", width = 8, height = 6)
draw(ht, padding = unit(c(4, 20, 4, 4), "mm"))
dev.off()
