library(tidyverse)
library(rfPermute)
library(ggplot2)
library(scales)
library(RColorBrewer)
## here is only consider topsoil and subsoil

data <- read.csv("Env_factors_cart.csv", header = T)

na_columns <- names(data)[colSums(is.na(data)) > 0]
print(na_columns)

data$depth_class <- as.factor(data$depth_class)
data$Ecosystem_type <- as.factor(data$Ecosystem_type)

num_cols <- setdiff(names(data),c("depth_class","Ecosystem_type"))
data[num_cols] <- lapply(data[num_cols],as.numeric)

data <- data %>%
  rename(Landuse = Ecosystem_type, pH = Mean_pH, Sand = Sand.content, Clay = Clay.content, PS = PS_Bio15, TS = TS_Bio4, 
         Silt = Silt.content, BD = Bulk.Density, AGB = AbovegroundBiomass, BGB = BelowgroundBiomass)

##overall soil profile_______________________________________________________________________________
# set.seed(629)
# 
# overall_data <- data %>%
#   mutate(depth_type=case_when(
#     depth_class == "0-30" ~ "Topsoil",
#     depth_class %in% c("30-60","60-100") ~ "Subsoil"
#   )) %>%
#   mutate(depth_type=factor(depth_type, levels = c("Topsoil","Subsoil"), labels = c("Topsoil","Subsoil")))
# 
# 
# rfP <- rfPermute(CUE ~ depth_type + MAP + MAT + PS + TS + AI + Elevation + 
#                    Slope + pH + Moisture + BD + CEC + Sand + Clay + Silt + RootDepth +
#                    Bedrock + AGB + BGB + LAI + Shannon_EVI + GPP,
#                  data=overall_data,ntree=1000, mtry=10, nrep = 9, num.cores = 15,
# importance=TRUE, proximity=TRUE, do.trace=TRUE, keep.forest=TRUE, rsq=TRUE)
# 
# summary(rfP)
# 
# importance_scale <- data.frame(importance(rfP,scale = T),check.names = F)
# importance_scale
# 
# importance_scale <- importance_scale[order(importance_scale$`%IncMSE`,decreasing = T),]
# importance_scale
# 
# importance_scale$name <- rownames(importance_scale)
# importance_scale$name <- factor(importance_scale$name, levels = importance_scale$name)
# 
# importance_scale$sig_label <- with(importance_scale,
#                                        ifelse(`%IncMSE.pval` < 0.001, '***',
#                                               ifelse(`%IncMSE.pval` < 0.01, '**',
#                                                      ifelse(`%IncMSE.pval` < 0.05, '*', ''))))
# 
# 
# num_sig <- sum(importance_scale$sig_label != "")
# sig_colors <- colorRampPalette(brewer.pal(5,"Purples"))(num_sig)
# 
# importance_scale$color <- "gray80"
# 
# importance_scale$color[importance_scale$sig_label != ""] <- sig_colors[
#   rank(importance_scale$`%IncMSE`[importance_scale$sig_label !=""])
# ]
# 
# p <- ggplot(importance_scale, aes(x=name, y=`%IncMSE`,fill=color)) +
#   geom_bar(stat = "identity", width = 0.7) +
#   scale_fill_identity() +
#   theme_classic() +
#   scale_y_continuous(expand = c(0, 2), limits = c(0, NA)) +
#   labs(title = expression("CUE"[ST] * " overall effects explained by environmental factors"))+
#   theme(panel.grid = element_blank(),
#         plot.title = element_text(hjust = 0.5,vjust=-2, size=12, face = "bold"),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = 'black'),
#         axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
#         axis.text.y = element_text(size = 10, color = "black"),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         # axis.line.x = element_line(linetype=1, color="black", size=1),
#         # axis.line.y = element_line(linetype=1, color="black", size=1),
#         # axis.ticks.x = element_line(linetype=1, color="black", size=1, lineend = 1),
#         # axis.ticks.y = element_line(linetype=1, color="black", size=1, lineend = 1),
#         axis.ticks.length = unit(0.3, "cm"),
#         legend.position = "none"
#   )
# 
# print(p)
# p_overall <- p +
#   geom_text(data = importance_scale, aes(x = name, y = `%IncMSE`, label = sig_label), nudge_y = 1) +
#   annotate('text',x=10, y= 28, label = "Mean of squared residuals: 0.00357",color = "#54278F",
#            family ="Arial",size=4,fontface="plain",hjust=0)+
#   annotate('text',x=10, y=23, label = "Var explained: 83%",color = "#54278F",
#            family ="Arial",size=4,fontface="plain",hjust=0)
# p_overall


##topsoil____________________________________________________________________________________________________

Top <- data %>%
  filter(depth_class=="0-30")

# ##correlation
# library(psych)
# library(corrplot)
# Top$Landuse <- as.factor(Top$Landuse)
# 
# Top_numeric <- Top[sapply(Top, is.numeric)]
# Top_cor <- corr.test(Top_numeric, use = "pairwise.complete.obs", adjust = "none")
# Top_cor_matrix <- Top_cor$r
# Top_cor$p
# 
# corrplot(Top_cor$r, type = "upper", order = "hclust", 
#          p.mat = Top_cor$p, sig.level = 0.05, insig = "blank",
#          tl.col = "black", tl.cex = 0.5)
# 
# # Get upper triangle of correlation matrix
# high_cor_pairs <- which(abs(Top_cor_matrix) > 0.9 & abs(Top_cor_matrix) < 1, arr.ind = TRUE)
# 
# # Create a data frame of the pairs
# Top_cor_pairs_df <- data.frame(
#   var1 = rownames(Top_cor_matrix)[high_cor_pairs[,1]],
#   var2 = colnames(Top_cor_matrix)[high_cor_pairs[,2]],
#   correlation = Top_cor_matrix[high_cor_pairs]
# )
# 
# # View
# print(Top_cor_pairs_df)

set.seed(629)
rfP_top <- rfPermute(CUE ~ MAP + MAT + PS + TS + 
                       pH + Moisture + BD + CEC + Sand + Clay + Silt + 
                       AGB + BGB,
                  data=Top,ntree=1000, mtry=10, nrep = 9, num.cores = 15,
                  importance=TRUE, proximity=TRUE, do.trace=TRUE, keep.forest=TRUE, rsq=TRUE)

summary(rfP_top)

mse_top <- data.frame(
  Trees = 1:rfP_top$rf$ntree,
  MSE = rfP_top$rf$mse
)
mse_top <- mse_top %>%
  mutate(Layer="Top")


importance_scale_top <- data.frame(importance(rfP_top,scale = T),check.names = F)
importance_scale_top 

importance_scale_top <- importance_scale_top[order(importance_scale_top$`%IncMSE`,decreasing = T),]
importance_scale_top

importance_scale_top$name <- rownames(importance_scale_top)
importance_scale_top$name <- factor(importance_scale_top$name, levels = importance_scale_top$name)
  
importance_scale_top$sig_label <- with(importance_scale_top, 
                                   ifelse(`%IncMSE.pval` < 0.001, '***',
                                   ifelse(`%IncMSE.pval` < 0.01, '**',
                                   ifelse(`%IncMSE.pval` < 0.05, '*', ''))))


num_sig_top <- sum(importance_scale_top$sig_label != "")
# sig_colors_top <- colorRampPalette(brewer.pal(5,"GnBu"))(num_sig_top)

# importance_scale_top$color <- "gray80"

importance_scale_top$color[importance_scale_top$sig_label != ""] <- sig_colors_top[
  rank(importance_scale_top$`%IncMSE`[importance_scale_top$sig_label !=""])
]

p_top <- ggplot(importance_scale_top, aes(x=name, y=`%IncMSE`,fill=color)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_identity() +
    theme_bw() +
    scale_y_continuous(expand = c(0, 2), limits = c(0, 80)) +
    # labs(title = expression("CUE"[M] * " explained by environmental factors in Topsoil"))+
    # labs(title = "CUE explained by environmental factors in Topsoil")+
    theme(panel.grid = element_blank(),
          # plot.title = element_text(hjust = 0.5,vjust=-2, size=12),
          panel.background = element_blank(),
          # axis.line = element_line(colour = 'black'),
          # axis.text = element_blank(),
          axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          # axis.line.x = element_line(linetype=1, color="black", size=1),
          # axis.line.y = element_line(linetype=1, color="black", size=1),
          # axis.ticks.x = element_line(linetype=1, color="black", size=1, lineend = 1),
          # axis.ticks.y = element_line(linetype=1, color="black", size=1, lineend = 1),
          # axis.ticks.length = unit(0.3, "cm"),
          legend.position = "none"
          )

print(p_top)
# ggsave("figures/rf_top_1.tif", height = 2.8, width = 6, dpi = 300)

p1 <- p_top +
  geom_text(data = importance_scale_top, aes(x = name, y = `%IncMSE`, label = sig_label), nudge_y = 1) +
  annotate('text',x=10, y= 63, label = "Mean of squared residuals: 0.003",color = "#0868AC", 
            family ="Arial",size=4,fontface="plain",hjust=0)+
  annotate('text',x=10, y=55, label = "Var explained: 85.3%",color = "#0868AC", 
            family ="Arial",size=4,fontface="plain",hjust=0)
p1



######__________________________________________________________________________________________

Sub <- data %>%
  filter(depth_class!="0-30")


set.seed(629)
rfP_sub <- rfPermute(CUE ~ MAP + MAT + PS + TS + 
                       pH + Moisture + BD + CEC + Sand + Clay + Silt + 
                       AGB + BGB,
                   data=Sub,ntree=1000, mtry=10, nrep = 9, num.cores = 15,
                   importance=TRUE, proximity=TRUE, do.trace=TRUE, keep.forest=TRUE, rsq=TRUE)

summary(rfP_sub)

mse_sub <- data.frame(
  Trees = 1:rfP_sub$rf$ntree,
  MSE = rfP_sub$rf$mse
)
mse_sub <- mse_sub %>%
  mutate(Layer="Sub")

importance_scale_sub <- data.frame(importance(rfP_sub,scale = T),check.names = F)
importance_scale_sub 

importance_scale_sub <- importance_scale_sub[order(importance_scale_sub$`%IncMSE`,decreasing = T),]
importance_scale_sub

importance_scale_sub$name <- rownames(importance_scale_sub)
importance_scale_sub$name <- factor(importance_scale_sub$name, levels = importance_scale_sub$name)

importance_scale_sub$sig_label <- with(importance_scale_sub, 
                                   ifelse(`%IncMSE.pval` < 0.001, '***',
                                          ifelse(`%IncMSE.pval` < 0.01, '**',
                                                 ifelse(`%IncMSE.pval` < 0.05, '*', ''))))


num_sig_sub <- sum(importance_scale_sub$sig_label != "")
sig_colors_sub <- colorRampPalette(brewer.pal(5,"Purples"))(num_sig_sub)

importance_scale_sub$color <- "gray80"

importance_scale_sub$color[importance_scale_sub$sig_label != ""] <- sig_colors_sub[
  rank(importance_scale_sub$`%IncMSE`[importance_scale_sub$sig_label !=""])
]

p_sub <- ggplot(importance_scale_sub, aes(x=name, y=`%IncMSE`,fill=color)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_identity() +
  theme_bw() +
  scale_y_continuous(expand = c(0, 2), limits = c(0, 48)) +
  # labs(title = expression("CUE"[M] * " explained by environmental factors in Subsoil"))+
  # labs(title = "CUE explained by environmental factors in Subsoil")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        # plot.title = element_text(hjust = 0.5, vjust = -2,size=12),
        # axis.line = element_line(colour = 'black'),
        # axis.text = element_blank(),
        axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # axis.line.x = element_line(linetype=1, color="black", size=1),
        # axis.line.y = element_line(linetype=1, color="black", size=1),
        # axis.ticks.x = element_line(linetype=1, color="black", size=1, lineend = 1),
        # axis.ticks.y = element_line(linetype=1, color="black", size=1, lineend = 1),
        # axis.ticks.length = unit(0.3, "cm"),
        legend.position = "none")

print(p_sub)
# ggsave("figures/rf_sub_1.tif", height = 2.8, width = 6, dpi = 300)
p2 <- p_sub +
  geom_text(data = importance_scale_sub, aes(x = name, y = `%IncMSE`, label = sig_label), nudge_y = 1) +
  annotate('text',x=10, y= 35, label = "Mean of squared residuals: 0.003",color = "#54278F", 
           family ="Arial",size=3,fontface="plain", hjust=0)+
  annotate('text',x=10, y=30, label = "Var explained: 88.4%",color = "#54278F", 
           family ="Arial",size=3,fontface="plain",hjust=0)
p2


library(cowplot)

combined <- plot_grid(p1, p2, ncol = 1,align = "v", axis = "lr",
                      labels = c("a", "b"),
                      label_size = 12,
                      label_fontface = "plain",
                      label_fontfamily = "Arial")
combined 
label_plot <- ggdraw() +
  annotate("text",
           x=0.2, y=0.6, label = "Increase in MSE (%)",
           angle = 90, vjust = 1.5,
           size = 4, family="Arial", fontface="plain")

final_pot <- ggdraw() +
  draw_plot(combined, x=0.05, width=0.9) +
  draw_plot(label_plot, x=0, width = 0.05 )

final_pot

ggsave("figures/FigS2.2.tiff", width = 6.5, height = 7, dpi = 300)

data_rf <- rbind(mse_top,mse_sub)

data_rf <- data_rf %>%
  mutate(Layer = factor(Layer, levels = c("Top","Sub"), labels = c("Topsoil","Subsoil")))

rf_tree <- ggplot(data_rf, aes(x=Trees, y=MSE))+
  facet_wrap(~Layer)+
  geom_line()+
  theme_bw()+
  coord_cartesian(ylim = c(0.0028,0.007), expand = T)+
  labs(x="Number of trees", y="Mean square error")+
  theme(axis.text = element_text(size=8, color="black", family="Arial"))
  
rf_tree
ggsave("figures/rf_tree.tiff", width = 7, height = 3.5, dpi = 300)






