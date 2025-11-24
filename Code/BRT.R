library(tidyverse)
library(ggplot2)
library(vegan)
library(scales)
library(RColorBrewer)
## here is only consider topsoil and subsoil

data <- read.csv("Env_factors_cart.csv", header = T)

na_columns <- names(data)[colSums(is.na(data)) > 0]
print(na_columns)

data <- data %>%
  mutate(depth_type = case_when(
    depth_class == "0-30" ~ "Top",
    depth_class %in% c("30-60","60-100") ~ "Deep"
  ))

data$depth_class <- as.factor(data$depth_class)
data$depth_type <- as.factor(data$depth_type)
data$Ecosystem_type <- as.factor(data$Ecosystem_type)

num_cols <- setdiff(names(data),c("depth_class","Ecosystem_type","depth_type"))
data[num_cols] <- lapply(data[num_cols],as.numeric)

data <- data %>%
  rename(Landuse=Ecosystem_type, pH=Mean_pH, Sand=Sand.content, Clay=Clay.content,
         Silt=Silt.content, BD=Bulk.Density, AGB=AbovegroundBiomass,BGB = BelowgroundBiomass,Degree=degree,
         Length=length)

##0-30____________________________________________________________________________________________________

Top <- data %>%
  filter(depth_class=="0-30")

Top.climate <- Top[4:14]

Top.pca.clim <- prcomp(Top.climate, center = T, scale. = T)
summary(Top.pca.clim)
# Top_eingvalues <- Top.pca.clim$sdev^2
# pc_indices <- which(Top_eingvalues > 1)
# pcs <- as.data.frame(Top.pca.clim$x[,pc_indices])
sort(abs(Top.pca.clim$rotation[,1]), decreasing=T)
sort(abs(Top.pca.clim$rotation[,2]), decreasing=T)

Top$Climate1 <- Top.pca.clim$x[,1]
Top$Climate2 <- Top.pca.clim$x[,2]

Top.soil <- Top[,c(17:22,24:27)]

Top.pca.soil <- prcomp(Top.soil, center = T, scale. = T)
summary(Top.pca.soil)

# Top_eingvalues <- Top.pca.soil$sdev^2
# pc_indices <- which(Top_eingvalues > 1)
# pcs <- as.data.frame(Top.pca.soil$x[,pc_indices])

sort(abs(Top.pca.soil$rotation[,1]), decreasing=T)
sort(abs(Top.pca.soil$rotation[,2]), decreasing=T)
sort(abs(Top.pca.soil$rotation[,3]), decreasing=T)

Top$Soil1 <- Top.pca.soil$x[,1]
Top$Soil2 <- Top.pca.soil$x[,2]
Top$Soil3 <- Top.pca.soil$x[,3]

Top.veg <- Top[,c(23,28:31)]

Top.pca.veg <- prcomp(Top.veg, center = T, scale. = T)
summary(Top.pca.veg)

Top_eingvalues <- Top.pca.veg$sdev^2
pc_indices <- which(Top_eingvalues > 1)
pcs <- as.data.frame(Top.pca.veg$x[,pc_indices])
sort(abs(Top.pca.veg$rotation[,1]), decreasing=T)
sort(abs(Top.pca.veg$rotation[,2]), decreasing=T)

Top$Veg1 <- Top.pca.veg$x[,1]
Top$Veg2 <- Top.pca.veg$x[,2]

library(gbm)
set.seed(629)
# 
# brt_model <- gbm(
#   CUE ~ Climate1 + Climate2 + Soil1 + Soil2 + Soil3 + Veg1 +Veg2 + Elevation + Slope,
#   data = Top,
#   distribution = "gaussian",
#   n.trees = 3000,
#   interaction.depth = 5,
#   shrinkage = 0.01,
#   bag.fraction = 0.7,
#   train.fraction = 0.8,
#   cv.folds = 5,
#   n.cores = parallel::detectCores() - 1
# )
# 
# best.iter <- gbm.perf(brt_model, method = "cv")
# 
# summary(brt_model, n.trees = best.iter)

n_boot <- 200
influence_list <- list()
r2_list <- numeric(n_boot)
Top1 <- Top[,c(15,16,37:43)]
climate_vars <- c("Climate1","Climate2")
soil_vars <- c("Soil1","Soil2","Soil3")
veg_vars <- c("Veg1","Veg2")
topo_vars <- c("Elevation","Slope")
all_predictors <- c(climate_vars, soil_vars, veg_vars, topo_vars)

brt_formula <- as.formula(paste("CUE ~", paste(all_predictors, collapse = " + ")))

# Start bootstrap loop
for (i in 1:n_boot) {
  # Bootstrap sample
  boot_data <- Top[sample(1:nrow(Top1), replace = TRUE), ]
  
  # Fit model
  model <- gbm(
    formula = brt_formula,
    data = boot_data,
    distribution = "gaussian",
    n.trees = 3000,
    interaction.depth = 5,
    shrinkage = 0.01,
    bag.fraction = 0.7,
    train.fraction = 0.8,
    cv.folds = 10,
    n.cores = parallel::detectCores() - 1,
    verbose = FALSE
  )
  
  best.iter <- gbm.perf(model, method = "cv", plot.it = FALSE)
  
  # Predictions & R²
  preds <- predict(model, newdata = boot_data, n.trees = best.iter)
  r2 <- cor(preds, boot_data$CUE, use = "complete.obs")^2
  r2_list[i] <- r2
  
  # Relative influence
  inf <- summary(model, n.trees = best.iter, plotit = FALSE)
  influence_df <- data.frame(Variable = inf$var, Influence = inf$rel.inf, Iteration = i)
  influence_list[[i]] <- influence_df
}

# Combine results
influence_all <- bind_rows(influence_list)
# write.csv(influence_all, file = "top_influence_all_200.csv")
# Normalize to 100% per iteration
influence_all <- influence_all %>%
  group_by(Iteration) %>%
  mutate(ScaledInfluence = Influence / sum(Influence) * 100) %>%
  ungroup()

# Add R²
r2_df <- data.frame(Iteration = 1:n_boot, R2 = r2_list)
influence_all <- influence_all %>% left_join(r2_df, by = "Iteration")

# influence_matrix <- influence_all %>%
#   select(Variable, Iteration, Influence) %>%
#   tidyr::pivot_wider(names_from = Iteration, values_from = Influence, values_fill = 0)
# 
# influence_long <- influence_matrix %>%
#   tidyr::pivot_longer(cols = -Variable, names_to = "Iteration", values_to = "Influence")
# influence_long$Iteration <- as.numeric(influence_long$Iteration)
# 
# ggplot(influence_long, aes(x = Iteration, y = Variable, fill = Influence)) +
#   geom_tile() +
#   scale_fill_viridis_c(option = "C", name = "Relative Influence") +
#   labs(
#     title = "Heatmap of Variable Influence Across Bootstraps",
#     x = "Bootstrap Iteration",
#     y = "Variable"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_text(size = 10),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   )

# Variable-level summary
variable_summary <- influence_all %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(ScaledInfluence),
    CI_low = quantile(ScaledInfluence, 0.025),
    CI_high = quantile(ScaledInfluence, 0.975),
    WeightedMean = weighted.mean(ScaledInfluence, R2)
  ) %>%
  arrange(desc(WeightedMean))

# write.csv(variable_summary, file = "top_variable_summary.csv")

influence_all <- influence_all %>%
  mutate(Group = case_when(
    Variable %in% climate_vars ~ "Climate",
    Variable %in% soil_vars ~ "Soil",
    Variable %in% veg_vars ~ "Vegetation",
    Variable %in% topo_vars ~ "Topography",
    TRUE ~ "Other"
  ))

group_summary <- influence_all %>%
  group_by(Iteration, Group) %>%
  summarise(GroupInfluence = sum(ScaledInfluence), .groups = "drop")

group_summary_stats <- group_summary %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(GroupInfluence),
    CI_low = quantile(GroupInfluence, 0.025),
    CI_high = quantile(GroupInfluence, 0.975),
    WeightedMean = weighted.mean(GroupInfluence, r2_list[Iteration])
  ) %>%
  arrange(desc(WeightedMean))

ggplot(variable_summary, aes(x = reorder(Variable, -WeightedMean), y = WeightedMean)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.3) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  ylab("Relative Influence (%)") + xlab("") +
  theme_bw()

Top_variable_long <- variable_summary %>%
  pivot_longer(cols = c(CI_low, WeightedMean, CI_high),
               names_to = "Metric", values_to = "Value")

ggplot(Top_variable_long, aes(x=Variable,y=Metric, fill = Value))+
  geom_tile(color="white")+
  scale_fill_gradient(low = "white",high = "blue")+
  theme_bw()
##here is to calculate how much percent explained by model
brt_model <- gbm(
  CUE ~  Climate1 + Climate2  + Soil1 + Soil2 + Soil3 + Elevation + Slope,
  data = Top,
  distribution = "gaussian",
  n.trees = 3000,
  interaction.depth = 5,
  shrinkage = 0.01,
  bag.fraction = 0.7,
  train.fraction = 1,
  cv.folds = 10,
  n.cores = parallel::detectCores() - 1
)
best.iter <- gbm.perf(brt_model, method = "cv")
# train_index <- 1:floor(0.8 * nrow(Top))
# test_index <- (floor(0.8 * nrow(Top)) + 1):nrow(Top)
# test_data <- Top[test_index, ]
# # Predict on test data
# preds <- predict(brt_model, newdata = test_data, n.trees = best.iter)
preds <- predict(brt_model, newdata = Top, n.trees = best.iter)
r2 <- cor(preds, Top$CUE)^2
# Print explained variance
cat("Explained variance (R²):", round(r2 * 100, 2), "%\n") # in total 93.18%
plot(preds, Top$CUE,pch = 16, col = "steelblue")
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)
# without climate 30.11 %; without veg 70.81 %; without soil 62.72 %

library(rfPermute)
rfP_top <- rfPermute(CUE ~ Climate1 + Climate2 + Veg1 + Veg2 + Soil1 + Soil2 +Soil3 + Slope + Elevation,
                     data=Top,ntree=1500, mtry=10, nrep = 9, num.cores = 15,
                     importance=TRUE, proximity=TRUE, do.trace=TRUE, keep.forest=TRUE, rsq=TRUE)

summary(rfP_top)
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
sig_colors_top <- colorRampPalette(brewer.pal(5,"GnBu"))(num_sig_top)

importance_scale_top$color <- "gray80"

importance_scale_top$color[importance_scale_top$sig_label != ""] <- sig_colors_top[
  rank(importance_scale_top$`%IncMSE`[importance_scale_top$sig_label !=""])
]

p_top <- ggplot(importance_scale_top, aes(x=name, y=`%IncMSE`,fill=color)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_identity() +
  theme_classic() +
  scale_y_continuous(expand = c(0, 2), limits = c(0, NA)) +
  labs(title = expression("CUE"[ST] * " explained by environmental factors in Topsoil"))+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust=-2, size=12, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # axis.line.x = element_line(linetype=1, color="black", size=1),
        # axis.line.y = element_line(linetype=1, color="black", size=1),
        # axis.ticks.x = element_line(linetype=1, color="black", size=1, lineend = 1),
        # axis.ticks.y = element_line(linetype=1, color="black", size=1, lineend = 1),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "none"
  )

print(p_top)
p1 <- p_top +
  geom_text(data = importance_scale_top, aes(x = name, y = `%IncMSE`, label = sig_label), nudge_y = 1) +
  annotate('text',x=10, y= 28, label = "Mean of squared residuals: 0.00369",color = "#0868AC", 
           family ="Arial",size=4,fontface="plain",hjust=0)+
  annotate('text',x=10, y=23, label = "Var explained: 82.8%",color = "#0868AC", 
           family ="Arial",size=4,fontface="plain",hjust=0)
p1

######__________________________________________________________________________________________
Deep <- data %>%
  filter(depth_class != "0-30")


Sub.climate <- Deep[4:14]

Sub.pca.clim <- prcomp(Sub.climate, center = T, scale. = T)
summary(Sub.pca.clim)
Sub_eingvalues <- Sub.pca.clim$sdev^2
pc_indices <- which(Sub_eingvalues > 1)
pcs <- as.data.frame(Sub.pca.clim$x[,pc_indices])
sort(abs(Sub.pca.clim$rotation[,1]), decreasing=T)
sort(abs(Sub.pca.clim$rotation[,2]), decreasing=T)

Deep$Climate1 <- Sub.pca.clim$x[,1]
Deep$Climate2 <- Sub.pca.clim$x[,2]

Sub.soil <- Deep[,c(17:22,24:27)]

Sub.pca.soil <- prcomp(Sub.soil, center = T, scale. = T)
summary(Sub.pca.soil)

Sub_eingvalues <- Sub.pca.soil$sdev^2
pc_indices <- which(Sub_eingvalues > 1)
pcs <- as.data.frame(Sub.pca.soil$x[,pc_indices])

sort(abs(Top.pca.soil$rotation[,1]), decreasing=T)
sort(abs(Top.pca.soil$rotation[,2]), decreasing=T)
sort(abs(Top.pca.soil$rotation[,3]), decreasing=T)

Deep$Soil1 <- Sub.pca.soil$x[,1]
Deep$Soil2 <- Sub.pca.soil$x[,2]
Deep$Soil3 <- Sub.pca.soil$x[,3]

Sub.veg <- Deep[,c(23,28:31)]

Sub.pca.veg <- prcomp(Sub.veg, center = T, scale. = T)
summary(Sub.pca.veg)

Sub_eingvalues <- Sub.pca.veg$sdev^2
pc_indices <- which(Sub_eingvalues > 1)
pcs <- as.data.frame(Sub.pca.veg$x[,pc_indices])
sort(abs(Sub.pca.veg$rotation[,1]), decreasing=T)
sort(abs(Sub.pca.veg$rotation[,2]), decreasing=T)

Deep$Veg1 <- Sub.pca.veg$x[,1]
Deep$Veg2 <- Sub.pca.veg$x[,2]

library(gbm)
set.seed(629)

brt_model <- gbm(
  CUE ~ Soil1 + Soil2 + Soil3 ,
  data = Deep,
  distribution = "gaussian",
  n.trees = 3000,
  interaction.depth = 5,
  shrinkage = 0.01,
  bag.fraction = 0.7,
  train.fraction = 0.8,
  cv.folds = 10,
  n.cores = parallel::detectCores() - 1
)

best.iter <- gbm.perf(brt_model, method = "cv")

summary(brt_model, n.trees = best.iter)
preds <- predict(brt_model, newdata = Deep, n.trees = best.iter)
r2 <- cor(preds, Deep$CUE)^2
# Print explained variance
cat("Explained variance (R²):", round(r2 * 100, 2), "%\n") # in total 93.18%
plot(preds, Deep$CUE,pch = 16, col = "steelblue")
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)
######__________________________________________________________________________________________




library(cowplot)

combined <- plot_grid(p1, p2,p3, ncol = 1,align = "v", axis = "lr")

label_plot <- ggdraw() +
  annotate("text",
           x=0.5, y=0.5, label = "Increase in MSE (%)",
           angle = 90, vjust = 1.5, 
           size = 4, family="Arial", fontface="plain")

final_pot <- ggdraw() +
  draw_plot(combined, x=0.05, width=0.9) +
  draw_plot(label_plot, x=0, width = 0.05 )

final_pot

ggsave("figures/Fig2.tiff", width = 8, height = 8, dpi = 300)


combined2 <- plot_grid(p1, p4, ncol = 1,align = "v", axis = "lr")

label_plot2 <- ggdraw() +
  annotate("text",
           x=0.5, y=0.5, label = "Increase in MSE (%)",
           angle = 90, vjust = 1.5, 
           size = 4, family="Arial", fontface="plain")

final_pot2 <- ggdraw() +
  draw_plot(combined2, x=0.05, width=0.9) +
  draw_plot(label_plot2, x=0, width = 0.05 )

final_pot2

ggsave("figures/Fig2.1.tiff", width = 8, height = 8, dpi = 300)

ggplot(data = Top,aes(x=BD, y=CUE)) +
  geom_point()+
  smplot2::sm_statCorr()


ggplot(data = Deep,aes(x=Clay, y=CUE)) +
  geom_point()+
  smplot2::sm_statCorr()









