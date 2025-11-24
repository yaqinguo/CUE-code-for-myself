library(tidyverse)
library(plspm)

## here is only consider topsoil and subsoil

data <- read.csv("Env_factors_cart.csv", header = T)

data$depth_class <- as.factor(data$depth_class)
data$Ecosystem_type <- as.factor(data$Ecosystem_type)

num_cols <- setdiff(names(data),c("depth_class","Ecosystem_type"))
data[num_cols] <- lapply(data[num_cols],as.numeric)

data <- data %>%
  dplyr::select(-c(X, Ecosystem_type,TC,length,degree)) %>%
  rename(pH = Mean_pH, Sand = Sand.content, Clay = Clay.content, PS = PS_Bio15, TS = TS_Bio4, 
         Silt = Silt.content, BD = Bulk.Density, AGB = AbovegroundBiomass, BGB = BelowgroundBiomass)

data_top <- data %>%
  filter(depth_class == "0-30") %>%
  select(-depth_class)

data_sub <- data %>%
  filter(depth_class != "0-30") %>%
  select(-depth_class)

LVs <- c("climate","season", "clay","vegetation", "soil", "CUE")

path_matrix <- matrix(0,nrow = length(LVs), ncol = length(LVs), 
                      dimnames = list(LVs, LVs))

path_matrix["climate", c("season","clay","vegetation")] <- 1
path_matrix["season", c("vegetation","soil","CUE")] <- 1
path_matrix["clay", c("soil")] <- 1
path_matrix["vegetation",c("soil","CUE")] <- 1
path_matrix["soil", c("CUE")] <- 1

path_matrix <- t(path_matrix)

blocks <- list(
  climate = c("MAT","MAP"),
  season = c("PS","TS"),
  clay = c("Clay"),
  Vegetation = c("GPP","BGB","Shannon_EVI"),
  Soil = c("BD","pH"),
  CUE = c("CUE")
)
modes <- c("A","A","A","A","A","A")

used_vars <- unique(unlist(blocks))
# dat_model <- data_top[,used_vars, drop=F]
dat_model <- data_sub[,used_vars, drop=F]
set.seed(629)
#fits the model without bootstrap (faster to inspect outer loadings)
first_fit <- plspm(dat_model, path_matrix, blocks, modes=modes, scaled = T, boot.val = F)
# summary(first_fit)

#find negatively loading reflective indicators and flip them

outer <- first_fit$outer_model %>%
  select(block, name, loading)

neg_reflective <- outer %>%
  filter(loading < 0) %>%
  pull(name)

for (v in neg_reflective) {
   if (is.numeric(dat_model[[v]])) {
     dat_model[[v]] <- -dat_model[[v]]
   } else {
     warning(sprintf("Skipping %s: not numeric (class = %s)", v, class(dat_model[[v]])))
   }
 }

set.seed(629)
pls_fit <- plspm(dat_model, path_matrix, blocks, modes=modes, scaled = T, 
                 boot.val = T, br=100)
summary(pls_fit)

# plspm stores a tidy table of bootstrap path results in pls_fit$boot$paths
paths_boot <- pls_fit$boot$paths %>%
  # keep common column names if present; otherwise, rename defensively
  rename_with(~sub("^original$", "estimate", .x)) %>%
  rename_with(~sub("^mean\\.boot$", "mean_boot", .x)) %>%
  rename_with(~sub("^std\\.boot$", "sd_boot", .x)) %>%
  rename_with(~sub("^perc\\.025$", "ci_low", .x)) %>%
  rename_with(~sub("^perc\\.975$", "ci_high", .x))

# Extract "from -> to" into separate columns
paths_tbl <- paths_boot %>%
  mutate(path = rownames(pls_fit$boot$paths),
         from = sub("\\s*->\\s*.*$", "", path),
         to   = sub("^.*->\\s*", "", path)) %>%
  select(from, to, Original, mean_boot = any_of("mean_boot"),
         sd_boot = any_of("sd_boot"),
         ci_low = any_of("ci_low"), ci_high = any_of("ci_high")) %>%
  mutate(sig_95 = ifelse(!is.na(ci_low) & !is.na(ci_high) & ci_low*ci_high > 0, TRUE, NA))

## 8) Loadings (all should be positive for reflective blocks)
## -----------------------------
loadings_tbl <- pls_fit$outer_model %>%
  select(block, name, loading) %>%
  arrange(block, desc(loading))
## 9) R² for endogenous latent variables
## -----------------------------
r2_tbl <- pls_fit$inner_summary %>%
  as.data.frame() %>%
  rownames_to_column(var = "latent") %>%
  select(latent, R2)

## 10) Direct / Indirect / Total effects among LVs
##     Using the standard SEM identity: Total = (I - B)^(-1) - I, where B has
##     entries b_ij = path from i -> j (rows: source, cols: target).
## -----------------------------
B <- pls_fit$path_coefs  # rownames/colnames are LVs
LV_order <- rownames(B)
I  <- diag(nrow(B))
Tot <- solve(I - B) - I      # total effects (sum of all orders of indirect + direct)
Dir <- B
Ind <- Tot - Dir             # indirect effects

# Put into long tables
mat_to_long <- function(M, name) {
  as.data.frame(M) %>%
    mutate(from = rownames(M)) %>%
    tidyr::pivot_longer(-from, names_to = "to", values_to = name) %>%
    select(from, to, !!name)
}
dir_tbl <- mat_to_long(Dir, "direct")
ind_tbl <- mat_to_long(Ind, "indirect")
tot_tbl <- mat_to_long(Tot, "total")

effects_tbl <- dir_tbl %>%
  left_join(ind_tbl, by = c("from","to")) %>%
  left_join(tot_tbl, by = c("from","to")) %>%
  filter(from != to) %>%
  arrange(from, to)

## 11) Print key results
## -----------------------------
cat("\n=== Path coefficients with 95% bootstrap CIs (200 resamples) ===\n")
print(paths_tbl, row.names = FALSE, digits = 4)

cat("\n=== Indicator loadings (post flip for reflective) ===\n")
print(loadings_tbl, row.names = FALSE, digits = 4)

cat("\n=== R-squared (endogenous LVs) ===\n")
print(r2_tbl, row.names = FALSE, digits = 4)

cat("\n=== Direct / Indirect / Total effects among LVs ===\n")
print(effects_tbl, row.names = FALSE, digits = 4)

cat("\n=== gof ===\n")
print(pls_fit$gof, row.names = FALSE, digits = 4)

## 12) Optional plots
## -----------------------------
# Path diagram
plot(pls_fit)





df1 <- pls_top$inner_model$climate 
df2 <- pls_top$inner_model$season
df3 <- pls_top$inner_model$clay
df4 <- pls_top$inner_model$soil
df5 <- pls_top$inner_model$vegetation
df6 <- pls_top$inner_model$CUE
significance_top <- rbind(df1, df2)
significance_top <- rbind(significance_top,df3)
significance_top <- rbind(significance_top,df4)
significance_top <- rbind(significance_top,df5)
significance_top <- rbind(significance_top,df6)
significance_top <- significance_top %>%
  data.frame() %>%
  mutate(P = Pr...t..)

significance_top <- significance_top %>%
    mutate(Significance = case_when(
      P <= 0.001 ~ "***",
      P <= 0.01 ~ "**",
      P <= 0.05 ~ "*",
      TRUE ~ ""
    ))
xlsx::write.xlsx(significance_top, file = "significance_top.xlsx")

innerplot(pls_top,txt.col = "black",colpos = "#6890c4BB", colneg = "#f9675dBB")

top_effect <- data.frame(
  Predictor = sub("-> CUE$","",pls_top$effects$relationships[grepl("-> CUE$",pls_top$effects$relationships)]),
  Effect = pls_top$effects$total[grepl("-> CUE$", pls_top$effects$relationships)],
  stringsAsFactors = F
)
top_effect <- top_effect %>%
  mutate(label = sprintf("%.2f",Effect),
         text_y=ifelse(Effect > 0,
                       Effect - 0.02,
                       Effect + 0.02
         ))
top_effect$Predictor <- trimws(top_effect$Predictor)

top_effect$Predictor <- factor(top_effect$Predictor, levels = c("topography","climate","season","clay","soil","vegetation"),
                               labels = c("Topography","Climatic factors","Climatic seasonality","Clay content","Soil properties","Vegetation"))
p1 <- ggplot(top_effect, aes(x=Predictor,y=Effect))+
  geom_hline(yintercept = 0, linewidth=0.5) +
  geom_col(width = 0.5, fill=c("#FFCC66","#660099","#6699CC","#336699","#99CC66"),alpha=.3)+
  # geom_text(aes(y=text_y, label=label),size=2, color="white",fontface="bold")+
  theme_classic()+
  # labs(x="",y="Total effect", title = "Key variables affecting CUE in Topsoil")+
  labs(x="",y="")+
  scale_y_continuous(limits = c(-0.5,0.5), expand = c(0,0))+
  scale_x_discrete(labels = label_wrap_gen(width = 5))+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust=-2, size=8),
        panel.background = element_blank(),
        plot.background = element_blank(),
        # axis.text = element_blank(),
        # axis.line = element_line(colour = 'black'),
        # axis.text.x = element_text(size = 8, color = "black"),
        # axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        # axis.title.y = element_text(size = 8, color = "black"),
        # axis.line.x = element_line(linetype=1, color="black", size=1),
        axis.line.y = element_line(linetype=1, color="black", size=0.5),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_line(linetype=1, color="black", size=1, lineend = 1),
        # axis.ticks.length = unit(0.3, "cm"),
        legend.position = "none"
  )

p1
ggsave("figures/pls_top.tif", height = 2.5, width = 3, dpi = 300)
set.seed(629)
pls_sub <- plspm(data_sub, path_matrix, blocks, modes, scaled = T)
summary(pls_sub)

df1 <- pls_sub$inner_model$climate 
df2 <- pls_sub$inner_model$season
df3 <- pls_sub$inner_model$clay
df4 <- pls_sub$inner_model$soil
df5 <- pls_sub$inner_model$vegetation
df6 <- pls_sub$inner_model$CUE
significance_sub <- rbind(df1, df2)
significance_sub <- rbind(significance_sub,df3)
significance_sub <- rbind(significance_sub,df4)
significance_sub <- rbind(significance_sub,df5)
significance_sub <- rbind(significance_sub,df6)
significance_sub <- significance_sub %>%
  data.frame() %>%
  mutate(P = Pr...t..)

significance_sub <- significance_sub %>%
  mutate(Significance = case_when(
    P <= 0.001 ~ "***",
    P <= 0.01 ~ "**",
    P <= 0.05 ~ "*",
    TRUE ~ ""
  ))
xlsx::write.xlsx(significance_sub, file = "significance_sub.xlsx")

innerplot(pls_sub,txt.col = "black",colpos = "#6890c4BB", colneg = "#f9675dBB")

sub_effect <- data.frame(
  Predictor = sub("-> CUE$","",pls_sub$effects$relationships[grepl("-> CUE$",pls_sub$effects$relationships)]),
  Effect = pls_sub$effects$total[grepl("-> CUE$", pls_sub$effects$relationships)],
  stringsAsFactors = F
)
sub_effect <- sub_effect %>%
  mutate(label = sprintf("%.2f",Effect),
         text_y=ifelse(Effect > 0,
                       Effect - 0.02,
                       Effect + 0.02
                         ))
sub_effect$Predictor <- trimws(sub_effect$Predictor)

sub_effect$Predictor <- factor(sub_effect$Predictor, levels = c("topography","climate","season","clay","soil","vegetation"),
                               labels = c("Topography","Climatic factors","Climatic seasonality","Clay content","Soil properties","Vegetation"))
p2 <- ggplot(sub_effect, aes(x=Predictor,y=Effect))+
  geom_hline(yintercept = 0, linewidth=0.5) +
  geom_col(width = 0.5, fill=c("#FF6633","#FFCC66","#660099","#6699CC","#336699","#99CC66"),alpha=.3)+
  # geom_text(aes(y=text_y, label=label),size=2, color="white",fontface="bold")+
  theme_classic()+
  labs(x="",y="")+
  # labs(x="",y="Total effect", title = "Key variables affecting CUE in Subsoil")+
  scale_y_continuous(limits = c(-0.6,0.6), expand = c(0,0), breaks = c(-0.6, -0.3, 0, 0.3, 0.6))+
  # coord_cartesian(ylim = c(-0.5,0.5), expand = T)+
  scale_x_discrete(labels = label_wrap_gen(width = 5))+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust=-2, size=8),
        panel.background = element_blank(),
        plot.background = element_blank(),
        # axis.text = element_blank(),
        # axis.line = element_line(colour = 'black'),
        # axis.text.x = element_text(size = 8, color = "black"),
        # axis.text.y = element_text(size = 8, color = "black"),
        # axis.title.x = element_blank(),
        # axis.title.y = element_text(size = 8, color = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_line(linetype=1, color="black", size=0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(linetype=1, size=.5, lineend = 1),
        # axis.ticks.length = unit(0.3, "cm"),
        legend.position = "none"
  )

p2
ggsave("figures/pls_sub.tif", height = 2.5, width = 3, dpi = 300)

library(cowplot)

combined <- plot_grid(p1, p2, nrow = 1,align = "v", axis = "lr",
                      labels = c("e)", "f)"),
                      label_size = 12,
                      label_fontface = "plain",
                      label_fontfamily = "Arial")
combined 
# label_plot2 <- ggdraw() +
#   annotate("text",
#            x=0.5, y=0.5, label = "Total effect",
#            angle = 90, vjust = 1.5, 
#            size = 4, family="Arial", fontface="plain")
# 
# final_pot2 <- ggdraw() +
#   draw_plot(combined, x=0.05, width=0.9) +
#   draw_plot(label_plot2, x=0, width = 0.05 )
# 
# final_pot2

ggsave("figures/Fig3.1.tiff", width = 7.5, height = 3, dpi = 300)






