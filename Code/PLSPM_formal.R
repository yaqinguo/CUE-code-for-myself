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

LVs <- c("topography","climate","season","texture","vegetation","soil","CUE")

path_matrix <- matrix(0,nrow = length(LVs), ncol = length(LVs), dimnames = list(LVs, LVs))

path_matrix["topography", c("climate","season","texture")] <- 1
path_matrix["climate", c("season","vegetation","CUE")] <- 1
path_matrix["season", c("texture","soil","vegetation","CUE")] <- 1
path_matrix["texture", c("soil","CUE")] <- 1
path_matrix["vegetation", c("soil","CUE")] <- 1
path_matrix["soil",c("CUE")] <- 1

path_matrix <- t(path_matrix)
blocks <- list(
  Topography = c("Elevation", "Slope"),
  climate = c("MAT","MAP"),
  season = c("PS","TS"),
  texture = c("Clay","Sand"),
  Vegetation = c("Shannon_EVI","BGB","GPP"),
  Soil = c("BD","pH","Moisture"),
  CUE = c("CUE")
)
modes <- c("A","A","A","A","B","B","A")

set.seed(629)
pls_top <- plspm(data_top, path_matrix, blocks, modes, scaled = T)
summary(pls_top)

df1 <- pls_top$inner_model$climate 
df2 <- pls_top$inner_model$season
df3 <- pls_top$inner_model$texture
df4 <- pls_top$inner_model$vegetation
df5 <- pls_top$inner_model$soil
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

top_effect$Predictor <- factor(top_effect$Predictor, levels = c("topography","climate","season","texture","vegetation","soil"),
                               labels = c("Topography","Climatic factors","Climatic seasonality","Soil texture","Vegetation","Soil properties"))
p1 <- ggplot(top_effect, aes(x=Predictor,y=Effect))+
  geom_hline(yintercept = 0, linewidth=0.5) +
  geom_col(width = 0.5, fill=c("#FF6633","#FFCC66","#660099","#6699CC","#99CC66","#336699"),alpha=.3)+
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
        axis.text = element_blank(),
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
df3 <- pls_sub$inner_model$texture
df4 <- pls_sub$inner_model$vegetation
df5 <- pls_sub$inner_model$soil
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

sub_effect$Predictor <- factor(sub_effect$Predictor, levels = c("topography","climate","season","texture","vegetation","soil"),
                               labels = c("Topography","Climatic factors","Climatic seasonality","Soil texture","Vegetation","Soil properties"))
p2 <- ggplot(sub_effect, aes(x=Predictor,y=Effect))+
  geom_hline(yintercept = 0, linewidth=0.5) +
  geom_col(width = 0.5, fill=c("#FF6633","#FFCC66","#660099","#6699CC","#99CC66","#336699"),alpha=.3)+
  # geom_text(aes(y=text_y, label=label),size=2, color="white",fontface="bold")+
  theme_classic()+
  labs(x="",y="")+
  # labs(x="",y="Total effect", title = "Key variables affecting CUE in Subsoil")+
  scale_y_continuous(limits = c(-0.7,0.35), expand = c(0,0),breaks = c(-0.7, -0.35, 0, 0.35))+
  # coord_cartesian(ylim = c(-0.5,0.5), expand = T)+
  scale_x_discrete(labels = label_wrap_gen(width = 5))+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust=-2, size=8),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text = element_blank(),
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






