library(tidyverse)
library(ggplot2)

col_types <- c("guess","guess","text", rep("guess",179))
data <- readxl::read_xlsx("data_filter.xlsx", col_types = col_types, na=c("","NA"))

data1 <- data %>%
  # select(`Paper.ID`, `Soil.depth`,Latitude, Longitude, Ecosystem_type, Soil_layers, Mean_SOC, CUE) %>%
  mutate(Soil_Depth = `Soil.depth` %>%
           str_replace_all("forest floor", "0_5") %>%
           str_replace_all("\\bBS\\b", "0_30") %>%
           str_replace_all("\\bRS\\b", "0_30") %>%
           str_replace_all("^OA$", "0_20") %>%
           str_replace_all("\\bB horizons\\b", "30_100") %>%
           str_replace_all("^B$", "30_100") %>%
           str_replace_all("^O$", "0_5") %>%
           str_replace_all("\\bO horizons\\b", "0_5") %>%
           str_replace_all("^C$", "100_200") %>%
           str_replace_all("^CA$", "100_200") %>%
           str_replace_all("^A$", "5_20") %>%
           str_replace_all("^AO$", "5_20") %>%
           str_replace_all("^AE$", "5_20") %>%
           str_replace_all("^EA$", "20_30") %>%
           str_replace_all("^E$", "20_30") %>%
           str_replace_all("–", "_") %>%
           str_replace_all("-", "_") %>%
           str_replace_all("cn","cm") %>%
           if_else(str_detect(., "cm$"), ., paste0(., "cm"))
  )

##https://doi.org/10.1016/j.soilbio.2025.109842 based on this article, data from the O layer and litter layer were excluded and only data from mineral soils were used;
##in this analysis, forest floor was also excluded, namely, all 0_5cm were excluded

drop_depths <- c(
  "0_5cm", "0_2cm", "0_3cm", "3_7cm",
  "2_10cm", "2_5cm", "7_12cm", "7_13cm",
  "100_200cm", "180_200cm"
)
data1 <- data1 %>%
  filter(!Soil_Depth %in% drop_depths) #517 data points were excluded

# xlsx::write.xlsx(data1, file = "data_filter_depth.xlsx",sheetName = "data_filter_depth")

depth <- data1 %>%
  group_by(Soil_Depth) %>%
  summarise(n=n())
#_______________________________________________________________________________________________________________________________________
#here is to calculate some data for publication

# data1$Paper.ID = factor(data1$Paper.ID)
# data1$Longitude = factor(data1$Longitude)
# summary(data1)
# 
# data1 %>% 
#   group_by(Paper.ID) %>%
#   summarise(n=n())
# data1 %>%
#   group_by(Longitude) %>%
#   summarise(n=n())
#end_______________________________________________________________________________________________________________________________________

data2 <- data1 %>%
  mutate(clean_depth = Soil_Depth %>%
           str_remove("cm")
         ) %>%
  separate(clean_depth, into = c("depth_min", "depth_max"), sep = "_", convert = T) %>%
  mutate(depth_mid = (depth_min + depth_max)/2)

# summary_stats <- data2 %>%
#   filter(!is.na(depth_mid), Soil_layers %in% c("Topsoil","Subsoil")) %>%
#   group_by(Soil_layers) %>%
#   summarise(mean_depth = mean(depth_mid, na.rm=T),
#             median_depth = median(depth_mid, na.rm = T))
# 
# ggplot(data2 %>% filter(Soil_layers %in% c("Topsoil", "Subsoil")),
#        aes(x=depth_mid, fill=Soil_layers))+
#   geom_density(alpha=0.5)+
#   scale_fill_manual(values = c("Topsoil"= "#1b9e77", "Subosil" = "#d95f02"))+
#   theme_minimal()
# 
# 
# data2 %>%
#   ggplot(aes(x=Soil_layers, y=CUE)) +
#   ggforce::geom_sina(aes(color=Soil_layers),alpha=0.3)+
#   geom_violin(aes(fill=Soil_layers), trim = T,  alpha=0.3)+
#   geom_boxplot(width=0.2,outlier.shape = NA)
# 
# 
# data3 <- data2 %>%
#   mutate(depth_class =case_when(
#     depth_max <= 20  ~ "0_20",
#     depth_min >=20  & depth_max <= 40  ~ "20_40",
#     depth_min >=40 & depth_max <= 60 ~ "40_60",
#     depth_min >=60  & depth_max <= 100 ~ "60_100"
#   )) %>%
#   filter(depth_class !="NA")
# 
# df <- data3 %>%
#   group_by(depth_class) %>%
#   summarise(mean_CUE = mean(CUE),n=n())
# 
# data3 %>%
#   ggplot(aes(x=depth_class, y=CUE)) +
#   ggforce::geom_sina(aes(color=depth_class),alpha=0.3)+
#   geom_violin(aes(fill=depth_class), trim = T,  alpha=0.3)+
#   geom_boxplot(width=0.2,outlier.shape = NA)
# 
# data3 %>%
#   ggplot(aes(x=depth_class, y=CUE)) +
#   ggforce::geom_sina(aes(color=depth_class),alpha=0.3)+
#   geom_violin(aes(fill=depth_class), trim = T,  alpha=0.3)+
#   geom_boxplot(width=0.2,outlier.shape = NA)
# 
# summary_stats <- data3 %>%
#   filter(!is.na(depth_mid), depth_class %in% c("0_20","20_40","40_60","60_100")) %>%
#   group_by(depth_class) %>%
#   summarise(mean_depth = mean(depth_mid, na.rm=T),
#             median_depth = median(depth_mid, na.rm = T))
# 
# p <- ggplot(data3 %>% filter(depth_class %in% c("0_20","20_40","40_60","60_100")),
#        aes(x=depth_mid, fill=depth_class))+
#   # facet_wrap(~Ecosystem_type)+
#   geom_density(alpha=0.5)+
#   scale_fill_manual(values = c("0_20"= "#1b9e77", "20_40" = "#d95f02", "40_60" = "#8da0cb","60_100"="#e78ac3"))+
#   scale_x_continuous(limits = c(-10,110))+
#   theme_classic()
# p
# 
# p +
#   geom_vline(data = summary_stats, aes(xintercept = mean_depth, color = depth_class),linetype = "dashed", size = 1) +
#   # geom_vline(data = summary_stats, aes(xintercept = median_depth, color = depth_class),linetype = "solid", size = 1) +
#   scale_color_manual(values = c("0_20"= "#1b9e77", "20_40" = "#d95f02", "40_60" = "#8da0cb","60_100"="#e78ac3"),
#                      name = "Statistics",
#                      labels = c("Topsoil Mean/Median", "Subsoil Mean/Median", "Deepsoil Mean/Median"))+
#   theme(legend.position = "none")
# 
# data3 <- data3 %>%
#   mutate(depth_class = factor(depth_class, levels = c("0_20","20_40","40_60","60_100")),
#          CUE=as.numeric(CUE))
# 
# set.seed(629)
# 
# bootstrip_ci <- function(data3, n_boot=1000){
#   boot_mean <- replicate(n_boot,{
#     sample_data <- sample(data3, replace = T)
#     mean(sample_data)
#   })
#   ci <- quantile(boot_mean, c(0.025, 0.975))
#   return(ci)
# }
# 
# summary_states_CUE <- data3 %>%
#   group_by(depth_class) %>%
#   summarise(boot_means = list(replicate(1000, mean(sample(CUE, replace = T), na.rm=T))),.groups = "drop") %>%
#   mutate(Mean=map_dbl(boot_means, ~mean(.x)),
#          lower_CI = map_dbl(boot_means, ~quantile(.x,0.025)),
#          upper_CI = map_dbl(boot_means, ~quantile(.x,0.975)),
#          depth_class=factor(depth_class, levels = c("0_20","20_40","40_60","60_100"))) %>%
#   select(-boot_means)
# 
# library(ggridges)
# library(ggpubr)
# 
# summary_states_CUE <- summary_states_CUE %>%
#   mutate(depth_num = as.numeric(depth_class))
# 
# p_CUE_CI <- ggplot(data = summary_states_CUE, aes(x=Mean, y=depth_class)) +
#   theme_bw() +
#   geom_density_ridges(data=data3, aes(x=CUE, y=depth_class, fill=depth_class, color=depth_class, height=stat(density)), alpha=0.2, rel_min_height=0.01, scale=0.2, linetype=2, linewidth=0.2)+
#   geom_jitter(data = data3, aes(x=CUE, y=depth_class, colour = depth_class), size=1, alpha=0.2,
#               width=0.05,height = 0.1
#               )+
#   geom_errorbarh(aes(xmin=lower_CI, xmax=upper_CI),color="red", height=0)+
#   geom_point(shape=21, size=1, aes(fill=depth_class), color="black")+
#   # geom_line(data = summary_states_CUE, aes(x=Mean, y=depth_num, group = 1), color="black", inherit.aes = F, linewidth=0.6)+
#   # coord_flip()+
#   labs(y="Soil Depth (cm)", x=expression(CUE[ST]))+
#   scale_x_continuous(position="top")+
#   scale_y_discrete(limits=rev(levels(summary_states_CUE$depth_class)))+
#   scale_color_brewer(palette = "Accent")+
#   scale_fill_brewer(palette = "Accent")+
#   theme(legend.position = "none")
# 
# p_CUE_CI
# 
# 
# model <- lmerTest::lmer(CUE ~ depth_class + (1|Paper.ID), data=data3)
# summary(model)


data4 <- data2 %>%
  mutate(depth_class =case_when(
    depth_max <= 30  ~ "0-30",
    depth_min >=20 & depth_max <=40 ~ "30-60",
    depth_min >=30  & depth_max <= 60  ~ "30-60",
    depth_min >=50  & depth_max <= 70  ~ "60-100",
    depth_min >=60  & depth_max <= 100  ~ "60-100"
  )) %>%
  filter(depth_class !="NA")

# write.csv(data4,file = "data_analysis.csv",row.names = F)

df <- data4 %>%
  group_by(depth_class) %>%
  summarise(mean_CUE = mean(CUE),n=n())

data_summary <- data4 %>%
  group_by(depth_class) %>%
  summarise(mean = mean(depth_mid),median = median(depth_mid), n=n())


ggplot(data4 %>% filter(depth_class %in% c("0-30","30-60", "60-100")),
       aes(x=depth_min, fill=depth_class))+
  geom_density(alpha=0.5,bw=5)+
  scale_fill_manual(values = c("0-30"= "#1b9e77", "30-60" = "#d95f02", "60-100" = "#8da0cb"))+
  scale_x_continuous(limits = c(0,100))+
  theme_minimal()

sample_counts <- data4 %>%
  group_by(depth_class) %>%
  summarise(n=n()) %>%
  mutate(x=c(1,2,3))

#statistical to test CUE 
model <- lmerTest::lmer(CUE ~ depth_class + (1|Paper.ID), data=data4)
summary(model)
anova(model)
em <- emmeans::emmeans(model,  ~ depth_class)
data_letter <- multcomp::cld(em, Letters= letters, adjudt="tukey")


library(ggforce)

Figb <- data4 %>%
  ggplot(aes(x=depth_class, y=CUE)) +
  geom_sina(aes(color=depth_class),alpha=0.3)+
  # geom_violin(aes(fill=depth_class), trim = T,  alpha=0.3)+
  geom_boxplot(width=0.2,outlier.shape = NA,aes(color=depth_class))+
  geom_text(data = sample_counts, 
            aes(x=x+0.25, y=0.05,label = paste0("(", n,")"),color=depth_class),
            inherit.aes = FALSE, size = 3) +
  geom_text(data = data_letter, aes(x=depth_class, label=.group, y=0.61))+
  scale_fill_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  scale_color_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  coord_cartesian(ylim = c(0,0.7))+
  theme_bw()+
  annotate("text",
           x = 2,
           y = 0.7,
          label = "italic(F) == 11.67 * \"; \"* italic(P) * \" < \" * 0.001",
          size = 4,
           parse=T) +
  labs(x="Soil Depth (cm)", y="CUE")+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=12,  family="Arial"),   
        axis.title = element_text(color="black",size=12, family="Arial")
  )

Figb

data_TD <- data4 %>%
  mutate(depth_type=case_when(
    depth_class == "0-30" ~ "Topsoil",
    depth_class %in% c("30-60","60-100") ~ "Subsoil"
  )) %>%
  mutate(depth_type=factor(depth_type, levels = c("Topsoil","Subsoil"), labels = c("Topsoil","Subsoil")))

#calculate depth 
data_TD %>%
  group_by(depth_type) %>%
  summarise(n=n(), mean1=mean(depth_min), median1=median(depth_min), mean2=mean(depth_max), median2=median(depth_max))


sample_counts_TD <- data_TD %>%
  group_by(depth_type) %>%
  summarise(n=n()) %>%
  mutate(x=c(1,2))

#statistical to test CUE 
model <- lmerTest::lmer(CUE ~ depth_type + (1|Paper.ID), data=data_TD)
summary(model)
anova(model)
em <- emmeans::emmeans(model,  ~ depth_type)
data_letter <- multcomp::cld(em, Letters= letters, adjudt="tukey")


Figb2 <- data_TD %>%
  ggplot(aes(x=depth_type, y=CUE)) +
  geom_sina(aes(color=depth_type),alpha=0.3)+
  # geom_violin(aes(fill=depth_class), trim = T,  alpha=0.3)+
  geom_boxplot(width=0.2,outlier.shape = NA,aes(color=depth_type))+
  geom_text(data = sample_counts_TD, 
            aes(x=x+0.25, y=0.05,label = paste0("(", n,")"),color=depth_type),
            inherit.aes = FALSE, size = 3) +
  geom_text(data = data_letter, aes(x=depth_type, label=.group, y=0.61))+
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  coord_cartesian(ylim = c(0,0.7))+
  theme_bw()+
  annotate("text",
           x = 1.5,
           y = 0.7,
           label = "italic(F) == 22.21 * \"; \"* italic(P) * \" < \" * 0.001",
           size = 4,
           parse=T) +
  labs(x="Soil layer", y="CUE")+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=12,  family="Arial"),   
        axis.title = element_text(color="black",size=12, family="Arial")
  )

Figb2
ggsave("figures/Figb2.tif", width = 5,height = 4)
data_TD_climate <- data_TD %>%
  mutate(Climatezone = case_when(
           Climatezone %in% c("1","2") ~ "Tropical",
           Climatezone %in% c("4","5","6","7") ~ "Arid",
           Climatezone %in% c("11","12","14","15") ~ "Temperate",
           Climatezone %in% c("21","22","23") ~ "Cold",
           Climatezone == "29" ~ "Cold",
           TRUE ~ NA_character_
           ))

data_TD_climate <- data_TD_climate %>%
  mutate(Climatezone=factor(Climatezone, levels = c("Tropical","Temperate","Arid","Cold")))

data_TD_climate %>%
  group_by(Climatezone,depth_type) %>%
  summarise(n=n())

library(rstatix)
library(ggpubr)
stat.test <- data_TD_climate %>%
  group_by(Climatezone) %>%
  wilcox_test(CUE ~ depth_type) %>%
  adjust_pvalue(method="bonferroni") %>%
  add_significance("p.adj")

Figk <- ggplot(data_TD_climate, aes(x=Climatezone, y=CUE)) +
  theme_bw()+
  geom_boxplot(aes(color=depth_type), position = position_dodge(0.9), width=0.3)+
  stat_compare_means(aes(group = depth_type), label = "p.signif")+
  geom_sina(aes(color=depth_type),alpha=0.3)+
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  coord_cartesian(ylim = c(0,0.7))+
  labs(x="Climate zone", y="CUE")+
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        axis.text=element_text(color="black",size=12,  family="Arial"),   
        axis.title = element_text(color="black",size=12, family="Arial")
  )
Figk 

Figk2 <- data_TD_climate %>%
  ggplot(aes(x=depth_type, y=CUE,color=Climatezone)) +
  geom_point(position = position_jitterdodge(), alpha=0.3)+
  geom_boxplot(outlier.shape = NA)+
  # geom_text(data = sample_counts_TD, 
  #           aes(x=x+0.25, y=0.05,label = paste0("(", n,")"),color=depth_type),
  #           inherit.aes = FALSE, size = 3) +
  # geom_text(data = stat.test, aes(x=Climatezone, label=p.adj.signif, y=0.61), inherit.aes = F)+
  scale_fill_brewer(palette = "Set2")+ 
  scale_color_brewer(palette = "Set2")+
  coord_cartesian(ylim = c(0,0.7))+
  theme_bw()+
  annotate("text",
           x = 1,
           y = 0.62,
           label = "italic(F) == 1.48 * \"; \"* italic(P) * \" = \" * 0.22",
           size = 4, color = '#2A83C7',
           parse=T) +
  annotate("text",
           x = 2,
           y = 0.62,
           label = "italic(F) == 1.16 * \"; \"* italic(P) * \" = \" * 0.33",
           size = 4, color = '#E88B33',
           parse=T) +
  labs(x="Soil layer", y="CUE")+
  theme(legend.position = c(0.5,0.93),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        axis.text=element_text(color="black",size=12,  family="Arial"),   
        axis.title = element_text(color="black",size=12, family="Arial")
  )

Figk2

data.top <- data_TD_climate %>%
  filter(depth_type == "Topsoil")
data.sub <- data_TD_climate %>%
  filter(depth_type=="Subsoil")
#statistical to test CUE 
model <- lmerTest::lmer(CUE ~ Climatezone + (1|Paper.ID), data=data.top)
summary(model)
anova(model)
em <- emmeans::emmeans(model,  ~ Climatezone)
data_letter <- multcomp::cld(em, Letters= letters, adjudt="tukey")

model <- lmerTest::lmer(CUE ~ Climatezone + (1|Paper.ID), data=data.sub)
summary(model)
anova(model)
em <- emmeans::emmeans(model,  ~ Climatezone)
data_letter <- multcomp::cld(em, Letters= letters, adjudt="tukey")




data4$Latitude<-as.numeric(data4$Latitude)

data_LAT <- data4 %>%
  mutate(Latitude = abs(Latitude)) %>%
  mutate(depth_class=factor(depth_class, levels = c("0-30", "30-60","60-100")))


library(ggpmisc)

Figc <- data_LAT %>%
  ggplot(aes(x=Latitude, y=CUE, fill = depth_class,color = depth_class))+
  theme_bw()+
  geom_point(alpha=0.3, size=3,shape=21,stroke=0.3,color="white") +
  # geom_smooth(method = "lm", formula = y ~ x,level=0.999,size=1.1,se=T)+
  stat_poly_line(formula = y ~ poly(x,2), se=T)+
  stat_poly_eq(formula = y ~ poly(x,2),
               aes(label=paste(..eq.label..,
                               ..adj.rr.label..,
                               ..p.value.label..,
                               sep = "~~~")), parse = T, vstep = 0.065,
               label.x.npc = 'left', label.y.npc = 'top', size=3) +
  # geom_smooth(method = "lm", formula = y ~ x, color = "#377eb8")+
  # facet_wrap(~Soil_layers)+
  scale_fill_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  scale_color_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60, by=20))+
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,0.7, by=0.2))+
  labs(x=paste('Absolute latitude (\u00b0)'), y="CUE")+
  theme(legend.position = "none",
        # legend.background = element_blank(),
        # legend.key.size = unit(4, "pt"),
        # legend.key.spacing.y = unit(4, "pt"),
        # # panel.grid = element_blank(),
        # legend.title = element_blank(),
        # legend.text = element_text(color="black",size=8,  family="Arial"),
        strip.text = element_text(color="black",size=12,  family="Arial"),
        axis.text=element_text(color="black",size=12,  family="Arial"),   
        axis.title=element_text(color="black",size=12, family="Arial"))
Figc   

data_TD_2 <- data_LAT %>%
  mutate(depth_type=case_when(
    depth_class == "0-30" ~ "Topsoil",
    depth_class %in% c("30-60","60-100") ~ "Subsoil"
  )) %>%
  mutate(depth_type=factor(depth_type, levels = c("Topsoil","Subsoil")))


Figc2 <- data_TD_2 %>%
  ggplot(aes(x=Latitude, y=CUE, fill = depth_type,color = depth_type))+
  theme_bw()+
  geom_point(alpha=0.3, size=3,shape=21,stroke=0.3,color="white") +
  # geom_smooth(method = "lm", formula = y ~ x,level=0.999,size=1.1,se=T)+
  stat_poly_line(formula = y ~ poly(x,2), se=T)+
  stat_poly_eq(formula = y ~ poly(x,2),
               aes(label=paste(..eq.label..,
                               ..adj.rr.label..,
                               ..p.value.label..,
                               sep = "~~~")), parse = T, vstep = 0.065,
               label.x.npc = 'left', label.y.npc = 'top', size=3) +
  # geom_smooth(method = "lm", formula = y ~ x, color = "#377eb8")+
  # facet_wrap(~Soil_layers)+
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60, by=20))+
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,0.7, by=0.2))+
  labs(x=paste('Absolute latitude (\u00b0)'), y="CUE")+
  theme(legend.position = "none",
        # legend.background = element_blank(),
        # legend.key.size = unit(4, "pt"),
        # legend.key.spacing.y = unit(4, "pt"),
        # # panel.grid = element_blank(),
        # legend.title = element_blank(),
        # legend.text = element_text(color="black",size=8,  family="Arial"),
        strip.text = element_text(color="black",size=12,  family="Arial"),
        axis.text=element_text(color="black",size=12,  family="Arial"),   
        axis.title=element_text(color="black",size=12, family="Arial"))
Figc2   


data4 <- data4 %>%
  mutate(
  Latitude = as.numeric(Latitude),
  Longitude = as.numeric(Longitude),
  CUE = as.numeric(CUE),
)
world_map <- map_data('world')

map1<-ggplot()+
  geom_polygon(data = world_map, aes(x=long, y=lat, group = group), color=NA,
               fill='#d9d9d9')+
  theme_bw()+
  geom_point(data = data4,size=3,
             aes(y = Latitude, x = Longitude, color= CUE),stroke=0.1,alpha=0.3)+
  scale_color_gradient(low="#fef0d9", high = "#b30000")+
  scale_x_continuous(breaks = seq(-180,180,30))+
  scale_y_continuous(breaks = seq(-90,90,30))+
  coord_cartesian(xlim = c(-155,175), ylim = c(-55,80))+
  labs(color="CUE",
       x=paste('Longitude (\u00b0)'),
       y=paste('Latitude (\u00b0)'))+
  theme(legend.position = c(0.1,0.3),
        legend.background = element_blank(),
        legend.title = element_text(margin = margin(b=1), color="black",size=10, family="Arial"),
        legend.text = element_text(color="black",size=10, family="Arial"),
        panel.grid = element_blank(),
        axis.text = element_text(color="black",size=12, family="Arial"),
        axis.title = element_text(color="black",size=12, family="Arial")
  )
map1

library(extrafont)
library(ggpubr)

below_row <- ggarrange(Figb2, Figc2,
                       ncol = 2, 
                       labels = c("b)", "c)"), 
                       # hjust = 0, vjust = 0, 
                       # align = "h", 
                       font.label = list(size = 14, family = "ARL", face = "plain")
                       )

# Then add Fig1.c below as a single plot
Fig.1 <- ggarrange(map1, below_row, 
                  ncol = 1, 
                  labels = "a)", 
                  # heights = c(1, 1),  # You can adjust relative heights
                  font.label = list(size = 14, family = "ARL", face = "plain")
                  )
Fig.1.1 <- ggarrange(Fig.1, Figk, 
                   ncol = 1, 
                   labels = c("","d)"), 
                   heights = c(1, 0.5),  # You can adjust relative heights
                   font.label = list(size = 14, family = "ARL", face = "plain")
)
# Print or save
print(Fig.1.1)

ggsave("figures/Fig1.1.tiff", width = 7, height = 8.5, dpi = 300)

FigS1<- ggarrange(Figb, Figc,
                       ncol = 2, 
                       labels = c("a)", "b)"), 
                       # hjust = 0, vjust = 0, 
                       # align = "h", 
                       font.label = list(size = 12, family = "ARL", face = "plain"))

print(FigS1)
ggsave("figures/FigS1.tiff", width = 7.5, height = 4, dpi = 300)

##________________________________________________________________________________________________________________________
#here i import other people data to compare________________________________________________________________________

cui <- read.csv("cui_2024.csv")

cui <- cui %>%
  mutate(Author="Cui") %>%
  mutate(CUE=CUEst) %>%
  select(Author, CUE)

he <- readxl::read_excel("he_2023.xlsx", sheet = "data")

he <- he %>%
  mutate(S_CN=((`BC:N`/`LC:N`)*1/`EEAC:N`),
         S_CP=((`BC:P`/`LC:P`)*1/`EEAC:P`)) 

he <- he %>%
  mutate(CUE=0.6 * ((S_CN*S_CP)/((0.5+S_CN) * (0.5+S_CP)))^0.5) %>%
  mutate(Author="He")

he <- he %>%
  filter(`General type` != "Farmland") %>%
  select(Author, CUE)

hu <- readxl::read_excel("hu_2025.xlsx", sheet = "sheet")

hu <- hu %>%
  filter(Method=="stoichiometry") %>%
  mutate(Author="Hu") %>%
  select(Author, CUE)

others <- rbind(cui, he)
others <- rbind(others, hu)

me <- data_TD %>%
  filter(depth_type == "Topsoil") %>%
  mutate(Author="This study") %>%
  select(Author,CUE)

total <- rbind(me, others)

total_summary <- total %>%
  group_by(Author) %>%
  summarise(mean=mean(CUE, na.rm=T),
            sd=sd(CUE,na.rm=T),
            n=n(),
            se=sd/sqrt(n),
            ci_lower=mean-1.96*se,
            ci_upper=mean+1.96*se,
            .groups = "drop") %>%
  mutate(Author=factor(Author, levels = c("This study","Hu","Cui","He"), labels = c("This study","Hu et al.","Cui et al.","He et al.")))

PO <- ggplot(data=total_summary, aes(x=Author, y=mean, fill = Author)) +
  geom_col(color="black", width = 0.3)+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width=0.1, linewidth=0.5)+
  geom_text(aes(label=n, y=0.4))+
  theme_bw()+
  scale_fill_brewer(palette = "Set2")+
  geom_hline(yintercept=0.322, linetype="dashed")+
  labs(x="",y="CUE")+
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        axis.text=element_text(color="black",size=12,  family="Arial"),   
        axis.title = element_text(color="black",size=12, family="Arial"))
  
PO

##now i want to calculate nutrient limitation___________________________________________________________________________

#Microbial C limitation increases with vector length
#The intensity of microbial P limitation increases with vector angle and the intensity of microbial N limitation increases when the vector angle decreases.
#length = sqrt(x^2 + y^2)
#Angle(degrees)=DEGREES(ATAN2(x,y))

#x=Cenzyme/(Cenzyme+Penzyme) = [BG/(BG+AP)]
#y=Cenzyme/(Cenzyme+Nenzyme) = [BG/(BG+NAG+LAP)]

data5 <- data4 %>%
  mutate(x=(`Mean_BG(β-1,4-glucosidase)`/(`Mean_BG(β-1,4-glucosidase)`+`Mean_AP(Acid.(alkaline).phosphatase)`)),
         y=(`Mean_BG(β-1,4-glucosidase)`/(`Mean_BG(β-1,4-glucosidase)`+`Mean_NAG(β-N-acetylglucosaminidase)`+`Mean_LAP(Leucyl.aminopeptidase)`))) %>%
  mutate(length=sqrt(x^2 + y^2),
         degree=atan2(y,x)*(180/pi)) 

data5 <- data5 %>%
  select(-x,-y)

# xlsx::write.xlsx(data5, file = "data_filter_1.xlsx",sheetName = "data_filter")

#statistical to test CUE 

model <- lmerTest::lmer(length ~ depth_class + (1|Paper.ID), data=data5)
summary(model)
anova(model)
em <- emmeans::emmeans(model,  ~ depth_class)
data_letter <- multcomp::cld(em, Letters= letters, adjudt="tukey")

Figd <- data5 %>%
  ggplot(aes(x=depth_class, y=length)) +
  geom_sina(aes(color=depth_class),alpha=0.3)+
  # geom_violin(aes(fill=depth_class), trim = T,  alpha=0.3)+
  geom_boxplot(width=0.2,outlier.shape = NA,aes(color=depth_class))+
  # geom_text(data = sample_counts, 
  #           aes(x = depth_class, y = 0.63, 
  #               label = paste0("(", n,")")),
  #           inherit.aes = FALSE, size = 4) +
  geom_text(data=data_letter, aes(x=depth_class, y= 1.3, label = .group))+
  scale_fill_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  scale_color_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  coord_cartesian(ylim = c(0,1.5))+
  theme_bw()+
  annotate("text",
           x = 2,
           y = 1.5,
           label = "italic(F) == 3.06 * \"; \"* italic(P) * \" < \" * 0.05",
           size = 4,
           parse=T) +
  labs(x="Soil Depth (cm)", y="Vector Length")+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=10,  family="Arial"),   
        axis.title = element_text(color="black",size=10, family="Arial")
  )

Figd

data_TD_3 <- data5 %>%
  mutate(depth_type=case_when(
    depth_class == "0-30" ~ "Topsoil",
    depth_class %in% c("30-60","60-100") ~ "Subsoil"
  )) %>%
  mutate(depth_type=factor(depth_type, levels = c("Topsoil","Subsoil")))

model <- lmerTest::lmer(length ~ depth_type + (1|Paper.ID), data=data_TD_3)
summary(model)
anova(model)
em <- emmeans::emmeans(model,  ~ depth_type)
data_letter <- multcomp::cld(em, Letters= letters, adjudt="tukey")


Figd2 <- data_TD_3 %>%
  ggplot(aes(x=depth_type, y=length)) +
  geom_sina(aes(color=depth_type),alpha=0.3)+
  # geom_violin(aes(fill=depth_class), trim = T,  alpha=0.3)+
  geom_boxplot(width=0.2,outlier.shape = NA,aes(color=depth_type))+
  # geom_text(data = sample_counts, 
  #           aes(x = depth_class, y = 0.63, 
  #               label = paste0("(", n,")")),
  #           inherit.aes = FALSE, size = 4) +
  geom_text(data=data_letter, aes(x=depth_type, y= 1.3, label = .group))+
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  coord_cartesian(ylim = c(0,1.5))+
  theme_bw()+
  annotate("text",
           x = 1.5,
           y = 1.5,
           label = "italic(F) == 1.77 * \"; \"* italic(P) * \" = \" * 0.18",
           size = 4,
           parse=T) +
  labs(x="Soil layer", y="Vector length")+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=10,  family="Arial"),   
        axis.title = element_text(color="black",size=10, family="Arial")
  )

Figd2

model <- lmerTest::lmer(degree ~ depth_class + (1|Paper.ID), data=data5)
summary(model)
anova(model)
em <- emmeans::emmeans(model,  ~ depth_class)
data_letter <- multcomp::cld(em, Letters= letters, adjudt="tukey")

Fige <- data5 %>%
  ggplot(aes(x=depth_class, y=degree)) +
  geom_hline(yintercept = 45, color= "red", linetype="dashed")+
  geom_sina(aes(color=depth_class),alpha=0.3)+
  # geom_violin(aes(fill=depth_class), trim = T,  alpha=0.3)+
  geom_boxplot(width=0.2,outlier.shape = NA,aes(color=depth_class))+
  # geom_text(data = sample_counts, 
  #           aes(x = depth_class, y = 0.63, 
  #               label = paste0("(", n,")")),
  #           inherit.aes = FALSE, size = 4) +
  geom_text(data = data_letter, aes(x=depth_class, y=92, label = .group))+
  scale_fill_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  scale_color_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  coord_cartesian(ylim = c(0,100))+
  theme_bw()+
  annotate("text",
           x = 2,
           y = 100,
           label = "italic(F) == 11.99 * \"; \"* italic(P) * \" < \" * 0.001",
           size = 4,
           parse=T) +
  labs(x="Soil Depth (cm)", y="Vector Angle")+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=10,  family="Arial"),   
        axis.title = element_text(color="black",size=10, family="Arial")
  )

Fige

model <- lmerTest::lmer(degree ~ depth_type + (1|Paper.ID), data=data_TD_3)
summary(model)
anova(model)
em <- emmeans::emmeans(model,  ~ depth_type)
data_letter <- multcomp::cld(em, Letters= letters, adjudt="tukey")

Fige2 <- data_TD_3 %>%
  ggplot(aes(x=depth_type, y=degree)) +
  geom_hline(yintercept = 45, color= "red", linetype="dashed")+
  geom_sina(aes(color=depth_type),alpha=0.3)+
  # geom_violin(aes(fill=depth_class), trim = T,  alpha=0.3)+
  geom_boxplot(width=0.2,outlier.shape = NA,aes(color=depth_type))+
  # geom_text(data = sample_counts, 
  #           aes(x = depth_class, y = 0.63, 
  #               label = paste0("(", n,")")),
  #           inherit.aes = FALSE, size = 4) +
  geom_text(data = data_letter, aes(x=depth_type, y=92, label = .group))+
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  coord_cartesian(ylim = c(0,100))+
  theme_bw()+
  annotate("text",
           x = 1.5,
           y = 100,
           label = "italic(F) == 6.65 * \"; \"* italic(P) * \" < \" * 0.05",
           size = 4,
           parse=T) +
  labs(x="Soil layer", y="Vector angle")+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=10,  family="Arial"),   
        axis.title = element_text(color="black",size=10, family="Arial")
  )

Fige2

library(ggpmisc)

figf <- ggplot(data5, aes(x=length, y=CUE, colour = depth_class, fill = depth_class))+
  geom_point(shape = 21,size=1.5, alpha=0.3)+
  # facet_wrap(~depth_class)+
  theme_bw()+
  stat_poly_line(formula = y ~ x, se=T)+
  stat_poly_eq(formula = y ~ x,
               aes(label=paste(..adj.rr.label..,
                               ..p.value.label..,
                               sep = "~~~")), parse = T, vstep = 0.065,
               label.x = "right", label.y= "top", size=3) +
  coord_cartesian(ylim = c(0,0.7), xlim = c(0,1.5))+
  labs(x="Carbon Limitation", y="CUE")+
  scale_fill_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  scale_color_manual(values = c('#2A83C7','#E88B33',"#8da0cb"))+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=10,  family="Arial"),   
        axis.title = element_text(color="black",size=10, family="Arial"))
 
figf 

figf2 <- ggplot(data_TD_3, aes(x=length, y=CUE, colour = depth_type, fill = depth_type))+
  geom_point(shape = 21,size=1.5, alpha=0.3)+
  # facet_wrap(~depth_class)+
  theme_bw()+
  stat_poly_line(formula = y ~ x, se=T)+
  stat_poly_eq(formula = y ~ x,
               aes(label=paste(..adj.rr.label..,
                               ..p.value.label..,
                               sep = "~~~")), parse = T, vstep = 0.065,
               label.x = "right", label.y= "top", size=3) +
  coord_cartesian(ylim = c(0,0.7), xlim = c(0,1.5))+
  labs(x="Carbon Limitation", y="CUE")+
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=10,  family="Arial"),   
        axis.title = element_text(color="black",size=10, family="Arial"))

figf2

figg2 <- ggplot(data_TD_3, aes(x=degree, y=CUE, colour = depth_type, fill = depth_type))+
  geom_point(shape = 21,size=1.5, alpha=0.3)+
  # facet_wrap(~depth_class)+
  theme_bw()+
  # stat_poly_line(formula = y ~ poly(x,2), se=T)+
  # stat_poly_eq(formula = y ~ poly(x,2),
  #              aes(label=paste(..adj.rr.label..,
  #                              ..p.value.label..,
  #                              sep = "~~~")), parse = T, vstep = 0.065,
  #              label.x = "right", label.y= "top", size=3) +
  # coord_cartesian(ylim = c(0,0.7), xlim = c(0,1.5))+
  labs(x="Nutrient Limitation", y="CUE")+
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  theme(legend.position = "none",axis.text=element_text(color="black",size=10,  family="Arial"),   
        axis.title = element_text(color="black",size=10, family="Arial"))

figg2
ggsave("figures/figS2.1.tiff", width = 5, height = 4, dpi = 300)
figS2 <- ggarrange(Figd, Fige, figf,figg2,
                  labels = c("a","b","c","d"),
                  font.label = list(size = 12, family = "ARL", face = "plain"),
                  ncol = 2, nrow = 2)
                  #      hjust = 0, vjust = 0, 
                  #      align = "h", 
                  #      # font.label = list(size = 60, family = "ARL", face = "plain")
                  # )


# Print or save
print(figS2)

ggsave("figures/figS2_C_N_P_limitation.tiff", width = 6, height = 6, dpi = 300)


fig2 <- ggarrange(Figd2, Fige2, figf2,ncol = 3,
                  labels = c("a)","b)","c)"),
                  font.label = list(size = 12, family = "ARL", face = "plain")
                  )
#      hjust = 0, vjust = 0, 
#      align = "h", 
#      # font.label = list(size = 60, family = "ARL", face = "plain")
# )


# Print or save
print(fig2)

ggsave("figures/fig2_C_N_P_limitation.tiff", width = 6, height = 3, dpi = 300)
