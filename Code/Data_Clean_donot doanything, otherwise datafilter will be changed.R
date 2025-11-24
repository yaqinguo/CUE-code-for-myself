library(tidyverse)

## here is only consider topsoil and subsoil
#Here datset is "Global Data Archive-2024.12.30.xlsx"

col_types <- c("text", rep("guess",292))
data <- readxl::read_xlsx("Global Data Archive-2024.12.30.xlsx", col_types = col_types, na=c("","NA"))[-1,]


#remove peatland & wetland

data <- data %>%
  filter(Vegetation_type !="Peatland" & Vegetation_type !="peatlands" & Vegetation_type !="Coastal wetland" & Vegetation_type !="Wetland")

# NAG or LAP has missing value, and change to zero

#this is to observe data
# df1 <- data_filter %>% select(`Mean_NAG(β-N-acetylglucosaminidase)`,`Mean_LAP(Leucyl aminopeptidase)`)

data_filter <- data %>%
  mutate(`Mean_NAG(β-N-acetylglucosaminidase)` = replace_na(as.numeric(`Mean_NAG(β-N-acetylglucosaminidase)`),0),
         `Mean_LAP(Leucyl aminopeptidase)` = replace_na(as.numeric(`Mean_LAP(Leucyl aminopeptidase)`),0))

#this is also to observe data
# df2 <- data_filter %>%
#   select(`Mean_NAG(β-N-acetylglucosaminidase)`,`Mean_LAP(Leucyl aminopeptidase)`) %>%
#   filter(`Mean_NAG(β-N-acetylglucosaminidase)`==0 & `Mean_LAP(Leucyl aminopeptidase)`!=0 |`Mean_NAG(β-N-acetylglucosaminidase)`!=0 & `Mean_LAP(Leucyl aminopeptidase)`==0)

# data_filter <- data_filter %>%
#   filter(`Mean_NAG(β-N-acetylglucosaminidase)`==0 & `Mean_LAP(Leucyl aminopeptidase)`!=0 |`Mean_NAG(β-N-acetylglucosaminidase)`!=0 & `Mean_LAP(Leucyl aminopeptidase)`==0) 

# in order to calculate, change to numeric 
data_filter <- data_filter %>%
  mutate(
    `Mean_BG(β-1,4-glucosidase)` = as.numeric(`Mean_BG(β-1,4-glucosidase)`),
    `Mean_NAG(β-N-acetylglucosaminidase)` = as.numeric(`Mean_NAG(β-N-acetylglucosaminidase)`),
    `Mean_LAP(Leucyl aminopeptidase)` = as.numeric(`Mean_LAP(Leucyl aminopeptidase)`),
    `Mean_AP(Acid (alkaline) phosphatase)` = as.numeric(`Mean_AP(Acid (alkaline) phosphatase)`),
    Mean_SOC = as.numeric(Mean_SOC),
    Mean_TN = as.numeric(Mean_TN),
    Mean_TP = as.numeric(Mean_TP),
    Mean_MBC = as.numeric(Mean_MBC),
    Mean_MBN = as.numeric(Mean_MBN),
    Mean_MBP = as.numeric(Mean_MBP)
  )


data_filter <- data_filter %>%
  drop_na(`Mean_BG(β-1,4-glucosidase)`) %>%
  filter(!(is.na(`Mean_NAG(β-N-acetylglucosaminidase)`)& is.na(`Mean_LAP(Leucyl aminopeptidase)`))) %>%
  drop_na(`Mean_AP(Acid (alkaline) phosphatase)`,Mean_SOC,Mean_TN,Mean_TP) 

data_filter <- data_filter %>%
  mutate(EEA_CN = `Mean_BG(β-1,4-glucosidase)`/(`Mean_NAG(β-N-acetylglucosaminidase)`+ `Mean_LAP(Leucyl aminopeptidase)`),
         EEA_CP = `Mean_BG(β-1,4-glucosidase)`/`Mean_AP(Acid (alkaline) phosphatase)`,
         L_CN = Mean_SOC/Mean_TN,
         L_CP = Mean_SOC/Mean_TP,
         B_CN = Mean_MBC/Mean_MBN,
         B_CP = Mean_MBC/Mean_MBP)

#this is to observe data
# df3 <- data_filter %>%
#   select(EEA_CN,EEA_CP,L_CN,L_CP,B_CN,B_CP,CUE)

## from the article (He et al. 2023) when microbial biomass C, N and P are not available, using B_CN=8.6 and B_CP=60
# here i want to change all NA values from B_CN and B_CP to 8.6 and 60, respectively

data_filter <- data_filter %>%
  mutate(B_CN=ifelse(is.na(B_CN), 8.6, B_CN),
         B_CP=ifelse(is.na(B_CP), 60, B_CP))

data_filter <- data_filter %>%
  mutate(S_CN=((B_CN/L_CN) * 1/EEA_CN),
         S_CP=((B_CP/L_CP) * 1/EEA_CP))

data_filter <- data_filter %>%
  mutate(CUE = 0.6 * ((S_CN*S_CP)/((0.5+S_CN) * (0.5+S_CP)))^0.5)

hist(data_filter$CUE, probability = T)
lines(density(data_filter$CUE), col="red", lwd=2)
shapiro.test(data_filter$CUE)

# data_top <- data_filter %>%
#   filter(Soil_layers=="Topsoil")
# 
# hist(data_top$CUE, probability = T)
# lines(density(data_top$CUE), col="red", lwd=2)
# shapiro.test(data_top$CUE)
# 
# data_sub <- data_filter %>%
#   filter(Soil_layers=="Subsoil")
# 
# hist(data_sub$CUE, probability = T)
# lines(density(data_sub$CUE), col="red", lwd=2)
# shapiro.test(data_sub$CUE)

##above, all data point has CUE

data_filter <- data_filter %>%
  mutate(Ecosystem_type = case_when(
    Vegetation_type == "Agri" ~ "Agri",
    grepl("Forest|forest", Vegetation_type) ~ "Forest",
    TRUE ~ "Grassland"
  ))

# topsoil and subsoil_____________________________________________________________________________________________________

data_filter %>%
  group_by(Soil_layers) %>%
  summarise(n=n())

data_filter <- data_filter %>%
  mutate(Soil_layers=case_when(
    Soil_layers %in% c("Bulk", "O", "Rhizosphere") ~ "Topsoil",
                       TRUE ~ Soil_layers)
  )

# papers_with_both <- data_filter %>%
#   group_by(`Paper ID`) %>%
#   summarise(
#     has_topsoil = any(Soil_layers=="Topsoil"),
#     has_subsoil = any(Soil_layers=="Subsoil")
#   ) %>%
#   filter(has_topsoil & has_subsoil) %>%
# select(`Paper ID`)
# 
# data_filter <- data_filter %>%
#   filter(`Paper ID` %in% papers_with_both$`Paper ID`)

#remove all columns which only have NA values
data_filter <- data_filter[, colSums(!is.na(data_filter))>0]

# xlsx::write.xlsx(data_filter, file = "data_filter_unpaired.xlsx",sheetName = "data_filter")

# xlsx::write.xlsx(data_filter, file = "data_filter_O.xlsx",sheetName = "data_filter")

##here i want to have global map

data_filter %>%
  group_by(Country, Continent) %>%
  summarise(n=n())
  
#here to explore data map

data_map <- data_filter %>%
  select(`Paper ID`, Latitude, Longitude, Country, Continent, Ecosystem_type, Soil_layers, CUE) %>%
  group_by(`Paper ID`, Latitude, Longitude, Country, Continent,Ecosystem_type, Soil_layers, CUE) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    CUE = as.numeric(CUE)
  )

world_map <- map_data('world')

map1<-ggplot()+
  geom_polygon(data = world_map, aes(x=long, y=lat, group = group), color=NA,
               fill='#d9d9d9')+
  theme_bw()+
  geom_point(data = data_map,size=3,
             aes(y = Latitude, x = Longitude, color= CUE),stroke=0.1,alpha=0.3)+
  scale_color_gradient(low="#fef0d9", high = "#b30000")+
  scale_x_continuous(breaks = seq(-180,180,30))+
  scale_y_continuous(breaks = seq(-90,90,30))+
  coord_cartesian(xlim = c(-155,175), ylim = c(-55,80))+
  labs(color=expression(CUE[ST]),
       x=paste('Longitude(\u00b0)'),
       y=paste('Latitude(\u00b0)'))+
  theme(legend.position = c(0.1,0.3),
        legend.background = element_blank(),
        legend.title = element_text(margin = margin(b=1), color="black",size=10, family="Arial"),
        legend.text = element_text(color="black",size=10, family="Arial"),
        panel.grid = element_blank(),
        axis.text = element_text(color="black",size=12, family="Arial"),
        axis.title = element_text(color="black",size=12, family="Arial")
        )
map1

## statistic mix model 

model <- lmerTest::lmer(CUE ~ Soil_layers + (1|`Paper ID`), data=data_map)
summary(model)
#F=(t)^2=(-11.23)^2=126.1  P<2e-16 (paired)
#F=-12.06^2=145.4436 P<2e-16 (unpaired)
data_map$Soil_layers=factor(data_map$Soil_layers, levels = c("Topsoil","Subsoil"))

sample_counts <- data_map %>%
  group_by(Soil_layers) %>%
  summarise(n=n())

library(ggforce)

Fig.1b <- data_map %>%
  ggplot(aes(x=Soil_layers, y=CUE)) +
  geom_sina(aes(color=Soil_layers),alpha=0.3)+
  geom_violin(aes(fill=Soil_layers), trim = T,  alpha=0.3)+
  geom_boxplot(width=0.2,outlier.shape = NA)+
  geom_text(data = sample_counts, 
            aes(x = Soil_layers, y = 0.63, 
                label = paste0("(", n,")")),
            inherit.aes = FALSE, size = 4) +
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  scale_y_continuous(limits = c(0,0.7))+
  theme_bw()+
  annotate("text",
           x = 1.5,
           y = 0.7,
           label = "italic(F) == 145.4 * \"; \"* italic(P) * \" < \" * 0.001",
           size = 4,
           parse=T) +
  labs(x="Soil Layer", y=expression(CUE[ST]))+
  theme(legend.position = "none",
        axis.text=element_text(color="black",size=12,  family="Arial"),   
        axis.title = element_text(color="black",size=12, family="Arial")
        )
  
Fig.1b

data_LAT <- data_map %>%
  mutate(Latitude = abs(Latitude)) %>%
  mutate(Soil_layers=factor(Soil_layers, levels = c("Topsoil", "Subsoil")))

# formula_quard <- y ~ poly(x, 2, raw = TRUE)
# formula_line <- y ~ x
library(ggpmisc)

Fig.1c <- data_LAT %>%
  ggplot(aes(x=Latitude, y=CUE, fill = Soil_layers,color = Soil_layers))+
  theme_bw()+
  geom_point(alpha=0.3, size=3,shape=21,stroke=0.3,color="white") +
  # geom_smooth(method = "lm", formula = y ~ poly(x,2),level=0.999,size=1.1,se=T)+
  stat_poly_line(formula = y ~ x, se=T)+
  # stat_poly_eq(formula = y ~ poly(x,2),
  #              aes(label=paste(..eq.label..,
  #                              ..adj.rr.label..,
  #                              ..p.value.label..,
  #                              sep = "~~~")), parse = T, vstep = 0.065,
  #              label.x.npc = 'left', label.y.npc = 'top', size=3) +
  # geom_smooth(method = "lm", formula = y ~ x, color = "#377eb8")+
  # facet_wrap(~Soil_layers)+
  scale_fill_manual(values = c('#2A83C7','#E88B33'))+
  scale_color_manual(values = c('#2A83C7','#E88B33'))+
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60, by=20))+
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,0.7, by=0.2))+
labs(x=paste('Longitude(\u00b0)'), y=expression(CUE[ST]))+
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
Fig.1c   

data_TOP <- data_LAT %>%
  filter(Soil_layers=="Topsoil")

data_SUB <- data_LAT %>%
  filter(Soil_layers=="Subsoil")

# lm_model <- lm(CUE ~ Latitude, data = data_TOP)
# summary(lm_model)

# lm_model <- lm(CUE ~ Latitude, data = data_SUB)
# summary(lm_model)

lm_quad <- lm(CUE ~ Latitude + I(Latitude^2), data = data_TOP)
summary(lm_quad)

lm_quad <- lm(CUE ~ Latitude + I(Latitude^2), data = data_SUB)
summary(lm_quad)


library(extrafont)
library(ggpubr)

below_row <- ggarrange(Fig.1b, Fig.1c,
                     ncol = 2, 
                     labels = c("", "",""), 
                     hjust = 0, vjust = 0, 
                     align = "h", 
                     font.label = list(size = 60, family = "ARL", face = "plain"))

# Then add Fig1.c below as a single plot
Fig1 <- ggarrange(map1, below_row, 
                  ncol = 1, 
                  labels = c(" ", " "), 
                  # heights = c(1, 1),  # You can adjust relative heights
                  font.label = list(size = 60, family = "ARL", face = "plain"))

# Print or save
print(Fig1)

ggsave("figures/Fig1_unpaired.tiff", width = 7.5, height = 7, dpi = 300)