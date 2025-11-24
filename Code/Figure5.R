library(tidyverse)
library(raster)
library(sf)

df_top <- read.csv("CUE_Top_mean_prediction.csv")

# load Köppen–Geiger raster (download needed, e.g. from Beck et al. 2018)
kg_raster <- raster("koppen_geiger_0p1.tif")

# turn your data into sf
top_data_sf <- st_as_sf(df_top, coords = c("x","y"), crs = 4326)

# extract climate zone for each point
df_top$climate <- raster::extract(kg_raster, top_data_sf)

legend_raw <- read.delim("legend.txt", header = F, stringsAsFactors = F)

legend_df <- legend_raw %>%
  slice(-1, -2) %>%
  filter(str_detect(V1, "^\\s*\\d+:")) %>%
  mutate(
    code  = as.integer(str_extract(V1, "^\\s*\\d+")),
    class = str_extract(V1, "^\\s*\\d+:\\s*(\\w{1,3})") %>% str_replace("^\\s*\\d+:\\s*", ""),
    rgb   = str_extract(V1, "\\[[0-9 ]+\\]"),
    desc  = V1 %>%
      # remove leading code+class
      str_remove("^\\s*\\d+:\\s*\\w{1,3}\\s*") %>%
      # remove trailing RGB
      str_remove("\\s*\\[[0-9 ]+\\]\\s*$") %>%
      str_trim()
  ) %>%
  dplyr::select(code, class, desc)

legend_df

df_top <- df_top %>%
  left_join(legend_df, by=c("climate"="code"))

top_df <- df_top %>%
  mutate(
    climate_group = str_extract(desc, "^[A-Za-z]+")
  ) %>%
  filter(!is.na(climate_group) & climate_group != "") %>%
  mutate(climate_group = factor(climate_group, levels = c("Tropical","Temperate","Arid","Cold","Polar")))

top_df %>%
  group_by(climate_group) %>%
  summarise(CUE=mean(mean_pred), lower_95 = mean(lower_95), upper_95 = mean(upper_95))

ggplot(data = top_df, aes(x=factor(climate_group),y=mean_pred))+
  # geom_sina(aes(color=factor(climate_group)),alpha=0.1)+
  geom_boxplot(width=0.2,outliers = F,aes(color=factor(climate_group)))+
  theme_bw()+
  labs(y="CUE", x="Climate zone")+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme(legend.position = "none")

df_sub <- read.csv("CUE_Sub_mean_prediction.csv")

sub_data_sf <- st_as_sf(df_sub, coords = c("x","y"), crs = 4326)

# extract climate zone for each point
df_sub$climate <- raster::extract(kg_raster, sub_data_sf)

df_sub <- df_sub %>%
  left_join(legend_df, by=c("climate"="code"))

sub_df <- df_sub %>%
  mutate(
    climate_group = str_extract(desc, "^[A-Za-z]+")
  ) %>%
  filter(!is.na(climate_group) & climate_group != "") %>%
  mutate(climate_group = factor(climate_group, levels = c("Tropical","Temperate","Arid","Cold","Polar")))

sub_df %>%
  group_by(climate_group) %>%
  summarise(CUE=mean(mean_pred), lower_95 = mean(lower_95), upper_95 = mean(upper_95))

ggplot(data = sub_df, aes(x=factor(climate_group),y=mean_pred))+
  # geom_sina(aes(color=factor(climate_group)),alpha=0.1)+
  geom_boxplot(width=0.2,outliers = F,aes(color=factor(climate_group)))+
  theme_bw()+
  labs(y="CUE", x="Climate zone")+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme(legend.position = "none")


combined_df <- bind_rows(
  top_df %>% mutate(dataset = "Top"),
  sub_df %>% mutate(dataset = "Sub")
)

ggplot(combined_df, aes(x=factor(climate_group), y=mean_pred,fill=dataset))+
  geom_boxplot(width=0.4, outlier.shape = NA, position = position_dodge(width = 0.6))+
  theme_bw()+
  labs(x="Climate zone",y="CUE")+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme(legend.position = "bottom")


combined_data <- combined_df %>%
  mutate(Location = case_when(
    y <=30 & y >=-30 ~ "30S-30N",
    y > 30 ~ "> 30N",
    y < -30 ~ "> 30S"
  ))


ggplot(combined_data, aes(x=factor(Location), y=mean_pred, fill=dataset))+
  geom_boxplot(width=0.4, outlier.shape = NA, position = position_dodge(width = 0.6))+
  theme_bw()+
  labs(x="Climate zone",y="CUE")+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme(legend.position = "bottom")

combined_with_overview <- combined_data %>%
  bind_rows(
    combined_data %>%
      group_by(dataset) %>%
      mutate(Location = "Overview")   # overwrite Location with "Overview"
  ) %>%
  mutate(dataset=factor(dataset, levels = c("Top","Sub"), labels = c("Topsoil","Subsoil")))
combined_with_overview$Location <- factor(
  combined_with_overview$Location,
  levels = c("Overview", "30S-30N", "> 30N", "> 30S")
)

ci_summary <- combined_with_overview %>%
  group_by(Location, dataset) %>%
  summarise(
    mean_pred = mean(mean_pred, na.rm = TRUE),
    lower_95  = mean(lower_95, na.rm = TRUE),
    upper_95  = mean(upper_95, na.rm = TRUE),
    .groups = "drop"
  )

p_global <- ggplot(combined_with_overview, aes(
  x = Location,y = mean_pred,fill = dataset)) +
  geom_errorbar(
    data = ci_summary,
    aes(x=Location,
        ymin = lower_95, ymax = upper_95),
    width = 0.2, position = position_dodge(width = 0.6)) +
  # Boxplots
  geom_boxplot(
    width = 0.4, outlier.shape = NA,
    position = position_dodge(width = 0.6),
    coef=0, # whiskers go to min/max but visually ignored
    size=0.5 # thinner outline so CI stands out
  ) +
  # Mean points + CI bars (from summarised data)
  # geom_point(
  #   data = ci_summary,shape=21, show.legend = F,
  #   aes(x = factor(Location, levels = c("Overview", sort(unique(combined_data$Location)))),
  #       y = mean_pred, fill = dataset),position = position_dodge(width = 0.6),size = 1) +
  geom_vline(xintercept = 1.5, linetype="dashed")+
  theme_bw() +
  labs(x = "", y = "CUE") +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6)) +
  scale_x_discrete(breaks=c("Overview", "30S-30N", "> 30N", "> 30S"),
    labels=c(
    "Overview" = "Global",
    "30S-30N" = "30˚S-30˚N",
    "> 30N" = "> 30˚N",
    "> 30S" = "> 30˚S"  ))+
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme(legend.position = c(0.1,0.9),
        legend.title = element_blank(), 
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.8,"line"),
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank())
p_global
ggsave("figures/Figure5.2.tif", height = 2.5, width = 3, dpi = 300)
##=================================biome=============================================================================

df_top <- read.csv("CUE_Top_mean_prediction.csv")

# load Köppen–Geiger raster (download needed, e.g. from Beck et al. 2018)
bio_raster <- raster("landcover_0.25deg.tif")

# turn your data into sf
top_data_sf <- st_as_sf(df_top, coords = c("x","y"), crs = 4326)

# extract climate zone for each point
df_top$biome <- raster::extract(bio_raster, top_data_sf)
df_top <- df_top %>%
  mutate(biome=round(biome,0)) %>%
  mutate(biome=factor(biome))

legend_raw <- read.delim("LandCoverLabel.txt", header = T, stringsAsFactors = F)

df_top <- df_top %>%
  left_join(legend_raw, by=c("biome"="Value"))

df_top %>%
  group_by(Label) %>%
  summarise(CUE=mean(mean_pred), lower_95 = mean(lower_95), upper_95 = mean(upper_95))

df_sub <- read.csv("CUE_Sub_mean_prediction.csv")

sub_data_sf <- st_as_sf(df_sub, coords = c("x","y"), crs = 4326)

# extract climate zone for each point
df_sub$biome <- raster::extract(bio_raster, sub_data_sf)
df_sub <- df_sub %>%
  mutate(biome=round(biome,0)) %>%
  mutate(biome=factor(biome))

df_sub <- df_sub %>%
  left_join(legend_raw, by=c("biome"="Value"))

combined_df <- bind_rows(
  df_top %>% mutate(dataset = "Topsoil"),
  df_sub %>% mutate(dataset = "Subsoil")
)
combined_df$dataset <- factor(combined_df$dataset, levels = c("Topsoil","Subsoil"))

combined_df <- combined_df %>%
  filter(Label == "Evergreen Needleleaf forest" | Label == "Evergreen Broadleaf forest" |
           Label == "Deciduous Needleleaf forest" | Label == "Deciduous Broadleaf forest"|
           Label == "Mixed forest" | Label == "Woody savannas" | Label == "Savannas"| Label=="Grasslands"|
           Label == "Closed shrublands" | Label == "Open shrublands")

combined_df <- combined_df %>%
  mutate(Type = case_when(
    Label == "Evergreen Needleleaf forest" | Label == "Evergreen Broadleaf forest" |
      Label == "Deciduous Needleleaf forest" | Label == "Deciduous Broadleaf forest"|
      Label == "Mixed forest"  ~ "Forest",
    Label == "Woody savannas" | Label == "Savannas" ~ "Savannas",
    Label == "Closed shrublands" | Label == "Open shrublands" ~ "shrublands",
    Label=="Grasslands" ~ "Grasslands"
  ))

ci_summary <- combined_df %>%
  group_by(Type, dataset) %>%
  summarise(
    mean_pred = mean(mean_pred, na.rm = TRUE),
    lower_95  = mean(lower_95, na.rm = TRUE),
    upper_95  = mean(upper_95, na.rm = TRUE),
    .groups = "drop"
  )



p_biome <- ggplot(combined_df, aes(
  x = Type,y = mean_pred,fill = dataset)) +
  geom_errorbar(
    data = ci_summary,
    aes(x=Type,
        ymin = lower_95, ymax = upper_95),
    width = 0.2, position = position_dodge(width = 0.6)) +
  # Boxplots
  geom_boxplot(
    width = 0.4, outlier.shape = NA,
    position = position_dodge(width = 0.6),
    coef=0, # whiskers go to min/max but visually ignored
    size=0.5 # thinner outline so CI stands out
  ) +
  # Mean points + CI bars (from summarised data)
  # geom_point(show.legend = F,
  #   data = ci_summary,shape=21,
  #   aes(x = factor(Type),
  #       y = mean_pred, fill = dataset),position = position_dodge(width = 0.6),size = 1) +
  theme_bw() +
  labs(x = "", y = "CUE") +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6)) +
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme(legend.position = c(0.1,0.9),
        legend.title = element_blank(), 
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(0.8,"line"),
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank())
p_biome
ggsave("figures/Figure5.3.tif", height = 2.5, width = 3, dpi = 300)


combined_df$Label <- factor(combined_df$Label, levels = c(
  "Evergreen Needleleaf forest","Evergreen Broadleaf forest","Deciduous Needleleaf forest","Deciduous Broadleaf forest","Mixed forest",
  "Grasslands","Woody savannas", "Savannas","Closed shrublands","Open shrublands"))

ci_summary2 <- combined_df %>%
  group_by(Label, dataset) %>%
  summarise(
    mean_pred = mean(mean_pred, na.rm = TRUE),
    lower_95  = mean(lower_95, na.rm = TRUE),
    upper_95  = mean(upper_95, na.rm = TRUE),
    .groups = "drop"
  )


p_biome2 <- ggplot(combined_df, aes(
  x = Label,y = mean_pred,fill = dataset)) +
  geom_errorbar(
    data = ci_summary2,
    aes(x=Label,
        ymin = lower_95, ymax = upper_95),
    width = 0.2, position = position_dodge(width = 0.6)) +
  # Boxplots
  geom_boxplot(
    width = 0.4, outlier.shape = NA,
    position = position_dodge(width = 0.6),
    coef=0, # whiskers go to min/max but visually ignored
    size=0.5 # thinner outline so CI stands out
  ) +
  # Mean points + CI bars (from summarised data)
  geom_point(
    data = ci_summary2,shape=21,
    aes(x = Label,
        y = mean_pred, fill = dataset),position = position_dodge(width = 0.6),size = 2) +
  theme_bw() +
  labs(x = "", y = "CUE") +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6)) +
  scale_x_discrete(labels=c(
    "Evergreen\nNeedleleaf\nforest",
    "Evergreen\nBroadleaf\nforest",
    "Deciduous\nNeedleleaf\nforest",
    "Deciduous\nBroadleaf\nforest",
    "Mixed\nforest", "Grasslands",
    "Woody\nsavannas", "Savannas",
    "Closed\nshrublands","Open\nshrublands"
  ))+
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme(legend.position = c(0.1,0.9),
        legend.title = element_blank(), 
        legend.background = element_blank(),
        plot.background = element_blank(),
        legend.text = element_text(size=8, family="Arial",color="black"),
        axis.text = element_text(size=8, family="Arial",color="black"),
        axis.title.y = element_text(size=10, family="Arial",color="black"))
p_biome2
ggsave("figures/FigureS5.tif", height = 5, width = 7, dpi = 300)



