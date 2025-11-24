library(terra)#load tif 
library(tidyterra) #for geom_spatraster and geom_spatvector
library(ggplot2)
library(rnaturalearth) #for global land
library(sf) #global land
library(rcolors)
library(ggpubr)

#global map projection of robinson (全球地图的robin投影)
crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

# crs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Get land polygons at coarse scale
global_land <- ne_countries(scale = 110, returnclass = "sf")

# Then transform projection to Robinson (same as your raster)
crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
global_land <- st_transform(global_land, crs_robin)
global_land <- st_make_valid(global_land)
# (Optionally) merge all polygons into one for simpler plotting
global_land <- st_union(global_land)
global_land_vect <- vect(global_land)


##this is creating solid and dashed line for all map

library(ggspatial)
# Define parallels and meridians
lat_breaks_dashed <- c(-30, 0, 30, 60)
lat_breaks_solid  <- c(-61.5, 88)   # top and bottom edges

lon_breaks_dashed <- c(-120, -60, 0, 60, 120)
lon_breaks_solid  <- c(-180, 180) # left and right edges

# # Labels
# lat_labels <- c("30°S", "0°", "30°N", "60°N")
# lon_labels <- c("120°W", "60°W", "0°", "60°E", "120°E")
# 
# lat_lab_df <- data.frame(lon = -180, lat = lat_breaks_dashed, lab = lat_labels)
# lon_lab_df <- data.frame(lon = lon_breaks_dashed, lat = -61.5, lab = lon_labels)  # bottom edge in degrees


##Topsoil projection__________________________________________________________________________________
rfn_top <- "CUE_Top_mean_prediction.tif"
r <- rast(rfn_top)
r <- terra::project(r, crs_robin)
summary(r)

p1 <- ggplot()+
  # horizontal dashed latitude lines
  annotation_spatial_hline(
    intercept = lat_breaks_dashed, crs = 4326,
    linetype = "dashed", linewidth = 0.3, colour = "grey40"
  ) +
  
  # vertical dashed longitude lines
  annotation_spatial_vline(
    intercept = lon_breaks_dashed, crs = 4326,
    linetype = "dashed", linewidth = 0.3, colour = "grey40"
  ) +
  
  # solid boundary lines
  annotation_spatial_vline(
    intercept = lon_breaks_solid, crs = 4326,
    linetype = "solid", linewidth = 0.4, colour = "black"
  ) +
  annotation_spatial_hline(
    intercept = lat_breaks_solid, crs = 4326,
    linetype = "solid", linewidth = 0.4, colour = "black"
  ) +
  
  # # latitude labels
  # geom_spatial_text(
  #   data = lat_lab_df,
  #   aes(x = lon, y = lat, label = lab),
  #   crs = 4326, size = 3, hjust=1
  # ) +
  # 
  # # longitude labels (bottom edge)
  # geom_spatial_text(
  #   data = lon_lab_df,
  #   aes(x = lon, y = lat, label = lab),
  #   crs = 4326, size = 3,vjust=1
  # ) +
  geom_spatraster(data=r, maxcell = 1e7,fill=NA,color=NA)+
  geom_spatvector(data = global_land_vect, fill=NA, size=0.2, color=NA)+
  coord_sf(crs = crs_robin)+
  scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
  scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))+
  labs(title = "")+
  theme_void()
  scale_fill_stepsn(
    colors = get_color("MPL_YlGn",n=6),
    na.value = NA,
    name="CUE",
    limits = c(0,0.6),
    breaks = seq(0,0.6,0.1),
    # labels = c("","0.1","","0.3","","0.5",""),
    guide = guide_colorbar(direction = "horizontal",
                           nrow=1,
                           title.position="top",
                           barwidth=20,
                           # barheight=3,
                           # ticks.colour="black",
                           # ticks.linewidth=0.3
    ))+
  # theme_minimal()+
  theme_void()+
  theme(legend.position = "none",
        # legend.text = element_text(size = 8,family = "Arial", color="black" ),
        # legend.title = element_text(size=10, family = "Arial", color="black"),
        plot.background = element_blank()
)
p1
ggsave("figures/Fig4_top.tiff", width = 6, height = 6, dpi = 300)

library(grid)
library(gridExtra)
library(cowplot)

g <- ggplotGrob(p1 + theme(legend.position = "bottom", legend.text = element_blank(), legend.title = element_blank()))
leg <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")][[1]]

grid.newpage()
grid.draw(leg) #exported via Rplot


#hre i want to have outside frame

p_frame <- ggplot()+
  # horizontal dashed latitude lines
  annotation_spatial_hline(
    intercept = lat_breaks_dashed, crs = 4326,
    linetype = "dashed", linewidth = 0.3, colour = "grey40"
  ) +
  
  # vertical dashed longitude lines
  annotation_spatial_vline(
    intercept = lon_breaks_dashed, crs = 4326,
    linetype = "dashed", linewidth = 0.3, colour = "grey40"
  ) +
  
  # solid boundary lines
  annotation_spatial_vline(
    intercept = lon_breaks_solid, crs = 4326,
    linetype = "solid", linewidth = 0.4, colour = "black"
  ) +
  annotation_spatial_hline(
    intercept = lat_breaks_solid, crs = 4326,
    linetype = "solid", linewidth = 0.4, colour = "black"
  ) +
  geom_spatraster(data=r, maxcell = 1e7,fill=NA,color=NA)+
  geom_spatvector(data = global_land_vect, fill=NA, size=0.2, color=NA)+
  coord_sf(crs = crs_robin)+
  scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
  scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))+
  labs(title = "")+
  theme_void()
p_frame

ggsave("figures/frame.tif", width = 6, height = 6, dpi = 300)

#plot uncertainty
u_top <- "CUE_Top_uncertainty.tif"
u <- rast(u_top)
u <- terra::project(u, crs_robin)

p1.1 <- ggplot()+
  geom_spatraster(data=u, maxcell = 1e7)+
  geom_spatvector(data = global_land_vect, fill=NA, size=0.2)+
  coord_sf(crs = crs_robin)+
  scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
  scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))+
  labs(title = "Uncertainty of CUE in Topsoil")+
  scale_fill_stepsn(
    colors = get_color("OrRd",n=10),
    na.value = NA,
    name="95% CI",
    limits = c(0,0.5),
    breaks = seq(0,0.5,0.1),
    guide = guide_colorbar(direction = "horizontal",
                           nrow=1,
                           title.position="top",
                           barwidth=20,
                           # ticks.colour="black",
                           # ticks.linewidth=0.3
    ))+
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10)
  )
p1.1

df_top <- read.csv("CUE_Top_mean_prediction.csv")

top_summary <- df_top %>%
  group_by(lat = round(y, 0.5)) %>%
  summarise(mean_cue = mean(mean_pred, na.rm=T),
            mean_upper = mean(upper_95, na.rm=T),
            mean_lower = mean(lower_95, na.rm=T))
head(top_summary)

pl1 <- ggplot(top_summary, aes(x=lat, y=mean_cue))+
  geom_ribbon(aes(ymin = mean_lower, ymax = mean_upper), fill="gray", alpha=0.2)+
  geom_smooth(se=F,color="black",linetype="dashed", size=0.5)+
  geom_line(color="darkgreen", size=0.5, alpha=0.6)+
  theme_bw()+
  scale_x_continuous(expand = c(0,0), breaks = c(-30,0,30,60), labels = c("30˚S","0","30˚N","60˚N"))+
  scale_y_continuous(expand = c(0,0),limits = c(0.2,0.5), labels = seq(0.2,0.5,0.1))+
  coord_flip()+
  labs(x="",y="CUE")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length.y = unit(0.1, "cm"),
        plot.background = element_blank())
pl1  
ggsave("figures/Fig4.1.tiff", width = 2, height = 3, dpi = 300)

##Subsoil projection______________________________________________________________________________
rfn_sub <- "CUE_Sub_mean_prediction.tif"
r <- rast(rfn_sub)
r <- terra::project(r, crs_robin)

p2 <- ggplot()+
  # horizontal dashed latitude lines
  annotation_spatial_hline(
    intercept = lat_breaks_dashed, crs = 4326,
    linetype = "dashed", linewidth = 0.3, colour = "grey40"
  ) +
  
  # vertical dashed longitude lines
  annotation_spatial_vline(
    intercept = lon_breaks_dashed, crs = 4326,
    linetype = "dashed", linewidth = 0.3, colour = "grey40"
  ) +
  
  # solid boundary lines
  annotation_spatial_vline(
    intercept = lon_breaks_solid, crs = 4326,
    linetype = "solid", linewidth = 0.4, colour = "black"
  ) +
  annotation_spatial_hline(
    intercept = lat_breaks_solid, crs = 4326,
    linetype = "solid", linewidth = 0.4, colour = "black"
  ) +
  geom_spatraster(data=r, maxcell = 1e7)+
  geom_spatvector(data = global_land_vect, fill=NA, size=0.2)+
  coord_sf(crs = crs_robin)+
  scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
  scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))+
  labs(title = "")+
  scale_fill_stepsn(
    colors = get_color("MPL_YlGn",,n=6),
    na.value = NA,
    name="CUE",
    limits = c(0,0.6),
    breaks = seq(0,0.6,0.1),
    # labels = c("","0.2","0.25","0.3","0.35","0.4","0.45","0.5",""),
    guide = guide_colorbar(direction = "horizontal",
                           nrow=1,
                           title.position="top",
                           barwidth=20,
                           # ticks.colour="black",
                           # ticks.linewidth=0.3
    ))+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_blank())

p2
ggsave("figures/Fig4_sub.tiff", width = 6, height = 6, dpi = 300)

#plot uncertainty
u_sub <- "CUE_Sub_uncertainty.tif"
u <- rast(u_sub)
u <- terra::project(u, crs_robin)

p2.2 <- ggplot()+
  geom_spatraster(data=u, maxcell = 1e7)+
  geom_spatvector(data = global_land_vect, fill=NA, size=0.2)+
  coord_sf(crs = crs_robin)+
  scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
  scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))+
  labs(title = "Uncertainty of CUE in Subsoil")+
  scale_fill_stepsn(
    colors = get_color("OrRd",n=10),
    na.value = NA,
    name="95% CI",
    limits = c(0,0.5),
    breaks = seq(0,0.5,0.1),
    guide = guide_colorbar(direction = "horizontal",
                           nrow=1,
                           title.position="top",
                           barwidth=20,
                           # ticks.colour="black",
                           # ticks.linewidth=0.3
    ))+
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10))

p2.2

df_sub <- read.csv("CUE_Sub_mean_prediction.csv")

sub_summary <- df_sub %>%
  group_by(lat = round(y, 0.5)) %>%
  summarise(mean_cue = mean(mean_pred, na.rm=T),
            mean_upper = mean(upper_95, na.rm=T),
            mean_lower = mean(lower_95, na.rm=T))
head(sub_summary)

pl2 <- ggplot(sub_summary, aes(x=lat, y=mean_cue))+
  geom_ribbon(aes(ymin = mean_lower, ymax = mean_upper), fill="gray", alpha=0.2)+
  geom_smooth(se=F,color="black",linetype="dashed", size=0.5)+
  geom_line(color="forestgreen", size=0.5)+
  theme_bw()+
  scale_x_continuous(expand = c(0,0), breaks = c(-30,0,30,60), labels = c("30˚S","0","30˚N","60˚N"))+
  scale_y_continuous(expand = c(0,0),limits = c(0.2,0.5), labels = seq(0.2,0.5,0.1))+
  coord_flip()+
  labs(x="",y="CUE")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length.y = unit(0.1, "cm"),
        plot.background = element_blank())
pl2 
ggsave("figures/Fig4.2.tiff", width = 2, height = 3, dpi = 300)

# p_top <- ggarrange(p1, p2, 
#                    common.legend = T, legend = "bottom",
#                      ncol = 1, 
#                      labels = c("",""), 
#                      # heights = c(1, 0.5),  # You can adjust relative heights
#                      font.label = list(size = 14, family = "ARL", face = "plain"))
# p_top
# ggsave("figures/Fig4.tiff", width = 6, height = 6, dpi = 300)

pp <- ggarrange(p1.1, p2.2, common.legend = T, legend = "bottom",
               ncol = 1, 
               labels = c("a)","b)"), 
               # heights = c(1, 0.5),  # You can adjust relative heights
               font.label = list(size = 12, family = "ARL", face = "plain"))
pp
ggsave("figures/FigS4.tiff", width = 6, height = 6, dpi = 300)

# Fig (hotspot for topsoil and subsoil) ---------------------------------------------------
















              