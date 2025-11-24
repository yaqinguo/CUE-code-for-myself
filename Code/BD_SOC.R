
data_BD <- read.csv("Env_factors_cart.csv", header = T)

data_SOC <- data4 %>%
  select(Mean_SOC)

data_all <- cbind(data_BD, data_SOC)

##here BD with SOC=========================================================================================
top_soc <- data_all %>%
  filter(depth_class=="0-30")

BD_SOC <- lm(data_all$Bulk.Density ~ data_all$BelowgroundBiomass)
summary(BD_SOC)

ggplot(data=data_all,aes(x=BelowgroundBiomass, y=Bulk.Density))+
  theme_bw()+
  geom_point(size=2,shape=19)+
  stat_smooth(method = "lm")
  coord_cartesian(xlim=c(-6,26), expand = T)+
  annotate("text",
           x = -2,
           y = 0.6,
           label = "R^2 == 0.17 * \"; \"* italic(P) * \" < \" * 0.05",
           size = 4,
           parse=T) 


