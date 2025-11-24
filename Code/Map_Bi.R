library(sp)
library(tidyverse)
library(raster)
library(classInt)
library(cowplot)
colmat <- function(nquantiles = 3, upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft= "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = TRUE) {
  require(classInt)
  my.data <- seq(0, 1, .01)
  my.class <- classInt::classIntervals(my.data,
                                       n = nquantiles,
                                       style = "quantile"
  )
  my.pal.1 <- findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- findColours(my.class, my.col)
  }
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>%
    mutate("Y" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
    gather(data = ., key = X, value = HEXCode, na.rm = FALSE, -Y) %>%
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    dplyr::select(-c(4)) %>%
    mutate("X" = rep(seq(from = 1, to = nquantiles, by = 1), each = nquantiles),
           "Y" = rep(seq(from = 1, to = nquantiles, by = 1), times = nquantiles)) %>%
    mutate("UID" = row_number())
  if (plotLeg) {
    p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
      geom_raster() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 12, colour = "black",hjust = 0.5, 
                                      vjust = 1),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
    print(p)
    assign(
      x = "BivLegend",
      value = p,
      pos = .GlobalEnv
    )
  }
  if (saveLeg) {
    ggsave(filename = "bivLegend.pdf", plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in",
           dpi = 300)
  }
  seqs <- seq(0, 100, (100 / nquantiles))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}

bivariate.map <- function(rasterx, rastery, colormatrix = col.matrix,
                          nquantiles = 3, export.colour.matrix = TRUE,
                          outname = paste0("colMatrix_rasValues", names(rasterx))) {
  quanmean <- getValues(rasterx)
  temp <- data.frame(quanmean, quantile = rep(NA, length(quanmean)))
  brks <- with(temp, quantile(temp,
                              na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles))
  ))
  r1 <- within(temp, quantile <- cut(quanmean,
                                     breaks = brks,
                                     labels = 2:length(brks),
                                     include.lowest = TRUE
  ))
  quantr <- data.frame(r1[, 2])
  quanvar <- getValues(rastery)
  temp <- data.frame(quanvar, quantile = rep(NA, length(quanvar)))
  brks <- with(temp, quantile(temp,
                              na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles))
  ))
  r2 <- within(temp, quantile <- cut(quanvar,
                                     breaks = brks,
                                     labels = 2:length(brks),
                                     include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colormatrix
  cn <- unique(colormatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1, col.matrix2[i] <- which(
             col.matrix2[i] == cn
           )[1]
    )
  }
  if (export.colour.matrix) {
    exportCols <- as.data.frame(cbind(
      as.vector(col.matrix2), as.vector(colormatrix),
      t(col2rgb(as.vector(colormatrix)))
    ))
    colnames(exportCols)[1:2] <- c("rasValue", "HEX")
    assign(
      x = outname,
      value = exportCols,
      pos = .GlobalEnv
    )
  }
  cols <- numeric(length(quantr[, 1]))
  for (i in 1:length(quantr[, 1])) {
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    cols[i] <- as.numeric(col.matrix2[b, a])
  }
  r <- rasterx
  r[1:length(r)] <- cols
  return(r)
}
nBreaks <- 6

col.matrix <- colmat(nquantiles = nBreaks, xlab = "", ylab = "",
                     upperleft = "#FFD166",    # soft yellow
                     upperright = "#EF476F",   # warm pink/red
                     bottomleft = "#06D6A0",   # mint green
                     bottomright = "#118AB2",  # soft blue
                     # upperleft = "#FFDDD2", upperright = "#FFB4A2",
                     # bottomleft = "#CDEAC0", bottomright = "#A2D2FF",
                     saveLeg = F, plotLeg = T) #using export to export then PPT combine

Top<-raster("CUE_Top_mean_prediction.tif")
Sub<-raster("CUE_Sub_mean_prediction.tif")
rh_resampled <-resample(x=Top,y=Sub)

bivmap <- bivariate.map(rasterx = rh_resampled, Sub,
                        export.colour.matrix = TRUE, outname = "bivMapCols",
                        colormatrix = col.matrix, nquantiles = nBreaks)

bivMapDF <- as.data.frame(bivmap, xy = TRUE) %>%
 tibble::as_tibble() %>%
  dplyr::rename("BivValue" = 3) %>%
  gather(key = Variable, value = bivVal, na.rm = FALSE, BivValue)

map <- ggplot(bivMapDF, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_fill_gradientn(colours = col.matrix, na.value = "transparent") + 
  theme_bw() +
  coord_quickmap(expand = FALSE) +
  scale_x_continuous(breaks = seq(-120,120, 60), labels = c("120˚W","60˚W","0˚","60˚E","120˚E"))+
  scale_y_continuous(breaks = c(-30,0,30,60), labels = c("30˚S","0˚","30˚N","60˚N"))+
  labs(x=paste('Longitude (\u00b0)'),
       y=paste('Latitude (\u00b0)'))+
  theme(legend.position = "none",
        plot.background = element_blank(),
        text = element_blank())
map
ggsave("figures/Fig5_bi.tiff", width = 8, height = 6, dpi = 300)
