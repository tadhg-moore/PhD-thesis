setwd("G:\\ISIMIP")

library(plyr)
library(glmtools)
library(tidyr)
library(lubridate)
source('Scripts/glm_functions-work.R')
source('Scripts/gotm_functions.R')

fils <- list.files('ISIMIP_output/obs_temp/', full.names = T)

got <- read.csv('MetaData/GOTM_cal_results.csv')
glm <- read.csv('MetaData/GLM_calib_RMSE.csv')
got <- got[,c('ISIMIP_name', 'RMSE', 'NSE')]
glm <- glm[,c('ISIMIP_name', 'RMSE', 'NSE')]
colnames(got)[-1] <- paste0('gotm_',colnames(got)[-1])
all <- merge(glm, got, by = 1)
idx <- which(all$RMSE <2 & all$NSE > 0.5 & all$gotm_RMSE <2 & all$gotm_NSE >0.5)
laks <- all[idx,1]
laks <- laks[laks != 'Kuivajarvi']

df <- ldply(fils, read.csv)

df$DateTime <- as.POSIXct(as.character(df$Date))
df$year <- year(df$DateTime)
df$month <- month(df$DateTime)

idx <- which(df$Lake %in% laks)
df <- df[idx,]


df2 <- ddply(df[df$year > 1979,], c('Lake', 'year'), function(x){
  data.frame(surftemp = mean(x$surftemp, na.rm = T),
             bottemp = mean(x$bottemp, na.rm = T))
}, .progress = 'text')

region <- read.csv('ISIMIP_output/Metadata/isimip_calib_metadata_regions_v3.csv')
morph <- read.csv('ISIMIP_output/Metadata/lakes_size_depth_class.csv')
df2 <- merge(df2, region[,c(3,20)], by = 1)
df3 <- merge(df[df$year > 2000,], region[,c(3,20)], by.x = 4, by.y = 1)
df4 <- merge(df3, morph, by = 1)
df4$depth_class <- factor(df4$depth_class, levels = c("shallow","medium", "deep", "very_deep"))
levels(df4$depth_class) <- c('Shallow', 'Medium', 'Deep', 'Very Deep')


df6 <- ddply(df, c('Lake', 'month'), function(x){
  data.frame(surftemp = mean(x$surftemp, na.rm = T),
             bottemp = mean(x$bottemp, na.rm = T))
})

df7 <- ddply(df6, c('Lake'), function(x){
  # print(x$Lake[1])
  month <- data.frame(month = x$month[1]:x$month[nrow(x)])
  df <- merge(x, month, by = 'month', all.y = T)
  # if(nrow(df) ){
  #    df <- df[1:365,]
  # }
 
  df[c(3,4)] <- na.approx(df[,c(3,4)])
  df$Lake <- x$Lake[1]
  return(df[,c(1,3,4)])
  
})

meta <- read.csv('MetaData/lakes_size_depth_class.csv', stringsAsFactors = T)
morph <- read.csv('MetaData/lakes_size_depth_class.csv')
region <- read.csv("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\WateXr\\ISIMIP_local/MetaData/isimip_all_metadata_regions_v3.csv")
region <- region[,c(3,ncol(region))]
labs <- read.csv('ISIMIP_output/Metadata/lake_labels.csv')
df7 <- merge(df7, labs, by.x = 'Lake', by.y = 'lake')
df7 <- merge(df7, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df7 <- merge(df7, region, by.x = 'Lake', by.y = 'ISIMIP_name')
df7$Region <- factor(df7$Region, levels = c("NEU","CEU", "MED",  "ENA", "CNA",  "WNA", "NAU",  "SAU" ))

df7$label <- factor(df7$label)
df7$label <- factor(df7$label, levels = unique(df7$label[rev(order(df7$Region))]), ordered = T)
# df7$label <- factor(df7$label, levels = unique(df7$label[rev(order(df7$Region))]), ordered = T)
# df7$variable <- factor(df7$variable)



df7$diff <- df7$surftemp - df7$bottemp
cols <- RColorBrewer::brewer.pal(11, 'Spectral')
idx <- which(df7$label != 'FI_Kui')
mlt <- melt(df7[idx,], id.vars = c('label', 'month'), measure.vars = c('surftemp', 'bottemp'))

# Bin colour
mlt <- mlt[!is.na(mlt$value),]
summary(mlt)
mlt$val_disc <- cut(mlt$value, seq(0,32,4) ,include.lowest = T)
levels(mlt$val_disc) <- seq(2,30,4)
mlt$val_disc <- factor(mlt$val_disc, levels = rev(levels(mlt$val_disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(0, 3600) + 0.5, y = rep(2:n1, each = 2) - 0.5)
sep <- data.frame(y = c(5,6,9,19,23,24,34)-0.5)
xmx <- 13.4
xcex <- 4.5

cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')
col_fn <- colorRampPalette(cols)

incol <- col_fn(length(levels(mlt$val_disc)))


p2 <- ggplot(mlt, aes(month, label))+
  # geom_tile(aes(fill = mean))+
  geom_tile(aes(fill = (val_disc)))+
  scale_fill_manual(values = (incol), name = '\u00B0C', drop = F, na.value = 'grey')+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-4,4), breaks = MakeBreaks(1))+
  facet_wrap(~variable, ncol = 2)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+ 
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 18, vjust = 1),
        strip.text = element_text(size = 16),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 39.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 23, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 20.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(1,12), clip = 'off')
# ggplot2::labs(tag = 'TEAST')+
# ggplot2::labs(tag = 'TEAST2')
# p1

ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/wtemp_obs_month_stripe_Region_v1.png', p2, dpi = 300,width = 270,height = 297, units = 'mm')


library(metR)
my.cols <- (RColorBrewer::brewer.pal(9, 'PiYG'))
binwidth = 0.5

df7$sdens <- water.density(df7$surftemp)
df7$bdens <- water.density(df7$bottemp)
df7$dens.diff <- df7$sdens - df7$bdens

idx <- which(df7$label != 'FI_Kui')
mlt <- melt(df7[idx,], id.vars = c('label', 'month'), measure.vars = c('dens.diff'))

# Bin colour
mlt$value[mlt$value > 0] <- 0
mlt <- na.exclude(mlt)

summary(mlt)
mlt$val_disc <- cut(mlt$value, seq(-4,4,1) ,include.lowest = T)
levels(mlt$val_disc) <- seq(-3.5,3.5,1)
mlt$val_disc <- factor(mlt$val_disc, levels = rev(levels(mlt$val_disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(0, 3600) + 0.5, y = rep(2:n1, each = 2) - 0.5)
sep <- data.frame(y = c(5,6,9,19,23,24,34)-0.5)
xmx <- 13
xcex <- 4.3

cols <- RColorBrewer::brewer.pal(11, 'PiYG')
col_fn <- colorRampPalette(cols)

incol <- col_fn(length(levels(mlt$val_disc)))

p2 <- ggplot(mlt, aes(month, label))+
  # geom_tile(aes(fill = mean))+
  geom_tile(aes(fill = (val_disc)), na.rm = T)+
  scale_fill_manual(values = (incol), name = expression(~ Delta * rho~ "(kg"~m^-3*")"), drop = F)+ #, na.value = 'grey')+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-4,4), breaks = MakeBreaks(1))+
  # facet_wrap(~variable, ncol = 2)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+ 
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        legend.key.height = unit(3, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 20, vjust = 1),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 18),
        plot.margin = margin(0,0.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,1, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 39.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 23, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 20.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(1,12), clip = 'off')

ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/wtemp_obs_month_stripe_density_diff_Region_v1.png', p2, dpi = 300,width = 270,height = 297, units = 'mm')


ggsave('analysis-isimip/plots/wtemp_obs_month_stripe_density_diff_Region_v3.png', p2, dpi = 300,width = 240,height = 220, units = 'mm')

p2 <- ggplot(mlt, aes(month, label))+
  geom_tile(aes(fill = value))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Temperature\n(degC)')+
  # scale_fill_gradient2(low = muted('blue'), mid = muted('yellow'), high = muted('red'))+
  scale_fill_gradientn(colours=my.cols, na.value = 'grey',
                       name= bquote("Density\nDifference (kg"~m^-3*')'),
                       breaks = MakeBreaks(binwidth), guide = guide_colorstrip(), limits = c(-4, 4))+
  # facet_wrap(~variable, nrow = 1)+
  xlab('Day of year')+
  ylab('')+
  # scale_y_discrete()+
  # scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 12))
p2
ggsave('analysis-isimip/plots/wtemp_obs_month_stripe_density_diff_Region.png', p2, dpi = 300,width = 240,height = 220, units = 'mm')
