setwd("G:\\ISIMIP")
# library(lubridate)
library(ggplot2)
library(ggpubr)
library(plyr)
library(metR)

out_vars <- read.csv('MetaData/ISIMIP_lake_output_vars_v2.csv', stringsAsFactors = F)
meta <- read.csv('MetaData/isimip_metadata_v1.csv', stringsAsFactors = F)
out_vars$Variable_name

model = c('GFDL-ESM2M',
          'HadGEM2-ES',
          'IPSL-CM5A-LR',
          'MIROC5'
)
period = c(#'piControl', #Pre-industrial climate and 286ppm CO2 concentration. 
  'historical', #Historical climate and CO2 concentration. 
  'rcp26', #Future climate and CO2 concentration from RCP2.6 	- limit warming to 1.50C
  'rcp60', #Future climate and CO2 concentration from RCP6.0 	- warming up to 30C
  'rcp85' #Future climate and CO2 concentration from RCP8.5
)
tim_periods <- c(#'pre-industrial',
  'historical',
  'future'#,
  #'future_extended'
)

glm <- read.csv('MetaData/calib_data/GLM_calib_RMSE.csv', stringsAsFactors = F)
gotm <- read.csv('MetaData/calib_data/GOTM_calib_results.csv', stringsAsFactors = F)
colnames(gotm)[-1] <- paste0('got_', colnames(gotm)[-1])
stats <- merge(glm, gotm)

cal_lakes <- stats$ISIMIP_name[which(stats$RMSE <2 & stats$got_RMSE <2)]


lakes <- meta$ISIMIP_name

lakes <- lakes[which(lakes %in% cal_lakes)] # Only select calib lakes

met_dir <- 'ISIMIP_output/GCM/'
fils <- list.files(met_dir)

met_df <- NULL
vars <- c('tas', 'sfcWind', 'rsds')
stat <- c('mean')

for(k in lakes){ #Just extract the first lake for now
  
  message('Beginning ', k, ' ', Sys.time())
  
  meta_ind <- which(meta$ISIMIP_name == k)
  
  
  
  # fil_pat = file.path('GCM_atmosphere', k)
  
  # List all netcdf files
  # fil = list.files(fil_pat)[grep('sfcWind_month_year.csv', list.files(fil_pat))]
  fil <- fils[grep(k, fils)]
  
  #If no netcdf skip
  if(length(fil) == 0){
    message('No met file for ', k, ' ', met_dir)
    next
  }
  
  met <- read.csv(file.path(met_dir,fil))
  met <- met[which(met$variable %in% vars),]
  met <- met[which(met$stat %in% stat),]
  
  if(is.null(met_df)){
    met_df <- met
  }else{
    met_df <- rbind.data.frame(met_df, met)
  }
  
}


met <- met_df
# met <- met[which(met$variable %in% vars),]
dim(met)
colnames(met) <- c('Year', 'Month', 'variable', 'stat', 'value', 'anom', 'GCM', 'Scenario', 'Lake')
levels(met$Scenario) <- c('Historical', 'RCP2.6', 'RCP6.0', 'RCP8.5')

library(plyr)

meta <- read.csv('MetaData/isimip_metadata_v2.csv', stringsAsFactors = F)
# met <- merge(met, meta, by.x = 'Lake', by.y = 'ISIMIP_name')


# linear trend
# pdf("ISIMIP_output/GCM/trend/test.pdf")


# Average across GCM - Monthly ----
idx <- which(met$Year >= 2069)
df2 <- ddply(met[idx,], c('Month', 'Lake', 'Scenario', 'variable'), .fun = function(x){
  # print(head(x))
  mn <- mean(x$anom)
  sd <- sd(x$anom)
  min = min(x$anom)
  mx <- max(x$anom)
  df <- data.frame(mean = mn, sd = sd, min = min, max = mx)
  return(df)
}, .progress = 'text')
# df2b <- merge(df2, meta, by.x = 'Lake', by.y = 'ISIMIP_name')

morph <- read.csv('MetaData/lakes_size_depth_class.csv')
region <- read.csv('ISIMIP_output/Metadata/isimip_calib_metadata_regions_v3.csv')
region <- region[,c("ISIMIP_name", "Region")]
labs <- read.csv('MetaData/lake_labels.csv')
df2 <- merge(df2, labs, by.x = 'Lake', by.y = 'lake')
df2 <- merge(df2, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df2 <- merge(df2, region, by.x = 'Lake', by.y = 'ISIMIP_name')
df2$Region <- factor(df2$Region, levels = c("NEU","CEU", "MED",  "ENA", "CNA",  "WNA", "NAU",  "SAU" ))
df2$label <- factor(df2$label)
df2$label <- factor(df2$label, levels = unique(df2$label[rev(order(df2$Region))]), ordered = T)
df2$variable <- factor(df2$variable)
# df2$variable <- factor(df2$variable, levels = levels(df2$variable)[c(2,1,3)])
levels(df2$Region)

# df2 <- merge(df2, trend2, by = c('Lake','Scenario', 'variable'))
# df2$label2 <- paste0(df2$label, df2$sig)


d.cols <- RColorBrewer::brewer.pal(4, 'Set2')
cols <- RColorBrewer::brewer.pal(11, 'RdBu')
cols2 <- RColorBrewer::brewer.pal(11, 'PiYG')
cols3 <- RColorBrewer::brewer.pal(11, 'RdYlBu')

df2 <- df2[df2$Lake != 'Kivu',]



# Horixontal lines
n1 <- length(unique(df2[idx, 'label']))
n1
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

## tas
idx <- which(df2$variable == 'tas' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])

df2$mean_disc <- cut(df2$mean, seq(-7,7,1), include.lowest = T)
levels(df2$mean_disc) <- seq(-6,7,1)
df2$mean_disc <- factor(df2$mean_disc, levels = rev(levels(df2$mean_disc)))

cols <- RColorBrewer::brewer.pal(11, 'RdBu')
col_fn <- colorRampPalette(cols)
incol <- col_fn(length(levels(df2$mean_disc)))

p1 <- ggplot(df2[idx,], aes(Month, label))+
  geom_tile(aes(fill = mean_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = (incol), name = 'Anomaly (\u00B0C)', drop = T)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p1

## sfcWind
idx <- which(df2$variable == 'sfcWind' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:5])

df2$mean_disc <- cut(df2$mean, seq(-0.5,0.5,0.1), include.lowest = T)
levels(df2$mean_disc) <- seq(-0.4,0.5,0.1)
df2$mean_disc <- factor(df2$mean_disc, levels = rev(levels(df2$mean_disc)))

col_fn <- colorRampPalette(cols2)
incol <- col_fn(length(levels(df2$mean_disc)))

p2 <- ggplot(df2[idx,], aes(Month, label))+
  geom_tile(aes(fill = mean_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = (incol), name = bquote('Anomaly (m'~s^-1*')'), drop = T)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p2


## rsds
idx <- which(df2$variable == 'rsds' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:5])

df2$mean_disc <- cut(df2$mean, seq(-40,40,5), include.lowest = T)
levels(df2$mean_disc) <- seq(-35,40,5)
df2$mean_disc <- factor(df2$mean_disc, levels = rev(levels(df2$mean_disc)))

col_fn <- colorRampPalette(cols3)
incol <- col_fn(length(levels(df2$mean_disc)))

p3 <- ggplot(df2[idx,], aes(Month, label))+
  geom_tile(aes(fill = mean_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = (incol), name = bquote('Anomaly (W'~m^-2*')'), drop = T)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(0.9, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p3



g1 <- ggpubr::ggarrange(p1, p2,p3, nrow = 1, labels = 'AUTO', align = 'hv')

# g1
ggsave('analysis-isimip/plots/met_month_2069-99_tas_sfcWind_rsds_warming_stripe_RCP6.0_anomaly_v2_wlabels.png', g1, dpi = 300,width = 340,height = 220, units = 'mm')

p2 <- p2 + theme(axis.text.y = element_blank())
p3 <- p3 + theme(axis.text.y = element_blank())
g2 <- ggpubr::ggarrange(p1, p2,p3, nrow = 1, labels = 'AUTO', align = 'hv')

# g1
ggsave('analysis-isimip/plots/met_month_2069-99_tas_sfcWind_rsds_warming_stripe_RCP6.0_anomaly_v2_nolabels.png', g2, dpi = 300,width = 340,height = 220, units = 'mm')



### Variation
cols <- RColorBrewer::brewer.pal(5,'Greens')
cols2 <- RColorBrewer::brewer.pal(5,'PuRd')
cols3 <- RColorBrewer::brewer.pal(5,'PuBuGn')
## tas
## tas
idx <- which(df2$variable == 'tas' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])

df2$sd_disc <- cut(df2$sd, seq(0,4.5,0.5), include.lowest = T)
levels(df2$sd_disc) <- seq(0.5,4.5,0.5)
df2$sd_disc <- factor(df2$sd_disc, levels = rev(levels(df2$sd_disc)))

col_fn <- colorRampPalette(cols)
incol <- col_fn(length(levels(df2$sd_disc)))

p1 <- ggplot(df2[idx,], aes(Month, label))+
  geom_tile(aes(fill = sd_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = rev(incol), name = 'SD (\u00B0C)', drop = T)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p1



## sfcWind
idx <- which(df2$variable == 'sfcWind' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])

df2$sd_disc <- cut(df2$sd, seq(0,1.2,0.2), include.lowest = T)
levels(df2$sd_disc) <- seq(0.2,1.2,0.2)
df2$sd_disc <- factor(df2$sd_disc, levels = rev(levels(df2$sd_disc)))

col_fn <- colorRampPalette(cols2)
incol <- col_fn(length(levels(df2$sd_disc)))

p2 <- ggplot(df2[idx,], aes(Month, label))+
  geom_tile(aes(fill = sd_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = rev(incol), name = bquote('SD (m'~s^-1*')'), drop = T)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p2



## rsds
idx <- which(df2$variable == 'rsds' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])

df2$sd_disc <- cut(df2$sd, seq(0,35,5), include.lowest = T)
levels(df2$sd_disc) <- seq(5,35,5)
df2$sd_disc <- factor(df2$sd_disc, levels = rev(levels(df2$sd_disc)))

col_fn <- colorRampPalette(cols3)
incol <- col_fn(length(levels(df2$sd_disc)))

p3 <- ggplot(df2[idx,], aes(Month, label))+
  geom_tile(aes(fill = sd_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = rev(incol), name = bquote('SD (W'~m^-2*')'), drop = T)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p3



g1 <- ggpubr::ggarrange(p1, p2,p3, nrow = 1, labels = 'AUTO', align = 'hv')

g1
ggsave('analysis-isimip/plots/met_month_2069-99_tas_sfcWind_rsds_warming_stripe_RCP6.0_sd_v2.png', g1, dpi = 300,width = 340,height = 220, units = 'mm')

##############################


# Average across GCM - Yearly
idx <- which(met$Scenario == 'RCP6.0')
df2 <- ddply(met[idx,], c('Year', 'Lake', 'Scenario', 'variable'), .fun = function(x){
  # print(head(x))
  mn <- mean(x$anom)
  sd <- sd(x$anom)
  min = min(x$anom)
  mx <- max(x$anom)
  df <- data.frame(mean = mn, sd = sd, min = min, max = mx)
  return(df)
}, .progress = 'text')

morph <- read.csv('MetaData/lakes_size_depth_class.csv')
region <- read.csv('ISIMIP_output/Metadata/isimip_calib_metadata_regions_v3.csv')
region <- region[,c("ISIMIP_name", "Region")]
labs <- read.csv('MetaData/lake_labels.csv')
df2 <- merge(df2, labs, by.x = 'Lake', by.y = 'lake')
df2 <- merge(df2, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df2 <- merge(df2, region, by.x = 'Lake', by.y = 'ISIMIP_name')
df2$Region <- factor(df2$Region, levels = c("NEU","CEU", "MED",  "ENA", "CNA",  "WNA", "NAU",  "SAU" ))
df2$label <- factor(df2$label)
df2$label <- factor(df2$label, levels = unique(df2$label[rev(order(df2$Region))]), ordered = T)
df2$variable <- factor(df2$variable)
# df2$variable <- factor(df2$variable, levels = levels(df2$variable)[c(2,1,3)])
levels(df2$Region)

# df2 <- merge(df2, trend2, by = c('Lake','Scenario', 'variable'))
# df2$label2 <- paste0(df2$label, df2$sig)


d.cols <- RColorBrewer::brewer.pal(4, 'Set2')
cols <- RColorBrewer::brewer.pal(11, 'RdBu')
cols2 <- RColorBrewer::brewer.pal(11, 'PiYG')
cols3 <- RColorBrewer::brewer.pal(11, 'RdYlBu')

df2 <- df2[df2$Lake != 'Kivu',]





## tas
idx <- which(df2$variable == 'tas' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])

# Horixontal lines
n1 <- length(unique(df2[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

df2$mean_disc <- cut(df2$mean, seq(-6,6,1), include.lowest = T)
levels(df2$mean_disc) <- seq(-5,6,1)
df2$mean_disc <- factor(df2$mean_disc, levels = rev(levels(df2$mean_disc)))

cols <- RColorBrewer::brewer.pal(11, 'RdBu')
col_fn <- colorRampPalette(cols)
incol <- col_fn(length(levels(df2$mean_disc)))

p1 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = (incol), name = 'Anomaly (\u00B0C)', drop = T)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p1

## sfcWind
idx <- which(df2$variable == 'sfcWind' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:5])

df2$mean_disc <- cut(df2$mean, seq(-0.6,0.6,0.1), include.lowest = T)
levels(df2$mean_disc) <- seq(-0.5,0.6,0.1)
df2$mean_disc <- factor(df2$mean_disc, levels = rev(levels(df2$mean_disc)))

col_fn <- colorRampPalette(cols2)
incol <- col_fn(length(levels(df2$mean_disc)))

p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = (incol), name = bquote('Anomaly (m'~s^-1*')'), drop = T)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(0.9, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p2


## rsds
idx <- which(df2$variable == 'rsds' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:5])

df2$mean_disc <- cut(df2$mean, seq(-20,20,5), include.lowest = T)
levels(df2$mean_disc) <- seq(-15,20,5)
df2$mean_disc <- factor(df2$mean_disc, levels = rev(levels(df2$mean_disc)))

col_fn <- colorRampPalette(cols3)
incol <- col_fn(length(levels(df2$mean_disc)))

p3 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = (incol), name = bquote('Anomaly (W'~m^-2*')'), drop = T)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.1, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p3



g1 <- ggpubr::ggarrange(p1, p2,p3, nrow = 1, labels = 'AUTO', align = 'hv')

# g1
ggsave('analysis-isimip/plots/met_year_tas_sfcWind_rsds_warming_stripe_RCP6.0_anomaly_v2.png', g1, dpi = 300,width = 340,height = 220, units = 'mm')

### Variation
cols <- RColorBrewer::brewer.pal(5,'Greens')
cols2 <- RColorBrewer::brewer.pal(5,'PuRd')
cols3 <- RColorBrewer::brewer.pal(5,'PuBuGn')
## tas
## tas
idx <- which(df2$variable == 'tas' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])

df2$sd_disc <- cut(df2$sd, seq(0,5,1), include.lowest = T)
levels(df2$sd_disc) <- seq(1,5,1)
df2$sd_disc <- factor(df2$sd_disc, levels = rev(levels(df2$sd_disc)))

col_fn <- colorRampPalette(cols)
incol <- col_fn(length(levels(df2$sd_disc)))

p1 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = rev(incol), name = 'SD (\u00B0C)', drop = F)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p1



## sfcWind
idx <- which(df2$variable == 'sfcWind' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])

df2$sd_disc <- cut(df2$sd, seq(0,1.2,0.2), include.lowest = T)
levels(df2$sd_disc) <- seq(0.2,1.2,0.2)
df2$sd_disc <- factor(df2$sd_disc, levels = rev(levels(df2$sd_disc)))

col_fn <- colorRampPalette(cols2)
incol <- col_fn(length(levels(df2$sd_disc)))

p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = rev(incol), name = bquote('SD (m'~s^-1*')'), drop = F)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p2



## rsds
idx <- which(df2$variable == 'rsds' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])

df2$sd_disc <- cut(df2$sd, seq(0,30,5), include.lowest = T)
levels(df2$sd_disc) <- seq(5,30,5)
df2$sd_disc <- factor(df2$sd_disc, levels = rev(levels(df2$sd_disc)))

col_fn <- colorRampPalette(cols3)
incol <- col_fn(length(levels(df2$sd_disc)))

p3 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd_disc))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-7,7), breaks = MakeBreaks(2))+
  scale_fill_manual(values = rev(incol), name = bquote('SD (W'~m^-2*')'), drop = F)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_x_continuous()+
  guides( fill = guide_legend(title.position = 'bottom',
                              nrow = 1,
                              label.position = 'bottom', reverse = T, title.hjust = 0.5))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'),
        legend.position = 'bottom', legend.spacing.x = unit(-0.1, 'cm'),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p3



g1 <- ggpubr::ggarrange(p1, p2,p3, nrow = 1, labels = 'AUTO', align = 'hv')

g1
ggsave('analysis-isimip/plots/met_Year_tas_sfcWind_rsds_warming_stripe_RCP6.0_sd_v2.png', g1, dpi = 300,width = 340,height = 220, units = 'mm')





# Average across months ----
df <- ddply(met, c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), .fun = function(x){
  mean(x$anom)
}, .progress = 'text')

# Plot GCM vs GCM ----
library(tidyr)
library(GGally)

idx <-which(df$Scenario != 'Historical')
scen <- ddply(df[idx,], c('Lake', 'Scenario', 'GCM', 'variable'), .fun = function(x){
  rollmeanr(x$V1, 30)
}, .progress = 'text')
colnames(scen)[4] <- 'met'
mlt <- melt(scen, id.vars = 1:4)

wid <- pivot_wider(mlt, id_cols = c(Lake, Scenario, met, variable), names_from = GCM, values_from = value)

idx <- which(wid$met == 'tas')
p <- ggpairs(wid[idx,], columns = 5:8, aes(colour=Scenario, alpha =0.2),
             upper = list(continuous = wrap("cor", size = 6)))+
  labs(tag = 'A')+
  xlab('Anomaly (\u00B0C)')+
  ylab('Anomaly (\u00B0C)')+
  theme_bw(base_size = 26)
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=d.cols[-1]) +
      scale_color_manual(values=d.cols[-1])  
  }
}
p1 <- p
p1
ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_tas_pairs_plot.png', p1, dpi = 300,width = 340,height = 340, units = 'mm')


idx <- which(wid$met == 'sfcWind')
p <- ggpairs(wid[idx,], columns = 5:8, aes(colour=Scenario, alpha =0.2),
             upper = list(continuous = wrap("cor", size = 6)))+
  labs(tag = 'B')+
  xlab(expression('Anomaly ('*m ~s^-1*')'))+
  ylab(expression('Anomaly ('*m ~s^-1*')'))+
  theme_bw(base_size = 26)
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=d.cols[-1]) +
      scale_color_manual(values=d.cols[-1])  
  }
}
p2 <- p
p2
ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_sfcWind_pairs_plot.png', p2, dpi = 300,width = 340,height = 340, units = 'mm')

idx <- which(wid$met == 'rsds')
p <- ggpairs(wid[idx,], columns = 5:8, aes(colour=Scenario, alpha =0.2),
             upper = list(continuous = wrap("cor", size = 6)))+
  labs(tag = 'C')+
  xlab(expression('Anomaly (W'~m^-2*')'))+
  ylab(expression('Anomaly (W'~m^-2*')'))+
  theme_bw(base_size = 26)
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=d.cols[-1]) +
      scale_color_manual(values=d.cols[-1])  
  }
}
p3 <- p
p3
ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_rsds_pairs_plot.png', p3, dpi = 300,width = 340,height = 340, units = 'mm')



#Average across GCM
df2 <- ddply(df, c('Year', 'Lake', 'Scenario', 'variable'), .fun = function(x){
  # print(head(x))
  mn <- mean(x$V1)
  sd <- sd(x$V1)
  min = min(x$V1)
  mx <- max(x$V1)
  df <- data.frame(mean = mn, sd = sd, min = min, max = mx)
  return(df)
}, .progress = 'text')
# df2b <- merge(df2, meta, by.x = 'Lake', by.y = 'ISIMIP_name')

morph <- read.csv('MetaData/lakes_size_depth_class.csv')
region <- read.csv("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\WateXr\\ISIMIP_local\\MetaData/isimip_calib_metadata_regions_v3.csv")
region <- region[,c('ISIMIP_name', 'Region')]
region$Region <- factor(region$Region, levels = c('NEU', 'CEU', 'MED', 'ENA', 'CNA', 'WNA', 'NAU', 'SAU')) 

library(forcats)

labs <- read.csv('MetaData/lake_labels.csv')
df2 <- merge(df2, labs, by.x = 'Lake', by.y = 'lake')
df2 <- merge(df2, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df2 <- merge(df2, region, by.x = 'Lake', by.y = 'ISIMIP_name')
df2$label <- factor(df2$label)
df2$label <- factor(df2$label, levels = unique(df2$label[order(df2$Region)]), ordered = T)
df2$variable <- factor(df2$variable)
# df2$variable <- factor(df2$variable, levels = levels(df2$variable)[c(2,1,3)])
levels(df2$variable)
d.cols <- RColorBrewer::brewer.pal(4, 'Set2')
cols <- RColorBrewer::brewer.pal(11, 'Spectral')
cols2 <- RColorBrewer::brewer.pal(11, 'PiYG')
cols3 <- RColorBrewer::brewer.pal(11, 'RdYlBu')

df2 <- df2[df2$Lake != 'Kivu',]
df2$flat <- factor(df2$lat)

## tas
idx <- which(df2$variable == 'tas' & df2$Scenario == 'RCP6.0')
p1 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-6,6), breaks = MakeBreaks(2))+
  xlab('Year')+
  ylab('')+
  guides( fill = guide_colorbar(title.position = 'top'))+
  scale_y_discrete(position = 'right')+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 10), legend.key.height = unit(0.5, 'cm'), legend.key.width = unit(1, 'cm'), legend.position = 'top', legend.title = element_text(size = 14, hjust = 0.5), legend.text = element_text(size = 12))
p1

## sfcWind
idx <- which(df2$variable == 'sfcWind' & df2$Scenario == 'RCP6.0')
p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols2), name = bquote('Anomaly (m'~s^-1*')'), limits = c(-0.5,0.5), breaks = MakeBreaks(0.2))+
  # facet_wrap(~Scenario, ncol = 3)+
  xlab('Year')+
  ylab('')+
  guides( fill = guide_colorbar(title.position = 'top'))+
  # scale_y_discrete(sec_axis(labels = label))+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_blank(), legend.key.height = unit(0.5, 'cm'), legend.key.width = unit(1, 'cm'), legend.position = 'top', legend.title = element_text(size = 14, hjust = 0.5), legend.text = element_text(size = 12))
p2

## rsds
idx <- which(df2$variable == 'rsds' & df2$Scenario == 'RCP6.0')
p3 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols3), name = bquote('Anomaly (W'~m^-2*')'), limits = c(-20,20), breaks = MakeBreaks(5))+
  # facet_wrap(~Scenario, ncol = 3)+
  xlab('Year')+
  ylab('')+
  guides( fill = guide_colorbar(title.position = 'top'))+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_blank(), legend.key.height = unit(0.5, 'cm'), legend.key.width = unit(1, 'cm'), legend.position = 'top', legend.title = element_text(size = 14, hjust = 0.5), legend.text = element_text(size = 12))
p3

g1 <- ggpubr::ggarrange(p1, p2,p3, nrow = 1, labels = 'AUTO', align = 'hv')

# g1
ggsave('analysis-isimip/plots/met_tas_sfcWind_rsds_warming_stripe_RCP6.0_anomaly_latitude.png', g1, dpi = 300,width = 340,height = 220, units = 'mm')

### Variation
cols <- RColorBrewer::brewer.pal(5,'Greens')
cols2 <- RColorBrewer::brewer.pal(5,'PuRd')
cols3 <- RColorBrewer::brewer.pal(5,'PuBuGn')
## tas
idx <- which(df2$variable == 'tas' & df2$Scenario == 'RCP6.0')
summary(df2[idx,1:7])
p1 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = (cols), name = 'SD (\u00B0C)', limits = c(0,3), breaks = MakeBreaks(0.5))+
  xlab('Year')+
  ylab('')+
  guides( fill = guide_colorbar(title.position = 'top'))+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 10), legend.key.height = unit(0.5, 'cm'), legend.key.width = unit(1, 'cm'), legend.position = 'top', legend.title = element_text(size = 14, hjust = 0.5), legend.text = element_text(size = 12))
p1

## sfcWind
idx <- which(df2$variable == 'sfcWind' & df2$Scenario == 'RCP6.0')
summary(df2[idx,1:7])
p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = (cols2), name = bquote('SD (m'~s^-1*')'), limits = c(0,1.1), breaks = MakeBreaks(0.25))+
  # facet_wrap(~Scenario, ncol = 3)+
  xlab('Year')+
  ylab('')+
  guides( fill = guide_colorbar(title.position = 'top'))+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_blank(), legend.key.height = unit(0.5, 'cm'), legend.key.width = unit(1, 'cm'), legend.position = 'top', legend.title = element_text(size = 14, hjust = 0.5), legend.text = element_text(size = 12))
p2

## rsds
idx <- which(df2$variable == 'rsds' & df2$Scenario == 'RCP6.0')
summary(df2[idx,1:7])
p3 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = (cols3), name = bquote('SD (W'~m^-2*')'), limits = c(0,20), breaks = MakeBreaks(5))+
  # facet_wrap(~Scenario, ncol = 3)+
  xlab('Year')+
  ylab('')+
  guides( fill = guide_colorbar(title.position = 'top'))+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_blank(), legend.key.height = unit(0.5, 'cm'), legend.key.width = unit(1, 'cm'), legend.position = 'top', legend.title = element_text(size = 14, hjust = 0.5), legend.text = element_text(size = 12))
p3

g1 <- ggpubr::ggarrange(p1, p2,p3, nrow = 1, labels = 'AUTO', align = 'hv')

# g1
ggsave('analysis-isimip/plots/met_tas_sfcWind_rsds_warming_stripe_RCP6.0_sd_region.png', g1, dpi = 300,width = 340,height = 220, units = 'mm')




















ggsave('analysis-isimip/plots/met_tas_warming_stripe_anomaly.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')

p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols), name = 'SD (degC)', limits = c(0,3.5), breaks = MakeBreaks(1))+
  facet_wrap(~Scenario, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 10), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2
ggsave('analysis-isimip/plots/met_tas_warming_stripe_sd.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')


### Wind
idx <- which(df2$variable == 'sfcWind' & df2$Scenario != 'Historical')
p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (m/s)', limits = c(-0.8,0.8), breaks = MakeBreaks(0.25))+
  facet_wrap(~Scenario, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 10), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2
ggsave('analysis-isimip/plots/met_sfcWind_warming_stripe_anomaly.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')

p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols), name = 'SD (m/s)', limits = c(0,1.1), breaks = MakeBreaks(0.25))+
  facet_wrap(~Scenario, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 10), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2
ggsave('analysis-isimip/plots/met_sfcWind_warming_stripe_sd.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')


idx <- which(df$Scenario == 'RCP6.0' & df$Lake == 'Feeagh')
p2 <- ggplot(df[idx,], aes(GCM, V1))+
  geom_hline(yintercept = 0)+
  geom_violin()+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (m/s)', limits = c(-0.8,0.8), breaks = MakeBreaks(0.25))+
  facet_wrap(~variable, nrow = 2, scales = 'free_y')+
  xlab('GCM')+
  ylab('')+
  # scale_y_discrete()+
  # scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 10), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2

dfb <- ddply(met, c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), .fun = function(x){
  val <- mean(x$value)
  anom = mean(x$anom)
  df <- data.frame(value = val, anom = anom)
  return(df)
}, .progress = 'text')

idx <- which(dfb$Scenario == 'RCP6.0' & df$Lake == 'Feeagh')
p1 <- ggplot(dfb[idx,], aes(GCM, value, fill = GCM))+
  # geom_hline(yintercept = 0)+
  geom_violin()+
  facet_wrap(~variable, nrow = 2, scales = 'free_y')+
  xlab('GCM')+
  ylab('')+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 10), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p1

p2 <- ggplot(dfb[idx,], aes(GCM, anom, fill = GCM))+
  geom_hline(yintercept = 0)+
  geom_violin()+
  facet_wrap(~variable, nrow = 2, scales = 'free_y')+
  xlab('GCM')+
  ylab('')+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 10), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2

ggarrange(p1, p2, nrow = 1, common.legend = T)
