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
got <- read.csv('MetaData/calib_data/GOTM_calib_results.csv', stringsAsFactors = F)
got <- got[,c('ISIMIP_name', 'RMSE', 'NSE')]
glm <- glm[,c('ISIMIP_name', 'RMSE', 'NSE')]
colnames(got)[-1] <- paste0('gotm_',colnames(got)[-1])
all <- merge(glm, got, by = 1)
idx <- which(all$RMSE <2 & all$NSE > 0.5 & all$gotm_RMSE <2 & all$gotm_NSE >0.5)
laks <- all[idx,1]
laks <- laks[laks != 'Kuivajarvi']

lakes <- meta$ISIMIP_name

lakes <- lakes[which(lakes %in% laks)] # Only select calib lakes

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
idx <- which(met$Year >= 2069 & met$Scenario == 'RCP6.0')

dfb <- ddply(met[idx,], c('Month', 'Lake', 'Scenario', 'GCM', 'variable'), .fun = function(x){
  # print(head(x))
  mn <- mean(x$anom)
  sd <- sd(x$anom)
  min = min(x$anom)
  mx <- max(x$anom)
  df <- data.frame(mean = mn, sd = sd, min = min, max = mx)
  return(df)
}, .progress = 'text')


df2 <- ddply(dfb, c('Month', 'Lake', 'Scenario', 'variable'), .fun = function(x){
  # print(head(x))
  mn <- mean(x$mean)
  sd <- sd(x$mean)
  min = min(x$mean)
  mx <- max(x$mean)
  df <- data.frame(mean = mn, sd = sd, min = min, max = mx)
  return(df)
}, .progress = 'text')
# df2b <- merge(df2, meta, by.x = 'Lake', by.y = 'ISIMIP_name')

morph <- read.csv('MetaData/lakes_size_depth_class.csv')
region <- read.csv("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\WateXr\\ISIMIP_local/MetaData/isimip_all_metadata_regions_v3.csv")
region <- region[,c("ISIMIP_name", "Region")]
labs <- read.csv('ISIMIP_output/Metadata/lake_labels.csv')
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

# Mean x SD ----
## tas
colnames(df2)[4] <- 'temp'
idx <- which(df2$temp == 'tas' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Month', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')

summary(mlt)

mlt$disc <- cut(mlt$value, seq(-7,7,1), include.lowest = T)
levels(mlt$disc) <- seq(-6.5,6.5,1)
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))



cols <- RColorBrewer::brewer.pal(n = 11, name = 'RdBu')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(0, 3600) + 0.5, y = rep(2:n1, each = 2) - 0.5)
sep <- data.frame(y = c(5,6,9,20,24,25,36)-0.5)
xmx <- 13.6
xcex <- 4.5

p2 <- ggplot(mlt, aes(Month, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = '\u00B0C', drop = F, na.value = 'grey')+
  facet_wrap(~variable, ncol = 2)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  labs(tag = 'A')+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+ 
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(1.5, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 18, vjust = 1),
        strip.text = element_text(size = 16),
        plot.tag = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 40.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 24, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 21.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(1,12), clip = 'off')


ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_tas_month_stripe_Region_v1.png', p2, dpi = 300,width = 270,height = 297, units = 'mm')

## sfcWind
idx <- which(df2$temp == 'sfcWind' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])
colnames(df2)[4] <- 'temp'
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Month', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')

summary(mlt)

mlt$disc <- cut(mlt$value, seq(-1.0,1.0,0.2), include.lowest = T)
levels(mlt$disc) <- seq(-0.9,0.9,0.2)
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

cols <- RColorBrewer::brewer.pal(n = 11, name = 'PiYG')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))


p2 <- ggplot(mlt, aes(Month, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = expression(~m ~s^-1), drop = F, na.value = 'grey')+
  facet_wrap(~variable, ncol = 2)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  labs(tag = 'B')+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+ 
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
        plot.tag = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 40.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 24, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 21.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(1,12), clip = 'off')


ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_sfcWind_month_stripe_Region_v1.png', p2, dpi = 300,width = 270,height = 297, units = 'mm')



## rsds
idx <- which(df2$temp == 'rsds' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Month', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')

summary(mlt)

mlt$disc <- cut(mlt$value, seq(-40,40,10), include.lowest = T)
levels(mlt$disc) <- seq(-35,35,10)
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

cols <- RColorBrewer::brewer.pal(n = 11, name = 'RdYlBu')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))


p2 <- ggplot(mlt, aes(Month, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = expression(~W ~m^-2), drop = F, na.value = 'grey')+
  facet_wrap(~variable, ncol = 2)+
  xlab('Month')+
  ylab('')+
  scale_y_discrete()+
  labs(tag = 'C')+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+ 
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
        plot.tag = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 40.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 24, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 21.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(1,12), clip = 'off')


ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_rsds_month_stripe_Region_v1.png', p2, dpi = 300,width = 270,height = 297, units = 'mm')

# 

##############################


# Average across GCM - Yearly
idx <- which(met$Scenario == 'RCP6.0')
# x <- met[(met$Year== 1984 & met$Scenario == 'Historical' & met$Lake == 'Allequash_Lake' & met$variable == 'tas'),]
dfb <- ddply(met[idx,], c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), .fun = function(x){
  # print(head(x))
  mn <- mean(x$anom)
  sd <- sd(x$anom)
  min = min(x$anom)
  mx <- max(x$anom)
  df <- data.frame(mean = mn, sd = sd, min = min, max = mx)
  return(df)
}, .progress = 'text')

# x <- dfb[(dfb$Year== 1984 & dfb$Scenario == 'Historical' & dfb$Lake == 'Allequash_Lake' & dfb$variable == 'tas'),]
df2 <- ddply(dfb, c('Year', 'Lake', 'Scenario', 'variable'), .fun = function(x){
  # print(head(x))
  mn <- mean(x$mean)
  sd <- sd(x$mean)
  min = min(x$mean)
  mx <- max(x$mean)
  df <- data.frame(mean = mn, sd = sd, min = min, max = mx)
  return(df)
}, .progress = 'text')


morph <- read.csv('MetaData/lakes_size_depth_class.csv')
region <- read.csv("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\WateXr\\ISIMIP_local/MetaData/isimip_all_metadata_regions_v3.csv")
region <- region[,c("ISIMIP_name", "Region")]
labs <- read.csv('ISIMIP_output/Metadata/lake_labels.csv')
df2 <- merge(df2, labs, by.x = 'Lake', by.y = 'lake')
df2 <- merge(df2, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df2 <- merge(df2, region, by.x = 'Lake', by.y = 'ISIMIP_name')
df2$Region <- factor(df2$Region, levels = c("NEU","CEU", "MED",  "ENA", "CNA",  "WNA", "NAU",  "SAU" ))
df2$label <- factor(df2$label)
df2$label <- factor(df2$label, levels = unique(df2$label[rev(order(df2$Region))]), ordered = T)
df2$variable <- factor(df2$variable)
# df2$variable <- factor(df2$variable, levels = levels(df2$variable)[c(2,1,3)])
levels(df2$Region)
levels(df2$label)

# df2 <- merge(df2, trend2, by = c('Lake','Scenario', 'variable'))
# df2$label2 <- paste0(df2$label, df2$sig)


d.cols <- RColorBrewer::brewer.pal(4, 'Set2')
cols <- RColorBrewer::brewer.pal(11, 'RdBu')
cols2 <- RColorBrewer::brewer.pal(11, 'PiYG')
cols3 <- RColorBrewer::brewer.pal(11, 'RdYlBu')

df2 <- df2[df2$Lake != 'Kivu',]



# Mean x SD ----
## tas
colnames(df2)[4] <- 'temp'
idx <- which(df2$temp == 'tas' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Year', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')

summary(mlt)

mlt$disc <- cut(mlt$value, seq(-6,6,2), include.lowest = T)
levels(mlt$disc) <- seq(-5,5,2)
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))
##

cols <- RColorBrewer::brewer.pal(n = 11, name = 'RdBu')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(0, 3600) + 0.5, y = rep(2:n1, each = 2) - 0.5)
sep <- data.frame(y = c(5,6,9,20,24,25,36)-0.5)
xmx <- 2112.5
xcex <- 4.5



p2 <- ggplot(mlt, aes(Year, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = '\u00B0C', drop = F, na.value = 'grey')+
  facet_wrap(~variable, ncol = 2)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  labs(tag = 'A')+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2.5, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 18, vjust = 1),
        strip.text = element_text(size = 16),
        plot.tag = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 40.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 24, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 21.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(2006,2100), clip = 'off')


ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_tas_year_stripe_Region_v1.png', p2, dpi = 300,width = 270,height = 297, units = 'mm')




## sfcWind
idx <- which(df2$temp == 'sfcWind' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])
colnames(df2)[4] <- 'temp'
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Year', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')

summary(mlt)

mlt$disc <- cut(mlt$value, seq(-1.2,1.2,0.2), include.lowest = T)
levels(mlt$disc) <- seq(-1.1,1.1,0.2)
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

cols <- RColorBrewer::brewer.pal(n = 11, name = 'PiYG')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))

p2 <- ggplot(mlt, aes(Year, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = expression(~m ~s^-1), drop = F, na.value = 'grey')+
  facet_wrap(~variable, ncol = 2)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  labs(tag = 'B')+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
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
        plot.tag = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 40.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 24, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 21.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(2006,2100), clip = 'off')


ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_sfcWind_year_stripe_Region_v1.png', p2, dpi = 300,width = 270,height = 297, units = 'mm')


## rsds
idx <- which(df2$temp == 'rsds' & df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Year', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')

summary(mlt)

mlt$disc <- cut(mlt$value, seq(-24,24,8), include.lowest = T)
levels(mlt$disc) <- seq(-20,20,8)
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

cols <- RColorBrewer::brewer.pal(n = 11, name = 'RdYlBu')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))

p2 <- ggplot(mlt, aes(Year, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = expression(~W ~m^-2), drop = F, na.value = 'grey')+
  facet_wrap(~variable, ncol = 2)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  labs(tag = 'C')+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
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
        plot.tag = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 40.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 24, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 21.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(2006,2100), clip = 'off')


ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/met_rsds_year_stripe_Region_v1.png', p2, dpi = 300,width = 270,height = 297, units = 'mm')

