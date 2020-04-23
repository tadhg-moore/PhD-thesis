setwd("G:\\ISIMIP")
library(ncdf4)
library(lubridate)
library(reshape)
library(rLakeAnalyzer)
library(ggplot2)
library(hydroTSM)
library(plyr)
library(metR)
library(doParallel)


meta <- read.csv('MetaData/lakes_size_depth_class.csv', stringsAsFactors = T)
dirs = c('GOTM', 'GLM')
# dirs <- dirs[-5]
print(dirs)
# 

# 'surftemp', 'bottemp', 'schmidtstability', 'thermodepth', 'wholelaketemp'
dly_vars <- c('surftemp', 'bottemp', 'wholelaketemp')

fpath <- 'ISIMIP_output/'

laks <- list.files(file.path(fpath, 'GOTM'))
# laks <- 'Feeagh'
dir.create('analysis-isimip/plots/')

res_list <- list()


for(target_var in dly_vars){
  
  out_fpath <- paste0('ISIMIP_output/', target_var)
  
  laks <- list.files(file.path(fpath, target_var))
  
  # Loop through each file and load it in
  
  # target_stat <- 'mean'
  for(k in laks){
    ind = length(res_list)+1
    message('Loading ', k, ' ', Sys.time())
    df2 <- tryCatch({read.csv(file.path(fpath, target_var,k), stringsAsFactors = T)
    }, error = function(cond){return(NA)},
    finally = {opt = TRUE})
    
    if(is.na(df2))next
    
    # Average across months
    idx <- which(df2$stat == 'mean')
    df <- ddply(df2[idx,], c('year', 'lake', 'scenario', 'model', 'lmodel', 'variable'), .fun = function(x){
      mean(x$anom)
    })
    
    res_list[[ind]] <- df
    
  }
  
}

out <- do.call('rbind', res_list)
summary(out)
colnames(out) <- c('Year', 'Lake','Scenario', 'GCM', 'LakeModel',  'variable','anom')
levels(out$Scenario) <- c('Historical', 'RCP2.6', 'RCP6.0', 'RCP8.5')
df <- out
# df <- na.exclude(df)




df2 <- ddply(df, .variables = c('Year','Scenario', 'variable', 'Lake'), .fun = function(x){
  mn = mean(x$anom)
  sd = sd(x$anom)
  df = data.frame(mean = mn, sd = sd)
  return(df)
}, .progress = 'text')




meta <- read.csv('MetaData/lakes_size_depth_class.csv', stringsAsFactors = T)
morph <- read.csv('MetaData/lakes_size_depth_class.csv')
region <- read.csv("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\WateXr\\ISIMIP_local/MetaData/isimip_all_metadata_regions_v3.csv")
region <- region[,c(3,ncol(region))]
labs <- read.csv('ISIMIP_output/Metadata/lake_labels.csv')
df2 <- merge(df2, labs, by.x = 'Lake', by.y = 'lake')
df2 <- merge(df2, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df2 <- merge(df2, region, by.x = 'Lake', by.y = 'ISIMIP_name')
df2$Region <- factor(df2$Region, levels = c("ALA", "NEU","CEU", "MED",  "ENA", "CNA",  "WNA", "NAU",  "SAU" ))



df2$label <- factor(df2$label)
df2$label <- factor(df2$label, levels = unique(df2$label[rev(order(df2$Region))]), ordered = T)

# Mean x SD ----
head(df2)
df2 <- df2[df2$Lake != 'Kivu',]
idx <- which(df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
colnames(df2)[4] <- 'temp'
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Year', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')

summary(mlt)

mlt$disc <- cut(mlt$value, seq(-4,4,1), include.lowest = T)
levels(mlt$disc) <- seq(-3.5,3.5,1)
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)
xmx <- 2114
sep <- data.frame(y = c(5,6,9,19,23,24,35)-0.5)
xcex <- 4

cols <- RColorBrewer::brewer.pal(n = 11, name = 'RdBu')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))
mlt$label <- factor(mlt$label)
xlbs <- levels(mlt$label)

idx <- which(mlt$temp == 'surftemp')
p1 <- ggplot(mlt[idx,], aes(Year, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = '\u00B0C', drop = F)+
  facet_wrap(~variable, ncol = 2)+
  xlab('Year')+
  labs(tag = 'A')+
  ylab('')+
  # scale_y_discrete(labels = lbs)+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  theme_classic()+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),# hjust = grid::unit(c(0.3,0,-0.3,0), 'cm')),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 20, hjust = 0),
        strip.text = element_text(size = 20, vjust = 0),
        plot.tag = element_text(size = 20),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1.3, 'cm')
  )+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 39, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 23, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 20.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 13.5, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(2006,2100), clip = 'off')

ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/all_lake_year_surftemp_RCP60_anomaly+SD_v1.png', p1, dpi = 300,width = 270,height = 297, units = 'mm')

## bottemp
idx <- which(mlt$temp == 'bottemp')
p1 <- ggplot(mlt[idx,], aes(Year, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = '\u00B0C', drop = F)+
  facet_wrap(~variable, ncol = 2)+
  xlab('Year')+
  labs(tag = 'B')+
  ylab('')+
  # scale_y_discrete(labels = lbs)+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  theme_classic()+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),# hjust = grid::unit(c(0.3,0,-0.3,0), 'cm')),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 20, hjust = 0),
        strip.text = element_text(size = 20, vjust = 0),
        plot.tag = element_text(size = 20),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1.3, 'cm')
  )+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 39, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 23, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 20.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 13.5, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(2006,2100), clip = 'off')

ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/all_lake_year_bottemp_RCP60_anomaly+SD_v1.png', p1, dpi = 300,width = 270,height = 297, units = 'mm')

idx <- which(mlt$temp == 'wholelaketemp')
p1 <- ggplot(mlt[idx,], aes(Year, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = '\u00B0C', drop = F)+
  facet_wrap(~variable, ncol = 2)+
  xlab('Year')+
  labs(tag = 'C')+
  ylab('')+
  # scale_y_discrete(labels = lbs)+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  theme_classic()+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),# hjust = grid::unit(c(0.3,0,-0.3,0), 'cm')),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 20, hjust = 0),
        strip.text = element_text(size = 20, vjust = 0),
        plot.tag = element_text(size = 20),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1.3, 'cm')
  )+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 39, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 23, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 20.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 13.5, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(2006,2100), clip = 'off')

ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/all_lake_year_wholelaketemp_RCP60_anomaly+SD_v1.png', p1, dpi = 300,width = 270,height = 297, units = 'mm')

p1 <- ggplot(mlt, aes(Year, (label)))+
  # geom_tile(aes(fill = sd))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = 'Temperature (\u00B0C)', drop = T)+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-4,4), breaks = MakeBreaks(1))+
  facet_wrap(~variable, ncol = 3)+
  facet_grid(temp~variable)+
  xlab('Year')+
  ylab('')+
  # scale_y_discrete()+
  # scale_y_continuous(breaks = seq(1.5,44.5,1), labels = xlbs, sec.axis = sec_axis(~., breaks = seq(1.5,44.5,1), labels = xlbs))+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 7),# hjust = grid::unit(c(0.3,0,-0.3,0), 'cm')),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 16, hjust = 0),
        strip.text = element_text(size = 20, vjust = 0),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,2.5,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1, 'cm'))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 0.6)+
  annotate('text', x = xmx, y = 40.5, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 29.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 24, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 21.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 14, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(2006,2100), clip = 'off')
# ggplot2::labs(tag = 'TEAST')+
# ggplot2::labs(tag = 'TEAST2')
# p1

ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v2/all_lake_year_surftemp_bottemp_wholelaketemp_RCP60_anomaly+SD_2069-99_v4.png', p1, dpi = 300,width = 300,height = 270, units = 'mm')

library(ggrepel)



#####

## Plot Month ----
res_list <- list()

for(target_var in dly_vars){
  
  out_fpath <- paste0('ISIMIP_output/', target_var)
  
  laks <- list.files(file.path(fpath, target_var))
  
  # Loop through each file and load it in
  
  # target_stat <- 'mean'
  for(k in laks){
    ind = length(res_list)+1
    message('Loading ', k, ' ', Sys.time())
    df2 <- tryCatch({read.csv(file.path(fpath, target_var,k), stringsAsFactors = T)
    }, error = function(cond){return(NA)},
    finally = {opt = TRUE})
    
    if(is.na(df2))next
    
    # Average across years
    idx <- which(df2$stat == 'mean' & df2$year >= 2069)
    df <- ddply(df2[idx,], c('month', 'lake', 'scenario', 'model', 'variable'), .fun = function(x){
      mean(x$anom)
    })
    
    res_list[[ind]] <- df
    
  }
  
}

out <- do.call('rbind', res_list)
summary(out)
colnames(out) <- c('Month', 'Lake','Scenario', 'GCM',  'variable','anom')
levels(out$Scenario) <- c('Historical', 'RCP2.6', 'RCP6.0', 'RCP8.5')
df <- out


df2 <- ddply(df, .variables = c('Month','Scenario', 'variable', 'Lake'), .fun = function(x){
  mn = mean(x$anom)
  sd = sd(x$anom)
  df = data.frame(mean = mn, sd = sd)
  return(df)
}, .progress = 'text')


meta <- read.csv('MetaData/lakes_size_depth_class.csv', stringsAsFactors = T)
morph <- read.csv('MetaData/lakes_size_depth_class.csv')
region <- read.csv("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\WateXr\\ISIMIP_local/MetaData/isimip_all_metadata_regions_v3.csv")
region <- region[,c(3,ncol(region))]
labs <- read.csv('ISIMIP_output/Metadata/lake_labels.csv')
df2 <- merge(df2, labs, by.x = 'Lake', by.y = 'lake')
df2 <- merge(df2, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df2 <- merge(df2, region, by.x = 'Lake', by.y = 'ISIMIP_name')
df2$Region <- factor(df2$Region, levels = c("NEU","CEU", "MED",  "ENA", "CNA",  "WNA", "NAU",  "SAU" ))



df2$label <- factor(df2$label)
df2$label <- factor(df2$label, levels = unique(df2$label[rev(order(df2$Region))]), ordered = T)

# Bin temp
df2 <- df2[df2$Lake != 'Kivu',]
idx <- which(df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui')
summary(df2[idx,1:6])
df2$mean_disc <- cut(df2$mean, seq(-6.5,6.5,1), include.lowest = T)
levels(df2$mean_disc) <- seq(-6,6,1)
df2$mean_disc <- factor(df2$mean_disc, levels = rev(levels(df2$mean_disc)))

n1 <- length(unique(df2[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)



# Mean x SD ----
head(df2)
colnames(df2)[4] <- 'temp'
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Month', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')

summary(mlt)

mlt$disc <- cut(mlt$value, seq(-6,6,1), include.lowest = T)
levels(mlt$disc) <- seq(-5.5,5.5,1)
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)
sep <- data.frame(y = c(5,6,9,19,23,24,35)-0.5)
xmx <- 13.3
xcex <- 4

cols <- RColorBrewer::brewer.pal(n = 11, name = 'RdBu')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))

# lbs <- levels(mlt$label)
# for(i in seq(1,length(lbs),2)){
#   lbs[i] <- paste0(lbs[i],'        ')
# }
# lbs[1] <- paste0(lbs[1],'    ')
# levels(mlt$temp) <- c('A', 'B', 'C') # A = surf, B = bott, C = Whole
idx <- which(mlt$temp == 'surftemp')
p1 <- ggplot(mlt[idx,], aes(Month, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = '\u00B0C', drop = F)+
  facet_wrap(~variable, ncol = 2)+
  xlab('Month')+
  labs(tag = 'A')+
  ylab('')+
  # scale_y_discrete(labels = lbs)+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+ 
  theme_classic()+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),# hjust = grid::unit(c(0.3,0,-0.3,0), 'cm')),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 20, hjust = 0),
        strip.text = element_text(size = 20, vjust = 0),
        plot.tag = element_text(size = 20),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1.3, 'cm')
  )+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 39, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 23, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 20.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 13.5, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(1,12), clip = 'off')

ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/all_lake_month_surftemp_RCP60_anomaly+SD_2069-99_v1.png', p1, dpi = 300,width = 270,height = 297, units = 'mm')

## bottemp
idx <- which(mlt$temp == 'bottemp')
p1 <- ggplot(mlt[idx,], aes(Month, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = '\u00B0C', drop = F)+
  facet_wrap(~variable, ncol = 2)+
  xlab('Month')+
  labs(tag = 'B')+
  ylab('')+
  # scale_y_discrete(labels = lbs)+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+ 
  theme_classic()+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),# hjust = grid::unit(c(0.3,0,-0.3,0), 'cm')),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 20, hjust = 0),
        strip.text = element_text(size = 20, vjust = 0),
        plot.tag = element_text(size = 20),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1.3, 'cm')
  )+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 39, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 23, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 20.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 13.5, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(1,12), clip = 'off')


ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/all_lake_month_bottemp_RCP60_anomaly+SD_2069-99_v1.png', p1, dpi = 300,width = 270,height = 297, units = 'mm')

## wholelaketemp
idx <- which(mlt$temp == 'wholelaketemp')
p1 <- ggplot(mlt[idx,], aes(Month, label))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = '\u00B0C', drop = F)+
  facet_wrap(~variable, ncol = 2)+
  xlab('Month')+
  labs(tag = 'C')+
  ylab('')+
  # scale_y_discrete(labels = lbs)+
  scale_x_continuous(breaks = seq(1,12,2), labels = month.abb[seq(1,12,2)])+ 
  theme_classic()+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 14),# hjust = grid::unit(c(0.3,0,-0.3,0), 'cm')),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 20, hjust = 0),
        strip.text = element_text(size = 20, vjust = 0),
        plot.tag = element_text(size = 20),
        # legend.spacing.y = unit(-0.1, 'cm'),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0,0,0, 'cm'),
        legend.margin = margin(0,0,0,0, 'cm'),
        legend.box.margin = margin(0,0,0,1.3, 'cm')
  )+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)+
  geom_hline(data = sep, aes(yintercept = y), size = 1)+
  annotate('text', x = xmx, y = 39, label = 'NEU', size = xcex)+
  annotate('text', x = xmx, y = 28.5, label = 'CEU', size = xcex)+
  annotate('text', x = xmx, y = 23, label = 'MED', size = xcex)+
  annotate('text', x = xmx, y = 20.5, label = 'ENA', size = xcex)+
  annotate('text', x = xmx, y = 13.5, label = 'CNA', size = xcex)+
  annotate('text', x = xmx, y = 7, label = 'WNA', size = xcex)+
  annotate('text', x = xmx, y = 5, label = 'NAU', size = xcex)+
  annotate('text', x = xmx, y = 2.5, label = 'SAU', size = xcex)+
  coord_cartesian(xlim = c(1,12), clip = 'off')


ggsave('C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\fig_v3/all_lake_month_wholelaketemp_RCP60_anomaly+SD_2069-99_v1.png', p1, dpi = 300,width = 270,height = 297, units = 'mm')


# ggsave('analysis-isimip/plots/all_lake_month_surftemp_bottemp_wholelaketemp_RCP60_month_anomaly+SD_2069-99_v4.png', p1, dpi = 300,width = 210,height = 297, units = 'mm')
 