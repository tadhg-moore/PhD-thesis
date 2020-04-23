setwd("G:\\ISIMIP")
library(ncdf4)
library(lubridate)
library(reshape)
library(rLakeAnalyzer)
library(ggplot2)
library(hydroTSM)
library(plyr)
library(doParallel)
library(metR)


meta <- read.csv('MetaData/lakes_size_depth_class.csv', stringsAsFactors = T)
dirs = c('GOTM', 'GLM')
# dirs <- dirs[-5]
print(dirs)
# 

target_var <- 'analyse_strat' 

fpath <- 'ISIMIP_output/'

# laks <- 'Feeagh'
dir.create('analysis-isimip/plots/')


laks <- list.files(file.path(fpath, 'analyse_strat'), pattern = 'anom')

res_list <- list()
for(k in laks){
  ind = length(res_list)+1
  message('Loading ', k, ' ', Sys.time())
  df2 <- tryCatch({read.csv(file.path(fpath, target_var,k), stringsAsFactors = T)
  }, error = function(cond){return(NA)},
  finally = {opt = TRUE})
  
  if(is.na(df2))next
  
  print(paste(k, sum(is.na(df2$anom))))
  
  
  res_list[[ind]] <- df2
  
}
  



out <- do.call('rbind', res_list)
summary(out)
colnames(out) <- c('Year', 'GCM','Scenario', 'LakeModel',  'Lake', 'variable','anom')
levels(out$Scenario) <- c('Historical', 'RCP2.6', 'RCP6.0', 'RCP8.5')
vars <- c( "StratStart" ,  "StratEnd",  "TotStratDur")
sub <- out[which(out$variable %in% vars),]
summary(sub[sub$anom > 200,])

df <- sub[!(sub$Lake %in% c('Kuivajarvi', 'Kivu'#,
                            #'Burley_Griffin', 'Argyle', 'Mt_Bold', 'Tarawera'
)),]

# Straton vs Strat off ----
idx <- which(df$variable == 'TotStratDur')

wid <- tidyr::pivot_wider(df[-idx,], id_cols = c('Year', 'Lake', 'Scenario', 'GCM', 'LakeModel'), names_from = 'variable', values_from = 'anom')
summary(wid)
cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')

lim <- 110

idx <- which(wid$Lake %in% c('Kivu','Kuivajarvi', 'Burley_Griffin', 'Argyle', 'Mt_Bold', 'Tarawera', 'Rotorua', 'Vortsjarv', 'Mueggelsee', 'Wingra', 'Annie'))

p1 <- ggplot(wid[-idx,], aes(StratStart, StratEnd))+
  geom_abline(slope = -1, intercept = 0, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_hex(binwidth = c(5,5))+
  scale_fill_gradientn(colours = rev(cols), name = 'Count')+#, breaks = c(seq(0,140,20)))+
  # geom_point(size = 0.1)+
  guides(colour = F)+
  xlab('Stratification Onset Anomaly (days)')+
  ylab('Stratification Offset Anomaly (days)')+
  coord_equal(xlim = c(-70,60), ylim = c(70,-60))+
  facet_grid(Scenario~GCM)+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.75, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 16, hjust = 0),
        legend.text = element_text(size = 16))
p1
ggsave('analysis-isimip/plots/all_lake_StratStart_StratEnd_GCM_Scenario_hex.png', p1, dpi = 300,width = 310,height = 310, units = 'mm')
#####

# TotStratDur ----
idx <- which(df$variable == 'TotStratDur')

wid <- tidyr::pivot_wider(df[idx,], id_cols = c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), names_from = 'LakeModel', values_from = 'anom')
summary(wid)
cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')

lim <- 110

p1 <- ggplot(wid, aes(GLM, GOTM))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_hex(binwidth = c(5,5))+
  scale_fill_gradientn(colours = rev(cols), name = 'Count')+#, breaks = c(seq(0,140,20)))+
  # geom_point(size = 0.1)+
  guides(colour = F)+
  xlab('GLM Anomaly (\u00B0C)')+
  ylab('GOTM Anomaly (\u00B0C)')+
  coord_equal(xlim = c(-lim,lim), ylim = c(-lim,lim))+
  facet_grid(Scenario~GCM)+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.75, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 16, hjust = 0),
        legend.text = element_text(size = 16))
p1
ggsave('analysis-isimip/plots/all_lake_TotStratDur_GOTM_GLM_hex.png', p1, dpi = 300,width = 310,height = 310, units = 'mm')
#####

# StratStart ----
idx <- which(df$variable == 'StratStart')

wid <- tidyr::pivot_wider(df[idx,], id_cols = c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), names_from = 'LakeModel', values_from = 'anom')
summary(wid)
cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')

lim <- 120

p1 <- ggplot(wid, aes(GLM, GOTM))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_hex(binwidth = c(5,5))+
  scale_fill_gradientn(colours = rev(cols), name = 'Count')+#, breaks = c(seq(0,140,20)))+
  # geom_point(size = 0.1)+
  guides(colour = F)+
  xlab('GLM Anomaly (\u00B0C)')+
  ylab('GOTM Anomaly (\u00B0C)')+
  coord_equal(xlim = c(-lim,lim), ylim = c(-lim,lim))+
  facet_grid(Scenario~GCM)+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.75, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 16, hjust = 0),
        legend.text = element_text(size = 16))
p1
ggsave('analysis-isimip/plots/all_lake_StratStart_GOTM_GLM_hex.png', p1, dpi = 300,width = 310,height = 310, units = 'mm')
###############

# StratEnd ----
idx <- which(df$variable == 'StratEnd')

wid <- tidyr::pivot_wider(df[idx,], id_cols = c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), names_from = 'LakeModel', values_from = 'anom')
summary(wid)
cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')

lim <- 120

p1 <- ggplot(wid, aes(GLM, GOTM))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_hex(binwidth = c(5,5))+
  scale_fill_gradientn(colours = rev(cols), name = 'Count')+#, breaks = c(seq(0,140,20)))+
  # geom_point(size = 0.1)+
  guides(colour = F)+
  xlab('GLM Anomaly (\u00B0C)')+
  ylab('GOTM Anomaly (\u00B0C)')+
  coord_equal(xlim = c(-lim,lim), ylim = c(-lim,lim))+
  facet_grid(Scenario~GCM)+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.75, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 16, hjust = 0),
        legend.text = element_text(size = 16))
p1
ggsave('analysis-isimip/plots/all_lake_StratEnd_GOTM_GLM_hex.png', p1, dpi = 300,width = 310,height = 310, units = 'mm')

######




# Average across lake model & GCM
df2 <- ddply(df, .variables = c('Year','Scenario', 'variable', 'Lake'), .fun = function(x){
  mn = mean(x$anom, na.rm = T)
  sd = sd(x$anom, na.rm = T)
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
df2 <- df2[!(df2$Lake %in% c('Kivu','Kuivajarvi', 'Burley_Griffin', 'Argyle', 'Mt_Bold', 'Tarawera', 'Rotorua', 'Vortsjarv', 'Mueggelsee', 'Wingra', 'Annie')),]


head(df2)
df2 <- df2[df2$Lake != 'Kivu',]
idx <- which(df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui' &df2$Year < 2099)
colnames(df2)[4] <- 'temp'
mlt <- melt(df2[idx,], id.vars = c('Lake', 'Scenario', 'Year', 'temp', 'label'), measure.vars = c('mean', 'sd'))
levels(mlt$variable) <- c('Mean', 'SD')
mlt$temp <- factor(mlt$temp)
mlt$temp <- factor(mlt$temp, levels = c( "StratStart", "StratEnd",  "TotStratDur"))

summary(mlt)

mlt$disc <- cut(mlt$value,
                c(seq(-50,40,10), 500),
                include.lowest = T)
levels(mlt$disc) <- c(seq(-45,35,10),'>35')
mlt$disc <- factor(mlt$disc, levels = rev(levels(mlt$disc)))

n1 <- length(unique(mlt[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

cols <- RColorBrewer::brewer.pal(n = 11, name = 'BrBG')
col_fn <- colorRampPalette(rev(cols))

incol <- col_fn(length(levels(mlt$disc)))

p1 <- ggplot(mlt, aes(Year, label))+
  # geom_tile(aes(fill = sd))+
  geom_tile(aes(fill = (disc)))+
  scale_fill_manual(values = rev(incol), name = 'Days', drop = T)+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-4,4), breaks = MakeBreaks(1))+
  facet_wrap(~variable, ncol = 3)+
  facet_grid(temp~variable)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 10),
        legend.key.height = unit(1.2, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)

ggsave('analysis-isimip/plots/all_lake_month_StratStart_StratEnd_TotStratDur_RCP60_year_anomaly+SD_2069-99_v4.png', p1, dpi = 300,width = 210,height = 297, units = 'mm')



# Bin temp
df2 <- df2[!(df2$Lake %in% c('Kivu','Kuivajarvi', 'Burley_Griffin', 'Argyle', 'Mt_Bold', 'Tarawera', 'Rotorua', 'Vortsjarv', 'Mueggelsee', 'Wingra', 'Annie')),]

idx <- which(df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui' & df2$Year < 2097 & !is.na(df2$mean))
summary(df2[idx,1:6])
df2$mean_disc <- cut(df2$mean,
                     c(seq(-50,40,10), 250),
                     include.lowest = T)
levels(df2$mean_disc) <- c(seq(-45,35,10),'>35')
df2$mean_disc <- factor(df2$mean_disc, levels = rev(levels(df2$mean_disc)))

n1 <- length(unique(df2[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

cols <- RColorBrewer::brewer.pal(11, 'PRGn')
col_fn <- colorRampPalette(cols)

incol <- col_fn(length(levels(df2$mean_disc)))
df2$variable <- factor(df2$variable)
df2$variable <- factor(df2$variable, levels = c("StratStart","StratEnd" , "TotStratDur"))
# df2 <- na.exclude(df2)
# levels(df2$variable) <- c("StratEnd" ,"StratStart", "TotStratDur")

p2 <- ggplot(df2[idx,], aes(Year, label))+
  # geom_tile(aes(fill = mean))+
  geom_tile(aes(fill = (mean_disc)))+
  scale_fill_manual(values = (incol), name = 'Anomaly (Days)', drop = F)+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-4,4), breaks = MakeBreaks(1))+
  facet_wrap(~variable, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p2

ggsave('analysis-isimip/plots/all_lake_StratStart_StratEnd_TotStratDur_RCP60_warming_stripe_anomaly_v2.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')

## SD
idx <- which(df2$Scenario == 'RCP6.0' & df2$label != 'FI_Kui' & df2$Year < 2097 & !is.na(df2$mean))
summary(df2[idx,1:6])
df2$sd_disc <- cut(df2$sd,
                     c(0,5,15,25,35, 500),
                     include.lowest = T)
levels(df2$sd_disc) <- c(2.5,10,20,30,'>30')
df2$sd_disc <- factor(df2$sd_disc, levels = rev(levels(df2$sd_disc)))

n1 <- length(unique(df2[idx, 'label']))
lns <- data.frame(x = c(2006, 2100) + 0.5, y = rep(2:n1, each = 2) - 0.5)

cols <- RColorBrewer::brewer.pal(5, 'Blues')
col_fn <- colorRampPalette(cols)

incol <- col_fn(length(levels(df2$sd_disc)))
df2$variable <- factor(df2$variable)
df2$variable <- factor(df2$variable, levels = c("StratStart","StratEnd" , "TotStratDur"))
# df2 <- na.exclude(df2)
# levels(df2$variable) <- c("StratEnd" ,"StratStart", "TotStratDur")

p2 <- ggplot(df2[idx,], aes(Year, label))+
  # geom_tile(aes(fill = sd))+
  geom_tile(aes(fill = (sd_disc)))+
  scale_fill_manual(values = rev(incol), name = 'SD (Days)', drop = F)+
  # scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (\u00B0C)', limits = c(-4,4), breaks = MakeBreaks(1))+
  facet_wrap(~variable, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 12))+
  geom_hline(data = lns, aes(yintercept = y), size = 0.1)
p2

ggsave('analysis-isimip/plots/all_lake_StratStart_StratEnd_TotStratDur_RCP60_warming_stripe_sd_v2.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')





wid <- pivot_wider(df2[idx,], c('Lake', 'Year', 'Scenario', 'depth_class'), names_from = 'variable', values_from = mean)


p1 <- ggplot(wid, aes(StratStart, StratEnd))+
  geom_abline(slope = -1, intercept = 0, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_hex(binwidth = c(2,2))+
  scale_fill_gradientn(colours = rev(cols), name = 'Count')+#, breaks = c(seq(0,140,20)))+
  # geom_point(size = 0.1)+
  guides(colour = F)+
  xlab('Stratification Onset Anomaly (days)')+
  ylab('Stratification Offset Anomaly (days)')+
  coord_equal(xlim = c(-40,20), ylim = c(-20,40))+
  # facet_grid(Scenario~GCM)+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(0.75, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 16, hjust = 0),
        legend.text = element_text(size = 16))
p1
ggsave('analysis-isimip/plots/all_lake_StratStart_StratEnd_RCP60_hex_v2.png', p1, dpi = 300,width = 310,height = 220, units = 'mm')



morph <- read.csv('MetaData/lakes_size_depth_class.csv')

labs <- read.csv('MetaData/lake_labels.csv')
df2 <- merge(df2, labs, by.x = 'Lake', by.y = 'lake')
df2 <- merge(df2, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df2$label <- factor(df2$label)
df2$label <- factor(df2$label, levels = unique(df2$label[order(-df2$mean_depth)]), ordered = T)
df2$variable <- factor(df2$variable)
df2$variable <- factor(df2$variable, levels = levels(df2$variable)[c(2,1,3)])
levels(df2$variable)




df2 <- df2[df2$Lake != 'Kivu',]
idx <- which(df2$Scenario == 'rcp60')
p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (days)', limits = c(-50,50), breaks = MakeBreaks(10))+
  facet_wrap(~variable, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 9), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2
ggsave('analysis-isimip/plots/all_lake_StratStart_StratEnd_TotStratDur_Scenario_warming_stripe_anomaly.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')

p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols), name = 'SD (days)', limits = c(0,50))+
  facet_wrap(~variable, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 9), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2
ggsave('analysis-isimip/plots/all_lake_StratStart_StratEnd_TotStratDur_Scenario_warming_stripe_sd.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')


# Average across lake model & GCM & Lake
idx <- which(df$Lake == 'Kuivajarvi' | (df$Scenario == 'RCP8.5' & df$Year < 2006) | df$anom > 80 | df$Year > 2097)

df3 <- ddply(df[-idx,], .variables = c('Year','Scenario', 'variable'), .fun = function(x){
  mn = mean(x$anom, na.rm = T)
  sd = sd(x$anom, na.rm = T)
  df = data.frame(mean = mn, sd = sd)
  return(df)
}, .progress = 'text')

df3$variable <- factor(df3$variable)
df3$variable <- factor(df3$variable, levels = levels(df3$variable)[c(2,1,3)])
levels(df3$variable) <- c('Stratification onset', 'Stratification offset', 'Stratification duration')

d.cols <- RColorBrewer::brewer.pal(4, 'Dark2')
idx <- which(df3$Year != 2006)
p1 <- ggplot(df3[idx,], aes(Year, mean))+
  geom_hline(yintercept = 0)+
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Scenario), alpha = 0.2)+
  geom_line(aes(colour = Scenario))+
  facet_wrap(~variable, nrow = 3)+
  scale_fill_manual(values = d.cols)+
  scale_colour_manual(values = d.cols)+
  xlab('Year')+
  ylab('Anomaly (Days)')+
  scale_x_continuous(breaks = seq(1960, 2100,20))+
  guides(colour = guide_legend(override.aes = list(size = 1.5)))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  coord_cartesian(ylim = c(-60,60))+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.position = 'right',
        legend.title = element_text(size = 16, hjust = 0),
        legend.text = element_text(size = 16))
p1
ggsave('analysis-isimip/plots/mean_lake-GCM_StratStart_StratEnd_TotStratDur_RCP_line_v2.png', p1, dpi = 300,width = 310,height = 220, units = 'mm')

write.csv(df3[idx,],  "C:\\Users\\mooret\\Dropbox\\ISIMIP_chapter\\tables/all_analyse_strat_Scenario.csv", quote = F, row.names = F)

###

vars <- c( "TsMax", "TbMax")
sub <- out[which(out$variable %in% vars),]

df2 <- ddply(sub, .variables = c('Year','Scenario', 'variable', 'Lake'), .fun = function(x){
  mn = mean(x$anom, na.rm = T)
  sd = sd(x$anom, na.rm = T)
  df = data.frame(mean = mn, sd = sd)
  return(df)
}, .progress = 'text')

# stopCluster(cl)


morph <- read.csv('MetaData/lakes_size_depth_class.csv')

labs <- read.csv('MetaData/lake_labels.csv')
df2 <- merge(df2, labs, by.x = 'Lake', by.y = 'lake')
df2 <- merge(df2, morph, by.x = 'Lake', by.y = 'ISIMIP_name')
df2$label <- factor(df2$label)
df2$label <- factor(df2$label, levels = unique(df2$label[order(-df2$mean_depth)]), ordered = T)
df2$variable <- factor(df2$variable)
df2$variable <- factor(df2$variable, levels = levels(df2$variable)[c(2,1,3)])
levels(df2$variable)




df2 <- df2[df2$Lake != 'Kivu',]
idx <- which(df2$Scenario == 'rcp60')
p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = mean))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols), name = 'Anomaly (degC)', limits = c(-6,6), breaks = MakeBreaks(2))+
  facet_wrap(~variable, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 9), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2
ggsave('analysis-isimip/plots/all_lake_TsMax_TbMin_Scenario_warming_stripe_anomaly.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')


p2 <- ggplot(df2[idx,], aes(Year, label))+
  geom_tile(aes(fill = sd))+
  # geom_tile(data = class[idx2,], aes(fill = mean))+
  scale_fill_gradientn(colours = rev(cols), name = 'SD (degC)', limits = c(0,5), breaks = MakeBreaks(1))+
  facet_wrap(~variable, ncol = 3)+
  xlab('Year')+
  ylab('')+
  scale_y_discrete()+
  scale_x_continuous(breaks = seq(1980, 2100,20))+
  # scale_y_continuous(breaks = seq(-2, 8,1))+
  theme_classic(base_size = 16)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.y = element_text(size = 9), legend.key.height = unit(3, 'cm'), legend.key.width = unit(1, 'cm'))
p2
ggsave('analysis-isimip/plots/all_lake_TsMax_TbMin_RCP60_warming_stripe_sd.png', p2, dpi = 300,width = 310,height = 220, units = 'mm')

