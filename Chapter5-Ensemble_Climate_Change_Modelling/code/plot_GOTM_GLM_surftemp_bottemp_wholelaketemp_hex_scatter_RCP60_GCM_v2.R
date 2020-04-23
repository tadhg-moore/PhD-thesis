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
    df <- ddply(df2[idx,], c('year', 'lake', 'scenario', 'model', 'variable', 'lmodel'), .fun = function(x){
      mean(x$anom)
    })
    
    res_list[[ind]] <- df
    
  }
  
}

out <- do.call('rbind', res_list)
summary(out)
colnames(out) <- c('Year', 'Lake','Scenario', 'GCM',  'variable', 'LakeModel','anom')
levels(out$Scenario) <- c('Historical', 'RCP2.6', 'RCP6.0', 'RCP8.5')
df <- out[!(df$Lake %in% c('Kuivajarvi', 'Kivu')),]


idx <- which(df$variable == 'surftemp')

wid <- tidyr::pivot_wider(df[idx,], id_cols = c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), names_from = 'LakeModel', values_from = 'anom')

cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')

p1 <- ggplot(wid, aes(GLM, GOTM))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_hex(binwidth = c(0.25,0.25))+
  scale_fill_gradientn(colours = rev(cols), name = 'Count')+#, breaks = c(seq(0,140,20)))+
  # geom_point(size = 0.1)+
  guides(colour = F)+
  xlab('GLM Anomaly (\u00B0C)')+
  ylab('GOTM Anomaly (\u00B0C)')+
  coord_equal(xlim = c(-3,8.5), ylim = c(-3,8.5))+
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
ggsave('analysis-isimip/plots/all_lake_surftemp_GOTM_GLM_hex.png', p1, dpi = 300,width = 310,height = 310, units = 'mm')


## bottemp ----
idx <- which(df$variable == 'bottemp')

wid <- tidyr::pivot_wider(df[idx,], id_cols = c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), names_from = 'LakeModel', values_from = 'anom')

cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')
summary(wid)

p1 <- ggplot(wid, aes(GLM, GOTM))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_hex(binwidth = c(0.25,0.25))+
  scale_fill_gradientn(colours = rev(cols), name = 'Count')+#, breaks = c(seq(0,140,20)))+
  # geom_point(size = 0.1)+
  scale_x_continuous(breaks = c(seq(-8,8,2)))+
  scale_y_continuous(breaks = c(seq(-8,8,2)))+
  guides(colour = F)+
  xlab('GLM Anomaly (\u00B0C)')+
  ylab('GOTM Anomaly (\u00B0C)')+
  coord_equal(xlim = c(-5,7), ylim = c(-5,7))+
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
ggsave('analysis-isimip/plots/all_lake_bottemp_GOTM_GLM_hex.png', p1, dpi = 300,width = 310,height = 310, units = 'mm')

## wholelaketemp ----
idx <- which(df$variable == 'wholelaketemp' & !(df$Lake %in% c('Kuivajarvi', 'Kivu')))

wid <- tidyr::pivot_wider(df[idx,], id_cols = c('Year', 'Lake', 'Scenario', 'GCM', 'variable'), names_from = 'LakeModel', values_from = 'anom')

cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')
summary(wid)

p1 <- ggplot(wid, aes(GLM, GOTM))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_hex(binwidth = c(0.25,0.25))+
  scale_fill_gradientn(colours = rev(cols), name = 'Count')+#, breaks = c(seq(0,140,20)))+
  # geom_point(size = 0.1)+
  scale_x_continuous(breaks = c(seq(-8,8,2)))+
  scale_y_continuous(breaks = c(seq(-8,8,2)))+
  guides(colour = F)+
  xlab('GLM Anomaly (\u00B0C)')+
  ylab('GOTM Anomaly (\u00B0C)')+
  coord_equal(xlim = c(-4,7), ylim = c(-4,7))+
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
ggsave('analysis-isimip/plots/all_lake_wholelaketemp_GOTM_GLM_hex.png', p1, dpi = 300,width = 310,height = 310, units = 'mm')


# Check for outliers
wid$dif <- wid$GLM - wid$GOTM

ind <- which(wid$dif > 4)
wid[ind,]
