setwd("G:\\Opt_freq")

library(tidyverse)

l.cols <- RColorBrewer::brewer.pal(8, 'Set2')
d.cols <- RColorBrewer::brewer.pal(8, 'Dark2')

fgh <- read.csv('feeagh/run4/MAE_analysis/start_fcdate_fctime_MAE_Bias_RMSE_status.csv')
ltj <- read.csv('langtjern/run4/MAE_analysis/start_fcdate_fctime_MAE_Bias_RMSE_status.csv')
kin <- read.csv('kinneret/run2/MAE_analysis/start_fcdate_fctime_MAE_Bias_RMSE_status.csv')

fgh2 <- read.csv('feeagh/run4/MAE_analysis/start_fcdate_fdepth_fctime_MAE_Bias_RMSE_status.csv')
ltj2 <- read.csv('langtjern/run4/MAE_analysis/start_fcdate_fdepth_fctime_MAE_Bias_RMSE_status.csv')
kin2 <- read.csv('kinneret/run2/MAE_analysis/start_fcdate_fdepth_fctime_MAE_Bias_RMSE_status.csv')

cnams <- c("fc_date","fdepth","mod", "obs","status", "init_date","fc_time","start","Lake" )

# Lims
library(tidyverse)
f.stralims <- read_csv('feeagh/run4/MAE_analysis/stralims.csv')
f.isolims <- read_csv('feeagh/run4/MAE_analysis/isolims.csv')
l.stralims <- read_csv('langtjern/run4/MAE_analysis/stralims.csv')
l.isolims <- read_csv('langtjern/run4/MAE_analysis/isolims.csv')
l.invlims <- read_csv('langtjern/run4/MAE_analysis/invlims.csv')
k.stralims <- read_csv('kinneret/run2/MAE_analysis/stralims.csv')
k.isolims <- read_csv('kinneret/run2/MAE_analysis/isolims.csv')

df <- rbind.data.frame(fgh, ltj, kin)
df2 <- rbind.data.frame(fgh2[,cnams], ltj2[,cnams], kin2[,cnams])
df$init_date <- as.POSIXct(df$init_date)
df2$init_date <- as.POSIXct(df2$init_date)

# Remove Inv Strat
df[(df$status == 'Inverse Stratified'),5:7] <- NA
df2[(df2$status == 'Inverse Stratified'),3:4] <- NA

# 
df$status2 <- as.character(df$status)
df$status2[df$fdepth > -2 & df$status == 'Stratified'] <- 'Strat Epi'
df$status2[df$fdepth < -30 & df$status == 'Stratified'] <- 'Strat Hyp'
df$status2[df$fdepth < -4 & df$Lake == 'Langtjern' & df$status == 'Stratified'] <- 'Strat Hyp'
df$status2 <- factor(df$status2)

df2$status2 <- as.character(df2$status)
df2$status2[df2$fdepth > -2 & df2$status == 'Stratified'] <- 'Strat Epi'
df2$status2[df2$fdepth < -30 & df2$status == 'Stratified'] <- 'Strat Hyp'
df2$status2[df2$fdepth < -4 & df2$Lake == 'Langtjern' & df2$status == 'Stratified'] <- 'Strat Hyp'
df2$status2 <- factor(df2$status2)


# Table 2
tab <- ddply(df2, c('Lake'), function(x){
  df <- data.frame(r2 = cor(na.exclude(x)$obs, na.exclude(x)$mod),
                   RMSE = sqrt(mean((x$mod - x$obs)^2, na.rm = T)),
                   MAE = mean(abs(x$mod - x$obs), na.rm = T),
                   Bias = mean((x$mod - x$obs), na.rm = T),
                   NSE = hydroGOF::NSE(sim = x$mod, x$obs, na.rm = T))
})
tab[,-1] <- round(tab[,-1],2)
write.csv(tab, 'tables/all_model_MAE_RMSE_NSE.csv', quote = F, row.names = F)

tab2b <- ddply(df2, c('Lake', 'status'), function(x){
  df <- data.frame(r2 = cor(na.exclude(x)$obs, na.exclude(x)$mod),
                   RMSE = sqrt(mean((x$mod - x$obs)^2, na.rm = T)),
                   MAE = mean(abs(x$mod - x$obs), na.rm = T),
                   Bias = mean((x$mod - x$obs), na.rm = T),
                   NSE = hydroGOF::NSE(sim = x$mod, x$obs, na.rm = T))
})
tab2b[-c(1,2)] <- round(tab2b[,-c(1,2)],2)
write.csv(na.exclude(tab2b), 'tables/MAE_table2_Iso_strat_RMSE_MAE_NSE.csv', quote = F, row.names = F)

tab2 <- ddply(df2, c('Lake', 'status2'), function(x){
  df <- data.frame(r2 = cor(na.exclude(x)$obs, na.exclude(x)$mod),
                   RMSE = sqrt(mean((x$mod - x$obs)^2, na.rm = T)),
                   MAE = mean(abs(x$mod - x$obs), na.rm = T),
                   Bias = mean((x$mod - x$obs), na.rm = T),
                   NSE = hydroGOF::NSE(sim = x$mod, x$obs, na.rm = T))
})
tab2[-c(1,2)] <- round(tab2[,-c(1,2)],2)
write.csv(na.exclude(tab2), 'tables/MAE_table2_Iso_strat_Epi_Hypo_RMSE_MAE_NSE.csv', quote = F, row.names = F)

# Table 3
tab <- ddply(df2, c('Lake'), function(x){
  surf <- max(x$fdepth)
  surfmax <- max(x$obs[x$fdepth == surf])
  maxsurfday <- x$fc_date[which(x$obs == surfmax)]
  surfmin <- min(x$obs[x$fdepth == surf])
  
  bott <- min(x$fdepth)
  bottmx <- max(x$obs[x$fdepth == bott])
  maxbottday <- x$fc_date[which(x$obs == bottmx)]
  bottmin <- min(x$obs[x$fdepth == bott])
  
  df <- data.frame(surf, surfmin, surfmax, maxsurfday, bott, bottmx, maxbottday, bottmin)
})



# Aggregate for plot
dat <- ddply(df, c('Lake', 'start', 'status2', 'fc_time'), function(x){
  data.frame(MAE = mean(x$MAE, na.rm = T),
             Bias = mean(x$Bias, na.rm = T))
})
dat2 <- ddply(df2, c('Lake', 'start', 'status2', 'fc_time'), function(x){
  
  # if(sum(is.na(x$obs))>0)return(data.frame(MAE = NA))
  data.frame(MAE = mean(abs(x$mod - x$obs), na.rm = T),
             Bias = mean((x$mod - x$obs), na.rm = T))
})
dat <- na.exclude(dat)
dat$status2 <- factor(dat$status2)
dat2 <- na.exclude(dat2)
dat2$status2 <- factor(dat2$status2)
dat$MAE[dat$MAE <= 0.001] <- 0.001

p1 <- ggplot(dat[dat$status2 != 'Stratified',], aes(fc_time, MAE))+
  geom_line(aes(colour = start))+
  geom_hline(yintercept = 0)+
  geom_point(aes(colour = start))+
  geom_line(data = dat2[dat2$status2 != 'Stratified',],aes(fc_time, MAE, colour = start))+
  geom_point(data = dat2[dat2$status2 != 'Stratified',],aes(fc_time, MAE, colour = start))+
  scale_y_log10(limits = c(0.001, 2.5))+
  annotation_logticks(sides = "l")+
  xlab('')+
  ylab('Mean Absolute Error (°C)')+
  # facet_wrap(~status2*Lake)+
  facet_grid(Lake~status2)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = NULL, linetype = NA), title = 'Initialization'), fill = F)+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig6b_MAE.png", p1, dpi = 300,width = 384,height = 280, units = 'mm')
tst <- rbind(dat, dat2)
out <- ddply(tst, c('Lake', 'status2', 'start'), function(x){
  round(data.frame(min = min(x$MAE),
                   median = median(x$MAE),
                   mean = mean(x$MAE),
                   sd = sd(x$MAE),
                   max = max(x$MAE)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig6b_MAE.csv")



# Aggregate for plot
dat <- ddply(df, c('Lake', 'start', 'status', 'fdepth'), function(x){
  data.frame(MAE = mean(x$MAE, na.rm = T),
             Bias = mean(x$Bias, na.rm = T))
})
dat2 <- ddply(df2, c('Lake', 'start', 'status', 'fdepth'), function(x){
  
  # if(sum(is.na(x$obs))>0)return(data.frame(MAE = NA))
  data.frame(MAE = mean(abs(x$mod - x$obs), na.rm = T),
             Bias = mean((x$mod - x$obs), na.rm = T))
})
dat <- na.exclude(dat)
# dat$status2 <- factor(dat$status2)
dat2 <- na.exclude(dat2)
# dat2$status2 <- factor(dat2$status2)

p1 <- ggplot(dat, aes(MAE, fdepth, colour = start))+
  geom_path()+
  geom_point()+
  geom_path(data = dat2, aes(MAE,fdepth, colour = start))+
  geom_point(data = dat2, aes(MAE,fdepth, colour = start))+
  ylab('Depth (m)')+
  xlab('Mean Absolute Error (°C)')+
  # facet_wrap(~lake*status, nrow = 3, scales = 'free_y')+
  facet_grid(Lake~status, scales = 'free_y')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = NULL, linetype = NULL), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  scale_x_log10()+
  annotation_logticks(sides = "b")+
  theme_classic(base_size = 25)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig7b_MAE.png", p1, dpi = 300,width = 284,height = 380, units = 'mm')

tst <- rbind(dat, dat2)
out <- ddply(tst, c('Lake', 'status', 'start'), function(x){
  round(data.frame(min = min(x$MAE),
             median = median(x$MAE),
             mean = mean(x$MAE),
             sd = sd(x$MAE),
             max = max(x$MAE)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig7b_MAE.csv")

df2$MAE <-  apply(df2[,3:4], 1, function(x)mean(abs(x[1] - x[2]), na.rm = T)) 
df2$Bias <-  apply(df2[,3:4], 1, function(x)mean((x[1] - x[2]), na.rm = T)) 

idx <- which(df$status2 %in% c( "Inverse Stratified", 'Stratified') )
idx2 <- which(df2$status2 %in% c( "Inverse Stratified", 'Stratified') )
dfb <- df[-idx,]
dfb$status2 <- factor(dfb$status2)
df2b <- df2[-idx2,]
df2b$status2 <- factor(df2b$status2)

p1 <- ggplot(dfb, aes(x = MAE))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_density(aes(colour = start),size = 1)+
  geom_density(data = df2b, aes(x = MAE, colour = start), size = 1)+
  xlab('')+
  facet_grid(Lake~status2, scales = 'free_y')+
  # facet_wrap(~Lake*status2, scales = 'free_y', nrow =  3)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 21, fill = d.cols[3:8]), title = 'Initialization'))+
  coord_cartesian(xlim = c(0.001,4))+
  scale_colour_manual(values = d.cols[3:8])+
  scale_x_log10(breaks = c(0.001,0.01,0.1,1,4))+
  annotation_logticks(sides = "b")+
  ylab('Density')+
  xlab('Mean Absolute Error (°C)')+
  theme_classic(base_size = 25)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig8b_MAE.png", p1, dpi = 300,width = 384,height = 280, units = 'mm')

tst <- rbind(dfb[,c('start', 'MAE', 'status2', 'Lake')], df2b[, c('start', 'MAE', 'status2', 'Lake')])
out <- ddply(tst, c('Lake', 'status2', 'start'), function(x){
  round(data.frame(min = min(x$MAE, na.rm = T),
                   median = median(x$MAE, na.rm = T),
                   mean = mean(x$MAE, na.rm = T),
                   sd = sd(x$MAE, na.rm = T),
                   max = max(x$MAE, na.rm = T)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig8b_MAE.csv")
