setwd("G:\\Opt_freq")

library(tidyverse)

l.cols <- RColorBrewer::brewer.pal(8, 'Set2')
d.cols <- RColorBrewer::brewer.pal(8, 'Dark2')

fgh <- read.csv('feeagh/run4/MAE_analysis_v2/start_fcdate_fctime_MAE_Bias_RMSE_status.csv')
ltj <- read.csv('langtjern/run4/MAE_analysis_v2/start_fcdate_fctime_MAE_Bias_RMSE_status.csv')
kin <- read.csv('kinneret/run2/MAE_analysis_v2/start_fcdate_fctime_MAE_Bias_RMSE_status.csv')

fgh2 <- read.csv('feeagh/run4/MAE_analysis_v2/start_fcdate_fdepth_fctime_MAE_Bias_RMSE_status.csv')
ltj2 <- read.csv('langtjern/run4/MAE_analysis_v2/start_fcdate_fdepth_fctime_MAE_Bias_RMSE_status.csv')
kin2 <- read.csv('kinneret/run2/MAE_analysis_v2/start_fcdate_fdepth_fctime_MAE_Bias_RMSE_status.csv')

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

# Make sure all T00
df$start[df$start == 'T0'] <- 'T000'
df$start[df$start == 'T24'] <- 'T024'
df$start <- factor(df$start)
df$start <- factor(df$start, levels = c('T000', 'T024', 'T168', 'T336', 'T672'))

# Reorder factor levels
# df$Lake <- factor(df$Lake, levels = levels(df$Lake)[c(1,3,2)])

# Calculate metalimnion
library(rLakeAnalyzer)
meta_lim <- ddply(df2, c('Lake', 'fc_date'), function(x){
  # print(x$fc_date[1])
  x$dup <- paste0(x$fc_date,x$fdepth)
  dup <- duplicated(x$dup)
  x <- x[!dup,]
  temp <- x$obs
  deps <- abs(x$fdepth)
  md <- meta.depths(temp,deps)
  return(md)
})

meta_lim$fc_date <- as.POSIXct(as.character(meta_lim$fc_date))
meta_lim$month <- month(meta_lim$fc_date)
ddply(meta_lim, 'Lake',nrow)
ddply(meta_lim, 'Lake', function(x){
  idx = which(!is.nan(x[,4]) & x$month %in% c(4:10))
  length(idx)
})

p1 <- ggplot(meta_lim, aes(fc_date, V1))+
  geom_line(aes(colour = 'Epilimnion'))+
  geom_line(aes(fc_date, V2, colour = 'Hypolimnion'))+
  facet_wrap(~Lake, scales = 'free', nrow = 3)+
  scale_y_reverse()+
  theme_classic()
p1

# Remove Inv Strat
df[(df$status == 'Inverse Stratified'),5:7] <- NA
df2[(df2$status == 'Inverse Stratified'),3:4] <- NA
df$status <- factor(df$status)
df2$status <- factor(df2$status)

#
meta_lim$month <- month(meta_lim$fc_date)
idx <- which(meta_lim$month %in% 4:9)
meta <- ddply(meta_lim[idx,], c('Lake'), function(x){
  data.frame(top = mean(x$V1, na.rm = T), btm = mean(x$V2, na.rm = T))
})
meta$btm[1] <- 15.6
meta

df$fc_date <- as.character(df$fc_date)
meta_lim$fc_date <- as.character(meta_lim$fc_date)
df$Lake <- as.character(df$Lake)
df$status2 <- NA

tst <- ldply(1:nrow(df), function(x){
  # tst <- llply(1:nrow(df), function(x){
    
  idx <- which(meta_lim$Lake == df$Lake[x] & meta_lim$fc_date == df$fc_date[x])
  if(length(idx) == 0){
    status <- NA
    return(status)
  }
  
  ref <- meta_lim[idx,]
  # print(x)
  
  if(is.nan(ref[1,3])){
    status <- 'Isothermal'
  }else if(df$fdepth[x] > -ref[1,3]){
    status <- 'Strat Epi'
  }else if(df$fdepth[x] < -ref[1,4]){
    status <- 'Strat Hyp'
  }else{
    status <- 'Stratified'
  }
  return(status)
  
  
})
df$status2 <- unlist(tst)
df$Lake <- factor(df$Lake)
df$status2 <- factor(df$status2)
write.csv(df, 'tables/df.csv')


tst <- ldply(1:nrow(df2), function(x){
  # tst <- llply(1:nrow(df), function(x){
  
  idx <- which(meta_lim$Lake == df2$Lake[x] & meta_lim$fc_date == df2$fc_date[x])
  if(length(idx) == 0){
    status <- NA
    return(status)
  }
  
  ref <- meta_lim[idx,]
  # print(x)
  
  if(is.nan(ref[1,3])){
    status <- 'Isothermal'
  }else if(df$fdepth[x] > -ref[1,3]){
    status <- 'Strat Epi'
  }else if(df$fdepth[x] < -ref[1,4]){
    status <- 'Strat Hyp'
  }else{
    status <- 'Stratified'
  }
  return(status)
  
  
})
df2$status2 <- unlist(tst)
df2$Lake <- factor(df2$Lake)
df2$status2 <- factor(df2$status2)

write.csv(df2, 'tables/df2.csv')


# 
# 
# df$status2 <- as.character(df$status)
# df$status2[df$fdepth > -2 & df$status == 'Stratified'] <- 'Strat Epi'
# df$status2[df$fdepth < -30 & df$status == 'Stratified'] <- 'Strat Hyp'
# df$status2[df$fdepth < -4 & df$Lake == 'Langtjern' & df$status == 'Stratified'] <- 'Strat Hyp'
# df$status2 <- factor(df$status2)
# 
# df2$status2 <- as.character(df2$status)
# df2$status2[df2$fdepth > -2 & df2$status == 'Stratified'] <- 'Strat Epi'
# df2$status2[df2$fdepth < -30 & df2$status == 'Stratified'] <- 'Strat Hyp'
# df2$status2[df2$fdepth < -4 & df2$Lake == 'Langtjern' & df2$status == 'Stratified'] <- 'Strat Hyp'
# df2$status2 <- factor(df2$status2)


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
  surfmax <- max(x$obs[x$fdepth == surf], na.rm = T)
  maxsurfday <- x$fc_date[which(x$obs == surfmax)][1]
  surfmin <- min(x$obs[x$fdepth == surf], na.rm = T)
  
  bott <- min(x$fdepth)
  bottmx <- max(x$obs[x$fdepth == bott], na.rm = T)
  maxbottday <- x$fc_date[which(x$obs == bottmx)][1]
  bottmin <- min(x$obs[x$fdepth == bott], na.rm = T)
  
  df <- data.frame(surf, surfmin, surfmax, maxsurfday, bott, bottmx, maxbottday, bottmin)
})
tab[,c(3,4,7,9)] <- round(tab[,c(3,4,7,9)], 2)
write.csv(tab, 'tables/obs_max_min_suf_bott_day.csv', quote = F, row.names = F)

tab2 <- ddply(df2, c('Lake', 'status'), function(x){
  surf <- max(x$fdepth)
  surfmax <- max(x$obs[x$fdepth == surf], na.rm = T)
  maxsurfday <- x$fc_date[which(x$obs == surfmax)][1]
  surfmin <- min(x$obs[x$fdepth == surf], na.rm = T)
  
  bott <- min(x$fdepth)
  bottmx <- max(x$obs[x$fdepth == bott], na.rm = T)
  bottmn <- mean(x$obs[x$fdepth == bott], na.rm = T)
  maxbottday <- x$fc_date[which(x$obs == bottmx)][1]
  bottmin <- min(x$obs[x$fdepth == bott], na.rm = T)
  bottsd <- sd(x$obs[x$fdepth == bott], na.rm = T)
  
  stemp <- x$obs[x$fdepth == surf]
  btemp <- x$obs[x$fdepth == bott]
  dif <- stemp - btemp
  mn_dif <- mean(dif, na.rm = T)
  sd_dif <- sd(dif, na.rm = T)
  mx_dif <- max(dif, na.rm = T)
  maxdifday <- x$fc_date[which(dif == mx_dif)][1]
  
  
  df <- data.frame(surf, surfmin, surfmax, maxsurfday, bott, bottmx, maxbottday, bottmin,bottmn, bottsd, mn_dif, sd_dif, mx_dif, maxdifday)
})
tab2[,c(4,5,8,10:15)] <- round(tab2[,c(4,5,8,10:15)], 2)
write.csv(tab2, 'tables/obs_max_min_suf_bott_status_diff.csv', quote = F, row.names = F)



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
dat$Lake <- factor(dat$Lake, levels = levels(dat$Lake)[c(1,3,2)])

p1 <- ggplot(dat[dat$status2 != 'Stratified',], aes(fc_time, MAE))+
  geom_line(aes(colour = start))+
  geom_hline(yintercept = 0)+
  geom_point(aes(colour = start))+
  geom_line(data = dat2[dat2$status2 != 'Stratified',],aes(fc_time, MAE, colour = start))+
  geom_point(data = dat2[dat2$status2 != 'Stratified',],aes(fc_time, MAE, colour = start))+
  # scale_y_log10(limits = c(0.001, 2.5))+
  # annotation_logticks(sides = "l")+
  scale_x_continuous(breaks = seq(2,14,2))+
  xlab('Forecast days')+
  ylab('Mean Absolute Error (°C)')+
  # facet_wrap(~status2*Lake)+
  facet_grid(Lake~status2)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = NULL, linetype = NA), title = 'Initialization'), fill = F)+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig6b_MAE_v3.png", p1, dpi = 300,width = 384,height = 280, units = 'mm')
tst <- rbind(dat, dat2)
out <- ddply(tst, c('Lake', 'status2', 'start'), function(x){
  round(data.frame(min = min(x$MAE),
                   median = median(x$MAE),
                   mean = mean(x$MAE),
                   sd = sd(x$MAE),
                   max = max(x$MAE)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig6b_MAE_v2.csv", quote = F, row.names = F)

# FC time Bias ----
p1 <- ggplot(dat[dat$status2 != 'Stratified',], aes(fc_time, Bias))+
  geom_line(aes(colour = start))+
  geom_hline(yintercept = 0)+
  geom_point(aes(colour = start))+
  geom_line(data = dat2[dat2$status2 != 'Stratified',],aes(fc_time, Bias, colour = start))+
  geom_point(data = dat2[dat2$status2 != 'Stratified',],aes(fc_time, Bias, colour = start))+
  # scale_y_log10(limits = c(0.001, 2.5))+
  # annotation_logticks(sides = "l")+
  xlab('')+
  ylab('Bias (°C)')+
  # facet_wrap(~status2*Lake)+
  facet_grid(Lake~status2)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = NULL, linetype = NA), title = 'Initialization'), fill = F)+
  theme_classic(base_size = 22)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig6b_Bias_v3.png", p1, dpi = 300,width = 384,height = 280, units = 'mm')
tst <- rbind(dat, dat2)
out <- ddply(tst, c('Lake', 'status2', 'start'), function(x){
  round(data.frame(min = min(x$Bias),
                   median = median(x$Bias),
                   mean = mean(x$Bias),
                   sd = sd(x$Bias),
                   max = max(x$Bias)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig6b_Bias_v2.csv", quote = F, row.names = F)



# Aggregate for plot
n <- ddply(df, c('Lake', 'status', 'start','fdepth'), function(x)length(unique(x$init_date)))
n[n$start == 'T000',]


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
meta$status <- 'Stratified'
meta$top <- -meta$top
meta$btm <- -meta$btm

# Profile MAE ----
p1 <- ggplot(dat, aes(MAE, fdepth, colour = start))+
  geom_vline(xintercept = 0)+
  geom_hline(data = meta, aes(yintercept = top), linetype = 'dashed')+
  geom_hline(data = meta, aes(yintercept = btm), linetype = 'dashed')+
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
  # scale_x_log10()+
  # annotation_logticks(sides = "b")+
  theme_classic(base_size = 25)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig7b_MAE_v3.png", p1, dpi = 300,width = 284,height = 380, units = 'mm')

tst <- rbind(dat, dat2)
out <- ddply(tst, c('Lake', 'status', 'start'), function(x){
  round(data.frame(min = min(x$MAE),
             median = median(x$MAE),
             mean = mean(x$MAE),
             sd = sd(x$MAE),
             max = max(x$MAE)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig7b_MAE.csv", quote = F, row.names = F)

## Bias ----
p2 <- ggplot(dat, aes(Bias, fdepth, colour = start))+
  geom_vline(xintercept = 0)+
  geom_hline(data = meta, aes(yintercept = top), linetype = 'dashed')+
  geom_hline(data = meta, aes(yintercept = btm), linetype = 'dashed')+
  geom_path()+
  geom_point()+
  geom_path(data = dat2, aes(Bias,fdepth, colour = start))+
  geom_point(data = dat2, aes(Bias,fdepth, colour = start))+
  ylab('Depth (m)')+
  xlab('Bias (°C)')+
  # facet_wrap(~lake*status, nrow = 3, scales = 'free_y')+
  facet_grid(Lake~status, scales = 'free_y')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = NULL, linetype = NULL), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  # scale_x_log10()+
  # annotation_logticks(sides = "b")+
  theme_classic(base_size = 25)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p2
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig7b_Bias_v3.png", p2, dpi = 300,width = 284,height = 380, units = 'mm')

ddply(dat, c('Lake', 'status'), nrow)

g1 <- ggarrange(p1, p2, ncol = 2, align = 'h', common.legend = T, legend = 'right')
g1

tst <- rbind(dat, dat2)
out <- ddply(tst, c('Lake', 'status', 'start'), function(x){
  round(data.frame(min = min(x$Bias),
                   median = median(x$Bias),
                   mean = mean(x$Bias),
                   sd = sd(x$Bias),
                   max = max(x$Bias)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig7b_Bias.csv", quote = F, row.names = F)






df2$MAE <-  apply(df2[,3:4], 1, function(x)mean(abs(x[1] - x[2]), na.rm = T)) 
df2$Bias <-  apply(df2[,3:4], 1, function(x)mean((x[1] - x[2]), na.rm = T)) 

idx <- which(df$status %in% c( "Isothermal", 'Stratified') )
idx2 <- which(df2$status2 %in% c( "Isothermal", 'Stratified') )
dfb <- df[idx,]
dfb$status2 <- factor(dfb$status2)
df2b <- df2[idx2,]
df2b$status2 <- factor(df2b$status2)

# Density distributions - MAE ----
p1 <- ggplot(dfb, aes(x = MAE))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_density(aes(colour = start),size = 1)+
  geom_density(data = df2b, aes(x = MAE, colour = start), size = 1)+
  xlab('')+
  facet_grid(Lake~status, scales = 'free_y')+
  # facet_wrap(~Lake*status2, scales = 'free_y', nrow =  3)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 21, fill = d.cols[3:8]), title = 'Initialization'))+
  # coord_cartesian(xlim = c(0.001,4))+
  scale_colour_manual(values = d.cols[3:8])+
  # scale_x_log10(breaks = c(0.001,0.01,0.1,1,4))+
  # annotation_logticks(sides = "b")+
  ylab('Density')+
  xlab('Mean Absolute Error (°C)')+
  theme_classic(base_size = 25)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig8b_MAE_v2.png", p1, dpi = 300,width = 384,height = 280, units = 'mm')

tst <- rbind(dfb[,c('start', 'MAE', 'status', 'Lake')], df2b[, c('start', 'MAE', 'status', 'Lake')])
out <- ddply(tst, c('Lake', 'status', 'start'), function(x){
  round(data.frame(min = min(x$MAE, na.rm = T),
                   median = median(x$MAE, na.rm = T),
                   mean = mean(x$MAE, na.rm = T),
                   sd = sd(x$MAE, na.rm = T),
                   max = max(x$MAE, na.rm = T)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig8b_MAE_v2.csv", quote = F, row.names = F)


# Improvement ----
improv <- ddply(out, c('Lake', 'status', 'start'), function(x){
  ref <- out[(out$Lake == x$Lake[1] & out$status == x$status[1] & out$start == 'NDA'),]
  mn_ref <- mean(ref$mean)
  MAE <- mean(x$mean)
  
  data.frame(improv = round(100*(mn_ref - MAE)/mn_ref, 2), MAE = MAE, NDA = mn_ref)
  # print(ref)
})
improv <- improv[improv$start != 'NDA',]
write.csv(improv, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/table4.csv", quote = F, row.names = F)
ggplot(improv, aes(start, improv, colour = status, group = status))+
  geom_hline(yintercept = 0)+
  geom_point()+
  geom_line()+
  facet_wrap(~Lake, nrow = 1)+
  theme_classic()

out <- ddply(tst, c('Lake','start'), function(x){
  round(data.frame(min = min(x$MAE, na.rm = T),
                   median = median(x$MAE, na.rm = T),
                   mean = mean(x$MAE, na.rm = T),
                   sd = sd(x$MAE, na.rm = T),
                   max = max(x$MAE, na.rm = T)), 2)
})
improv <- ddply(out, c('Lake', 'start'), function(x){
  ref <- out[(out$Lake == x$Lake[1]  & out$start == 'NDA'),]
  mn_ref <- mean(ref$mean)
  MAE <- mean(x$mean)
  
  data.frame(improv = round(100*(mn_ref - MAE)/mn_ref, 2), MAE = MAE, NDA = mn_ref)
  # print(ref)
})
improv <- improv[improv$start != 'NDA',]
improv
write.csv(improv, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/table4_b.csv", quote = F, row.names = F)



# Density distributions - Bias ----
p1 <- ggplot(dfb, aes(x = Bias))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_density(aes(colour = start),size = 1)+
  geom_density(data = df2b, aes(x = Bias, colour = start), size = 1)+
  xlab('')+
  facet_grid(Lake~status2, scales = 'free_y')+
  # facet_wrap(~Lake*status2, scales = 'free_y', nrow =  3)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 22, fill = d.cols[3:8]), title = 'Initialization'))+
  # coord_cartesian(xlim = c(0.001,4))+
  scale_colour_manual(values = d.cols[3:8])+
  # scale_x_log10(breaks = c(0.001,0.01,0.1,1,4))+
  # annotation_logticks(sides = "b")+
  ylab('Density')+
  xlab('Mean Absolute Error (°C)')+
  theme_classic(base_size = 25)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig8b_Bias_v2.png", p1, dpi = 300,width = 384,height = 280, units = 'mm')

tst <- rbind(dfb[,c('start', 'Bias', 'status2', 'Lake')], df2b[, c('start', 'Bias', 'status2', 'Lake')])
out <- ddply(tst, c('Lake', 'status2', 'start'), function(x){
  round(data.frame(min = min(x$Bias, na.rm = T),
                   median = median(x$Bias, na.rm = T),
                   mean = mean(x$Bias, na.rm = T),
                   sd = sd(x$Bias, na.rm = T),
                   max = max(x$Bias, na.rm = T)), 2)
})
write.csv(out, "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig8b_Bias_v2.csv", quote = F, row.names = F)


wid <- pivot_wider(improv, id_cols = c(Lake, start), names_from = status, values_from = c(improv, MAE, NDA))

