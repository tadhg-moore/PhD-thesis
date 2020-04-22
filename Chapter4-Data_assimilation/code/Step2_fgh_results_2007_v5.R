setwd( "G:\\Opt_freq\\feeagh")

library(ggplot2)
library(hydroGOF)
library(forcats)
library(RColorBrewer)
library(lubridate)
library(tidyr)
library(verification)
library(plyr)
library(gotmtools)
library(reshape)

dir <- 'run4/'
plot_dir <- file.path(dir, 'fig')
res_dir <- file.path(dir, 'analysis')
tab_dir <- file.path(dir, 'tables')
dir.create(res_dir)
dir.create(plot_dir)
dir.create(tab_dir)

l.cols <- RColorBrewer::brewer.pal(8, 'Set2')
d.cols <- RColorBrewer::brewer.pal(8, 'Dark2')
dramp <- colorRampPalette(RColorBrewer::brewer.pal(8, 'Dark2'))
display.brewer.all(colorblindFriendly = T)

fils <- list.files(dir)[grep('v1_20', list.files(dir))]
nam = strsplit(fils[1],'_')[[1]]
fc_start = as.POSIXct(paste(nam[5], nam[6], strsplit(nam[7], '.csv'), sep = '-'), tz = 'UTC')
nam = strsplit(fils[length(fils)],'_')[[1]]
fcend = as.POSIXct(paste(nam[5], nam[6], strsplit(nam[7], '.csv'), sep = '-'), tz = 'UTC') + 15*24*60*60

#Seasons
win <- c(12,1,2)
spr <- c(3,4,5)
sum <- c(6,7,8)
aut <- c(9,10,11)

#Load in observed Lake data
obs.file = 'C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\feeagh\\acpy/Lev3_wtemp/obs_1hr_lev3_adj.dat'
obs_wtemp = load_obs(obs.file)
obs_wtemp <- obs_wtemp[(obs_wtemp$date >= fc_start & obs_wtemp$date < (fcend)),]
colnames(obs_wtemp)[3] <- 'obs'
obs_wtemp$fdepth <- factor(obs_wtemp$depths)
obs_wtemp$fdepth <- fct_rev(obs_wtemp$fdepth)
pfgh <- ggplot(obs_wtemp, aes(date, obs, colour = fdepth))+
  # ggtitle(main) + 
  ylab('Temperature (°C)') +
  geom_line()+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depths'), fill = guide_legend(title = 'Status'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%b-%y'), date_breaks = '2 month')+
  # coord_cartesian(xlim = range(res6c$init_date))+
  scale_colour_manual(values = dramp(13))+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  xlab('')+
  theme_bw(base_size = 14)
pfgh
# obs_wtemp$month <- month(obs_wtemp$date)
# obs_wtemp$seas <- NA
# obs_wtemp$seas[(obs_wtemp$month %in% win)] <- 'win'
# obs_wtemp$seas[(obs_wtemp$month %in% spr)] <- 'spr'
# obs_wtemp$seas[(obs_wtemp$month %in% sum)] <- 'sum'
# obs_wtemp$seas[(obs_wtemp$month %in% aut)] <- 'aut'
# obs_wtemp$seas <- factor(obs_wtemp$seas)
# obs_wtemp$seas <- factor(obs_wtemp$seas, levels = c('spr', 'sum', 'aut', 'win'), labels = c('Spring', 'Summer', 'Autumn', 'Winter'))

#Classify lake status
#Density
obs_wtemp$dens <- water.density(obs_wtemp$obs)
status <- ddply(obs_wtemp, c('date'), function(x){
  dif = x$dens[which.max(x$depths)]  - x$dens[which.min(x$depths)]
  tmp = x$obs[which.max(x$depths)]
  if(dif < -0.05 & tmp > 4){
    sts = 'Stratified'
  }else if(dif < -0.05 & tmp < 4){
    sts = 'Inverse stratified'
  }else{
    sts = 'Isothermal'
  }
  return(c(sts, dif))
})
obs_wtemp$dens <- NULL

#Temperature
# status <- ddply(obs_wtemp, c('date'), function(x){
#   dif = x$obs[which.max(x$depths)]  - x$obs[which.min(x$depths)]
#   if(dif > 1){
#     sts = 'Stratified'
#   }else if(dif < -1){
#     sts = 'Inverse stratified'
#   }else{
#     sts = 'Isothermal'
#   }
#   return(sts)
# })
colnames(status)[2] <- 'status'
status$status <- factor(status$status)
ggplot(status, aes(date, status))+
  geom_point()

iso <- status[status$status == 'Isothermal',]
s <- split(iso$date, cumsum(c(TRUE, diff(iso$date) != 1))) #Split into consecutive years
isolims <- as.data.frame(do.call(rbind, lapply(s, function(x)return(c(min(x), max(x)))))) #Horribly messy, but dataframe of xmin and xmax from min and max in sequential data
isolims[,1] <- as.POSIXct(isolims[,1], origin = '1970-01-01', tx = 'UTC')
isolims[,2] <- as.POSIXct(isolims[,2], origin = '1970-01-01', tx = 'UTC')
colnames(isolims) <- c('xmin', 'xmax') #Rename colnames

stra <- status[status$status == 'Stratified',]
s <- split(stra$date, cumsum(c(TRUE, diff(stra$date) != 1))) #Split into consecutive years
stralims <- as.data.frame(do.call(rbind, lapply(s, function(x)return(c(min(x), max(x)))))) #Horribly messy, but dataframe of xmin and xmax from min and max in sequential data
stralims[,1] <- as.POSIXct(stralims[,1], origin = '1970-01-01', tx = 'UTC')
stralims[,2] <- as.POSIXct(stralims[,2], origin = '1970-01-01', tx = 'UTC')
colnames(stralims) <- c('xmin', 'xmax') #Rename colnames

write.csv(isolims, file.path(res_dir, 'isolims.csv'), quote = F, row.names = F)
write.csv(stralims, file.path(res_dir, 'stralims.csv'), quote = F, row.names = F)

for(i in fils){
  wtemp <- read.csv(file.path(dir,i), stringsAsFactors = F)
  wtemp$start[wtemp$start == 'T0'] <- 'T000'
  wtemp$start[wtemp$start == 'T24'] <- 'T024'
  wtemp$start <- factor(wtemp$start)
  nas <- sum(is.na(wtemp$mod))
  if(nas > 0){
    message('Incomplete forecast ', i)
    write(i, file = 'run4/analysis/fc_files_incomplete.txt', append = T)
    next
    # print(wtemp[is.na(wtemp$mod),])
    # wtemp <- na.exclude(wtemp)
  }
  summ = summary(wtemp$start)
  if(sum(duplicated(summ)) != 4){
    message('Unequal number of forecasts in ', i)
    write(i, file = 'run4/analysis/fc_files_incomplete.txt', append = T)
    next
  }
  
  
  colnames(wtemp)[5] <- 'mod'
  
  wtemp$init_date <- as.POSIXct(wtemp$init_date, tz = 'UTC')
  wtemp$fc_date <- as.POSIXct(wtemp$fc_date, tz = 'UTC')
  # wtemp$obs <- wtemp$mod - wtemp$resid
  # wtemp$rmse <- sqrt(wtemp$resid^2)
  wtemp$par_id <- factor(wtemp$par_id)
  wtemp$fdepth <- fct_rev(factor(wtemp$depths))
  wtemp$resid <- NULL
  # wtemp$mod[is.na(wtemp$mod)]
  
  # x = wtemp[(wtemp$start == 'T672' & wtemp$fdepth == '-1.5'),]
  res2 <- ddply(wtemp, c('start', 'fdepth'), function(x){
    #Remove duplicates
    x$dup <- paste(x$fc_date, x$depths, x$par_id, sep ='_')
    dup_ind <- which(duplicated(x$dup))
    if(length(dup_ind) > 0){
      x <- x[-c(dup_ind),]
    }
    x <- x[,c("start", "init_date", "fc_date", "depths", "mod", "par_id", "fdepth")]
    ens = spread(x, par_id, mod)
    full_date = data.frame('date' = seq.POSIXt(from = x[1,3], to = x[nrow(x),3], by = '1 hour'))
    obs <- obs_wtemp[which((obs_wtemp$date %in% ens$fc_date) & obs_wtemp$depths == ens$depths[1]),]
    indx = which(!(full_date %in% obs$date))
    if(nrow(obs)==0)return(rep(NA,ncol(ens)))
    dat = merge(obs, ens, by.x = 1, by.y = 3)
    # val = crpsDecomposition(obs = obs[,3], eps = ens[,6:ncol(x)])
    mat = as.matrix(data.frame('mean' = apply(dat[,8:ncol(dat)], 1, mean, na.rm = T), 'std' = apply(dat[,8:ncol(dat)], 1, sd, na.rm = T)))
    vals = crps(obs = dat$obs, pred = mat)
    
    samp <- data.frame(date = dat$date, vals = vals$crps)
    samp2 <- merge(full_date, samp, by = 'date', all.x = T)
    # return(val$CRPS)
    # print(paste(x$start[1], x$depths[1], length(vals$crps)))
    
    return(samp2$vals)
  })
  
  df <- melt(res2, id.vars = c('start', 'fdepth'))
  colnames(df)[4] <- 'crps'
  df$fc_time <- as.numeric(df$variable)/24
  df$init_date <- wtemp$init_date[1]
  df$fc_date <- df$init_date + df$fc_time*24*60*60
  df <- df[,c('start', 'init_date', 'fc_time', 'fdepth', 'crps')]
  
  if(i == fils[1]){
    all_df <- df
  }else{
    all_df <- rbind.data.frame(all_df, df)
  }
  
  message(Sys.time(),' Finished analyzing ', i)
}

write.csv(all_df, file.path(res_dir, 'total_fc_crps_start_depth_init_date.csv'), row.names = F, quote = F)

dat <- read.csv(file.path(res_dir, 'total_fc_crps_start_depth_init_date.csv'))
# dat <- na.exclude(dat)
dat$fdepth <- fct_rev(factor(dat$fdepth))
dat$fc_date <- as.POSIXct(as.character(dat$init_date), tz = 'UTC') + dat$fc_time*24*60*60
#Assign status
dat$status <- 2
# dat$status[dat$fc_date %in% status$date[status$status == 'Inv_strat']] <- 1
dat$status[dat$fc_date %in% status$date[status$status == 'Stratified']] <- 1
dat$status[dat$fc_date %in% status$date[status$status == 'Isothermal']] <- 2
dat$status <- na.approx(dat$status)
dat$status <- factor(dat$status)
dat$status <- factor(dat$status,levels = c('1','2'), labels = c('Stratified', 'Isothermal'))
# ggplot(dat, aes(fc_date, status))+
#   geom_point()

# dat1 <- merge(dat, status, by.x = 'fc_date', by.y = 'date')
dat$month <- month(dat$fc_date)
dat$seas <- NA
dat$seas[(dat$month %in% win)] <- 'win'
dat$seas[(dat$month %in% spr)] <- 'spr'
dat$seas[(dat$month %in% sum)] <- 'sum'
dat$seas[(dat$month %in% aut)] <- 'aut'
dat$seas <- factor(dat$seas)
dat$seas <- factor(dat$seas, levels = c('spr', 'sum', 'aut', 'win'), labels = c('Spring', 'Summer', 'Autumn', 'Winter'))
dat$month <- NULL
# dat <- na.exclude(dat)
# samp <- merge(dat, obs[,c('date', 'seas')], by.x = 'fc_date', by.y = 'date')

fc_dates <- as.POSIXct(as.character(unique(dat$init_date)), tz = 'UTC')


#Load calibrated data
cal <- read.csv('run4/fc_3day_2007_v1_run_cal.csv', stringsAsFactors = T)
cal$start <- 'No Restart'
cal$fdepth <- fct_rev(factor(cal$depths))
cal$fc_date <- as.POSIXct(as.character(cal$fc_date), tz = 'UTC')
# cal$init_date <- as.POSIXct(as.character(cal$init_date), tz = 'UTC')
cal <- cal[cal$fc_date >= dat$fc_date[1] & cal$fc_date <= dat$fc_date[nrow(dat)],]
#Assign status
cal$status <- NA
# cal$status[1] <- 1
cal$status[cal$fc_date %in% status$date[status$status == 'Stratified']] <- 2
cal$status[cal$fc_date %in% status$date[status$status == 'Isothermal']] <- 1
cal$status <- round(na.approx(cal$status))
cal$status <- factor(cal$status)
cal$status <- factor(cal$status,levels = c('1','2'), labels = c( 'Isothermal','Stratified'))
# cal <- na.approx(cal)
# dat1 <- merge(dat, status, by.x = 'fc_date', by.y = 'date')
cal$month <- month(cal$fc_date)
cal$seas <- NA
cal$seas[(cal$month %in% win)] <- 'win'
cal$seas[(cal$month %in% spr)] <- 'spr'
cal$seas[(cal$month %in% sum)] <- 'sum'
cal$seas[(cal$month %in% aut)] <- 'aut'
cal$seas <- factor(cal$seas)
cal$seas <- factor(cal$seas, levels = c('spr', 'sum', 'aut', 'win'), labels = c('Spring', 'Summer', 'Autumn', 'Winter'))
cal$month <- NULL
summary(cal)


#Calculate N sample - seasons
seasN <- ddply(dat, c('seas'), function(x){
  x = x[!duplicated(x$init_date),]
  val = nrow(x[(x$fdepth == -0.9 & x$start == 'T000'),])
  return(val)
})
colnames(seasN)[2] <- 'num_fc' 
write.csv(seasN, file.path(res_dir, 'seas_num_forecasts.csv'), row.names = F, quote = F)

#Calculate N sample - seasons
statusN <- ddply(dat, c('status'), function(x){
  x = x[!duplicated(x$init_date),]
  val = nrow(x[(x$fdepth == -0.9 & x$start == 'T000'),])
  return(val)
})
colnames(statusN)[2] <- 'num_fc' 
write.csv(statusN, file.path(res_dir, 'status_num_forecasts.csv'), row.names = F, quote = F)

res1 <- ddply(dat, c('start', 'fdepth', 'status'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res1)[4] <- 'crps_mean'
res1$depth <- as.numeric(as.character(res1$fdepth))
ggplot(res1, aes(crps_mean, depth, colour = start))+
  geom_path()+
  facet_wrap(~status, nrow =1)+
  geom_point()+
  theme_bw()

dat$r_fc_time <- round_any(dat$fc_time,0.1)
dat$fct_fc_time <- factor(dat$r_fc_time)
res2 <- ddply(dat, c('start', 'fdepth', 'fct_fc_time', 'status'), function(x){
  val = mean(x$crps, na.rm = T)
  sd = sd(x$crps, na.rm = T)
  return(c(val, sd))
})
colnames(res2)[5:6] <- c('crps_mean', 'crps_sd')
res2$fc_time <- as.numeric(as.character(res2$fct_fc_time))
res2$depth <- as.numeric(as.character(res2$fdepth))
res2$day <- round(res2$fc_time)

# x <- cal[cal$fdepth ==-0.9 & cal$status == levels(cal$status)[1],]
cal2 <- ddply(cal, c('start', 'fdepth',  'status'), function(x){
  set_d = fc_dates[which(fc_dates %in% status$date[status$status == x$status[1]])]
  srt <- lapply(set_d, function(y){
    
    # sub1 <- unique(dat[(dat$init_date == y),6])
    dif <- difftime(x$fc_date, y, units = 'day')
    sub <- x[dif <= 14 & dif >0,]
    dif <- round_any(dif[dif <= 14 & dif >0], 0.1)
    resu = (sub$mod - sub$obs)^2
    # fc_time = round(seq(0.04, 14, length.out = nrow(sub)),2)
    df = data.frame(fc_time = factor(as.numeric(dif)), diff = resu)
    df = ddply(df, 'fc_time', function(z)mean(z[,2], na.rm =T))
    
    # if(nrow(df) < 141){
    #   # smp <- data.frame(fc_time = 1:336)
    #   # df <- merge(smp, df, by =1, all.x = T)
    #   return(NULL)
    # }
    colnames(df)[2] <- as.character(y)
    # df[,1]  <- as.character(df[,1])
    return(df)
  })
  # srt = srt[-which(sapply(srt, is.null))]
  print(x[1,])
  M <- Reduce(function(x,y)merge(x,y,by=1,all = T), srt)
  # if(nrow(M) != 141){
  #   print(x$fdepth[1])
  #   # M <- M[!(duplicated(M[,1])),]
  # }
  plot(sqrt(M[,2]), type = 'l', ylim = c(0,3))
  for(t in 3:ncol(M)){
    lines(sqrt(M[,t]), col =t)
  }
  lines(sqrt(rowMeans(M[,-1], na.rm = T)), lwd =4, col =1)
  
  
  val = as.numeric(sqrt((rowMeans(M[,-1], na.rm = T)))) #RMSE
  return(c(val))
})

# dif <- round_any(dif[dif <= 14 & dif >0], 0.1)
tdiff <- unique(round_any(seq(0.04166667, 14, length.out = 336),0.1))

colnames(cal2)[4:ncol(cal2)] <- as.character(tdiff)
cal2 <- melt(cal2, id.vars = c('start', 'fdepth',  'status'))
colnames(cal2)[4:5] <- c('fct_fc_time', 'RMSE')
cal2$fc_time <- as.numeric(as.character(cal2$fct_fc_time))
cal2$depth <- as.numeric(as.character(cal2$fdepth))
cal2$day <- round(cal2$fc_time,1)

res2 <- res2[,c("start", "fdepth", "status", "fc_time" , "crps_mean")]
cal2 <- cal2[,c("start", "fdepth", "status", "fc_time" , "RMSE")]
res2$lake <- 'Feeagh'
cal2$lake <- 'Feeagh'
write.csv(res2, file.path(res_dir, 'start_fdepth_fctime_mcrps_status.csv'), row.names = F, quote = F)
write.csv(cal2, file.path(res_dir, 'start_fdepth_fctime_rmse_status.csv'), row.names = F, quote = F)


p1b <- ggplot(res2, aes(fc_time, crps_mean, colour = start))+
  stat_summary(fun.y = mean, geom = 'line')+
  stat_summary(data = cal2,aes(fc_time, RMSE, colour = start),fun.y = mean, geom = 'line')+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  facet_wrap(~status, nrow = 2)+
  scale_x_continuous(breaks = seq(0,14,1))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  theme_bw(base_size = 14)
p1b
ggsave(file.path(plot_dir, 'CRPS_fc_time_int_depth_status.png'), p1b, dpi = 300,width = 384,height = 216, units = 'mm')

res2b <- ddply(res2, c('start', 'fct_fc_time', 'status'), function(x){
  val = mean(x$crps_mean, na.rm = T)
  sd = sd(x$crps_mean, na.rm = T)
  return(c(val, sd))
})
cal2b <- ddply(cal2, c('start', 'fct_fc_time', 'status'), function(x){
  val = mean(x$RMSE, na.rm = T)
  # sd = sd(x$crps_mean, na.rm = T)
  return(c(val))
})
x = res2b[res2b$start == res2b$start[1] & res2b$status == res2b$status[1] & res2b$fct_fc_time == res2b$fct_fc_time[1],]
res2c <- ddply(res2b, c('start', 'fct_fc_time', 'status'), function(x){
  y = cal2b[cal2b$status == x$status[1] & cal2b$fct_fc_time == x$fct_fc_time[1],]
  rat = 1 -x$V1/y$V1
  return(rat)
})
colnames(res2c)[4] <- 'ratio'
res2c$fc_time <- as.numeric(as.character(res2c$fct_fc_time))

p1c <- ggplot(res2c, aes(fc_time, ratio, colour = start))+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line()+
  # stat_summary(fun.y = mean, geom = 'line')+
  # stat_summary(data = cal2,aes(fc_time, RMSE, colour = start),fun.y = mean, geom = 'line')+
  ylab('Ratio to no assimilation')+
  xlab('Forecast Days')+
  facet_wrap(~status, nrow = 2)+
  scale_x_continuous(breaks = seq(0,14,1))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  theme_bw(base_size = 14)
p1c
ggsave(file.path(plot_dir, 'CRPS_fc_time_int_depth_status.png'), p1b, dpi = 300,width = 384,height = 216, units = 'mm')


p1 <- ggplot(res2[res2$fdepth == -0.9,], aes(fc_time, crps_mean, colour = start))+
  geom_line()+
  geom_line(data = cal2[cal2$fdepth == -0.9,], aes(fc_time, RMSE, colour = start))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~status, nrow = 3)+
  scale_x_continuous(breaks = seq(0,14,1))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p1
ggsave(file.path(plot_dir, 'CRPS_surface_status_depth.png'), p1, dpi = 300,width = 384,height = 216, units = 'mm')




res2a <- spread(res2[,1:5], fdepth, crps_mean)
write.csv(res2a, file.path(tab_dir, 'start_fc_time_depth_crps_mean_error.csv'), row.names = F, quote = F)

# p1b <- ggplot(res2, aes(day, depth, colour = crps_mean, size = crps_sd))+
#   geom_point()+
#   ylab('CRPS (°C)')+
#   xlab('Forecast Days')+
#   # facet_wrap(~fdepth*status, nrow = 8)+
#   facet_wrap(~start, nrow = 2)+
#   scale_x_continuous(breaks = seq(0,14,2))+
#   scale_colour_gradientn(colours=brewer.pal(n=5, name="YlOrRd"), name = 'Mean CRPS')+
#   # guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Mean'))+
#   # geom_point()+
#   theme_bw(base_size = 14)
# p1b


res3 <- ddply(dat, c('start', 'fdepth', 'fct_fc_time', 'seas'), function(x){
  val = mean(x$crps, na.rm = T)
  sd = sd(x$crps, na.rm = T)
  return(c(val, sd))
})
colnames(res3)[5:6] <- c('crps_mean', 'crps_sd')
res3$fc_time <- as.numeric(as.character(res3$fct_fc_time))


cal3 <- ddply(cal, c('start', 'fdepth',  'seas'), function(x){
  srt <- lapply(fc_dates, function(y){
    
    # sub1 <- unique(dat[(dat$init_date == y),6])
    dif <- difftime(x$fc_date, y, units = 'day')
    sub <- x[dif <= 14 & dif >0,]
    dif <- round_any(dif[dif <= 14 & dif >0], 0.1)
    resu = (sub$mod - sub$obs)^2
    # fc_time = round(seq(0.04, 14, length.out = nrow(sub)),2)
    df = data.frame(fc_time = factor(as.numeric(dif)), diff = resu)
    df = ddply(df, 'fc_time', function(z)mean(z[,2], na.rm =T))
    
    if(nrow(df) < 141){
      # smp <- data.frame(fc_time = 1:336)
      # df <- merge(smp, df, by =1, all.x = T)
      return(NULL)
    }
    colnames(df)[2] <- as.character(y)
    # df[,1]  <- as.character(df[,1])
    return(df)
  })
  srt = srt[-which(sapply(srt, is.null))]
  M <- Reduce(function(x,y)merge(x,y,by=1,all.x = T), srt)
  # if(nrow(M) != 141){
  #   print(x$fdepth[1])
  #   # M <- M[!(duplicated(M[,1])),]
  # }
  val = as.numeric(sqrt((rowMeans(M[,-1], na.rm = T)))) #RMSE
  return(c(val))
})
colnames(cal3)[4:ncol(cal3)] <- as.character(unique(res3$fct_fc_time))
cal3 <- melt(cal3, id.vars = c('start', 'fdepth',  'seas'))
colnames(cal3)[4:5] <- c('fct_fc_time', 'RMSE')
cal3$fc_time <- as.numeric(as.character(cal3$fct_fc_time))
cal3$depth <- as.numeric(as.character(cal3$fdepth))
cal3$day <- round(cal3$fc_time)

p2 <- ggplot(res3[res3$fdepth == -0.9,], aes(fc_time, crps_mean, colour = start))+
  # geom_ribbon(aes(ymin = crps_mean - crps_sd, ymax = crps_mean + crps_sd, fill = start), alpha = 0.2, colour = NA)+
  geom_line()+
  geom_line(data = cal3[cal3$fdepth == -0.9,], aes(fc_time, RMSE, colour = start))+
  # geom_line(data = res3[res3$fdepth == 0 & !is.na(res3$crps_mean),])+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*seas, nrow = 8)+
  facet_wrap(~seas, nrow = 2)+
  scale_x_continuous(breaks = seq(0,14,2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p2
ggsave(file.path(plot_dir, 'CRPS_surface_season_depth.png'), p2, dpi = 300,width = 384,height = 216, units = 'mm')
# res3[res3$seas == 'Summer',]
p2b <- ggplot(res3[res3$fdepth == -0.9,], aes(fc_time, crps_sd, colour = start))+
  geom_line()+
  geom_line(data = res3[res3$fdepth == -0.9 & !is.na(res3$crps_mean),])+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*seas, nrow = 8)+
  facet_wrap(~seas, nrow = 2)+
  scale_x_continuous(breaks = seq(0,14,2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  # geom_point()+
  theme_bw(base_size = 14)
p2b


#Profile - status
res4 <- ddply(dat, c('start', 'fdepth', 'status'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res4)[4] <- 'crps_mean'
res4$depth <- as.numeric(as.character(res4$fdepth))

cal4 <- ddply(cal, c('start', 'fdepth', 'status'), function(x){
  rmse = sqrt((mean((x$mod - x$obs)^2, na.rm = T)))
  val = rmse
  return(val)
})
colnames(cal4)[4] <- 'RMSE'
cal4$depth <- as.numeric(as.character(cal4$fdepth))

res4$lake <- 'Feeagh'
cal4$lake <- 'Feeagh'
write.csv(res4, file.path(res_dir, 'start_fdepth_mcrps_status.csv'), row.names = F, quote = F)
write.csv(cal4, file.path(res_dir, 'start_fdepth_rmse_status.csv'), row.names = F, quote = F)



p3 <- ggplot(res4, aes(crps_mean,depth, colour = start))+
  geom_path()+
  geom_point()+
  geom_path(data = cal4, aes(RMSE,depth, colour = start))+
  geom_point(data = cal4, aes(RMSE,depth, colour = start))+
  ylab('Depth (m)')+
  xlab('CRPS (°C)')+
  facet_wrap(~status, nrow = 1)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p3
ggsave(file.path(plot_dir, 'CRPS_profile_status.png'), p3, dpi = 300,width = 384,height = 216, units = 'mm')

#Profile - seasons
res5 <- ddply(dat, c('start', 'fdepth', 'seas'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res5)[4] <- 'crps_mean'
res5$depth <- as.numeric(as.character(res5$fdepth))


cal5 <- ddply(cal, c('start', 'fdepth', 'seas'), function(x){
  val = rmse(x$mod, x$obs)
  return(val)
})
colnames(cal5)[4] <- 'RMSE'
cal5$depth <- as.numeric(as.character(cal5$fdepth))

p4 <- ggplot(res5, aes(crps_mean,depth, colour = start))+
  geom_path()+
  geom_point()+
  geom_path(data = cal5, aes(RMSE,depth, colour = start))+
  geom_point(data = cal5, aes(RMSE,depth, colour = start))+
  ylab('Depth (m)')+
  xlab('CRPS (°C)')+
  facet_wrap(~seas, nrow = 1)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p4
ggsave(file.path(plot_dir, 'CRPS_profile_season.png'), p4, dpi = 300,width = 384,height = 216, units = 'mm')


#Yearly performance
res6 <- ddply(dat, c('start', 'init_date', 'fdepth'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res6)[4] <- 'crps_mean'
res6$depth <- as.numeric(as.character(res6$fdepth))
res6$init_date <- as.POSIXct(as.character(res6$init_date))

cal6 <- ddply(cal, c('start', 'fdepth'), function(x){
  srt <- lapply(fc_dates, function(y){
    
    # sub1 <- unique(dat[(dat$init_date == y),6])
    dif <- difftime(x$fc_date, y, units = 'hour')
    sub <- x[dif <= 336 & dif >0,]
    dif <- dif[dif <= 336 & dif >0]
    resu = rmse(sub$mod, sub$obs)
    return(resu)
  })
  val <- unlist(srt)
  return(c(val))
})
colnames(cal6)[3:ncol(cal6)] <- as.character(fc_dates)
cal6 <- melt(cal6, id.vars = c('start', 'fdepth'))
colnames(cal6)[3:4] <- c('init_date', 'RMSE')
cal6$depth <- as.numeric(as.character(cal6$fdepth))
cal6$init_date <- as.POSIXct(as.character(cal6$init_date))

res6$lake <- 'Feeagh'
cal6$lake <- 'Feeagh'

write.csv(res6, file.path(res_dir, 'start_init_date_depth_mcrps.csv'), row.names = F, quote = F)
write.csv(cal6, file.path(res_dir, 'start_init_date_depth_rmse.csv'), row.names = F, quote = F)

p5 <- ggplot(res6[res6$fdepth == -0.9,], aes(init_date, crps_mean, colour = start))+
  geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_line(data = cal6[cal6$fdepth == -0.9,], aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('Time')+
  ylab('CRPS (°C)')+
  # facet_wrap(~seas, nrow = 1)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m/%d'), date_breaks = '1 month')+
  coord_cartesian(xlim = range(res6$init_date))+
  scale_colour_manual(values = d.cols[3:8])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  # geom_point()+
  # guides(colour = F, fill = F)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  theme_bw(base_size = 14)
p5

#Integrate depths
#Yearly performance
res6c <- ddply(dat, c('start', 'init_date'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res6c)[3] <- 'crps_mean'
res6c$init_date <- as.POSIXct(as.character(res6c$init_date))

cal6c <- ddply(cal, c('start'), function(x){
  srt <- lapply(fc_dates, function(y){
    
    # sub1 <- unique(dat[(dat$init_date == y),6])
    dif <- difftime(x$fc_date, y, units = 'hour')
    sub <- x[dif <= 336 & dif >0,]
    dif <- dif[dif <= 336 & dif >0]
    resu = rmse(sub$mod, sub$obs)
    return(resu)
  })
  val <- unlist(srt)
  return(c(val))
})
colnames(cal6c)[2:ncol(cal6c)] <- as.character(fc_dates)
cal6c <- melt(cal6c, id.vars = c('start'))
colnames(cal6c)[2:3] <- c('init_date', 'RMSE')
cal6c$init_date <- as.POSIXct(as.character(cal6c$init_date))

p5c <- ggplot(res6c, aes(init_date, crps_mean, colour = start))+
  geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_line(data = cal6c, aes(init_date, RMSE, colour = start))+
  geom_point(data = cal6c, aes(init_date, RMSE, colour = start))+
  geom_point()+
  xlab('Time')+
  ylab('CRPS (°C)')+
  # facet_wrap(~seas, nrow = 1)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m/%d'), date_breaks = '1 month')+
  coord_cartesian(xlim = range(res6c$init_date))+
  scale_colour_manual(values = d.cols[3:8])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  # # geom_point()+
  # guides(colour = F, fill = F)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  theme_bw(base_size = 14)
p5c



obs_wtemp$fdepth <- factor(obs_wtemp$depths)
obs_wtemp$fdepth <- fct_rev(obs_wtemp$fdepth)
p5b <- ggplot(obs_wtemp, aes(date, obs, colour = fdepth))+
  # ggtitle(main) + 
  ylab('Temperature (°C)') +
  # geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  # geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_line()+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depths'), fill = guide_legend(title = 'Status'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m/%d'), date_breaks = '1 month')+coord_cartesian(xlim = range(res6c$init_date))+
  scale_colour_manual(values = dramp(13))+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  xlab('')+
  theme_bw(base_size = 14)
p5b
p5fb <- p5b

library(ggpubr)
g1 <- ggarrange(p5c, p5b, nrow = 2, align = 'v', labels = 'AUTO')
g1
ggsave(file.path(plot_dir, 'CRPS_profile_fc_annual_int_depths_obs.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')


p5a <- ggplot(res6[res6$fdepth == -42,], aes(init_date, crps_mean, colour = start))+
  geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.2, inherit.aes = F)+
  geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.2, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  # geom_point()+
  xlab('Time')+
  ylab('CRPS (°C)')+
  # facet_wrap(~seas, nrow = 1)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m/%d'), date_breaks = '1 month')+
  # geom_point()+
  theme_bw(base_size = 14)
p5a

g1 <- ggarrange(p5b, p5a, nrow = 2, align = 'v', labels = 'AUTO')
g1
ggsave(file.path(plot_dir, 'CRPS_42m_fc_annual_obs.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')


p6 <- ggplot(res6, aes(init_date, crps_mean, colour = fdepth))+
  geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.2, inherit.aes = F)+
  geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.2, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  # geom_point()+
  xlab('Time')+
  ylab('CRPS (°C)')+
  facet_wrap(~start, ncol = 2)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m/%d'), date_breaks = '1 month')+
  # geom_point()+
  theme_bw(base_size = 14)
p6

########### fac
res2_sub <- res2[res2$fdepth %in% c(-0.9, -5, -11, -16, -20, -22, -27, -42),]
res2_sub$fdepth <- factor(res2_sub$fdepth)
cal2_sub <- cal2[cal2$fdepth %in% c(-0.9, -5, -11, -16, -20, -22, -27, -42),]
cal2_sub$fdepth <- factor(cal2_sub$fdepth)

p8 <- ggplot(res2_sub, aes(fc_time, crps_mean, colour = fdepth))+
  geom_line()+
  geom_line(data = cal2_sub, aes(fc_time, RMSE, colour = fdepth))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~status*start, nrow = 2)+
  scale_x_continuous(breaks = seq(0,14,2))+
  scale_y_continuous(breaks = seq(0,1.6,0.2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depth (m)'))+
  scale_colour_manual(values = dramp(13))+
  # geom_point()+
  theme_bw(base_size = 14)
p8
ggsave(file.path(plot_dir, 'CRPS_status_start_depth.png'), p8, dpi = 300,width = 384,height = 216, units = 'mm')

res3_sub <- res3[res3$fdepth %in% c(-0.9, -5, -11, -16, -20, -22, -27, -42),]
res3_sub$fdepth <- factor(res3_sub$fdepth)
cal3_sub <- cal3[cal3$fdepth %in% c(-0.9, -5, -11, -16, -20, -22, -27, -42),]
cal3_sub$fdepth <- factor(cal3_sub$fdepth)

p9 <- ggplot(res3_sub, aes(fc_time, crps_mean, colour = fdepth))+
  geom_line()+
  geom_line(data = cal3_sub, aes(fc_time, RMSE, colour = fdepth))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~seas*start, nrow = 4)+
  scale_x_continuous(breaks = seq(0,14,2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depth (m)'))+
  scale_colour_manual(values = dramp(13))+
  # geom_point()+
  theme_bw(base_size = 14)
p9
ggsave(file.path(plot_dir, 'CRPS_season_start_depth.png'), p9, dpi = 300,width = 384,height = 216, units = 'mm')

###############
res8 <- ddply(dat, c('start','fct_fc_time', 'status'), function(x){
  val = mean(x$crps, na.rm = T)
  sd = sd(x$crps, na.rm = T)
  return(c(val, sd))
})
colnames(res8)[4:5] <- c('crps_mean', 'crps_sd')
res8$fc_time <- as.numeric(as.character(res8$fct_fc_time))
res8$day <- round(res8$fc_time)

x <- cal[cal$fdepth ==-0.9 & cal$status == levels(cal$status)[1],]
cal8 <- ddply(cal, c('start', 'status'), function(x){
  srt <- lapply(fc_dates, function(y){
    
    # sub1 <- unique(dat[(dat$init_date == y),6])
    dif <- difftime(x$fc_date, y, units = 'day')
    sub <- x[dif <= 14 & dif >0,]
    dif <- round_any(dif[dif <= 14 & dif >0], 0.1)
    resu = (sub$mod - sub$obs)^2
    # fc_time = round(seq(0.04, 14, length.out = nrow(sub)),2)
    df = data.frame(fc_time = factor(as.numeric(dif)), diff = resu)
    df = ddply(df, 'fc_time', function(z)mean(z[,2], na.rm =T))
    
    if(nrow(df) < 141){
      # smp <- data.frame(fc_time = 1:336)
      # df <- merge(smp, df, by =1, all.x = T)
      return(NULL)
    }
    colnames(df)[2] <- as.character(y)
    # df[,1]  <- as.character(df[,1])
    return(df)
  })
  srt = srt[-which(sapply(srt, is.null))]
  print(x$fc_date[1])
  M <- Reduce(function(x,y)merge(x,y,by=1,all.x = T), srt)
  # if(nrow(M) != 141){
  #   print(x$fdepth[1])
  #   # M <- M[!(duplicated(M[,1])),]
  # }
  val = as.numeric(sqrt((rowMeans(M[,-1], na.rm = T)))) #RMSE
  return(c(val))
})

dif <- round_any(dif[dif <= 14 & dif >0], 0.1)
tdiff <- unique(round_any(seq(0.04166667, 14, length.out = 336),0.1))

colnames(cal8)[3:ncol(cal8)] <- as.character(tdiff)
cal8 <- melt(cal8, id.vars = c('start', 'status'))
colnames(cal8)[3:4] <- c('fct_fc_time', 'RMSE')
cal8$fc_time <- as.numeric(as.character(cal8$fct_fc_time))
cal8$day <- round(cal8$fc_time,1)

p10 <- ggplot(res8, aes(fc_time, crps_mean, colour = start))+
  geom_line()+
  geom_line(data = cal8, aes(fc_time, RMSE, colour = start))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~status, nrow = 3)+
  scale_x_continuous(breaks = seq(0,14,2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p10
ggsave(file.path(plot_dir, 'CRPS_surface_status_depth.png'), p1, dpi = 300,width = 384,height = 216, units = 'mm')

library(cowplot)
g1 <- ggdraw() +
  draw_plot(p5, x = 0, y = 0.5, width = 1, height = 0.5) +
  draw_plot(p5b, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0.2, 0.2), y = c(0.4, 0.9))
g1



p5b <- ggplot(obs_wtemp, aes(date, obs, colour = 'Obs'))+
  geom_line()+
  # geom_point()+
  xlab('Time')+
  ylab('Temperature (°C)')+
  # facet_wrap(~seas, nrow = 1)+
  # guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  # geom_point()+
  theme_bw(base_size = 14)
p5b
ggsave(file.path(plot_dir, 'CRPS_profile_season.png'), p4, dpi = 300,width = 384,height = 216, units = 'mm')

#distributions
#Assign status
res6c$status <- NA
res6c$status[res6c$init_date %in% status$date[status$status == 'Stratified']] <- 1
res6c$status[res6c$init_date %in% status$date[status$status == 'Isothermal']] <- 2
res6c$status <- round(na.approx(res6c$status))
res6c$status <- factor(res6c$status)
res6c$status <- factor(res6c$status,levels = c('1','2'), labels = c('Stratified', 'Isothermal'))


#distributions
cal6c$status <- 2
cal6c$status[cal6c$init_date %in% status$date[status$status == 'Stratified']] <- 1
cal6c$status[cal6c$init_date %in% status$date[status$status == 'Isothermal']] <- 2
cal6c$status <- round(na.approx(cal6c$status))
cal6c$status <- factor(cal6c$status)
cal6c$status <- factor(cal6c$status,levels = c('1','2'), labels = c('Stratified', 'Isothermal'))

p10 <- ggplot(res6c, aes(x = crps_mean))+
  geom_density(aes(colour = start),size = 1)+
  geom_density(data = cal6c, aes(x = RMSE, colour = start), size = 1)+
  xlab('')+
  facet_wrap(~status, nrow = 3)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  ylab('')+
  xlab('Error (K)')+
  theme_bw(base_size = 14)
p10
ggsave(file.path(plot_dir, 'CRPS_distribution_status.png'), p10, dpi = 300,width = 384,height = 216, units = 'mm')

res6c$lake <- 'Feeagh'
cal6c$lake <- 'Feeagh'
write.csv(res6c, file.path(res_dir, 'start_mcrps_status_year.csv'), row.names = F, quote = F)
write.csv(cal6c, file.path(res_dir, 'start_rmse_status_year.csv'), row.names = F, quote = F)


#########
res2 <- ddply(dat, c('start', 'fct_fc_time', 'status'), function(x){
  val = mean(x$crps, na.rm = T)
  sd = sd(x$crps, na.rm = T)
  return(c(val, sd))
})
colnames(res2)[4:5] <- c('crps_mean', 'crps_sd')
res2$fc_time <- as.numeric(as.character(res2$fct_fc_time))
res2$day <- round(res2$fc_time)

x <- cal[cal$status == levels(cal$status)[1],]
cal2 <- ddply(cal, c('start','status'), function(x){
  srt <- lapply(fc_dates, function(y){
    
    # sub1 <- unique(dat[(dat$init_date == y),6])
    dif <- difftime(x$fc_date, y, units = 'day')
    sub <- x[dif <= 14 & dif >0,]
    dif <- round_any(dif[dif <= 14 & dif >0], 0.1)
    resu = (sub$mod - sub$obs)^2
    # fc_time = round(seq(0.04, 14, length.out = nrow(sub)),2)
    df = data.frame(fc_time = factor(as.numeric(dif)), diff = resu)
    df = ddply(df, 'fc_time', function(z)mean(z[,2], na.rm =T))
    
    if(nrow(df) < 141){
      # smp <- data.frame(fc_time = 1:336)
      # df <- merge(smp, df, by =1, all.x = T)
      return(NULL)
    }
    colnames(df)[2] <- as.character(y)
    # df[,1]  <- as.character(df[,1])
    return(df)
  })
  srt = srt[-which(sapply(srt, is.null))]
  print(x$fc_date[1])
  M <- Reduce(function(x,y)merge(x,y,by=1,all.x = T), srt)
  # if(nrow(M) != 141){
  #   print(x$fdepth[1])
  #   # M <- M[!(duplicated(M[,1])),]
  # }
  val = as.numeric(sqrt((rowMeans(M[,-1], na.rm = T)))) #RMSE
  return(c(val))
})

dif <- round_any(dif[dif <= 14 & dif >0], 0.1)
tdiff <- unique(round_any(seq(0.04166667, 14, length.out = 336),0.1))

colnames(cal2)[3:ncol(cal2)] <- as.character(tdiff)
cal2 <- melt(cal2, id.vars = c('start',  'status'))
colnames(cal2)[3:4] <- c('fct_fc_time', 'RMSE')
cal2$fc_time <- as.numeric(as.character(cal2$fct_fc_time))
cal2$day <- round(cal2$fc_time,1)

p11 <- ggplot(res2, aes(fc_time, crps_mean, colour = start))+
  geom_line()+
  geom_line(data = cal2, aes(fc_time, RMSE, colour = start))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~status, nrow = 2)+
  scale_x_continuous(breaks = seq(0,14,2))+
  scale_y_continuous(breaks = seq(0,1.6,0.2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depth (m)'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p11

res2b <- ddply(dat, c('start','fct_fc_time', 'status'), function(x){
  val = mean(x$crps, na.rm = T)
  qt = quantile(x$crps, probs = c(0.025, 0.975), na.rm = T)
  sd = sd(x$crps, na.rm = T)
  return(c(val, sd, qt))
})
colnames(res2b)[4:7] <- c('crps_mean', 'crps_sd', 'p0.025', 'p0.975')
res2b$fc_time <- as.numeric(as.character(res2b$fct_fc_time))
res2b$day <- round(res2b$fc_time)



p1 <- ggplot(res2b, aes(fc_time, crps_mean, colour = start, fill = start))+
  # geom_ribbon(aes(ymin = crps_mean - crps_sd, ymax = crps_mean + crps_sd), alpha =0.2)+
  geom_line()+
  geom_line(aes(fc_time, crps_mean - crps_sd, colour = start), linetype =2)+
  geom_line(aes(fc_time, crps_mean + crps_sd, colour = start), linetype =2)+
  # geom_line(data = cal2, aes(fc_time, RMSE, colour = start))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~status, nrow = 2)+
  scale_x_continuous(breaks = seq(0,14,2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  scale_fill_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p1

## Calculate % improvement
# x = dat[(dat$start == dat$start[1] & dat$status == dat$status[1]),]
res9 <- ddply(dat, c('start',  'status'), function(x){
  sub = cal[(cal$status == x$status[1]),]
  mn = rmse(sim = sub$mod, obs = sub$obs)
  val = mean(x$crps, na.rm = T)
  ans = 100*(mn-val)/mn
  return(c(val,mn,ans))
})
colnames(res9)[3:5] <- c('MCRPS', 'RMSE', 'Improvement (%)')
res9 <- res9[order(res9$status),]
res9[,3:5] <- round(res9[,3:5],3)
write.csv(res9, file.path(res_dir, 'status_MCRPS_RMSE_improvement.csv'), row.names = F, quote = F)

res9$hour <- as.numeric(gsub('T','',as.character(res9$start)))
ggplot(res9, aes(hour, `Improvement (%)`, colour = status))+
  geom_point(size = 1.5)+geom_line()+
  coord_cartesian(ylim = c(0,100))+
  xlab('Sampling Frequency')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Status'))+
  theme_bw(base_size = 14)
