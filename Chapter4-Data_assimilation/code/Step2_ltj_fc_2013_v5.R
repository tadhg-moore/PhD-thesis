setwd("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\langtjern/")

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

#classification of stratified
classification = 'density' #temperature

dir <- "G:\\Opt_freq\\langtjern\\run4/"
plot_dir <- file.path(dir, 'fig')
res_dir <- file.path(dir, 'analysis')
dir.create(res_dir)
dir.create(plot_dir)

ice_date <- read.csv('ice_dates/ice_dates.txt', stringsAsFactors = F)
ice_date[,1] <- as.POSIXct(ice_date[,1], tz = 'UTC')
ice_date[,2] <- as.POSIXct(ice_date[,2], tz = 'UTC')

fils <- list.files(dir)[grep('v1_20', list.files(dir))]
nam = strsplit(fils[1],'_')[[1]]
fc_start = as.POSIXct(paste(nam[5], nam[6], strsplit(nam[7], '.csv'), sep = '-'), tz = 'UTC')
nam = strsplit(fils[length(fils)],'_')[[1]]
fcend = as.POSIXct(paste(nam[5], nam[6], strsplit(nam[7], '.csv'), sep = '-'), tz = 'UTC')

l.cols <- RColorBrewer::brewer.pal(8, 'Set2')
d.cols <- RColorBrewer::brewer.pal(8, 'Dark2')
dramp <- colorRampPalette(RColorBrewer::brewer.pal(8, 'Dark2'))
display.brewer.all(colorblindFriendly = T)

#Seasons
win <- c(12,1,2)
spr <- c(3,4,5)
sum <- c(6,7,8)
aut <- c(9,10,11)

#Load in observed Lake data
obs.file = 'GLM/langtjern_1hr_tprof.csv'
obs_wtemp = load_obs(obs.file, header = T, sep =',')
obs_wtemp[,2] <- -obs_wtemp[,2]
obs_wtemp <- obs_wtemp[(obs_wtemp$date >= fc_start & obs_wtemp$date < (fcend + 15*24*60*60)),]
colnames(obs_wtemp)[3] <- 'obs'
obs_wtemp$fdepth <- factor(obs_wtemp$depths)
obs_wtemp$fdepth <- fct_rev(obs_wtemp$fdepth)
pltj <- ggplot(obs_wtemp, aes(date, obs, colour = fdepth))+
  # ggtitle(main) + 
  ylab('Temperature (°C)') +
  geom_line()+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depths'), fill = guide_legend(title = 'Status'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%b-%y'), date_breaks = '2 month')+
  # coord_cartesian(xlim = range(res6c$init_date))+
  scale_colour_manual(values = dramp(8))+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  xlab('')+
  theme_bw(base_size = 14)
pltj
library(ggpubr)
g1 <- ggarrange(pfgh, pltj, pkin, nrow = 3, align = 'v', labels = 'AUTO')
g1
ggsave(file.path(plot_dir, 'Lakes_3_wtemp.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')
# obs_wtemp$month <- month(obs_wtemp$date)
# obs_wtemp$seas <- NA
# obs_wtemp$seas[(obs_wtemp$month %in% win)] <- 'win'
# obs_wtemp$seas[(obs_wtemp$month %in% spr)] <- 'spr'
# obs_wtemp$seas[(obs_wtemp$month %in% sum)] <- 'sum'
# obs_wtemp$seas[(obs_wtemp$month %in% aut)] <- 'aut'
# obs_wtemp$seas <- factor(obs_wtemp$seas)
# obs_wtemp$seas <- factor(obs_wtemp$seas, levels = c('spr', 'sum', 'aut', 'win'), labels = c('Spring', 'Summer', 'Autumn', 'Winter'))


x = obs_wtemp[obs_wtemp$date == '2014-12-01 00:00:00',]
#Classify lake status
if(classification == 'density'){
  #Density
  obs_wtemp$dens <- water.density(obs_wtemp$obs)
  status <- ddply(obs_wtemp, c('date'), function(x){
    dif = x$dens[which.max(x$depths)]  - x$dens[which.min(x$depths)]
    tmp = x$obs[which.max(x$depths)]
    if(dif < -0.05 & tmp > 4){
      sts = 'Stratified'
    }else if(dif < -0.05 & tmp < 4){
      sts = 'Inverse Stratified'
    }else{
      sts = 'Isothermal'
    }
    return(c(sts, dif))
  })
  obs_wtemp$dens <- NULL
}else if(classification == 'temperature'){
  #temperature
  status <- ddply(obs_wtemp, c('date'), function(x){
    dif = x$obs[which.max(x$depths)]  - x$obs[which.min(x$depths)]
    if(dif > 1){
      sts = 'Stratified'
    }else if(dif < -1){
      sts = 'Inverse Stratified'
    }else{
      sts = 'Isothermal'
    }
    return(sts)
  })
}
colnames(status)[2] <- c('status')
status$status <- factor(status$status)
# status$dens_diff <- as.numeric(status$dens_diff)
ggplot(status, aes(date, status))+
  geom_point()
# ggplot(status, aes(date, dens_diff))+
#   geom_line()+
#   coord_cartesian(ylim = c(0,-0.2))+
#   geom_hline(yintercept = -0.1, linetype = 'dashed')+
#   geom_hline(yintercept = -0.05, linetype = 'dashed', colour = 'red')
#   

iso <- status[status$status == 'Isothermal',]
s <- split(iso$date, cumsum(c(TRUE, !(diff(iso$date) %in% c(30,60,120))))) #Split into consecutive years
isolims <- as.data.frame(do.call(rbind, lapply(s, function(x)return(c(min(x), max(x)))))) #Horribly messy, but dataframe of xmin and xmax from min and max in sequential data
isolims[,1] <- as.POSIXct(isolims[,1], origin = '1970-01-01', tx = 'UTC')
isolims[,2] <- as.POSIXct(isolims[,2], origin = '1970-01-01', tx = 'UTC')
colnames(isolims) <- c('xmin', 'xmax') #Rename colnames

stra <- status[status$status == 'Stratified',]
s <- split(stra$date, cumsum(c(TRUE, !(diff(stra$date) %in% c(30,60,120))))) #Split into consecutive years
stralims <- as.data.frame(do.call(rbind, lapply(s, function(x)return(c(min(x), max(x)))))) #Horribly messy, but dataframe of xmin and xmax from min and max in sequential data
stralims[,1] <- as.POSIXct(stralims[,1], origin = '1970-01-01', tx = 'UTC')
stralims[,2] <- as.POSIXct(stralims[,2], origin = '1970-01-01', tx = 'UTC')
colnames(stralims) <- c('xmin', 'xmax') #Rename colnames

inv <- status[status$status == 'Inverse Stratified',]
s <- split(inv$date, cumsum(c(TRUE, !(diff(inv$date) %in% c(30,60,120))))) #Split into consecutive years
invlims <- as.data.frame(do.call(rbind, lapply(s, function(x)return(c(min(x), max(x)))))) #Horribly messy, but dataframe of xmin and xmax from min and max in sequential data
invlims[,1] <- as.POSIXct(invlims[,1], origin = '1970-01-01', tx = 'UTC')
invlims[,2] <- as.POSIXct(invlims[,2], origin = '1970-01-01', tx = 'UTC')
colnames(invlims) <- c('xmin', 'xmax') #Rename colnames

write.csv(isolims, file.path(res_dir, 'isolims.csv'), quote = F, row.names = F)
write.csv(stralims, file.path(res_dir, 'stralims.csv'), quote = F, row.names = F)
write.csv(invlims, file.path(res_dir, 'invlims.csv'), quote = F, row.names = F)

obs_wtemp$fdepth <- factor(obs_wtemp$depths)

for(i in fils){
  wtemp <- read.csv(file.path(dir,i), stringsAsFactors = F)
  wtemp$start[wtemp$start == 'T0'] <- 'T000'
  wtemp$start[wtemp$start == 'T24'] <- 'T024'
  wtemp$start <- factor(wtemp$start)
  colnames(wtemp)[5] <- 'mod'
  
  wtemp$init_date <- as.POSIXct(wtemp$init_date, tz = 'UTC')
  wtemp$fc_date <- as.POSIXct(wtemp$fc_date, tz = 'UTC')
  # wtemp$obs <- wtemp$mod - wtemp$resid
  # wtemp$rmse <- sqrt(wtemp$resid^2)
  wtemp$par_id <- factor(wtemp$par_id)
  wtemp$fdepth <- fct_rev(factor(wtemp$depths))
  wtemp$resid <- NULL
  # wtemp$mod[is.na(wtemp$mod)]
  # ggplot(wtemp[wtemp$depths == -0.5 & wtemp$start == 'T672',], aes(fc_date, mod, colour = par_id))+
  #   geom_line()+
  #   geom_line(data = wtemp[wtemp$depths == -1 & wtemp$start == 'T672',], aes(fc_date, mod, colour = par_id))+
  #   geom_line(data = wtemp[wtemp$depths == -2 & wtemp$start == 'T672',], aes(fc_date, mod, colour = par_id))+
  #   geom_line(data = wtemp[wtemp$depths == -6 & wtemp$start == 'T672',], aes(fc_date, mod, colour = par_id))+
  #   geom_point(data = obs_wtemp, aes(date, obs, colour = fdepth), size = 0.5)+
  #   coord_cartesian(xlim = range(wtemp$fc_date))+
  #   guides(colour = F)+
  #   theme_bw()
  
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
dat$fdepth <- fct_rev(factor(dat$fdepth))
dat$fc_date <- as.POSIXct(as.character(dat$init_date), tz = 'UTC') + dat$fc_time*24*60*60
#Assign status
dat$status <- NA
dat$status[dat$fc_date %in% status$date[status$status == 'Inverse Stratified']] <- 1
dat$status[dat$fc_date %in% status$date[status$status == 'Stratified']] <- 2
dat$status[dat$fc_date %in% status$date[status$status == 'Isothermal']] <- 3
dat$status <- round(na.approx(dat$status))
dat$status <- factor(dat$status)
dat$status <- factor(dat$status,levels = c('1','2','3'), labels = c('Inverse Stratified', 'Stratified', 'Isothermal'))
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
# samp <- merge(dat, obs[,c('date', 'seas')], by.x = 'fc_date', by.y = 'date')

fc_dates <- as.POSIXct(as.character(unique(dat$init_date)), tz = 'UTC')


#Load calibrated data
cal <- read.csv('Output/opt_freq/run4/fc_3day_2014_v1_run_cal.csv', stringsAsFactors = T)
cal$start <- 'No Restart'
cal$fdepth <- fct_rev(factor(cal$depths))
cal$fc_date <- as.POSIXct(as.character(cal$fc_date), tz = 'UTC')
cal <- cal[cal$fc_date >= dat$fc_date[1] & cal$fc_date <= dat$fc_date[nrow(dat)],]
#Assign status
cal$status <- NA
# cal$status[cal$fc_date %in% status$date[status$status == 'Inverse Stratified']] <- 1
cal$status[cal$fc_date %in% status$date[status$status == 'Stratified']] <- 2
cal$status[cal$fc_date %in% status$date[status$status == 'Isothermal']] <- 3
cal$status[cal$fc_date %in% status$date[status$status == 'Inverse Stratified']] <- 1
cal$status <- round(na.approx(cal$status))
cal$status <- factor(cal$status)
cal$status <- factor(cal$status,levels = c('1','2','3'), labels = c('Inverse Stratified','Stratified', 'Isothermal'))
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

cal$RMSE <- sqrt((cal$mod - cal$obs)^2)
summary(cal)


#Calculate N sample - seasons
seasN <- ddply(dat, c('seas'), function(x){
  x = x[!duplicated(x$init_date),]
  val = nrow(x[(x$fdepth == -0.5 & x$start == 'T000'),])
  return(val)
})
colnames(seasN)[2] <- 'num_fc' 
write.csv(seasN, file.path(res_dir, 'seas_num_forecasts.csv'), row.names = F, quote = F)

#Calculate N sample - seasons
statusN <- ddply(dat, c('status'), function(x){
  x = x[!duplicated(x$init_date),]
  val = nrow(x[(x$fdepth == -0.5 & x$start == 'T000'),])
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

dat$r_fc_time <- round_any(dat$fc_time, 0.1)
dat$fct_fc_time <- factor(dat$r_fc_time)
res2 <- ddply(dat, c('start', 'fdepth', 'fct_fc_time', 'status'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res2)[5] <- 'crps_mean'
res2$fc_time <- as.numeric(as.character(res2$fct_fc_time))

tdiff <- round(seq(0.04166667, 14, length.out = 336),2)
x <- cal[cal$fdepth ==-0.5 & cal$status == levels(cal$status)[1] & cal$start == cal$start[1],]
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
  plot(sqrt(M[,2]), type = 'l', ylim = c(0,6))
  for(t in 3:ncol(M)){
    lines(sqrt(M[,t]), col =t)
  }
  lines(sqrt(rowMeans(M[,-1], na.rm = T)), lwd =4, col =1)
  
  
  val = as.numeric(sqrt((rowMeans(M[,-1], na.rm = T)))) #RMSE
  return(c(val))
})
colnames(cal2)[4:ncol(cal2)] <- as.character(unique(dat$fct_fc_time))
cal2 <- melt(cal2, id.vars = c('start', 'fdepth',  'status'))
colnames(cal2)[4:5] <- c('fct_fc_time', 'RMSE')
cal2$fc_time <- as.numeric(as.character(cal2$fct_fc_time))
cal2$depth <- as.numeric(as.character(cal2$fdepth))
cal2$day <- round(cal2$fc_time,1)

res2 <- res2[,c("start", "fdepth", "status", "fc_time" , "crps_mean")]
cal2 <- cal2[,c("start", "fdepth", "status", "fc_time" , "RMSE")]
res2$lake <- 'Langtjern'
cal2$lake <- 'Langtjern'
write.csv(res2, file.path(res_dir, 'start_fdepth_fctime_mcrps_status.csv'), row.names = F, quote = F)
write.csv(cal2, file.path(res_dir, 'start_fdepth_fctime_rmse_status.csv'), row.names = F, quote = F)


p1 <- ggplot(res2[res2$fdepth == -0.5,], aes(fc_time, crps_mean, colour = start))+
  geom_line()+
  geom_line(data = cal2[cal2$fdepth == -0.5,], aes(fc_time, RMSE, colour = start))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~status, nrow = 3)+
  scale_x_continuous(breaks = seq(0,14,2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p1
ggsave(file.path(plot_dir, 'CRPS_surface_status_depth.png'), p1, dpi = 300,width = 384,height = 216, units = 'mm')

p1b <- ggplot(res2, aes(fc_time, crps_mean, colour = start))+
  stat_summary(fun.y = mean, geom = 'line')+
  stat_summary(data = cal2,aes(fc_time, RMSE, colour = start),fun.y = mean, geom = 'line')+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  facet_wrap(~status, nrow = 3)+
  scale_x_continuous(breaks = seq(0,14,1))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  theme_bw(base_size = 14)
p1b
ggsave(file.path(plot_dir, 'CRPS_fc_time_int_depth_status.png'), p1b, dpi = 300,width = 384,height = 216, units = 'mm')


res3 <- ddply(dat, c('start', 'fdepth', 'fct_fc_time', 'seas'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res3)[5] <- 'crps_mean'
res3$fc_time <- as.numeric(as.character(res3$fct_fc_time))

x <- cal[cal$fdepth ==-0.5 & cal$seas == levels(cal$seas)[4],]
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

p2 <- ggplot(res3[res3$fdepth == -0.5,], aes(fc_time, crps_mean, colour = start))+
  # geom_ribbon(aes(ymin = crps_mean - crps_sd, ymax = crps_mean + crps_sd, fill = start), alpha = 0.2, colour = NA)+
  geom_line()+
  geom_line(data = cal3[cal3$fdepth == -0.5,], aes(fc_time, RMSE, colour = start))+
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
  coord_cartesian(xlim = c(0, max(res4$crps_mean, cal4$RMSE, na.rm = T)))+
  # geom_point()+
  theme_bw(base_size = 14)
p3
ggsave(file.path(plot_dir, 'CRPS_profile_status.png'), p3, dpi = 300,width = 384,height = 216, units = 'mm')

res4$lake <- 'Langtjern'
cal4$lake <- 'Langtjern'
write.csv(res4, file.path(res_dir, 'start_fdepth_mcrps_status.csv'), row.names = F, quote = F)
write.csv(cal4, file.path(res_dir, 'start_fdepth_rmse_status.csv'), row.names = F, quote = F)


res4b <- res4[res4$status != "Inverse Stratified",]
res4b$status <- factor(res4b$status)
cal4b <- cal4[cal4$status != "Inverse Stratified",]
cal4b$status <- factor(cal4b$status)

p3b <- ggplot(res4b, aes(crps_mean,depth, colour = start))+
  geom_path()+
  geom_point()+
  geom_path(data = cal4b, aes(RMSE,depth, colour = start))+
  geom_point(data = cal4b, aes(RMSE,depth, colour = start))+
  ylab('Depth (m)')+
  xlab('CRPS (°C)')+
  facet_wrap(~status, nrow = 1)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  coord_cartesian(xlim = c(0, max(res4$crps_mean, cal4$RMSE, na.rm = T)))+
  # geom_point()+
  theme_bw(base_size = 14)
p3b
ggsave(file.path(plot_dir, 'CRPS_profile_status_b.png'), p3b, dpi = 300,width = 384,height = 216, units = 'mm')

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
  coord_cartesian(xlim = c(0, max(res5$crps_mean, cal5$RMSE, na.rm = T)))+
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

res6$lake <- 'Langtjern'
cal6$lake <- 'Langtjern'

write.csv(res6, file.path(res_dir, 'start_init_date_depth_mcrps.csv'), row.names = F, quote = F)
write.csv(cal6, file.path(res_dir, 'start_init_date_depth_rmse.csv'), row.names = F, quote = F)

p5 <- ggplot(res6[res6$fdepth == -0.5,], aes(init_date, crps_mean, colour = start))+
  geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inverse Stratification', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_point()+
  geom_line(data = cal6[cal6$fdepth == -0.5,], aes(init_date, RMSE, colour = start))+
  geom_point(data = cal6[cal6$fdepth == -0.5,], aes(init_date, RMSE, colour = start))+
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
ggsave(file.path(plot_dir, 'CRPS_fc_annual.png'), p5, dpi = 300,width = 384,height = 216, units = 'mm')

obs_wtemp$fdepth <- factor(obs_wtemp$depths)
obs_wtemp$fdepth <- fct_rev(obs_wtemp$fdepth)
p5b <- ggplot(obs_wtemp, aes(date, obs, colour = fdepth))+
  # ggtitle(main) + 
  ylab('Temperature (°C)') +
  # geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  # geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_line()+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depths'), fill = guide_legend(title = 'Status'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%b-%y'), date_breaks = '2 month')+
  coord_cartesian(xlim = range(res6$init_date))+
  scale_colour_manual(values = dramp(13))+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  xlab('')+
  theme_bw(base_size = 14)
p5b
p5lb <- p5b

p5b <- long_lineplot(obs_wtemp, main = '')+
  # geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  # geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  # geom_rect(data = invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inverse Stratification', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  # geom_vline(data = ice_date, aes(xintercept = ice_date$ice_off, linetype = 'Off'), colour = 'black', size = 1.2)+
  # geom_vline(data = ice_date, aes(xintercept = ice_date$ice_on, linetype = 'On'), colour = 'black', size = 1.2)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depths'), fill = F, linetype = guide_legend(title = 'Ice'))+
  scale_colour_manual(values = d.cols[1:8])+
  scale_linetype_manual(values = c(2,3))+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  coord_cartesian(xlim = range(res6$init_date))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m/%d'), date_breaks = '1 month')+
  xlab('')+
  theme_bw(base_size = 14)
p5b

library(ggpubr)
g1 <- ggarrange(p5, p5b, nrow = 2, align = 'v', labels = 'AUTO')
g1
ggsave(file.path(plot_dir, 'CRPS_profile_fc_annual_obs.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')



##################
#Yearly performance
res7 <- ddply(dat, c('start', 'init_date'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res7)[3] <- 'crps_mean'
res7$init_date <- as.POSIXct(as.character(res7$init_date))

cal7 <- ddply(cal, c('start'), function(x){
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
colnames(cal7)[2:ncol(cal7)] <- as.character(fc_dates)
cal7 <- melt(cal7, id.vars = c('start'))
colnames(cal7)[2:3] <- c('init_date', 'RMSE')
cal7$init_date <- as.POSIXct(as.character(cal7$init_date))

p6 <- ggplot(res7, aes(init_date, crps_mean, colour = start))+
  geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inverse Stratification', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_point()+
  geom_line(data = cal7, aes(init_date, RMSE, colour = start))+
  geom_point(data = cal7, aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('Time')+
  ylab('CRPS (°C)')+
  # facet_wrap(~seas, nrow = 1)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m/%d'), date_breaks = '1 month')+
  coord_cartesian(xlim = range(res7$init_date))+
  scale_colour_manual(values = d.cols[3:8])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  # geom_point()+
  # guides(colour = F, fill = F)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  theme_bw(base_size = 14)
p6
ggsave(file.path(plot_dir, 'CRPS_fc_profile_annual.png'), p6, dpi = 300,width = 384,height = 216, units = 'mm')

library(ggpubr)
g1 <- ggarrange(p5, p5b, nrow = 2, align = 'v', labels = 'AUTO')
g1
ggsave(file.path(plot_dir, 'CRPS_profile_fc_annual_obs.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')


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
  geom_rect(data = invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inverse Stratification', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
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
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  # # geom_point()+
  # guides(colour = F, fill = F)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  theme_bw(base_size = 14)
p5c

g1 <- ggarrange(p5c, p5b, nrow = 2, align = 'v', labels = 'AUTO')
g1
ggsave(file.path(plot_dir, 'CRPS_profile_fc_annual_int_depths_obs.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')

p5d <- ggplot(res6c, aes(init_date, crps_mean, colour = start))+
  geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inverse Stratification', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
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
  coord_cartesian(xlim = as.POSIXct(c('2015-05-15', '2015-06-25')))+
  scale_colour_manual(values = d.cols[3:8])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  # # geom_point()+
  # guides(colour = F, fill = F)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  theme_bw(base_size = 14)
p5d

#exploring obs data


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


p8 <- ggplot(res2, aes(fc_time, crps_mean, colour = fdepth))+
  geom_line()+
  geom_line(data = cal2, aes(fc_time, RMSE, colour = fdepth))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~status*start, nrow = 3)+
  scale_x_continuous(breaks = seq(0,14,2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depth (m)'))+
  scale_colour_manual(values = d.cols[1:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p8
ggsave(file.path(plot_dir, 'CRPS_status_start_depth.png'), p8, dpi = 300,width = 384,height = 216, units = 'mm')

p9 <- ggplot(res3, aes(fc_time, crps_mean, colour = fdepth))+
  geom_line()+
  geom_line(data = cal3, aes(fc_time, RMSE, colour = fdepth))+
  ylab('CRPS (°C)')+
  xlab('Forecast Days')+
  # facet_wrap(~fdepth*status, nrow = 8)+
  facet_wrap(~seas*start, nrow = 4)+
  scale_x_continuous(breaks = seq(0,14,2))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depth (m)'))+
  scale_colour_manual(values = d.cols[1:8])+
  # geom_point()+
  theme_bw(base_size = 14)
p9
ggsave(file.path(plot_dir, 'CRPS_season_start_depth.png'), p9, dpi = 300,width = 384,height = 216, units = 'mm')


##############
#Yearly performance
res7 <- ddply(dat, c('start', 'init_date', 'fdepth'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})
colnames(res7)[4] <- 'crps_mean'
res7$init_date <- as.POSIXct(as.character(res7$init_date))

cal7 <- ddply(cal, c('start', 'fdepth'), function(x){
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
colnames(cal7)[3:ncol(cal7)] <- as.character(fc_dates)
cal7 <- melt(cal7, id.vars = c('start', 'fdepth'))
colnames(cal7)[3:4] <- c('init_date', 'RMSE')
cal7$init_date <- as.POSIXct(as.character(cal7$init_date))

p6 <- ggplot(res7, aes(init_date, crps_mean, colour = fdepth))+
  geom_rect(data = isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inverse Stratification', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_line(data = cal7, aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('Time')+
  ylab('CRPS (°C)')+
  facet_wrap(~start, nrow = 2)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m/%d'), date_breaks = '1 month')+
  coord_cartesian(xlim = range(res7$init_date))+
  scale_colour_manual(values = c(l.cols[1],d.cols[1:8]))+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  # geom_point()+
  # guides(colour = F, fill = F)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  theme_bw(base_size = 14)
p6


####### Profile
dy_err <- aggregate(resid ~ start* fdepth*day, wtemp, mean)
dy_sd <- aggregate(resid ~ start* fdepth*day, wtemp, sd)
dat$day <- ceiling(dat$fc_time)
res8 <- ddply(dat, c('start', 'day', 'fdepth','status'), function(x){
  val = mean(x$crps, na.rm = T)
  sd = sd(x$crps, na.rm = T)
  return(c(val,sd))
})
colnames(res8)[5:6] <- c('mean', 'sd')
res8$depths <- as.numeric(as.character(res8$fdepth))

cal8 <- ddply(cal, c('start', 'fdepth', 'status'), function(x){
  srt <- lapply(fc_dates, function(y){
    
    # sub1 <- unique(dat[(dat$init_date == y),6])
    dif <- difftime(x$fc_date, y, units = 'day')
    sub <- x[dif <= 14 & dif >0,]
    dif <- ceiling(dif[dif <= 14 & dif >0])
    resu = (sub$mod - sub$obs)^2
    # fc_time = round(seq(0.04, 14, length.out = nrow(sub)),2)
    df = data.frame(day = factor(as.numeric(dif)), diff = resu)
    df = ddply(df, 'day', function(z)mean(z[,2], na.rm =T))
    
    if(nrow(df) != 14){
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
colnames(cal8)[4:ncol(cal8)] <- 1:14
cal8 <- melt(cal8, id.vars = c('start', 'fdepth', 'status'))
colnames(cal8)[4:5] <- c('day', 'mean')
cal8$depths <- as.numeric(as.character(cal8$fdepth))
cal8$day <- as.numeric(as.character(cal8$day))


p3b <- ggplot(res8, aes(day, depths, colour = mean))+
  geom_point(aes(size = sd))+
  geom_point(data = cal8, aes(day, depths, colour = mean, size =0.6))+
  facet_wrap(~ status*start, nrow = 3)+
  # scale_color_gradient2(low = "blue", mid = "white", high = "red", name = 'RMSE')+
  scale_color_gradientn(colours = l.cols[1:6], limits = c(0,2))+
  scale_size_continuous(breaks = seq(0, 1.5, 0.25), name = 'Std. Dev')+
  xlab('Forecast Days')+
  coord_cartesian(xlim = c(1,13))+
  ylab('Depth (m)')+
  
  theme_bw(base_size = 16)
p3b

#distributions
#Assign status
res6c$status <- 2
res6c$status[res6c$init_date %in% status$date[status$status == 'Inverse Stratified']] <- 1
res6c$status[res6c$init_date %in% status$date[status$status == 'Stratified']] <- 2
res6c$status[res6c$init_date %in% status$date[status$status == 'Isothermal']] <- 3
res6c$status <- round(na.approx(res6c$status))
res6c$status <- factor(res6c$status)
res6c$status <- factor(res6c$status,levels = c('1','2','3'), labels = c('Inverse Stratified', 'Stratified', 'Isothermal'))


#distributions
cal6c$status <- 2
cal6c$status[cal6c$init_date %in% status$date[status$status == 'Inverse Stratified']] <- 1
cal6c$status[cal6c$init_date %in% status$date[status$status == 'Stratified']] <- 2
cal6c$status[cal6c$init_date %in% status$date[status$status == 'Isothermal']] <- 3
cal6c$status <- round(na.approx(cal6c$status))
cal6c$status <- factor(cal6c$status)
cal6c$status <- factor(cal6c$status,levels = c('1','2','3'), labels = c('Inverse Stratified', 'Stratified', 'Isothermal'))

p10 <- ggplot(dat, aes(x = crps))+
  geom_density(aes(colour = start),size = 1)+
  geom_density(data = cal, aes(x = RMSE, colour = start), size = 1)+
  xlab('')+
  # stat_function(fun = median, geom = 'line')+
  # geom_vline(aes(xintercept = V1, colour = start), linetype = 'dashed', data = res9)+
  # geom_vline(aes(xintercept = V1, colour = start), linetype = 'dashed', data = cal9)+
  facet_wrap(~status, nrow = 3)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  ylab('')+
  xlab('Error (K)')+
  theme_bw(base_size = 14)
p10
ggsave(file.path(plot_dir, 'CRPS_distribution_status.png'), p10, dpi = 300,width = 384,height = 216, units = 'mm')

res6c$lake <- 'Langtjern'
cal6c$lake <- 'Langtjern'
write.csv(res6c, file.path(res_dir, 'start_mcrps_status_year.csv'), row.names = F, quote = F)
write.csv(cal6c, file.path(res_dir, 'start_rmse_status_year.csv'), row.names = F, quote = F)

res9 <- ddply(dat, c('start', 'status'), function(x){
  val = median(x$crps, na.rm = T)
  return(val)
})

cal9 <- ddply(cal, c('start', 'status'), function(x){
  val = median(sqrt((x$mod-x$obs)^2), na.rm = T)
  return(val)
})

dat$fc_day <- factor(ceiling(dat$fc_time))

res10 <- ddply(dat, c('start', 'status', 'fc_day'), function(x){
  val = mean(x$crps, na.rm = T)
  return(val)
})

res10$day <- as.numeric(as.character(res10$fc_day))
colnames(res10)[4] <- 'crps_mean'

p11 <- ggplot(res10, aes(x = day, y = crps_mean))+
  geom_line(aes(colour = start),size = 1)+
  facet_wrap(~status, nrow = 3)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  ylab('')+
  xlab('Days')+
  theme_bw(base_size = 14)
p11

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
