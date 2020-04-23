setwd("G:\\ISIMIP")
library(ncdf4)
library(lubridate)
library(reshape)
library(rLakeAnalyzer)
library(ggplot2)
library(hydroTSM)
library(plyr)
library(doParallel)

out_vars <- read.csv('MetaData/ISIMIP_lake_output_vars.csv', stringsAsFactors = F)
meta <- read.csv('MetaData/GOTM-ISIMIP-Status_v2.csv', stringsAsFactors = F)
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
stats$ISIMIP_name[stats$RMSE<2 & stats$got_RMSE<2]

dirs = c('GOTM', 'GLM')
# dirs <- dirs[-5]
print(dirs)
# 
# 'bottemp', 'latentheatf', 'lwup', 'schmidtstability', 'sensheatf', 'surftemp', 'thermodepth', 'wholelaketemp'
dly_vars <- c('wholelaketemp','surftemp', 'bottemp', 'latentheatf', 'lwup', 'schmidtstability', 'sensheatf', 'thermodepth')

fpath <- 'ISIMIP_output/'
list.files(fpath)

laks <- list.files(file.path(fpath, 'GOTM'))
laks <- c("Kuivajarvi")

for(target_var in dly_vars){
  
  
  out_fpath <- paste0('ISIMIP_output/', target_var)
  dir.create(out_fpath, showWarnings = F)
  
  
  for(k in laks){
    
    out_df <- NULL
    
    print(paste(k, '[', Sys.time(),']'))
    # if(k == 'Tahoe'){
    #   next
    # }
    
    meta_ind <- which(meta$ISIMIP_name == k)
    lak_nam <- tolower(meta$Lake.Short.Name[meta_ind])
    stat_idx <- which(stats$ISIMIP_name == k)
    if(length(stat_idx) == 0){
      message('Skipping ',k)
      next
    }
    
    if(stats$RMSE[stat_idx] > 2 | stats$got_RMSE[stat_idx] >2){
      message('Model fit too poor for ', k)
      next
    }
    
    for(mod in model){
      
      
      
      for(per in tim_periods){
        
        # # dir.create(file.path(pdf_dir, k, mod, per))
        # if(per=='historical'){
        #   folder = 'historical'
        # }else{
        #   folder = 'future'
        # }
        
        
        fil_pat = file.path(fpath,'GOTM', k, 'isimip-simulations_v3',mod,per)
        fil = list.files(fil_pat)
        if(length(fil) == 0){
          message('No NetCDF files in ', fil_pat)
          next
        }
        fil <- fil[grep(target_var, fil)]
        # fil <- fil[grep(per, fil)]
        
        
        for(nc in fil){
          # nc = fil
          
          nme <- strsplit(nc, '_')[[1]]
          # model = nme[1]
          gcm = mod
          scen = nme[4]
          
          if(scen == 'picontrol'){
            message('Skipping picontrol...')
            next
          }
          
          var <- nme[7]
          if(var == "extcoeff"){
            next
          }
          # print(var)
          
          rind <- which(out_vars$Variable_name == var)
          
          
          
          fid <- nc_open(file.path(fil_pat, nc), write = T)
          
          # lat <- ncvar_get(fid, 'lat')
          # lon <- ncvar_get(fid, 'lon')
          if(var == 'watertemp'){
            z <- abs(ncvar_get(fid, 'z')) #Reverse sign make depths positive
          }
          
          vals <- ncvar_get(fid, var)
          lname <- ncatt_get(fid, varid = var, attname = 'long_name')$value
          
          if(var %in% dly_vars){
            time <- ncvar_get(fid, 'time')
            tstep = 'daily'
            time <- as.POSIXct(time*86400, tz = 'UTC', origin = '1661-01-01 00:00:00')
          }else if(var %in% ann_vars){
            time <- ncvar_get(fid, 'year')
            # time <- c(time[1]+1660,time +1661)
            time <- c(time +1661)
            tstep = 'annual'
            years = as.Date(lubridate::ymd(time, truncated = 2L), format = '%Y-%m-%d')
            time <- as.POSIXct(years, tz = 'UTC', origin = '1661-01-01 00:00:00')
            time <- with_tz(time, tzone = 'UTC')
          }
          
          nc_close(fid)
          
          
          
          dat = data.frame(Date = time, variable = var, value = vals, model = mod, lake = k, year = year(time))
          dat <- dat[(dat$Date > as.POSIXct('1970-01-01') &
                        dat$Date < as.POSIXct('2100-01-01')),]
          
          dat$month <- month(dat$Date)
          dat$year <- year(dat$Date)
          dat$Date <- NULL
          
          # x <- dat[dat$year == dat$year[1] & dat$month == 1 & dat$variable ==dat$variable[1],]
          
          # nodes <- detectCores()
          # cl <- makeCluster(nodes)
          # registerDoParallel(cl)
          
          mlt2 <- ddply(dat, c('month', 'year', 'variable'), .fun = function(x){
            min = min(x$value, na.rm = TRUE)
            if(is.na(min) | is.infinite(min))min = 0
            mean = mean(x$value, na.rm = TRUE)
            if(is.na(mean) | is.infinite(mean))mean = 0
            median = median(x$value, na.rm = TRUE)
            if(is.na(median) | is.infinite(median))median = 0
            max = max(x$value, na.rm = TRUE)
            if(is.na(max) | is.infinite(max))max = 0
            df = data.frame(min = min, mean = mean, median = median, max = max)
            return(df)
          }, .parallel = FALSE)
          
          # stopCluster(cl)
          
          
          mlt3 <- melt(mlt2, id.vars = c('year', 'month', 'variable'))
          colnames(mlt3)[4] <- 'stat'
          
          
          if(scen == 'historical'){
            
            got_ref <- ddply(mlt3, c('month', 'variable', 'stat'), .fun = function(x)mean(x$value))
            
            anom <- ddply(mlt3, c('year', 'month', 'variable', 'stat'), .fun = function(x){
              ref2 = got_ref$V1[which(got_ref$month == x$month[1] & got_ref$variable == x$variable[1] & got_ref$stat == x$stat[1])]
              df = data.frame(anom = c(x$value - ref2))
              return(df)
            })
            df2 <- merge(mlt3, anom, c('year', 'month', 'variable', 'stat'))
            
            
            df2$model <- mod
            df2$scenario <- scen
            df2$lake <- k
            df2$lmodel <- 'GOTM'
            if(is.null(out_df)){
              out_df = df2
            }else{
              out_df <- rbind.data.frame(out_df, df2) 
            }
            
          }else{
            
            anom <- ddply(mlt3, c('year', 'month', 'variable', 'stat'), .fun = function(x){
              ref2 = got_ref$V1[which(got_ref$month == x$month[1] & got_ref$variable == x$variable[1] & got_ref$stat == x$stat[1])]
              df = data.frame(anom = c(x$value - ref2))
              return(df)
            })
            df2 <- merge(mlt3, anom, c('year', 'month', 'variable', 'stat'))
            
            df2$model <- mod
            df2$scenario <- scen
            df2$lake <- k
            df2$lmodel <- 'GOTM'
            if(is.null(out_df)){
              out_df = df2
            }else{
              out_df <- rbind.data.frame(out_df, df2) 
            }
            
          }
        }
        
        # GLM
        #####
        
        fil_pat = file.path(fpath,'GLM', k, 'isimip-simulations_v3',mod,per)
        fil = list.files(fil_pat)
        if(length(fil) == 0){
          message('No NetCDF files in ', fil_pat)
          next
        }
        fil <- fil[grep(target_var, fil)]
        # fil <- fil[grep(per, fil)]
        
        for(nc in fil){
          # nc = fil
          
          nme <- strsplit(nc, '_')[[1]]
          # model = nme[1]
          gcm = mod
          scen = nme[4]
          
          if(scen == 'picontrol'){
            message('Skipping picontrol...')
            next
          }
          
          var <- nme[7]
          if(var == "extcoeff"){
            next
          }
          print(var)
          
          rind <- which(out_vars$Variable_name == var)
          
          
          
          fid <- nc_open(file.path(fil_pat, nc), write = T)
          
          
          
          vals <- ncvar_get(fid, var)
          lname <- ncatt_get(fid, varid = var, attname = 'long_name')$value
          
          if(var %in% dly_vars){
            time <- ncvar_get(fid, 'time')
            tstep = 'daily'
            time <- as.POSIXct(time*86400, tz = 'UTC', origin = '1661-01-01 00:00:00')
          }else if(var %in% ann_vars){
            time <- ncvar_get(fid, 'year')
            # time <- c(time[1]+1660,time +1661)
            time <- c(time +1661)
            tstep = 'annual'
            years = as.Date(lubridate::ymd(time, truncated = 2L), format = '%Y-%m-%d')
            time <- as.POSIXct(years, tz = 'UTC', origin = '1661-01-01 00:00:00')
            time <- with_tz(time, tzone = 'UTC')
          }
          
          nc_close(fid)
          
          
          dat = data.frame(Date = time, variable = var, value = vals, year = year(time))
          dat <- dat[(dat$Date > as.POSIXct('1970-01-01') &
                        dat$Date < as.POSIXct('2100-01-01')),]
          
          
          
          dat$month <- month(dat$Date)
          dat$year <- year(dat$Date)
          dat$Date <- NULL
          
          x <- dat[dat$year == dat$year[1] & dat$month == 1 & dat$variable ==dat$variable[1],]
          mlt2 <- ddply(dat, c('month', 'year', 'variable'), .fun = function(x){
            min = min(x$value, na.rm = TRUE)
            if(is.na(min) | is.infinite(min))min = 0
            mean = mean(x$value, na.rm = TRUE)
            if(is.na(mean) | is.infinite(mean))mean = 0
            median = median(x$value, na.rm = TRUE)
            if(is.na(median) | is.infinite(median))median = 0
            max = max(x$value, na.rm = TRUE)
            if(is.na(max) | is.infinite(max))max = 0
            df = data.frame(min = min, mean = mean, median = median, max = max)
            return(df)
          })
          
          mlt3 <- melt(mlt2, id.vars = c('year', 'month', 'variable'))
          colnames(mlt3)[4] <- 'stat'
          
          
          if(scen == 'historical'){
            
            glm_ref <- ddply(mlt3, c('month', 'variable', 'stat'), .fun = function(x)mean(x$value))
            
            anom <- ddply(mlt3, c('year', 'month', 'variable', 'stat'), .fun = function(x){
              ref2 = glm_ref$V1[which(glm_ref$month == x$month[1] & glm_ref$variable == x$variable[1] & glm_ref$stat == x$stat[1])]
              df = data.frame(anom = c(x$value - ref2))
              return(df)
            })
            df2 <- merge(mlt3, anom, c('year', 'month', 'variable', 'stat'))
            
            
            df2$model <- mod
            df2$scenario <- scen
            df2$lake <- k
            df2$lmodel <- 'GLM'
            if(is.null(out_df)){
              out_df = df2
            }else{
              out_df <- rbind.data.frame(out_df, df2) 
            }
            
          }else{
            
            anom <- ddply(mlt3, c('year', 'month', 'variable', 'stat'), .fun = function(x){
              ref2 = glm_ref$V1[which(glm_ref$month == x$month[1] & glm_ref$variable == x$variable[1] & glm_ref$stat == x$stat[1])]
              df = data.frame(anom = c(x$value - ref2))
              return(df)
            })
            df2 <- merge(mlt3, anom, c('year', 'month', 'variable', 'stat'))
            
            df2$model <- mod
            df2$scenario <- scen
            df2$lake <- k
            df2$lmodel <- 'GLM'
            if(is.null(out_df)){
              out_df = df2
            }else{
              out_df <- rbind.data.frame(out_df, df2) 
            }
            
          }
        }
      }
    }
    
    
    if(!is.null(out_df)){
      out_df[,5:6] <- round(out_df[,5:6],2)
      
      
      write.csv(out_df, file.path(out_fpath, paste0(k, '_',target_var,'_month_year.csv')), quote = F, row.names = F)
    }
    
    # out_df <- NULL
    
  }
}



##### Monthly Analysis
# Average across months
df <- ddply(out_df, c('month', 'lake', 'scenario', 'model', 'variable', 'lmodel', 'stat'), .fun = function(x){
  mean(x$anom)
})
# x <- df[(df$year == df$year[1] & df$lake == df$lake[1] & df$scenario == df$scenario[1] & df$variable == df$variable[1]),]

#Average across GCM
df2 <- ddply(df, c('month', 'lake', 'scenario', 'variable', 'stat'), .fun = function(x){
  # print(head(x))
  mn <- mean(x$V1)
  sd <- sd(x$V1)
  df <- data.frame(mean = mn, sd = sd)
  return(df)
})


p1 <- ggplot(df2, aes(month, mean, colour = lake))+
  geom_hline(yintercept = 0)+
  geom_point(aes(colour = lake), size = 0.1, alpha = 0.5)+
  geom_line()+
  # geom_ma(n = 30, linetype = 1)+
  geom_ribbon(aes(ymin = mean -sd, ymax = mean + sd, fill = lake), alpha = 0.5)+
  guides(colour = F, fill = F)+
  scale_x_continuous(breaks = 1:12, labels = month.abb)+
  facet_wrap(~scenario*stat, nrow = 4)+
  theme_classic()

p1

# df_fin$lmodel <- 'GOTM'
got_df <- df_fin
summary(df_fin)
got_df <- got_df[-which(got_df$lake == 'Geneva'),]
got_df <- got_df[-which(got_df$lake == 'Kivu'),]
smp = got_df[got_df$diff >8,]
ddply(smp, c('lake'), function(x)sd(x$diff))
ggplot(got_df, aes(year, diff, colour = scenario))+
  facet_wrap(~model*lmodel, nrow = 4)+
  geom_line()

write.csv(df_fin, 'MetaData/surftemp_results_lakes_anomaly.csv', row.names = F, quote = F)

