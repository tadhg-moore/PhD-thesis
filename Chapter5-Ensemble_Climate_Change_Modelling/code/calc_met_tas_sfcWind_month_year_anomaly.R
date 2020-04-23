setwd("G:\\ISIMIP")
library(ncdf4)
library(lubridate)
library(openair)
library(hydroTSM)

out_vars <- read.csv('MetaData/ISIMIP_lake_output_vars_v2.csv', stringsAsFactors = F)
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

met_vars <- c('tas', 'sfcWind', 'hurs', 'rsds', 'rlds')

lakes <- list.files('GCM_atmosphere/')

out_fpath <- 'ISIMIP_output/GCM'
dir.create(out_fpath, showWarnings = FALSE)

## GOTM

df <- NULL
for(k in lakes){ #Just extract the first lake for now
  
  meta_ind <- which(meta$ISIMIP_name == k)
  
  
  
  fil_pat = file.path('GCM_atmosphere', k)
  
  # List all netcdf files
  fils = list.files(fil_pat)[grep('dat', list.files(fil_pat))]
  
  #If no netcdf skip
  if(length(fils) == 0){
    message('No met files in ', fil_pat)
    next
  }
  
  for(mod in model){
    for(scen in period){
      
      fils_mod = fils[grep(mod, fils)]
      fil = fils_mod[grep(scen, fils_mod)]
      
      if(length(fil) == 0){
        message('Skipping ', k,' ', mod, ' ', scen)
        next
      }else{
        message('Beginning ', k, ' ', fil, ' ', Sys.time())
      }
      
      fil <- file.path(fil_pat,fil)
      
      # read met file
      met <- read.delim(fil)
      met[,1] <- as.POSIXct(met[,1], tz = 'GMT')
      met <- met[(met[,1] >= '1970-01-01' & met[,1] < '2100-01-01'),]
      met$month <- month(met$time)
      met$year <- year(met$time)
      
      mlt <- melt(met, id.vars = c('time','year', 'month'), measure.vars = met_vars)
      mlt2 <- ddply(mlt, c('year', 'month', 'variable'), .fun = function(x){
        min = min(x$value)
        mean = mean(x$value)
        median = median(x$value)
        max = max(x$value)
        df = data.frame(min = min, mean = mean,median = median, max = max)
      })
      
      mlt3 <- melt(mlt2, id.vars = c('year', 'month', 'variable'))
      colnames(mlt3)[4] <- 'stat'
      
      
      # tas_min <- aggregate(tas ~ year*month, data = met, FUN = min)
      # colnames(tas_min)[3] <- 'value'
      # tas_min$variable <- 'tasmin'
      # tas_max <- aggregate(tas ~ year*month, data = met, FUN = max)
      # colnames(tas_max)[3] <- 'value'
      # tas_max$variable <- 'tasmax'
      # tas_mean <- aggregate(tas ~ year*month, data = met, FUN = mean)
      # colnames(tas_mean)[3] <- 'value'
      # tas_mean$variable <- 'tas'
      # wind_mean <- aggregate(sfcWind ~ year*month, data = met, FUN = mean)
      # colnames(wind_mean)[3] <- 'value'
      # wind_mean$variable <- 'sfcWind'
      
      
      if(scen == 'historical'){
        
        ref <- ddply(mlt3, c('month', 'variable', 'stat'), .fun = function(x)mean(x$value))
        
        anom <- ddply(mlt3, c('year', 'month', 'variable', 'stat'), .fun = function(x){
          ref2 = ref$V1[which(ref$month == x$month[1] & ref$variable == x$variable[1] & ref$stat == x$stat[1])]
          df = data.frame(anom = c(x$value - ref2))
          return(df)
        })
        df2 <- merge(mlt3, anom, c('year', 'month', 'variable', 'stat'))
        
        # ref_tas_min <- ddply(tas_min, c('month'), .fun = function(x){
        #   mean(x$value)
        # })
        # 
        # 
        # ref_tas_min <- mean(tas_min$value)
        # ref_tas_max <- mean(tas_max$value)
        # ref_tas_mean <- mean(tas_mean$value)
        # ref_wind_mean <- mean(wind_mean$value)
        # 
        # 
        # tst <- ddply(tas_min, c('year', 'month', 'variable'), .fun = function(x){
        #   ref = ref_tas_min$V1[ref_tas_min$month == x$month]
        #   anom = data.frame(anom = (x$value - ref))
        #   return(anom)
        # })
        # tas_min$anom <- tas_min$value - ref_tas_min
        # 
        # 
        # tas_max$anom <- tas_max$value - ref_tas_max
        # tas_mean$anom <- tas_mean$value - ref_tas_mean
        # wind_mean$anom <- wind_mean$value - ref_wind_mean
        # 
        # 
        # df2 <- rbind.data.frame(tas_min, tas_max, tas_mean, wind_mean)
        # 
        df2$model <- mod
        df2$scenario <- scen
        df2$lake <- k
        if(is.null(df2)){
          df = df2
        }else{
          df <- rbind.data.frame(df, df2) 
        }
        
      }else{
        
        # tas_min$anom <- tas_min$value - ref_tas_min
        # tas_max$anom <- tas_max$value - ref_tas_max
        # tas_mean$anom <- tas_mean$value - ref_tas_mean
        # wind_mean$anom <- wind_mean$value - ref_wind_mean
        
        anom <- ddply(mlt3, c('year', 'month', 'variable', 'stat'), .fun = function(x){
          ref2 = ref$V1[which(ref$month == x$month[1] & ref$variable == x$variable[1] & ref$stat == x$stat[1])]
          df = data.frame(anom = c(x$value - ref2))
          return(df)
        })
        df2 <- merge(mlt3, anom, c('year', 'month', 'variable', 'stat'))
        
        
        # df2 <- rbind.data.frame(tas_min, tas_max, tas_mean, wind_mean)
        
        df2$model <- mod
        df2$scenario <- scen
        df2$lake <- k
        
        if(is.null(df2)){
          df = df2
        }else{
          df <- rbind.data.frame(df, df2) 
        }
        
      }
      
      
    }
    
  }
  
  df[,5:6] <- round(df[,5:6], 3)
  
  
  write.csv(df, file.path(out_fpath, paste0(k,'_met_', paste0(met_vars, collapse = '_'), '_month_year.csv')), quote = F, row.names = F)
  
  df <- NULL
  
}


summary(df)

met = df

##### Monthly Analysis
# Average across months
df <- ddply(met, c('month', 'lake', 'scenario', 'model', 'variable', 'stat'), .fun = function(x){
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

idx <- which(df2$variable == 'tas' & df2$stat == 'max')
# idx <- which(df2$variable == 'tas' & df2$lake == 'Feeagh')
p1 <- ggplot(df2[idx,], aes(month, mean, colour = lake))+
  geom_hline(yintercept = 0)+
  geom_point(aes(colour = lake), size = 1, alpha = 0.5)+
  # geom_ma(n = 30, linetype = 1)+
  geom_ribbon(aes(ymin = mean -sd, ymax = mean + sd, fill = lake), alpha = 0.5)+
  guides(colour = F)+
  facet_wrap(~scenario, nrow = 4)+
  theme_classic()

p1


