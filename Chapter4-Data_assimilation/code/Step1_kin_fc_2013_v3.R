setwd("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\kinneret")

library(yaml)
library(XML)
library(RSQLite)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ncdf4)
library(gotmtools)
library(GOTMr)
source('../R_Scripts/plot_param.R')
library(reshape2)
library(grid)
library(MASS)                 

fc.days = 14
fcstart = '2011-03-18 23:00:00' #Need 23:00 for restart function
fcend = '2011-05-31'
spin_start = '2010-01-01 00:00:00'
fcstep = 3 #Days between forecast
rest_step <- c(0, 24, 168, 336, 672)
catchment_model = FALSE 
bgc_model = FALSE
compare_rest = FALSE #Compare model run with and w/o restart
input_lake_level = TRUE
out_file = 'Output/opt_freq/run2/fc_3day_2010_v2' #Leave out .csv
num = 1 #Always leave at 1
par_ens <- TRUE
par_runs <- 100
use_pars <- TRUE
par_ens_file <- 'Output/opt_freq/run2/ens_parameters_v1.csv'

#Load in observed Lake data
obs.file2 = 'acpy/profiler_1hr_UTC_2013-15.dat'
obs.file = 'acpy/thermistor_1hr_UTC_2010-11.dat'
zeta.file = 'zeta.dat'
#docb.file = 'acpy/kinneret_doc_watras_correct.obs'
out = 'output.nc'
rest = 'restart.nc'
rest_temp_file = 'restart_temp.nc'
obs_wtemp = load_obs(obs.file)
obs_wtemp2 = load_obs(obs.file2)
colnames(obs_wtemp)[3] <- 'obs'
obs_wtemp <- obs_wtemp[(obs_wtemp$date >= spin_start & obs_wtemp$date < (as.POSIXct(fcend) + 15*24*60*60)),]
# long_lineplot(obs_wtemp)+
#   guides(colour = F)
# long_lineplot(obs_wtemp2)+
#   guides(colour = F)
# zeta <- load_input(input_file = zeta.file)

#
obs_input <- obs_wtemp
# obs_input$date <- obs_input$date + 3*60*60
# obs_input$hour <- factor(hour(obs_input$date))
# plot(obs_input$date, obs_input$hour)
# zeta[,2] <- zeta[,2] + 253 #Tibor et al. 2012
# zeta[,1] <- format(zeta[,1], '%Y-%m-%d %H:%M:%S')
# write.table(zeta, 'zeta_new.dat', quote = F, col.names = F, row.names = F, sep = '\t')
zeta.file = 'zeta_new.dat'
zeta <- load_input(zeta.file)
colnames(zeta) <- c('Date', 'obs')
zeta <- zeta[(zeta[,1] >= spin_start & zeta[,1] < (as.POSIXct(fcend) + 15*24*60*60)),]

ggplot(zeta, aes(Date,obs))+
  geom_line()
orig_hyp <- read.delim('hypsograph_original.dat', header = T)
colnames(orig_hyp) <- c('44', '1')

create_new_hypso <- function(hypsograph, level = NULL, zeta, date, fname){
  if(is.null(level)){
    level <- zeta[which(zeta[,1] == date),2]
  }
  dif = abs(min(hypsograph[,1])) - level
  hypsograph[,1] <- hypsograph[,1] + dif
  write.table(hypsograph, fname, sep = '\t', row.names = F, col.names = T, quote = F)
}

create_new_hypso(hypsograph = orig_hyp, zeta = zeta, date = zeta[1,1], fname = 'hypsograph.dat')

# file.copy('hypsograph_original.dat', 'hypsograph.dat', overwrite = T)
#Create output folder
dir.create('Output/opt_freq/run2/')

#Load in calibration parameter data

db <- 'acpy/kinneret_rd2_v2.db'
acpyXML <- 'acpy/config_acpy_rd2.xml'
plot_calib(dbFile = db, acpyXML = acpyXML)
pars <- get_param(dbFile = db, acpyXML = acpyXML)
pars$lnlikelihood <- abs(pars$lnlikelihood)
mlt <- melt(pars, id.vars = c("id", "run", "time", "lnlikelihood"))
mlt <- mlt[-which(mlt$variable == 'dummy'),]
mlt$variable <- factor(mlt$variable)
rnge <- range(mlt$lnlikelihood)
cutoff <- (min(rnge)+ 0.1*(rnge[2]- rnge[1]))
g1 <- plot_param(df = mlt, hline = cutoff, base_size = 16)
g1
ggsave(paste0(out_file,'_params.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')	
cal_pars <- read_bestparam(db, acpyXML = acpyXML)
bpar <- which.max(pars$lnlikelihood)
if(!use_pars){
  sub_pars <- pars[pars$lnlikelihood < cutoff,]
  mu <- colMeans(sub_pars[,4:(ncol(sub_pars)-2)])
  cov_mat <- cov(sub_pars[,4:(ncol(sub_pars)-2)])
  ens_pars <- as.data.frame(abs(mvrnorm(par_runs, mu = mu, Sigma = cov_mat)))
  ens_pars <- rbind.data.frame(cal_pars[,1:(ncol(cal_pars)-1)], ens_pars)
  ens_pars$id <- c('cal',1:par_runs)
  write.csv(ens_pars, 'Output/opt_freq/run2/ens_parameters_v1.csv', row.names = F, quote = F)
  mlt <- melt(ens_pars, id.vars = 'id')
  
  # mlt <- melt(samp[,3:9])
  mlt$variable <- factor(mlt$variable)
  # RColorBrewer::display.brewer.all(colorblindFriendly = T)
  my.cols <- RColorBrewer::brewer.pal(6, 'Dark2')
  g1 <- plot_param_dist(mlt)
  
  g1
  ggsave(paste0(out_file,'_par_dist.png'), g1, dpi = 300,width = 300,height = 300, units = 'mm')
}else{
  ens_pars <- read.csv(par_ens_file)
}
par.nams <- colnames(ens_pars)[-c(ncol(ens_pars))]
																																	  
# results = list(bl.dis = c(rep(NA,365)),bl.doc = c(rep(NA,365)), gl.dis = c(rep(NA,365)),gl.doc = c(rep(NA,365)),stemp = c(rep(NA,365)), btemp = c(rep(NA,365)), strat = c(rep(NA,365)), lake.doc = c(rep(NA,365)))
# results_rest = list(bl.dis = c(rep(NA,365)),bl.doc = c(rep(NA,365)), gl.dis = c(rep(NA,365)),gl.doc = c(rep(NA,365)),stemp = c(rep(NA,365)), btemp = c(rep(NA,365)), strat = c(rep(NA,365)), lake.doc = c(rep(NA,365)))
file.copy('met_files/met_local_obs_hr.dat', 'meteo_file.dat', overwrite = T)
#Prepare namelist files
#fc.date <- '2018-12-07 04:00:00'
# xml_file = 'kinneret_fc.xml'
# nlev = 88 # Number of depth levels used to describe the water column
input_nml(val = 84, nml = 'gotmrun', par = 'nlev')
input_nml(val = 3600, nml = 'gotmrun', par = 'dt')
input_nml(val = 1, nml = 'gotmrun', par = 'mld_method')
input_nml(val = 3, nml = 'gotmmean', par = 'water_balance_method')
input_nml(val = 'meteo_file.dat', nml = 'airsea', par = 'meteo_file')
input_nml(val = 'swr.dat', nml = 'airsea', par = 'swr_file')
input_nml(val = 'precip.dat', nml = 'airsea', par = 'precip_file')
input_nml(val = 1, nml = 'airsea', par = 'back_radiation_method')
input_nml(val = 1, nml = 'airsea', par = 'hum_method')
input_nml(val = 2, nml = 'airsea', par = 'swr_method')
input_nml(val = 2, nml = 'airsea', par = 'precip_method')
input_nml(val = 1, nml = 'airsea', par = 'precip_factor')
input_nml(val = pars$shf_factor[bpar], nml = 'airsea', par = 'shf_factor')
input_nml(val = 1, nml = 'airsea', par = 'swr_factor')
input_nml(val = 1, nml = 'airsea', par = 'wind_factor')
input_nml(val = pars$k_min[bpar], nml = 'gotmturb', par = 'k_min')
input_nml(val = pars$g1[bpar], nml = 'obs.nml', par = 'g1')
input_nml(val = pars$g2[bpar], nml = 'obs.nml', par = 'g2')                                                          
input_nml(val = 'init_t_prof.dat', nml = 'obs.nml', par = 't_prof_file')
input_nml(val = 's_prof.dat', nml = 'obs.nml', par = 's_prof_file')
input_nml(val = 0.55, nml = 'obs.nml', par = 'A')
input_nml(val = 3, nml = 'obs.nml', par = 'zeta_method')
input_nml(val = 'zeta.dat', nml = 'obs.nml', par = 'zeta_file')
input_nml(val = 0, nml = 'obs.nml', par = 'zeta_offset')

init_prof(obs.file, '2010-05-01 00:00:00', 'init_t_prof.dat')

if(bgc_model == TRUE){
  input_nml(val = TRUE, nml = 'fabm.nml', par = 'fabm_calc')
  input_nml(val = TRUE, nml = 'fabm.nml', par = 'bioshade_feedback')
}else{
  input_nml(val = FALSE, nml = 'fabm.nml', par = 'fabm_calc')
  input_nml(val = FALSE, nml = 'fabm.nml', par = 'bioshade_feedback')
}

fc_init <- seq.POSIXt(from = as.POSIXct(paste(fcstart), tz = 'UTC'), to = as.POSIXct(fcend, tz = 'UTC'), by = paste(fcstep,'days'))

file.copy('streams_surface.nml', 'streams.nml', overwrite = T)

#Append forecast data to observed data and write to file and plot data for GOTM.

wdir <- "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\kinneret\\"
# met_dir <- "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\kinneret\\met_files\\"
# catchdir <- "C:\\Users\\mooret\\Desktop\\Git\\PROGNOS\\kinneret\\catchment_model\\"

#fc_dir <- "C:\\Users\\mooret\\Desktop\\Git\\PROGNOS\\kinneret\\forecast_files\\"

#Begin Loop!
for(i in 1:length(fc_init)){ #length(fc_init)
  
  fc.date <- fc_init[i]
  # indx1 <- which(fc_init[i] == fc_init)
  fc.enddate <- (fc.days*24*60*60 + as.POSIXct(fc.date))
  
  #met_file <- paste0(fc_dir,list.files(fc_dir)[grep(paste0('met_',fc.ref), list.files(fc_dir))])
  # met_gotm_file <- paste0(met_dir,list.files(met_dir)[grep(paste0('obs'), list.files(met_dir))])
  # 
  # # met <- read.delim(met_file, sep = '\t', header = F, skip =1)
  # # met$DateTime <- as.POSIXct(paste(met[,1], met[,2]), tz = 'UTC')
  # # met <- met[,c(9,3:8)]
  # # colnames(met) <- paste0('V',1:7)
  # met_gotm <- read.delim(met_gotm_file, sep = '\t', header = F)
  # met_gotm[,1] <- as.POSIXct(met_gotm[,1], tz = 'UTC')
  # met_gotm <- met_gotm[(met_gotm[,1] <= fc.enddate),]
  # 
  # 
  # met_gotm[,1] <- format(met_gotm[,1], format = '%Y-%m-%d %H:%M:%S')
  # write.table(met_gotm, paste0(met_dir,'obs_met_fc.dat'),col.names = F, row.names = F, sep = '\t', quote = F)
  
  ########## Make plots ###################
  #make plots
  # met_gotm[,1] <- as.POSIXct(met_gotm[,1], tz = 'UTC')
  # met_gotm$ws <- sqrt(met_gotm[,2]^2 + met_gotm[,3]^2)
  # 
  # ind = which(met_gotm[,1] >= (as.POSIXct(fcstart, tz = 'UTC') - 7*24*60*60))
  # 
  # wnd <- ggplot(data = met_gotm[ind,], aes(V1, ws))+
  #   geom_point()+
  #   geom_line()+
  #   xlab('Time')+
  #   ylab('Wind Speed (m/s)')+
  #   theme_bw()+
  #   annotate("text", label = "Observed", x = (as.POSIXct(fc.date) - 5*24*60*60), y = 20, color = "black")+
  #   annotate("text", label = "Forecast", x = (as.POSIXct(fc.date) + 7*24*60*60), y = 20, color = "black")+
  #   geom_vline(xintercept = as.POSIXct(fc.date))+
  #   coord_cartesian(ylim = c(0,25))
  # 
  # mslp = ggplot(data = met_gotm[ind,], aes(V1,V4))+
  #   geom_line()+
  #   coord_cartesian(ylim = c(960,1040))+
  #   geom_hline(yintercept = 1013, linetype = 'dashed')+
  #   geom_vline(xintercept = as.POSIXct(fc.date))+
  #   xlab('Time')+
  #   ylab('Mean Sea Level Pressure (mbar)')+
  #   annotate("text", label = "Observed", x = (as.POSIXct(fc.date) - 5*24*60*60), y = 1030, color = "black")+
  #   annotate("text", label = "Forecast", x = (as.POSIXct(fc.date) + 7*24*60*60), y = 1030, color = "black")+
  #   theme_bw()
  # 
  # temp = ggplot(data = met_gotm[ind,], aes(V1,V5))+
  #   geom_line(aes(colour = 'AirT'))+
  #   geom_line(aes(V1, V6, colour = 'DewT'))+
  #   coord_cartesian(ylim = c(-5,25))+
  #   geom_vline(xintercept = as.POSIXct(fc.date))+
  #   geom_hline(yintercept = 0)+
  #   scale_colour_manual(values = c('red','green'))+
  #   annotate("text", label = "Observed", x = (as.POSIXct(fc.date) - 5*24*60*60), y = 20, color = "black")+
  #   annotate("text", label = "Forecast", x = (as.POSIXct(fc.date) + 7*24*60*60), y = 20, color = "black")+
  #   xlab('Time')+
  #   ylab('Temperature (C)')+
  #   theme_bw()
  # 
  # cc = ggplot(data = met_gotm[ind,], aes(V1, V7))+
  #   geom_bar(stat = 'identity',
  #            position ='dodge',
  #            fill = 'grey',
  #            colour = 'grey')+
  #   coord_cartesian(ylim = c(0,1))+
  #   annotate("text", label = "Observed", x = (as.POSIXct(fc.date) - 5*24*60*60), y = 0.8, color = "black")+
  #   annotate("text", label = "Forecast", x = (as.POSIXct(fc.date) + 7*24*60*60), y = 0.8, color = "black")+
  #   geom_vline(xintercept = as.POSIXct(fc.date))+
  #   xlab('Time')+
  #   ylab('Cloud Cover (fraction)')+
  #   theme_bw()
  # 
  # 
  # grid.arrange(wnd,mslp,temp,cc, top = textGrob(paste("Lough kinneret - Yr Forecast at ", fc.date),gp=gpar(fontsize=15,font=3)))
  
  
  ###############
  
  
  
  #Append forecast data to observed data and write to file and plot data for INCA-C.
  if(catchment_model == TRUE){
    
  }
  
  
  ############# GOTM Lake Model #################
  
  # Set working directory and extract start and stop dates from met file
  
  setwd(wdir)
  
  for(tstep in rest_step){
    
    start = format(as.POSIXct(spin_start, tz = 'UTC'), '%Y-%m-%d %H:%M:%S')
    stop = format((fc.date -23*60*60 -tstep*60*60), '%Y-%m-%d %H:%M:%S')  #format((fc.days*24*60*60 + as.POSIXct(fc.date) -23*60*60), '%Y-%m-%d %H:%M:%S') 
    #Reset parameters
    input_nml(val = pars$shf_factor[bpar], nml = 'airsea', par = 'shf_factor')
    # input_nml(val = pars$swr_factor[bpar], nml = 'airsea', par = 'swr_factor')
    # input_nml(val = pars$wind_factor[bpar], nml = 'airsea', par = 'wind_factor')
    input_nml(val = pars$k_min[bpar], nml = 'gotmturb', par = 'k_min')
    input_nml(val = pars$g1[bpar], nml = 'obs.nml', par = 'g1')
    input_nml(val = pars$g2[bpar], nml = 'obs.nml', par = 'g2')
    input_nml(val = start, nml = 'gotmrun', par = 'start')
    input_nml(val = stop, nml = 'gotmrun', par = 'stop')
    input_nml(val = FALSE, nml = 'gotmrun', par = 'restart_offline')
    input_nml(val = FALSE, nml = 'gotmrun', par = 'restart_allow_missing_variable')
    
    if(bgc_model == TRUE){
      #Edit fabm.yaml file
      dom <- yaml.load_file("fabm_dom.yaml")
      #Parameters
      kOM1 = 0 #0.8966003286335084 # OM1 degradation (yr**-1) #Effect when raised to 300
      kOM2 = 0.1 #0.1487389656795044  # OM2 degradation (yr**-1) #Controls DOMb concentration
      Km_O2 = 1.23e-2 #1.23e7 # Respiration (mmol/m**3)
      Km_NO3 = 1e-2 # Denitrification (mmol/m3)
      Kin_O2 = 0.3 # Inhibition of denitrification by O2 (mmol/m**3)
      oc_DOM = 0 #8.3e-4 # Optical cross-section of DOM (m**2.mmolC) #Slight
      qy_DOM = 1e-3#8.3e-3 # Quantum yield (mmolC/mol)
      f_par = 0.45 #0.45 # Fraction of PAR in incoming solar radiation (Unitless)
      e_par = 240800 # Average energy of PAR photons (J/mol)
      theta = 1 #1.46730801290982  # Temperature adjustment coefficient (unitless) #Large effect on DOC concentrations
      k_floc = 1e-09 #3.156974352812838e-15           # Flocculation coefficient (s**-1)
      rfc = 6.625 # carbon : nitrogen ratio (mol C/mol N), default = 6.625
      #Initialization
      DOMa = 0 #Has a small effect
      DOMb = 8500
      
      dom$instances$dom$parameters$kOM1 <- kOM1
      dom$instances$dom$parameters$kOM2 <- kOM2
      dom$instances$dom$parameters$Km_O2 <- Km_O2
      dom$instances$dom$parameters$Km_NO3 <- Km_NO3
      dom$instances$dom$parameters$Kin_O2 <- Kin_O2
      dom$instances$dom$parameters$oc_DOM <- oc_DOM
      dom$instances$dom$parameters$qy_DOM <- qy_DOM
      dom$instances$dom$parameters$f_par <- f_par
      dom$instances$dom$parameters$e_par <- e_par
      dom$instances$dom$parameters$theta <- theta
      dom$instances$dom$parameters$k_floc <- k_floc 
      dom$instances$dom$parameters$rfc <- rfc
      #Initialization values
      dom$instances$dom$initialization$DOMa <- DOMa
      dom$instances$dom$initialization$DOMb <- DOMb
      
      
      
      write_yaml(dom, 'fabm.yaml')
      file.copy('output_acpy.yaml', 'output.yaml', overwrite = T)
      file.copy('fabm_input_INCA.nml', 'fabm_input.nml', overwrite = T)
      file.copy('streams_fc.nml', 'streams.nml', overwrite = T)
      file.copy('gotm_fabm_true.nml', 'gotm_fabm.nml', overwrite = T)
      
      system('yaml_cli -i fabm.yaml -o fabm.yaml -b instances:cyanobacteria:parameters:nitrogen_fixation true')
    }else{
      file.copy(from = 'output_wtemp_zeta.yaml', to = 'output.yaml', overwrite = TRUE)
    }
    
    # run_gotm()
    system('../GOTMr\\inst\\extbin\\win64GOTM/gotm.exe')
    
    
    #Copy restart file
    file.copy(rest, rest_temp_file, overwrite = T)
    
    #Reset nml file
    start = format((fc.date -23*60*60-tstep*60*60), format = '%Y-%m-%d %H:%M:%S')
    stop = format((fc.enddate - 23*60*60), format = '%Y-%m-%d %H:%M:%S')
    input_nml(val = start, nml = 'gotmrun', par = 'start')
    input_nml(val = stop, nml = 'gotmrun', par = 'stop')
    input_nml(val = TRUE, nml = 'gotmrun', par = 'restart_offline')
    
    obs_samp <- obs_wtemp[(obs_wtemp$date >= start & obs_wtemp$date <= stop),]                                                               
    
    if(bgc_model == TRUE){
      file.copy('gotm_fabm_true.nml', 'gotm_fabm.nml', overwrite = T)
    }

    if(compare_rest){
      #Load in forecast data 
      wtr_fc1 = get_var(out, 'temp')
      domb_fc1 = get_var(out, 'dom_DOMb')
      deps1 = get_var(out, 'z')
      
      obs_fc = obs_wtemp[(which(obs_wtemp[,1] >= wtr_fc1[1,1] & obs_wtemp[,1] <= wtr_fc1[nrow(wtr_fc1),1] )),]
      fgh.docb_fc = fgh.docb[(which(fgh.docb[,1] >= as.POSIXct(start, tz = 'UTC') & fgh.docb[,1] < as.POSIXct(stop, tz = 'UTC'))),]
      
      if(nrow(fgh.docb_fc) != 0){
        domb_fc1 <- merge(fgh.docb_fc[,1], domb_fc1, by = 1)
        domb_deps <- merge(fgh.docb_fc[,1], deps1,by = 1)
        domb_fc1 = setmodDepths(domb_fc1, deps1,fgh.docb_fc)
        results[["lake.doc"]][indx1] = rmse(domb_fc1[,3], fgh.docb_fc[,3])
      }
      
      tmp_fc1 <- match_tstep(obs_fc, wtr_fc1)
      tmp_deps <- match_tstep(obs_fc, deps1)
      tmp_fc1 = setmodDepths(tmp_fc1, tmp_deps,obs_fc)
      lims = range(domb_fc1[,3], fgh.docb_fc[,3])
      stats1 = sum_stat(tmp_fc1, obs_fc,depth = T, depth.range = c(0,-5))
      stats2 = sum_stat(tmp_fc1, obs_fc,depth = T, depth.range = c(-30,-44))
      results[['stemp']][indx1] <- stats1$RMSE
      results[['btemp']][indx1] <- stats2$RMSE
      
      #MLD
      obs_mld = ts.mld(obs_fc)
      fc_mld = ts.mld(tmp_fc1)
      results[['strat']][indx1] <- rmse(fc_mld[,2], obs_mld[,2])
    }
     
     #Begin parameter ensemble
     if(par_ens){
       for(j in 1:nrow(ens_pars)){
         
         ## Copy restart file back 
         file.copy(rest_temp_file, rest, overwrite = T)
         
         #Input observed data
         rest_temp = get_vari(rest, 'temp')
         deps1 = get_vari(rest, 'z')[1,-1]
         obs_rt = obs_input[(which(obs_input[,1] == rest_temp[1,1])),]
         
         if(input_lake_level == TRUE){
           di = get_vari(rest, 'zi')
           # level = zeta[(which(zeta[,1] == start)),2]
           level = zeta[(which.min(abs(zeta[,1] - as.POSIXct(start, tz = 'UTC')))),2]
           
           deps1 <- seq(-0.25,-(level-0.25), length.out = (length(deps1)))
           new_di <- seq(0,-level, length.out = (length(di)-1))
           create_new_hypso(hypsograph = orig_hyp, level = level, zeta = zeta, date = start, fname = 'hypsograph.dat')
         }
         
         if(nrow(obs_rt) == 0){
           message('No data at ', rest_temp[1,1])
           new_date = obs_wtemp[(findInterval(as.POSIXct(start, tz = 'UTC'), obs_wtemp[,1])),1]
           obs_rt = obs_wtemp[(which(obs_wtemp[,1] == new_date)),]
           # 
           # #Reset nml file
           # start = format((new_date), format = '%Y-%m-%d %H:%M:%S')
           # fc.enddate <- (fc.days*24*60*60 + as.POSIXct(new_date))
           # stop = format((fc.enddate), format = '%Y-%m-%d %H:%M:%S')
           # input_nml(val = start, nml = 'gotmrun', par = 'start')
           # input_nml(val = stop, nml = 'gotmrun', par = 'stop')
           
           write(paste0('No data at ', rest_temp[1,1], ' for T', tstep, ' ', Sys.time()),
                 file="Output/opt_freq/run2/log.txt",append=TRUE)
           write(paste0('Using data from ', new_date, ' for T', tstep, ' ', Sys.time()),
                 file="Output/opt_freq/run2/log.txt",append=TRUE)
           # next
         }
         depths = deps1
         obs_interp = rev(approx(obs_rt[,2],obs_rt[,3],depths, rule = 2)$y)
         
         ##################################
         #Input observed data into model
         fid <- nc_open(rest, write = T)
         var1 = ncvar_get(fid, 'temp')
         #samp = 1:50
         ncvar_put(fid, varid = 'temp', vals = obs_interp)
         if(input_lake_level == TRUE){
           ncvar_put(fid, varid = 'z', vals = deps1)
           ncvar_put(fid, varid = 'zi', vals = new_di)
         }
         #ncvar_put(fid, varid = 'temp', vals = samp)
         nc_close(fid)
         
         ##################################
         if(bgc_model){
           if(sum(fgh.docb[,1] == rest_temp[1,1]) == 1){
             obs_domb_interp = rep(fgh.docb_fc[1,3], length(depths))
             fid <- nc_open(rest, write = T)
             ncvar_put(fid, varid = 'dom_DOMb', vals = obs_domb_interp)
             nc_close(fid)
           }
         }
         
         #Input parameters
         for(k in par.nams){
           indx <- which(k == par.nams)
           if(k == 'k_min'){
             input_nml(ens_pars[j, indx], 'gotmturb', k)
           }else if(k %in% c('g1','g2')){
             input_nml(ens_pars[j, indx], 'obs.nml', k)
           }else{
             input_nml(ens_pars[j, indx], 'airsea', k)
           }
         }
         
         #Re-run with observed data input
         run_gotm()
         
         #Load in forecast data 
         wtr_fc1 = get_vari(out, 'temp')
         
         if(bgc_model){
           domb_fc1 = get_vari(out, 'dom_DOMb')
           if(nrow(fgh.docb_fc) != 0){
             domb_fc1 <- merge(fgh.docb_fc[,1], domb_fc1, by = 1)
             domb_deps <- merge(fgh.docb_fc[,1], deps1,by = 1)
             domb_fc1 = setmodDepths(domb_fc1, deps1,fgh.docb_fc)
             results_rest[["lake.doc"]][indx1] = rmse(domb_fc1[,3], fgh.docb_fc[,3])
           }
         }
         
         deps1 = get_vari(out, 'z')
         
         #Subset to 14-day forecast
         wtr_fc1 <- wtr_fc1[(wtr_fc1[,1] >= (fc.date - 23*60*60)),]
         deps1 <- deps1[(deps1[,1] >= (fc.date - 23*60*60)),]
         
         if(compare_rest){
           tmp_fc1 <- match_tstep(obs_fc, wtr_fc1)
           tmp_deps <- match_tstep(obs_fc, deps1)
           tmp_fc1 = setmodDepths(tmp_fc1, tmp_deps,obs_fc)
         }else{
           tmp_fc1 = setmodDepths(mod.val = wtr_fc1, mod.dep = deps1, depths = unique(obs_wtemp$depths))
         }
         colnames(tmp_fc1)[3] <- 'mod'
         
         obs_sub <- merge(tmp_fc1, obs_samp, by.x = c(1,2), by.y = c(1,2), all.y = T)
         obs_sub$resid <- obs_sub$mod - obs_sub$obs
         # head(obs_sub)
         # plot(obs_sub[(obs_sub$depths > -1),1], obs_sub[(obs_sub$depths > -1),5], type ='l')
         # tmp_fc1 <- merge(tmp_fc1, obs_sub[,-c(3,4)], by = 1:2)
         tmp_fc1 <- obs_sub
         tmp_fc1 <- tmp_fc1[order(tmp_fc1$date, -tmp_fc1$depths),]
         
         if(input_lake_level){
           mod_zeta <- get_vari(out, 'zeta')
           colnames(mod_zeta)[2] <- 'mod'
           mod_zeta[,2] <- mod_zeta[,2] + level
           mod_zeta <- mod_zeta[(mod_zeta[,1] >= (fc.date - 23*60*60)),]
           mod_zeta <- merge(zeta, mod_zeta, by = 1)
           mod_zeta$resid <- mod_zeta$mod - mod_zeta$obs
           
           #Set column names
           mod_zeta$init_date <- format((fc.date - 23*60*60), '%Y-%m-%d %H:%M:%S')
           mod_zeta$date <- format(mod_zeta$date, '%Y-%m-%d %H:%M:%S')
           mod_zeta$start <- paste0('T', tstep)
           colnames(mod_zeta)[1] <- 'fc_date'
           
           mod_zeta <- mod_zeta[,c('start', 'init_date', 'fc_date', 'mod', 'resid')]
           
         }
         
         if(compare_rest){
           lims = range(domb_fc1[,3], fgh.docb_fc[,3])
           stats1 = sum_stat(tmp_fc1, obs_fc,depth = T, depth.range = c(0,-5))
           stats2 = sum_stat(tmp_fc1, obs_fc,depth = T, depth.range = c(-30,-44))
           results_rest[['stemp']][indx1] <- stats1$RMSE
           results_rest[['btemp']][indx1] <- stats2$RMSE
           
           #MLD
           obs_mld = ts.mld(obs_fc)
           fc_mld = ts.mld(tmp_fc1)
           results_rest[['strat']][indx1] <- rmse(fc_mld[,2], obs_mld[,2]) 
         }
         
         #Set column names
         tmp_fc1$init_date <- format((fc.date - 23*60*60), '%Y-%m-%d %H:%M:%S')
         tmp_fc1$date <- format(tmp_fc1$date, '%Y-%m-%d %H:%M:%S')
         tmp_fc1$start <- paste0('T', stringr::str_pad(tstep, 3, pad = '0'))
         tmp_fc1$par_id <- stringr::str_pad(as.character(ens_pars$id[j]), 3, pad = '0')
         
         colnames(tmp_fc1)[1] <- 'fc_date'
         
         tmp_fc1 <- tmp_fc1[,c('start', 'init_date', 'fc_date', 'depths', 'mod', 'resid', 'par_id')]
         
         if(i == 1 & j == 1 & tstep == rest_step[1]){
           all_tmp = NULL
           all_zeta = NULL
         }
         
         if((i == 1 & j == 1 & tstep == rest_step[1]) | is.null(all_tmp)){
           all_tmp <- tmp_fc1
           all_zeta <- mod_zeta
           }else{
             all_tmp <- rbind.data.frame(all_tmp, tmp_fc1)
             all_zeta <- rbind.data.frame(all_zeta, mod_zeta)
             }
         
         lab <- format((fc.date - 23*60*60), '%Y_%m_%d')
         
         if(tstep == rest_step[length(rest_step)] & j == nrow(ens_pars)){
           write.csv(all_tmp, paste0(out_file,'_',lab,'.csv'), row.names = F, quote = F)
           write.csv(all_zeta, paste0(out_file,'_zeta_',lab,'.csv'), row.names = F, quote = F)
           all_tmp <- NULL
           # num = num + 1
         }
       }
     }else{ #Using no parameter ensemble
       
       ## Copy restart file back 
       file.copy(rest_temp_file, rest, overwrite = T)
       
       #Input observed data
       rest_temp = get_vari(rest, 'temp')
       deps1 = get_vari(rest, 'z')[1,-1]
       obs_rt = obs_wtemp[(which(obs_wtemp[,1] == rest_temp[1,1])),]
       
       if(input_lake_level == TRUE){
         di = get_vari(rest, 'zi')
         # level = zeta[(which(zeta[,1] == start)),2]
         level = zeta[(which.min(abs(zeta[,1] - as.POSIXct(start, tz = 'UTC')))),2]
         
         deps1 <- seq(-0.25,-(level-0.25), length.out = (length(deps1)))
         new_di <- seq(0,-level, length.out = (length(di)-1))
         create_new_hypso(hypsograph = orig_hyp, level = level, zeta = zeta, date = start, fname = 'hypsograph.dat')
       }
       
       if(nrow(obs_rt) == 0){
         message('No data at ', rest_temp[1,1])
         next
       }
       depths = deps1
       obs_interp = rev(approx(obs_rt[,2],obs_rt[,3],depths, rule = 2)$y)
       
       ##################################
       #Input observed data into model
       fid <- nc_open(rest, write = T)
       var1 = ncvar_get(fid, 'temp')
       #samp = 1:50
       ncvar_put(fid, varid = 'temp', vals = obs_interp)
       if(input_lake_level == TRUE){
         ncvar_put(fid, varid = 'z', vals = deps1)
         ncvar_put(fid, varid = 'zi', vals = new_di)
       }
       #ncvar_put(fid, varid = 'temp', vals = samp)
       nc_close(fid)
       
       ##################################
       if(bgc_model){
         if(sum(fgh.docb[,1] == rest_temp[1,1]) == 1){
           obs_domb_interp = rep(fgh.docb_fc[1,3], length(depths))
           fid <- nc_open(rest, write = T)
           ncvar_put(fid, varid = 'dom_DOMb', vals = obs_domb_interp)
           nc_close(fid)
         }
       }
       
       #Re-run with observed data input
       run_gotm()
       
       #Load in forecast data 
       wtr_fc1 = get_vari(out, 'temp')
       
       if(bgc_model){
         domb_fc1 = get_vari(out, 'dom_DOMb')
         if(nrow(fgh.docb_fc) != 0){
           domb_fc1 <- merge(fgh.docb_fc[,1], domb_fc1, by = 1)
           domb_deps <- merge(fgh.docb_fc[,1], deps1,by = 1)
           domb_fc1 = setmodDepths(domb_fc1, deps1,fgh.docb_fc)
           results_rest[["lake.doc"]][indx1] = rmse(domb_fc1[,3], fgh.docb_fc[,3])
         }
       }
       
       deps1 = get_vari(out, 'z')
       
       #Subset to 14-day forecast
       wtr_fc1 <- wtr_fc1[(wtr_fc1[,1] >= (fc.date - 23*60*60)),]
       deps1 <- deps1[(deps1[,1] >= (fc.date - 23*60*60)),]
       
       if(compare_rest){
         tmp_fc1 <- match_tstep(obs_fc, wtr_fc1)
         tmp_deps <- match_tstep(obs_fc, deps1)
         tmp_fc1 = setmodDepths(tmp_fc1, tmp_deps,obs_fc)
       }else{
         tmp_fc1 = setmodDepths(mod.val = wtr_fc1, mod.dep = deps1, depths = unique(obs_wtemp$depths))
       }
       colnames(tmp_fc1)[3] <- 'mod'
       
       obs_sub <- merge(tmp_fc1, obs_wtemp, by.x = c(1,2), by.y = c(1,2), all.x = T)
       obs_sub$resid <- obs_sub$mod - obs_sub$obs
       # head(obs_sub)
       # plot(obs_sub[(obs_sub$depths > -1),1], obs_sub[(obs_sub$depths > -1),5], type ='l')
       tmp_fc1 <- merge(tmp_fc1, obs_sub[,-c(3,4)], by = 1:2)
       tmp_fc1 <- tmp_fc1[order(tmp_fc1$date, -tmp_fc1$depths),]
       
       if(input_lake_level){
         mod_zeta <- get_vari(out, 'zeta')
         colnames(mod_zeta)[2] <- 'mod'
         mod_zeta[,2] <- mod_zeta[,2] + level
         mod_zeta <- mod_zeta[(mod_zeta[,1] >= (fc.date - 23*60*60)),]
         mod_zeta <- merge(zeta, mod_zeta, by = 1)
         mod_zeta$resid <- mod_zeta$mod - mod_zeta$obs
         
         #Set column names
         mod_zeta$init_date <- format((fc.date - 23*60*60), '%Y-%m-%d %H:%M:%S')
         mod_zeta$date <- format(mod_zeta$date, '%Y-%m-%d %H:%M:%S')
         mod_zeta$start <- paste0('T', tstep)
         colnames(mod_zeta)[1] <- 'fc_date'
         
         mod_zeta <- mod_zeta[,c('start', 'init_date', 'fc_date', 'mod', 'resid')]
         
       }
       
       if(compare_rest){
         lims = range(domb_fc1[,3], fgh.docb_fc[,3])
         stats1 = sum_stat(tmp_fc1, obs_fc,depth = T, depth.range = c(0,-5))
         stats2 = sum_stat(tmp_fc1, obs_fc,depth = T, depth.range = c(-30,-44))
         results_rest[['stemp']][indx1] <- stats1$RMSE
         results_rest[['btemp']][indx1] <- stats2$RMSE
         
         #MLD
         obs_mld = ts.mld(obs_fc)
         fc_mld = ts.mld(tmp_fc1)
         results_rest[['strat']][indx1] <- rmse(fc_mld[,2], obs_mld[,2]) 
       }
       
       #Set column names
       tmp_fc1$init_date <- format((fc.date - 23*60*60), '%Y-%m-%d %H:%M:%S')
       tmp_fc1$date <- format(tmp_fc1$date, '%Y-%m-%d %H:%M:%S')
       tmp_fc1$start <- paste0('T', tstep)
       colnames(tmp_fc1)[1] <- 'fc_date'
       
       tmp_fc1 <- tmp_fc1[,c('start', 'init_date', 'fc_date', 'depths', 'mod', 'resid')]
       
       if(i == 1){
         all_tmp = NULL
       }
       
       if((i == 1 & tstep == rest_step[1]) | is.null(all_tmp)){
         all_tmp <- tmp_fc1
         all_zeta <- mod_zeta
       }else{
         all_tmp <- rbind.data.frame(all_tmp, tmp_fc1)
         all_zeta <- rbind.data.frame(all_zeta, mod_zeta)
       }
       
       if(nrow(all_tmp) > 200000 & tstep == rest_step[length(rest_step)]){
         write.csv(all_tmp, paste0(out_file,'_run',num,'.csv'), row.names = F, quote = F)
         write.csv(all_zeta, paste0(out_file,'_zeta_run',num,'.csv'), row.names = F, quote = F)
         
         all_tmp <- NULL
         num = num + 1
       }
       
     }
    
    message(paste('Finished forecast for', fc_init[i], 'at', paste0('T', tstep)))
  }
}


