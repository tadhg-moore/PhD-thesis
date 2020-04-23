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


# Aggregate for plot
dat <- ddply(df, c('Lake', 'start', 'init_date'), function(x){
  data.frame(MAE = mean(x$MAE, na.rm = T),
             Bias = mean(x$Bias))
  })
dat2 <- ddply(df2, c('Lake', 'start', 'init_date'), function(x){
  
  # if(sum(is.na(x$obs))>0)return(data.frame(MAE = NA))
  data.frame(MAE = mean(abs(x$mod - x$obs), na.rm = T),
             Bias = mean((x$mod - x$obs), na.rm = T))
})
# dat2 <- dat2[dat2$init_date %in% dat$init_date,]
# Remove Inv Strat
# dat[(dat$status == 'Inverse Stratified'),4:5] <- NA
# dat2[(dat2$status == 'Inverse Stratified'),4:5] <- NA
# Feeagh peaks
arr_len = 0.15
Date = as.POSIXct(c('2007-02-22', '2007-05-16', '2007-06-15', '2007-08-14', '2007-09-25'))
y = c(1.2,1.55,1.5,1.7, 1.85)
yend = c(y-0.5)
fgh.arr <- data.frame(x = Date, y = y, xend =Date, yend = yend,  label = 1:length(Date))

p1 <- ggplot(dat[dat$Lake == 'Feeagh',], aes(init_date, MAE))+
  geom_rect(data = f.isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = f.stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line(aes(colour = start))+
  geom_point(aes(colour = start))+
  geom_line(data = dat2[dat2$Lake == 'Feeagh',],aes(init_date, MAE, colour = start))+
  geom_point(data = dat2[dat2$Lake == 'Feeagh',],aes(init_date, MAE, colour = start))+
  geom_point(data = fgh.arr, aes(Date,  y))+
  geom_segment(data = fgh.arr, aes(x = x, y = y, xend = x, yend = yend), linejoin = 'mitre',
               size = 2, arrow = arrow(length = unit(arr_len, "inches")))+
  geom_text(data = fgh.arr, aes(x = x, y = y, label = label), vjust = 'outside', nudge_y = 0.2, size = 6) +
  # geom_line(data = dat2, aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('')+
  ylab('Error (°C)')+
  # facet_wrap(~lake, nrow = 3, scales = 'free_x')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m-%Y'), date_breaks = '2 month')+
  # coord_cartesian(xlim = range(res6$init_date))+
  coord_cartesian(ylim = c(0,2.5))+#, xlim = as.POSIXct(c('2007-09-01', '2007-12-31')))+
  scale_colour_manual(values = d.cols[3:8])+
  # scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  # guides(colour = F, fill = F)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = NULL, linetype = NA), title = 'Initialization'), fill = F)+
  theme_classic(base_size = 14)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p1


# Langtjern peaks
Date = as.POSIXct(c('2014-09-09', '2014-10-27', '2015-06-05', '2015-06-25', '2015-07-20', '2015-08-14', '2015-09-22', '2015-10-15'))
y = c(2.2, 1.2, 1.9, 2, 1.3, 1.4,1.35,1.7)
yend = c(y-0.3)
ltj.arr <- data.frame(x = Date, y = y, xend =Date, yend = yend,  label = 1:length(Date))

p2 <- ggplot(dat[dat$Lake == 'Langtjern',], aes(init_date, MAE))+
  geom_rect(data = l.isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = l.stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = l.invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inv Strat', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line(aes(colour = start))+
  geom_point(aes(colour = start))+
  geom_line(data = dat2[dat2$Lake == 'Langtjern',],aes(init_date, MAE, colour = start))+
  geom_point(data = dat2[dat2$Lake == 'Langtjern',],aes(init_date, MAE, colour = start))+
  geom_segment(data = ltj.arr, aes(x = x, y = y, xend = x, yend = yend), linejoin = 'mitre',
               size = 2, arrow = arrow(length = unit(arr_len, "inches")))+
  geom_text(data = ltj.arr, aes(x = x, y = y, label = label), vjust = 'outside', nudge_y = 0.15, size = 6) +
  # geom_line(data = dat2, aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('')+
  ylab('Error (°C)')+
  # facet_wrap(~lake, nrow = 3, scales = 'free_x')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = NULL, linetype = NA), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m-%Y'), date_breaks = '2 month')+
  # coord_cartesian(xlim = range(res6$init_date))+
  scale_colour_manual(values = d.cols[3:8])+
  # scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inv Strat'), values = l.cols[c(3,1,2)])+
  # guides(colour = F, fill = F)+
  coord_cartesian(ylim = c(0,2.55), xlim = as.POSIXct(c('2014-08-31 01:00:00','2015-12-08 13:30:00'), tz = 'UTC'))+
  # guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  theme_classic(base_size = 14)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p2

# Kinneret peaks
Date = as.POSIXct(c('2010-11-20', '2011-01-30'))
y = c(1.9, 1.2)
yend = c(y-0.4)
kin.arr <- data.frame(x = Date, y = y, xend =Date, yend = yend,  label = 1:length(Date))

p3 <- ggplot(dat[dat$Lake == 'Kinneret',], aes(init_date, MAE))+
  geom_rect(data = k.isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = k.stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line(aes(colour = start))+
  geom_point(aes(colour = start))+
  geom_line(data = dat2[dat2$Lake == 'Kinneret',],aes(init_date, MAE, colour = start))+
  geom_point(data = dat2[dat2$Lake == 'Kinneret',],aes(init_date, MAE, colour = start))+
  # geom_segment(data = kin.arr, aes(x = x, y = y, xend = x, yend = yend), linejoin = 'mitre',
  #              size = 2, arrow = arrow(length = unit(arr_len, "inches")))+
  # geom_text(data = kin.arr, aes(x = x, y = y, label = label), vjust = 'outside', nudge_y = 0.15, size = 6) +
  # geom_line(data = df2, aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('')+
  ylab('Error (°C)')+
  # facet_wrap(~lake, nrow = 3, scales = 'free_x')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m-%Y'), date_breaks = '2 month')+
  # coord_cartesian(xlim = range(res6$init_date))+
  scale_colour_manual(values = d.cols[3:8])+
  # scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  # guides(colour = F, fill = F)+
  coord_cartesian(ylim = c(0,2.55))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'), fill = F)+
  theme_bw(base_size = 14)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3

g1 <- ggarrange(p1,p2,p3, nrow = 3, align = 'v', labels = 'AUTO', common.legend = F, legend = 'right')
g1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig4b_MAE.png", g1, dpi = 300,width = 384,height = 280, units = 'mm')







p2 <- ggplot(dat[dat$Lake == 'Langtjern',], aes(init_date, MAE, colour = start))+
  geom_rect(data = l.isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = l.stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = l.invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inv Strat', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_point()+
  geom_line(data = dat2[dat2$Lake == 'Langtjern',],aes(init_date, MAE, colour = start))+
  geom_point(data = dat2[dat2$Lake == 'Langtjern',],aes(init_date, MAE, colour = start))+
  xlab('')+
  ylab('MAE (°C)')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inv Strat'), values = l.cols[c(3,1,2)])+
  coord_cartesian(ylim = c(0,2), xlim = as.POSIXct(c('2014-08-31 01:00:00','2015-11-25 13:30:00'), tz = 'UTC'))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = NULL, linetype = NA), title = 'Initialization'))+
  theme_classic(base_size = 14)+
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
p2




p1 <- ggplot(df[df$Lake == 'Feeagh',], aes(init_date, crps_mean, colour = start))+
  geom_rect(data = f.isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = f.stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_point()+
  geom_line(data = df2[df2$Lake == 'Feeagh',],aes(init_date, RMSE, colour = start))+
  geom_point(data = df2[df2$Lake == 'Feeagh',],aes(init_date, RMSE, colour = start))+
  # geom_line(data = df2, aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('')+
  ylab('Error (°C)')+
  # facet_wrap(~Lake, nrow = 3, scales = 'free_x')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m-%Y'), date_breaks = '2 month')+
  # coord_cartesian(xlim = range(res6$init_date))+
  coord_cartesian(ylim = c(0,2.5))+
  scale_colour_manual(values = d.cols[3:8])+
  # scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  # guides(colour = F, fill = F)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'), fill = F)+
  theme_bw(base_size = 14)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1



p2 <- ggplot(df[df$Lake == 'Langtjern',], aes(init_date, crps_mean, colour = start))+
  geom_rect(data = l.isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = l.stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = l.invlims, aes(xmin = xmin, xmax = xmax, fill = 'Inv Strat', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_point()+
  geom_line(data = df2[df2$Lake == 'Langtjern',],aes(init_date, RMSE, colour = start))+
  geom_point(data = df2[df2$Lake == 'Langtjern',],aes(init_date, RMSE, colour = start))+
  # geom_line(data = df2, aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('')+
  ylab('Error (°C)')+
  # facet_wrap(~Lake, nrow = 3, scales = 'free_x')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m-%Y'), date_breaks = '2 month')+
  # coord_cartesian(xlim = range(res6$init_date))+
  scale_colour_manual(values = d.cols[3:8])+
  # scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inv Strat'), values = l.cols[c(3,1,2)])+
  # guides(colour = F, fill = F)+
  coord_cartesian(ylim = c(0,2.5), xlim = as.POSIXct(c('2014-08-31 01:00:00','2015-11-25 13:30:00'), tz = 'UTC'))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  theme_bw(base_size = 14)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2

p3 <- ggplot(df[df$Lake == 'Kinneret',], aes(init_date, crps_mean, colour = start))+
  geom_rect(data = k.isolims, aes(xmin = xmin, xmax = xmax, fill = 'Isothermal', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  geom_rect(data = k.stralims, aes(xmin = xmin, xmax = xmax, fill = 'Stratified', ymin = -Inf, ymax = Inf), alpha = 0.4, inherit.aes = F)+
  guides(fill = guide_legend(title = 'Status'))+
  geom_line()+
  geom_point()+
  geom_line(data = df2[df2$Lake == 'Kinneret',],aes(init_date, RMSE, colour = start))+
  geom_point(data = df2[df2$Lake == 'Kinneret',],aes(init_date, RMSE, colour = start))+
  # geom_line(data = df2, aes(init_date, RMSE, colour = start))+
  # geom_point()+
  xlab('')+
  ylab('Error (°C)')+
  # facet_wrap(~Lake, nrow = 3, scales = 'free_x')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_x_datetime(breaks='month', labels= scales::date_format('%m-%Y'), date_breaks = '2 month')+
  # coord_cartesian(xlim = range(res6$init_date))+
  scale_colour_manual(values = d.cols[3:8])+
  # scale_fill_manual(breaks = c('Isothermal', 'Stratified', 'Inverse Stratification'), values = l.cols[c(3,1,2)])+
  scale_fill_manual(breaks = c('Isothermal', 'Stratified'), values = l.cols[c(1,2)])+
  # guides(colour = F, fill = F)+
  coord_cartesian(ylim = c(0,2.5))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'), fill = F)+
  theme_bw(base_size = 14)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3

g1 <- ggarrange(p1,p2,p3, nrow = 3, align = 'v', labels = 'AUTO', common.legend = F, legend = 'right')
g1
ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig3.png", g1, dpi = 300,width = 384,height = 280, units = 'mm')


