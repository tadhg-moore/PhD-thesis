setwd("G:\\Opt_freq")

fgh <- read.csv('feeagh/run4/analysis/start_fdepth_mcrps_status.csv', stringsAsFactors = F)
ltj <- read.csv('langtjern/run4/analysis/start_fdepth_mcrps_status.csv', stringsAsFactors = F)
ltj$lake <- 'Langtjern'
kin <- read.csv('kinneret/run2/analysis/start_fdepth_mcrps_status.csv', stringsAsFactors = F)

fgh2 <- read.csv('feeagh/run4/analysis/start_fdepth_rmse_status.csv', stringsAsFactors = F)
ltj2 <- read.csv('langtjern/run4/analysis/start_fdepth_rmse_status.csv', stringsAsFactors = F)
kin2 <- read.csv('kinneret/run2/analysis/start_fdepth_rmse_status.csv', stringsAsFactors = F)

df <- rbind.data.frame(fgh, ltj, kin)
df2 <- rbind.data.frame(fgh2, ltj2, kin2)

d.cols <- RColorBrewer::brewer.pal(8, 'Dark2')

meta <- data.frame(lake = c('Feeagh', 'Langtjern', 'Kinneret'),
                   top = c(15.5, 2, 15),
                   btm = c(16, 4.8, 22))

# Remove Inv Strat
df <- df[(df$status != 'Inverse Stratified'),]
df2 <- df2[(df2$status != 'Inverse Stratified'),]
df2$start <- 'NDA'

p1 <- ggplot(df, aes(crps_mean,fdepth, colour = start))+
  geom_path()+
  geom_point()+
  geom_path(data = df2, aes(RMSE,fdepth, colour = start))+
  geom_point(data = df2, aes(RMSE,fdepth, colour = start))+
  ylab('Depth (m)')+
  xlab('Error (°C)')+
  facet_wrap(~lake*status, nrow = 3, scales = 'free_y')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  # geom_point()+
  theme_bw(base_size = 18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p1

ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig7b.png", p1, dpi = 300,width = 304,height = 360, units = 'mm')


