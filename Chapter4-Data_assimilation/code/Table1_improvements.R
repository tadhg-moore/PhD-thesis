setwd("G:\\Opt_freq")

library(ggplot2)

fgh <- read.csv('feeagh/run4/analysis/status_MCRPS_RMSE_improvement.csv', stringsAsFactors = F)
ltj <- read.csv('langtjern/run4/analysis/status_MCRPS_RMSE_improvement.csv', stringsAsFactors = F)
kin <- read.csv('kinneret/run2/analysis/status_MCRPS_RMSE_improvement.csv', stringsAsFactors = F)

fgh$lake <- 'Feeagh'
ltj$lake <- 'Langtjern'
kin$lake <- 'Kinneret'

df <- rbind.data.frame(fgh, ltj, kin)
df[3:5] <- round(df[,3:5],2)
colnames(df)[5] <- 'Improvement (%)'

# Remove Inv Strat
df <- df[(df$status != 'Inverse Stratified'),]

df$hour <- as.numeric(gsub('T','',as.character(df$start)))
p1 <- ggplot(df, aes(hour, `Improvement (%)`, colour = status))+
  geom_point(size = 1.5)+geom_line()+
  facet_wrap(~lake, ncol = 3)+
  coord_cartesian(ylim = c(0,100))+
  xlab('Sampling Frequency (hours)')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Status'))+
  theme_bw(base_size = 20)+
  # theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave('C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\tables\\opt_freq/table1_plot.png', p1, dpi = 300,width = 384,height = 216, units = 'mm')


df2 <- df[order(df$start, df$status),1:6]
colnames(df2) <- c('Frequency', 'Status', 'MCRPS', 'RMSE', 'Improvement (%)', 'Lake')
write.csv(df2, 'C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\tables\\opt_freq/table1.csv', quote = F, row.names = F)
