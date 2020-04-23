setwd("G:\\Opt_freq")

library(ggplot2)

fgh <- read.csv('feeagh/run4/analysis/start_mcrps_status_year.csv', stringsAsFactors = T)
ltj <- read.csv('langtjern/run4/analysis/start_mcrps_status_year.csv', stringsAsFactors = F)
kin <- read.csv('kinneret/run2/analysis/start_mcrps_status_year.csv', stringsAsFactors = F)

fgh2 <- read.csv('feeagh/run4/analysis/start_rmse_status_year.csv', stringsAsFactors = F)
ltj2 <- read.csv('langtjern/run4/analysis/start_rmse_status_year.csv', stringsAsFactors = F)
kin2 <- read.csv('kinneret/run2/analysis/start_rmse_status_year.csv', stringsAsFactors = F)

df <- rbind.data.frame(fgh, ltj, kin)
df2 <- rbind.data.frame(fgh2, ltj2, kin2)
df2$start <- 'NDA'

d.cols <- RColorBrewer::brewer.pal(8, 'Dark2')


# Remove Inv Strat
df <- df[(df$status != 'Inverse Stratified'),]
df2 <- df2[(df2$status != 'Inverse Stratified'),]

p1 <- ggplot(df, aes(x = crps_mean))+
  geom_density(aes(colour = start),size = 1)+
  geom_density(data = df2, aes(x = RMSE, colour = start), size = 1)+
  xlab('')+
  facet_wrap(~status*lake)+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Initialization'))+
  scale_colour_manual(values = d.cols[3:8])+
  ylab('')+
  xlab('Error (°C)')+
  theme_bw(base_size = 18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p1

ggsave("C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\Plots\\opt_temp_freq/Fig8b.png", p1, dpi = 300,width = 384,height = 216, units = 'mm')


library(plyr)

res <- ddply(df, c('start', 'lake', 'status'), function(x){
  val = mean(x$crps_mean, na.rm = T)
  med = median(x$crps_mean, na.rm = T)
  sd = sd(x$crps_mean, na.rm = T)
  min = min(x$crps_mean, na.rm = T)
  max = max(x$crps_mean, na.rm = T)
  return(c(val, med, sd, min, max))
})
res
colnames(res)[4:8] <- c('mean', 'median', 'sd', 'min', 'max')

res2 <- ddply(df2, c('start', 'lake', 'status'), function(x){
  val = mean(x$RMSE, na.rm = T)
  med = median(x$RMSE, na.rm = T)
  sd = sd(x$RMSE, na.rm = T)
  min = min(x$RMSE, na.rm = T)
  max = max(x$RMSE, na.rm = T)
  return(c(val, med, sd, min, max))
})
res2
colnames(res2)[4:8] <- c('mean', 'median', 'sd', 'min', 'max')

res <- rbind.data.frame(res, res2)
res <- res[order(res$lake, res$status),]
res[,4:8] <- signif(res[,4:8],2)
write.csv(res, 'C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\Met_Comparison\\tables\\opt_freq/error_dist_mean_median_sd.csv', quote = F, row.names = F)

m1 <- glm(crps_mean ~ start, family = Gamma(link = 'log'), data = fgh[fgh$status == 'Stratified',])
summary(m1)
plot(m1$residuals)
abline(h = 0)

m2 <- kruskal.test(crps_mean ~ start, data = fgh[fgh$status == 'Stratified',])
summary(m2)
