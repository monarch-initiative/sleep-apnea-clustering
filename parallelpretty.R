
parallel = fa.parallel(hc$correlations,n.obs=N,
                                 #fm = 'ml',
                                 fa = 'fa',
                                 n.iter = 50,
                                 #SMC = TRUE,
                                 quant = .95)

#Create data frame &amp;amp;amp;amp;amp;quot;obs&amp;amp;amp;amp;amp;quot; from observed eigenvalue data
obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

#Calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile = apply(parallel$values,2,function(x) quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile1 = percentile[min:max]

#Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
sim = data.frame(parallel$fa.sim)
sim$type = c('Simulated Data (95th %ile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = c('eigenvalue', 'type', 'num')

#Merge the two data frames (obs and sim) together into data frame called eigendat
eigendat = rbind(obs[1:50,],sim[1:50,])

apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
p = ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  #Add lines connecting data points
  geom_line()+
  #Add the data points.
  geom_point(size=4)+
  #Label the y-axis 'Eigenvalue'
  scale_y_continuous(name='Eigenvalue')+
  #Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
  scale_x_continuous(name='Factor Number', breaks=seq(0, max(eigendat$num), by=2))+
  #Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
  scale_shape_manual(values=c(16,1)) +
  #Add vertical line indicating parallel analysis suggested max # of factors to retain
  geom_vline(xintercept = parallel$nfact, linetype = 'dashed')+
  #Apply our apa-formatting theme
  apatheme
#Call the plot. Looks pretty!
p

ggsave('parallel.png', width=6, height=6, unit='in', dpi=300)

db=dbscan(gower_dist, eps = 0.15, MinPts = 5, method="dist")

# Plot DBSCAN results
plot(db, gower_dist, main = "DBSCAN", frame = FALSE)

kNNdistplot(df, k =  5)

