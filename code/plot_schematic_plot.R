sst <- seq(0, 30, length=1000)
u <- dnorm(x, mean=10, sd=3)
df <- data.frame(sst=sst, u=u)
p <- ggplot(df, aes(x=sst,y=u)) + theme_bw()

x1=0
x2=30
p + geom_line(data=subset(df, sst>=5 & sst <= 10), aes(linetype='solid line')) +
  geom_line(data=subset(df, sst <5 || sst > 10), aes(linetype='dotted line')) +
  scale_linetype_manual('type of line', values=c(2,1)) +
  annotate("rect", xmin = x1, xmax = x2, ymin =0, ymax = 0.25, alpha = .2) +xlim(c(-5, 35))

# range limit change
x1=3
x2=33
p2 <- p + geom_line(data=subset(df, sst>=x1 & sst <= x2), aes(linetype='solid line')) +
  geom_line(data=subset(df, sst <x1 || sst > x2), aes(linetype='dotted line')) +
  scale_linetype_manual('type of line', values=c(2,1)) +
  annotate("rect", xmin = x1, xmax = x2, ymin =0, ymax = 0.25, alpha = .2)+xlim(c(-5, 35))

# optimal niche change
x1=3
x2=33
p3 <- ggplot(df, aes(x=sst+3,y=u)) + theme_bw() + geom_line(data=subset(df, sst>=x1 & sst <= x2), aes(linetype='solid line')) +
  geom_line(data=subset(df, sst <x1 || sst > x2), aes(linetype='dotted line')) +
  scale_linetype_manual('type of line', values=c(2,1)) +
  annotate("rect", xmin = x1, xmax = x2, ymin =0, ymax = 0.25, alpha = .2)+xlim(c(-5, 35))

x1=5
x2=35
p + geom_line(data=subset(df, sst>=x1 & sst <= x2), aes(linetype='solid line')) +
  geom_line(data=subset(df, sst <x1 || sst > x2), aes(linetype='dotted line')) +
  scale_linetype_manual('type of line', values=c(2,1)) +
  annotate("rect", xmin = x1, xmax = x2, ymin =0, ymax = 0.25, alpha = .2) +xlim(c(-5, 35))
