ggl <- fread(file.path(dataout,"COVID_County.csv"))


#### PARTY DENSITY PLOTS #### 
ggl <- ggl[!is.na(ggl$republican),]

ggl$Party <- ggl$republican > 0.5
mu <- ddply(ggl, "rep", summarise, grp.median = median(human))

ggplot(ggl, aes(x=human, fill=Party)) + geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.median, color=rep), linetype="dashed") +
  theme(text = element_text(family = 'CM Roman')) +
  xlab("Percent of Population") +
  ylab("Density")  +  theme(axis.title=element_text(size=14), axis.text=element_text(size=12)) +
  scale_colour_manual(values = c("blue", "red")) + 
  scale_fill_manual(values = c("blue", "red")) +
  theme(legend.position="none")

plotname <- "ccDists"
ggsave(file.path(figs, paste(plotname, ".pdf", sep="")), dpi=300, width=9, height=6)
embed_fonts(file.path(figs, paste(plotname, ".pdf", sep="")))



