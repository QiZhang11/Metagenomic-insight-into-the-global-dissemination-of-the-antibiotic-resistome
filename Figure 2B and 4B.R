
otu <- read.delim('resistome.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))


group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

library(vegan)

distance <- vegdist(otu, method = 'bray')
pcoa <- cmdscale(distance, k = (nrow(otu) - 1), eig = TRUE)
write.csv(as.matrix(distance), file = "pcoa-sd.csv")

pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)

sample_site <- data.frame({pcoa$points})[1:2]
sample_site$name <- rownames(sample_site)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')

sample_site <- merge(sample_site, group, by ='name', all.x = TRUE)

library(ggplot2)

pcoa_plot <- ggplot(sample_site, aes(PCoA1, PCoA2, group = group1)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  geom_hline(yintercept = 0, color = 'black', size = 0.3) + 
  geom_point(aes(color = group1), size = 5, alpha = 0.8) + 
  labs(x = paste('PCoA axis1: ', round(100 * pcoa_eig[1], 2), '%'), y = paste('PCoA axis2: ', round(100 * pcoa_eig[2], 2), '%'))

pcoa_plot

P2 <- pcoa_plot+stat_ellipse(data=sample_site,
                             geom = "polygon",level=0.9,
                             linetype = 1,size=0.8,
                             aes(fill=group),
                             alpha=0.1,
                             show.legend = T)+
  scale_fill_manual(values = c("#1597A5","#FFC24B","#FEA3AE","#1527B5","#FFA24B","#FEC3AE")) 


P2

ggsave('resistome.pdf', pcoa_plot, width = 8, height = 5)
