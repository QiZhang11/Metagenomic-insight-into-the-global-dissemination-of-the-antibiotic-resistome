otu <- read.delim('resistome.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
dune.pairwise.adonis <- pairwise.adonis(x=otu, factors=group$group, 
                                        sim.function = "vegdist",
                                        sim.method = "bray",
                                        p.adjust.m = "BH",
                                        reduce = NULL,
                                        perm = 999)
dune.pairwise.adonis
write.csv(as.matrix(dune.pairwise.adonis), file = "pairwise.ALL-pcoa2.csv")