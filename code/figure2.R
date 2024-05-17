# Robert Behnke's phylogeny (See Figure 2, Shiozawa et al 2018)

library(ggtree)
library(ggplot2)
library(ape)

path1 <- "~/compute/cutthroat/data/figure2/tree1.nwk"

tree1 <- read.tree(path1)

tree1 <- drop.tip(tree1, c("Whitehorse", "Alvord",
                         "Paiute", "SnakeRiver", "Yellowfin"))

tree1$tip.label <- c("Rainbow", "Coastal", "Westslope", "Humboldt", "Lahontan",
                     "Bonneville", "Yellowstone", "Rio Grande", "Colorado", "Greenback")

df1 <- data.frame(taxa = tree1$tip.label, 
                 category = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))

p1 <- ggtree(tree1, branch.length = "none") +
  xlim(0, 10) + #Change to 19 for Trees.png
  geom_tiplab() 
  

p1 <- p1 %<+% df1 +
  geom_tiplab(aes(fill=category), color = "white", geom = "label", label.padding = unit(0.15, "lines"), label.size = 0)

p1 <- p1 +
  scale_fill_manual(values = c(A = "maroon4", B = "salmon", C = "brown3", 
                               D = "cyan4", E = "skyblue2", F = "orange2", 
                               G = "gold2", H = "pink2", I = "steelblue4",
                               J = "grey")) +
  theme(legend.position = "none") 

p1 <- ggtree::rotate(p1, 19)
p1 <- ggtree::rotate(p1, 17)
p1 <- ggtree::rotate(p1, 15)

ggsave("Tree1.png", plot = p1, path = "~/compute/cutthroat/figures/", width = 4, height = 4)

# Shiozawa et al 2018 Phylogeny (See Figure 13)

library(ggtree)
library(ggplot2)
library(ape)

path2 <- "~/compute/cutthroat/data/figure2/tree2.nwk"

tree2 <- read.tree(path2)

tree2$tip.label <- c("Rainbow", "Coastal", "Westslope", "Lahontan", "Yellowstone", 
                     "Bear River Bonneville", "Colorado (Blue)", "Bonneville", 
                     "Rio Grande", "Greenback", "Colorado (Green)")

df2 <- data.frame(taxa = tree2$tip.label, 
                 category = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"))

p2 <- ggtree(tree2, branch.length = "none") 

p2 <- p2 %<+% df2 +
  geom_tiplab(aes(fill=category), color = "white", geom = "label",
              label.padding = unit(0.15, "lines"), label.size = 0, hjust = 1) 

p2 <- p2 +
  scale_fill_manual(values = c(A = "maroon4", B = "salmon", 
                               C = "brown3", D = "skyblue2",
                               E = "gold2", F = "burlywood3",
                               G = "steelblue4", H = "orange2",
                               I = "pink2", J = "grey",
                               K = "olivedrab")) +
  scale_x_reverse() +
  xlim(20, 0) +
  theme(legend.position = "none") +
  geom_text2(aes(label = label,
  subset = !is.na(as.numeric(label)) & as.numeric(label) > 50), size = 2, hjust = -.2, vjust = -.6)

p2 <- ggtree::rotate(p2, 18)

ggsave("Tree2.png", plot = p2, path = "~/compute/cutthroat/figures/", width = 4, height = 4)

library(ggpubr)

p3 <- ggarrange(p1, p2, font.label = list(size = 20))

ggsave("Trees.pdf", plot = p3, path = "~/compute/cutthroat/figures/", width = 7, height = 4)
