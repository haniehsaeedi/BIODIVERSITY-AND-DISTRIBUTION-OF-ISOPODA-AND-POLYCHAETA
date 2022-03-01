library(readxl)
library(openxlsx)
library(worms)
library(tidyverse)
library(cowplot)
library(sf)
library(vegan)
library(pvclust)

# setwd("D:/nils/Documents/Paper")
# setwd("D:/nils/Documents/Paper/02_Frontiers/Nils_Paper/figures jpg new")

# Import and Formatting

data <- read_excel("data.xlsx")
data$dep <- data$dep * -1
data$group <- as.factor(data$group)
data <- mutate(data, lat_5 = ceiling(lat/5) * 5)
data$lat_5 <- formatC(data$lat_5, width = 2, format = "d", flag = "0")
data <- mutate(data, dep_rnd = floor(dep/100) * 100)
data$dep_rnd[data$dep_rnd == 0] <- -100
data <- mutate(data, dep_rnd_200 = floor(dep/200) * 200)
data$dep_rnd_200[data$dep_rnd_200 == 0] <- -200
data <- mutate(data, area = factor(case_when(lat <= 60 ~ "nwp", lat > 60 ~ "arc")))
data <- mutate(data, dep_rat = factor(case_when(dep >= -500 ~ "shallow", dep < -500 ~ "deep")))
poly_taxa <- read_excel("poly_taxa.xlsx")
data <- merge(data, poly_taxa, by = "scientific", all = TRUE)
data$free <- as.factor(data$free)
## labels_violin_poly <- read_excel("labels_violin_poly.xlsx")
## data <- merge(data, labels_violin_poly, by = "suborder", all = TRUE)

# # Worms
# ## scientific <- c(subset(data, group == "pol")$scientific)
# ## worms_df <- wormsbynames(scientific)
# ## write.xlsx(worms_df, "pol_df")
# 
# # Basic numbers
# ## summary(subset(data, group == "pol")) 
# ## subset(data, group == "pol", select = scientific) %>% n_distinct()
# ## a <- subset(data, group == "pol") %>% group_by(scientific, dep_rat) %>% summarise() %>% na.omit()
# ## b <- a[!(duplicated(a$scientific) | duplicated(a$scientific, fromLast = TRUE)), ]
# ## subset(b, dep_rat == "deep") %>% nrow()
# 
# # Plot (records) latitude / longitude
# ## ggplot(data, aes(x = long, y = lat, colour = group)) +
# ##   geom_jitter(alpha = 0.5) +
# ##   xlab("Longitude [°]") + ylab("Latitude [°]")
# 
# # Plot (records) latitude against depth
# ## ggplot(data, aes(x = lat, y = dep, colour = group)) +
# ##   geom_jitter(alpha = 0.5) +
# ##   xlab("Latitude [°]") + ylab("Depth [m]")
# 
# # Different options for the graphs
# ### add the following to the ggplots aes to look at iso and poly individually: , fill = group
# ### subset(data, group == "pol" & free == "x") to look at polychaetes with a free swimming larval phase
# ### ggtitle("") to add title
# 
# # Saving graphs
# ### 1 ###
#   # png('Fig. S36.png', units="cm", width=20, height=20, res=300)
#   # plot #
#   # dev.off()
# ### 2 ###
# 
# # plot_grid(a, b, align = "hv", nrow = 1, labels = c("(a)", "(b)", "(c)", "(d)"), label_x = -0.02) # to combine multiple graphs
# #   # , label_x = -0.02
# # ggsave("Fig. S26.png", units = "cm", width = 20, height = 20, dpi = 300) # block of 4
# # ggsave("Fig. S37.png", units = "cm", width = 50, height = 25, dpi = 300) # 2 blocks, 1 row
# # ggsave("fig04.png", units = "cm", width = 25, height = 30, dpi = 300) # two violins stacked
# # ggsave("Fig. S31.png", units = "cm", width = 20, height = 20, dpi = 300) # 1 block
# 
# # Plot records per latitude 
# 
# ggplot(data, aes(x = lat_5, fill = group)) + 
#   geom_bar(position = "dodge") +
#   xlab("Latitude [°]") + ylab("Number of Records") +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# # Plot shallow records per latitude 
# 
# ggplot(subset(data, dep >= -500), aes(x = lat_5)) + 
#   geom_bar(position = "dodge") +
#   xlab("Latitude [°]") + ylab("Number of Records") +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# # Plot deep records per latitude 
# 
# ggplot(subset(data, dep < -500), aes(x = lat_5, fill = group)) + 
#   geom_bar(position = "dodge") +
#   xlab("Latitude [°]") + ylab("Number of Records") +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# # Plot records per depth
# 
# ggplot(subset(data), aes(x = dep_rnd, fill = group)) + 
#   geom_bar(position = "dodge") +
#   xlab("Depth [m]") + ylab("Number of Records") +
#   coord_flip() +
#   ylim(0, 2001) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#   )
# 
# # Plot NW Pacific records per depth
# ## ggplot(subset(data, lat <= 60), aes(x = dep_rnd, fill = group)) +
# ##   geom_bar(position="dodge") +
# ##   xlab("Depth [m]") + ylab("Number of Records") +
# ##   coord_flip() +
# ##   theme(legend.position="none")
# 
# # Plot Arctic records per depth
# ## ggplot(subset(data, lat > 60), aes(x = dep_rnd, fill = group)) +
# ##   geom_bar(position="dodge") +
# ##   xlab("Depth [m]") + ylab("Number of Records") +
# ##   coord_flip() +
# ##   theme(legend.position="none")
# 
# # Plot species per latitude 
# 
# distinct(subset(data), scientific, lat_5, .keep_all = TRUE) %>% 
#   ggplot(aes(x = lat_5, fill = group)) + 
#   geom_bar(position = "dodge") +
#   xlab("Latitude [°]") + ylab("Number of Species") +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# # Plot shallow species per latitude 
# 
# distinct(subset(data, dep >= -500), scientific, lat_5, .keep_all = TRUE) %>% 
#   ggplot(aes(x = lat_5, fill = group)) + 
#   geom_bar(position = "dodge") +
#   xlab("Latitude [°]") + ylab("Number of Species") +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# # Plot deep species per latitude 
# 
# distinct(subset(data, dep < -500), scientific, lat_5, .keep_all = TRUE) %>% 
#   ggplot(aes(x = lat_5, fill = group)) + 
#   geom_bar(position = "dodge") +
#   xlab("Latitude [°]") + ylab("Number of Species") +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# # Plot species per depth
# 
# distinct(subset(data), scientific, dep_rnd, .keep_all = TRUE) %>% 
#   ggplot(aes(x = dep_rnd, fill = group)) + 
#   geom_bar(position = "dodge") +
#   xlab("Depth [m]") + ylab("Number of Species") +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#   )
# 
# # Plot NW Pacific species per depth
# ## distinct(subset(data, lat <= 60), scientific, dep_rnd, .keep_all = TRUE) %>% 
# ##   ggplot(aes(x = dep_rnd, fill = group)) +
# ##   geom_bar(position="dodge") +
# ##   xlab("Depth [m]") + ylab("Number of Species") +
# ##   coord_flip() +
# ##   theme(legend.position="none")
# 
# # Plot Arctic species per depth
# ## distinct(subset(data, lat > 60), scientific, dep_rnd, .keep_all = TRUE) %>% 
# ##   ggplot(aes(x = dep_rnd, fill = group)) +
# ##   geom_bar(position="dodge") +
# ##   xlab("Depth [m]") + ylab("Number of Species") +
# ##   coord_flip() +
# ##   theme(legend.position="none")

# Matrix Function

abu_matrix <- function(data, sites.col, sp.col, keep.n = TRUE) {
  
  stopifnot(
    length(sites.col) == 1,
    length(sp.col) == 1,
    sites.col != sp.col,
    sites.col %in% 1 : ncol(data) | sites.col %in% names(data),
    sp.col %in% 1 : ncol(data) | sp.col %in% names(data),
    is.logical(keep.n)
  )
  
  presabs <- table(data[ , c(sites.col, sp.col)])
  presabs <- as.data.frame(unclass(presabs))
  if (!keep.n)  presabs[presabs > 1] <- 1
  presabs <- data.frame(row.names(presabs), presabs)
  names(presabs)[1] <- names(subset(data, select = sites.col))
  rownames(presabs) <- NULL
  return(presabs)
}

# Matrix

data_lat <- abu_matrix(data, "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
shallow_lat <- abu_matrix(subset(data, dep >= -500), "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
deep_lat <- abu_matrix(subset(data, dep < -500), "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
data_dep <- abu_matrix(data, "dep_rnd_200", "scientific") %>% 
  column_to_rownames(var = "dep_rnd_200")
## data_dep_nwp <- abu_matrix(subset(data, area == "nwp"), "dep_rnd_200", "scientific") %>% 
##   column_to_rownames(var = "dep_rnd_200")
## data_dep_arc <- abu_matrix(subset(data, area == "arc"), "dep_rnd_200", "scientific") %>% 
##   column_to_rownames(var = "dep_rnd_200")

iso_lat <- abu_matrix(subset(data, group == "iso"), "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
iso_lat_shallow <- abu_matrix(subset(data, group == "iso" & dep >= -500), "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
iso_lat_deep <- abu_matrix(subset(data, group == "iso" & dep < -500), "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
iso_dep <- abu_matrix(subset(data, group == "iso"), "dep_rnd_200", "scientific") %>% 
  column_to_rownames(var = "dep_rnd_200")
## iso_dep_nwp <- abu_matrix(subset(data, group == "iso" & area == "nwp"), "dep_rnd_200", "scientific") %>% 
##   column_to_rownames(var = "dep_rnd_200")
## iso_dep_arc <- abu_matrix(subset(data, group == "iso" & area == "arc"), "dep_rnd_200", "scientific") %>% 
##   column_to_rownames(var = "dep_rnd_200")

pol_lat <- abu_matrix(subset(data, group == "pol"), "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
pol_lat_shallow <- abu_matrix(subset(data, group == "pol" & dep >= -500), "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
pol_lat_deep <- abu_matrix(subset(data, group == "pol" & dep < -500), "lat_5", "scientific") %>% 
  column_to_rownames(var = "lat_5")
pol_dep <- abu_matrix(subset(data, group == "pol"), "dep_rnd_200", "scientific") %>% 
  column_to_rownames(var = "dep_rnd_200")
## pol_dep_nwp <- abu_matrix(subset(data, group == "pol" & area == "nwp"), "dep_rnd_200", "scientific") %>% 
##   column_to_rownames(var = "dep_rnd_200")
## pol_dep_arc <- abu_matrix(subset(data, group == "pol" & area == "arc"), "dep_rnd_200", "scientific") %>% 
##   column_to_rownames(var = "dep_rnd_200")

# # Abundance 
# ## abu_iso_lat <- apply(iso_lat, MARGIN = 1, sum)
# 
# # Gamma richness; per 5° Latitudinal bands
# ## rich_iso_lat <- specnumber(iso_lat)
# 
# # Rarefaction - dropped stations with only 1 record so that the rarecurve produces an output
# ## checking for smallest site maximum with rowSums(iso_dep_arc)
# 
# rarecurve(data_lat, step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# ## rarecurve(shallow_lat, step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# ## rarecurve(deep_lat, step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# 
# rarecurve(subset(data_dep, rowSums(data_dep) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# ## rarecurve(subset(data_dep_nwp, rowSums(data_dep_nwp) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# ## rarecurve(subset(data_dep_arc, rowSums(data_dep_arc) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# 
# rarecurve(iso_lat, step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# ## rarecurve(iso_lat_shallow, step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE, main = "Shallow Isopoda")
# ## rarecurve(iso_lat_deep[-c(3,5),], step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE, main = "Deep Isopoda")
# 
# rarecurve(subset(iso_dep, rowSums(iso_dep) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# ## rarecurve(subset(iso_dep_nwp, rowSums(iso_dep_nwp) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE, main = "NW-Pacific Isopoda")
# ## rarecurve(subset(iso_dep_arc, rowSums(iso_dep_arc) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE, main = "Arctic Isopoda")
# 
# rarecurve(pol_lat, step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# ## rarecurve(pol_lat_shallow, step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE, main = "Shallow Polychaeta")
# ## rarecurve(pol_lat_deep, step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE, main = "Deep Polychaeta")
# 
# rarecurve(subset(pol_dep, rowSums(pol_dep) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE)
# ## rarecurve(subset(pol_dep_nwp, rowSums(pol_dep_nwp) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE, main = "NW-Pacific Polychaeta")
# ## rarecurve(subset(pol_dep_arc, rowSums(pol_dep_arc) >= 2), step = 20, sample = 15, col = "blue", cex = 0.6, label = TRUE, main = "Arctic Polychaeta")
# 
# # Rarefaction plots
# ## checking for smallest site maximum with rowSums(pol_lat_deep)
# 
# subset(data_lat, rowSums(data_lat) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>% 
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(shallow_lat, rowSums(shallow_lat) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>% 
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(deep_lat, rowSums(deep_lat) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>% 
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(iso_lat, rowSums(iso_lat) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>% 
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(iso_lat_shallow, rowSums(iso_lat_shallow) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>% 
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
#   
# subset(iso_lat_deep, rowSums(iso_lat_deep) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>%
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(pol_lat, rowSums(pol_lat) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>%
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(pol_lat_shallow, rowSums(pol_lat_shallow) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>%
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(pol_lat_deep, rowSums(pol_lat_deep) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Latitude") %>%
#   ggplot(aes(x = Latitude, y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Latitude [°]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(data_dep, rowSums(data_dep) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Depth") %>%
#   ggplot(aes(x = reorder(Depth, sort(as.numeric(Depth), decreasing = TRUE)), y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Depth [m]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# subset(iso_dep, rowSums(iso_dep) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Depth") %>%
#   ggplot(aes(x = reorder(Depth, sort(as.numeric(Depth), decreasing = TRUE)), y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Depth [m]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# ## subset(iso_dep_nwp, rowSums(iso_dep_nwp) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
# ##   as.data.frame() %>% rownames_to_column("Depth") %>%
# ##     ggplot(aes(x = reorder(Depth, sort(as.numeric(Depth), decreasing = TRUE)), y = .)) + 
# ##       geom_col(position = "dodge") +
# ##       xlab("Depth [m]") + ylab("ES 15") +
# ##       ggtitle("NW-Pacific Isopoda")
# 
# ## subset(iso_dep_arc, rowSums(iso_dep_arc) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
# ##   as.data.frame() %>% rownames_to_column("Depth") %>%
# ##     ggplot(aes(x = reorder(Depth, sort(as.numeric(Depth), decreasing = TRUE)), y = .)) + 
# ##       geom_col(position = "dodge") +
# ##       xlab("Depth [m]") + ylab("ES 15") +
# ##       ggtitle("Arctic Isopoda")
# 
# subset(pol_dep, rowSums(pol_dep) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
#   as.data.frame() %>% rownames_to_column("Depth") %>%
#   ggplot(aes(x = reorder(Depth, sort(as.numeric(Depth), decreasing = TRUE)), y = .)) + 
#   geom_col(position = "dodge") +
#   xlab("Depth [m]") + ylab("ES 15") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# ## subset(pol_dep_nwp, rowSums(pol_dep_nwp) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
# ##   as.data.frame() %>% rownames_to_column("Depth") %>%
# ##     ggplot(aes(x = reorder(Depth, sort(as.numeric(Depth), decreasing = TRUE)), y = .)) + 
# ##       geom_col(position = "dodge") +
# ##       xlab("Depth [m]") + ylab("ES 15") +
# ##       ggtitle("NW-Pacific Polychaeta")
# 
# ## subset(pol_dep_arc, rowSums(pol_dep_arc) >= 15) %>% rarefy(sample = 15, MARGIN = 1) %>% 
# ##   as.data.frame() %>% rownames_to_column("Depth") %>%
# ##     ggplot(aes(x = reorder(Depth, sort(as.numeric(Depth), decreasing = TRUE)), y = .)) + 
# ##       geom_col(position = "dodge") +
# ##       xlab("Depth [m]") + ylab("ES 15") +
# ##       ggtitle("Arctic Polychaeta")

# Shapefile Data

#land <- st_read("D:/nils/Documents/Bachelor/Thesis/ne_110m_land/ne_110m_land.shp")
land <- st_read("ne_110m_land.shp")
data_sf <- data[, c("scientific", "lat", "long")]
data_sf <- data_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(data_sf) = 4326

shallow_sf <- subset(data, dep >= -500)[, c("scientific", "lat", "long")]
shallow_sf <- shallow_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(shallow_sf) = 4326

deep_sf <- subset(data, dep < -500)[, c("scientific", "lat", "long")]
deep_sf <- deep_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(deep_sf) = 4326

iso_sf <- subset(data, group == "iso")[, c("scientific", "lat", "long")]
iso_sf <- iso_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(iso_sf) = 4326

iso_shallow_sf <- subset(data, group == "iso" & dep >= -500)[, c("scientific", "lat", "long")]
iso_shallow_sf <- iso_shallow_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(iso_shallow_sf) = 4326

iso_deep_sf <- subset(data, group == "iso" & dep < -500)[, c("scientific", "lat", "long")]
iso_deep_sf <- iso_deep_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(iso_deep_sf) = 4326

pol_sf <- subset(data, group == "pol")[, c("scientific", "lat", "long")]
pol_sf <- pol_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(pol_sf) = 4326

pol_shallow_sf <- subset(data, group == "pol" & dep >= -500)[, c("scientific", "lat", "long")]
pol_shallow_sf <- pol_shallow_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(pol_shallow_sf) = 4326

pol_deep_sf <- subset(data, group == "pol" & dep < -500)[, c("scientific", "lat", "long")]
pol_deep_sf <- pol_deep_sf %>% st_as_sf(coords = c('long','lat'))
st_crs(pol_deep_sf) = 4326

pol_sf_free <- subset(data, group == "pol" & free == "x")[, c("scientific", "lat", "long")]
pol_sf_free <- pol_sf_free %>% st_as_sf(coords = c('long','lat'))
st_crs(pol_sf_free) = 4326

pol_sf_notfree <- subset(data, group == "pol" & is.na(free)) [, c("scientific", "lat", "long")]
pol_sf_notfree <- pol_sf_notfree %>% st_as_sf(coords = c('long','lat'))
st_crs(pol_sf_notfree) = 4326
                  
pol_shallow_sf_free <- subset(data, group == "pol" & dep >= -500 & free == "x")[, c("scientific", "lat", "long")]
pol_shallow_sf_free <- pol_shallow_sf_free %>% st_as_sf(coords = c('long','lat'))
st_crs(pol_shallow_sf_free) = 4326

pol_deep_sf_free <- subset(data, group == "pol" & dep < -500 & free == "x")[, c("scientific", "lat", "long")]
pol_deep_sf_free <- pol_deep_sf_free %>% st_as_sf(coords = c('long','lat'))
st_crs(pol_deep_sf_free) = 4326

# Spatial Function
# ggsave("Fig. S19.png", units = "cm", width = 30, height = 15, dpi = 300)
## the last graph is excluding all areas with abundance below 15 so the rarefaction is working properly

spatial_fun <- function(sf_land, sf_cat, sf_data) {
  
  spatial_data <- st_join(sf_cat, sf_data, join = st_contains) %>% 
    group_by(ID) %>% 
    summarise(Effort = length(scientific[!is.na(scientific)]), 
              Richness = n_distinct(scientific[!is.na(scientific)]))
  spatial_data <- spatial_data %>% as.data.frame()
  row_sub <- apply(spatial_data[,c(2,3)], 1, function(row) all()) # row != 0 to exclude zeros; in the bracket of the all() fun
  spatial_data <- spatial_data[row_sub,]
  
  spatial_data2 <- st_join(sf_data, sf_cat, join = st_intersects) %>% 
    as.data.frame() %>% 
    abu_matrix("ID", "scientific")
  
  spatial_data_r <- rarefy(spatial_data2[,-1], sample = 15, MARGIN = 1) %>% 
    as.data.frame()
  colnames(spatial_data_r) <- "ES15"
  spatial_data_r$ID <- spatial_data2$ID
  spatial_data <- spatial_data %>% 
    merge(spatial_data_r, by = "ID", all.x = TRUE) %>% 
    st_as_sf()
  
  p1 <- ggplot() +
    geom_sf(data = spatial_data, aes(fill = Effort)) +
    scale_fill_gradient(low = "khaki", high = "darkred") + # to chnage the color code to white and red, change the low to white
    geom_sf(data = sf_land, fill = "light grey") +
    coord_sf(xlim = c(100, 180), ylim = c(0, 90)) +
    theme(legend.position = c(0.15, 0.6))
  
  p2 <- ggplot() +
    geom_sf(data = spatial_data, aes(fill = Richness)) +
    scale_fill_gradient(low = "khaki", high = "darkred") +
    geom_sf(data = sf_land, fill = "light grey") +
    coord_sf(xlim = c(100, 180), ylim = c(0, 90)) +
    theme(legend.position = c(0.15, 0.6))
  
  p3 <- ggplot() +
    geom_sf(data = subset(spatial_data, Effort >= 15), aes(fill = ES15)) + # subset(spatial_data, Effort >= 15) instead of spatial_data to exclude every abundance below 15
    scale_fill_gradient(low = "khaki", high = "darkred") +
    geom_sf(data = sf_land, fill = "light grey") +
    coord_sf(xlim = c(100, 180), ylim = c(0, 90)) +
    theme(legend.position = c(0.15, 0.6))
  
  plot_grid(p1, p2, p3, align = "hv", nrow = 1, labels = c("(a)", "(b)", "(c)"))

  ggsave("Fig. 3.jpg", units = "cm", width = 30, height = 15, dpi = 300)
}  

# spatial_data <- st_join(hex3, data_sf, join = st_contains) %>% group_by(ID)
# st_write(spatial_data, "data_sf.xlsx")
# write.csv(data_sf, "data_sf.csv")

# Hexagons

hex3 <- st_read("hexgrid3.shp")

spatial_fun(land, hex3, data_sf)
spatial_fun(land, hex3, shallow_sf)
spatial_fun(land, hex3, deep_sf)
spatial_fun(land, hex3, iso_sf)
spatial_fun(land, hex3, iso_shallow_sf)
spatial_fun(land, hex3, iso_deep_sf)
spatial_fun(land, hex3, pol_sf)
spatial_fun(land, hex3, pol_shallow_sf)
spatial_fun(land, hex3, pol_deep_sf)
spatial_fun(land, hex3, pol_sf_free)
spatial_fun(land, hex3, pol_sf_notfree)
spatial_fun(land, hex3, pol_shallow_sf_free)
spatial_fun(land, hex3, pol_deep_sf_free)

# # Ecoregions
# 
# eco <- st_read("D:/nils/Documents/Bachelor/Thesis/MEOW_Ecoregions/meow_ecos.shp")
# eco <- as.data.frame(eco)
# colnames(eco)[1] <- "ID"
# eco <- st_as_sf(eco)
# 
# spatial_fun(land, eco, data_sf)
# spatial_fun(land, eco, shallow_sf)
# spatial_fun(land, eco, deep_sf)
# spatial_fun(land, eco, iso_sf)
# spatial_fun(land, eco, iso_shallow_sf)
# spatial_fun(land, eco, iso_deep_sf)
# spatial_fun(land, eco, pol_sf)
# spatial_fun(land, eco, pol_shallow_sf)
# spatial_fun(land, eco, pol_deep_sf)
# ## spatial_fun(land, eco, pol_sf_free)
# ## spatial_fun(land, eco, pol_shallow_sf_free)
# ## spatial_fun(land, eco, pol_deep_sf_free)

# # Violin plots
# ## unique(subset(data, group == "iso")$family)
# 
# ggplot(subset(data, group == "iso"), aes(x = suborder, y = lat)) + 
#   geom_violin(scale = "width", adjust = 0.5, fill = "#090974", color = "black", alpha = 1) +
#   xlab("Suborder") + ylab("Latitude [°]") +
#   theme_bw() +
#   theme(
#     axis.title.x = element_text(size=16), 
#     axis.title.y = element_text(size=16),
#     axis.text = element_text(size = 13),
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# ggplot(subset(data, group == "iso"), aes(x = suborder, y = dep)) + 
#   geom_violin(scale = "width", adjust = 0.5, fill = "#090974", color = "black", alpha = 1) +
#   xlab("Suborder") + ylab("Depth [m]") +
#   theme_bw() +
#   theme(
#     axis.title.x = element_text(size=16), 
#     axis.title.y = element_text(size=16),
#     axis.text = element_text(size = 13),
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# ggplot(subset(data, group == "pol"), aes(x = suborder, y = lat)) + 
#   geom_violin(scale = "width", adjust = 0.5, fill = "#090974", color = "black", alpha = 1) +
#   xlab("Suborder") + ylab("Latitude [°]") +
#   theme_bw() +
#   theme(axis.title.x = element_text(size=16), 
#         axis.title.y = element_text(size=16),
#         axis.text = element_text(size = 13.5),
#         panel.border = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#   )
# 
# ggplot(subset(data, group == "pol"), aes(x = suborder, y = dep)) + 
#   geom_violin(scale = "width", adjust = 0.5, fill = "#090974", color = "black", alpha = 1) +
#   xlab("Suborder") + ylab("Depth [m]") +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 16),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title = element_text(size = 18),
#     panel.border = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# # Cluster analysis (with uncertainty assessment) Eco regions
# 
# eco_data_c <- st_join(eco, data_sf, join = st_contains) %>% 
#   group_by(ID)
# cluster_eco <- eco_data_c[,c("ECOREGION", "scientific")]
# cluster_eco <- cluster_eco %>% st_set_geometry(NULL) %>% 
#   na.omit()
# cluster_eco$ECOREGION <- as.character(cluster_eco$ECOREGION)
# cluster_eco <- cluster_eco %>% abu_matrix("scientific", "ECOREGION") %>% 
#   column_to_rownames(var = "scientific")
# cluster_eco <- 1 * (cluster_eco > 0) %>% 
#   as.data.frame()
# 
# cluster_eco <- pvclust(cluster_eco, method.hclust = "average", nboot = 10000)
# plot(cluster_eco, cex = 0.8, cex.pv = 0.7, main = "")
