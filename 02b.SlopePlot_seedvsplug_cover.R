library(ggplot2)
library(ggrepel)
library(directlabels)
library(ggforce)
library(RColorBrewer)
library(lubridate)
library(CGPfunctions)

##################################
##MIXTURES in SEED PLOTS
##################################
subdirs <- dir("../DATA" , pattern="+_VegetationCover", full=TRUE)
myFiles <- list.files(path = subdirs, pattern="COVER", full=TRUE)
R <- lapply(myFiles, FUN=read.csv, header=TRUE)
datCov = do.call(rbind, R) #args is one argument
#datCov <- rbind(as.planted, df)

abund.mix <- datCov
#abund.spp <- intersect(abund.mono$sp, names(abund.mix[5:ncol(abund.mix)])) #127 checks out
abund.mix.long <- melt(abund.mix, id.vars = c("plot", "year", "type", "Code"), variable.name = "Species")
abund.mix.long2 <- dplyr::filter(abund.mix.long, value !="NA") #drop NA columns - remove rows from df.cover.long for which value = NA (but keeping those with cover = 0)
names(abund.mix.long2) <- c("plot","year", "type" ,"Code","Species", "mix.cover")
#get mean cover by species
abund.dat <- abund.mix.long2 %>%
  group_by(Species, year, type) %>%
  summarise(mean_value = mean(mix.cover), SD=sd(mix.cover)) #, relCov = covTrans/SD) 
#order by species
abund.dat <- abund.dat[order(abund.dat$Species),] #fails if factors
split(abund.dat, abund.dat$type) -> abund.dat.type

abund.dat.type$Seed -> mean.abund.seed
names(mean.abund.seed) <- c("Species","year", "type" ,"mean_value_seed", "SD_seed")

##################################
##MIXTURES in PLUG PLOTS
##################################
abund.dat.type$Plug -> mean.abund.plug
mean.abund.plug <- mean.abund.plug[order(mean.abund.plug$Species),]
names(mean.abund.plug) <- c("Species","year", "type" ,"mean_value_plug", "SD_plug")

##############SLOPE PLOTS FINAL SURVEYS SEED vs PLUG#########
##################################
abund.dat <- merge(mean.abund.seed, mean.abund.plug, by=c("Species", "year"))

#rescale data
#abund.dat$seed.log <- log(abund.dat$mean_value_seed)
#abund.dat$plug.log <- log(abund.dat$mean_value_plug)
#is.na(abund.dat)<-sapply(abund.dat, is.infinite) #Does this make sense? Convert Inf or NA to 0
#abund.dat[is.na(abund.dat)] <- 0

#MEAN SCALING
#abund.dat$scale.seed <- scale(abund.dat$mean_value_seed)
#abund.dat$scale.plug <- scale(abund.dat$mean_value_plug)

#Subset the data for plotting
split(abund.dat, abund.dat$year) -> abund.dat.year
abund.dat.sub <- abund.dat.year$`2019`
#abund.dat.sub <- subset(abund.dat.sub, select=c(Species, mean_value_seed, mean_value_plug))
#abund.dat.sub <- subset(abund.dat.sub, select=c(Species, scale.seed, scale.plug))
#names(abund.dat.sub) <- c("Species", "seed.cover", "plug.cover")


#Make slope chart for comparing seed vs plug http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Correlogram


abund.dat.sub$class <- ifelse( (abund.dat.sub$mean_value_seed - abund.dat.sub$mean_value_plug) < 0, "green", "purple")

abund.dat.sub2 <- abund.dat.sub %>%
  group_by(Species) %>%
  mutate(Diff = abs(mean_value_seed-mean_value_plug))

abund.dat.sub2[order(-abund.dat.sub2$Diff),] -> abund.dat.sub3
#abund.dat.sub4 <- abund.dat.sub3[1:20,]
#as.data.frame(abund.dat.sub4) -> abund.dat.sub4 #WTF abund.dat.sub works below but not abund.dat.sub4
#abund.dat.sub4 -> abund.dat.sub

#order by diff and green take 10 and diff and red take 10
split(abund.dat.sub3, abund.dat.sub3$class) -> top
as.data.frame(top$green[1:10,]) -> topgreen
as.data.frame(top$red[1:10,]) -> topred
as.data.frame(rbind(topgreen, topred)) -> top.diff
top.diff -> abund.dat.sub


######################################subset by significant differences in sig.species 02.a
sig.species$Species -> to.keep

abund.dat.year$`2019`

dplyr::filter(abund.dat.year$`2019`, Species %in% to.keep) -> abund.dat.sub
#assign colors based on if seed or plug </>
abund.dat.sub$class <- ifelse( (abund.dat.sub$mean_value_seed > abund.dat.sub$mean_value_plug), "purple", "green")

#30 species are significantly different. plot those with greatest differences:
abund.dat.sub2 <- abund.dat.sub %>% group_by(Species)  %>% 
  mutate(DIFF = abs(mean_value_seed - mean_value_plug)) #, relCov = covTrans/SD) 

abund.dat.sub2[order(abund.dat.sub2$DIFF),] -> abund.dat.sub2
abund.dat.sub2[30:10,] -> abund.dat.sub3

abund.dat.sub3-> abund.dat.sub
left_label <- paste(abund.dat.sub$Species) 
right_label <- paste(abund.dat.sub$Species)

p <- ggplot(abund.dat.sub) + geom_segment(aes(x=1, xend=2, y=mean_value_seed, yend=mean_value_plug, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"= "green", "purple"="purple")) +  # color of lines
  labs(x="", y="Absolute Mean Cover - final survey") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(abund.dat.sub$mean_value_seed, abund.dat.sub$mean_value_plug))))  # X and Y axis limits

# Add texts
p <- p + geom_text_repel(label=left_label, y=abund.dat.sub$mean_value_seed, x=rep(1, NROW(abund.dat.sub)), hjust=1.1, size=2)
p <- p + geom_text_repel(label=right_label, y=abund.dat.sub$mean_value_plug, x=rep(2, NROW(abund.dat.sub)), hjust=-0.1, size=2)
p <- p + geom_text(label="Seed", x=1, y=1.1*(max(abund.dat.sub$mean_value_seed, abund.dat.sub$mean_value_plug)), hjust=1.2, size=4)  # title
p <- p + geom_text(label="Plug", x=2, y=1.1*(max(abund.dat.sub$mean_value_seed, abund.dat.sub$mean_value_plug)), hjust=-0.1, size=4)  # title

# Minify theme
p <- p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"))

p
#ggsave(filename = "../OUT/scaled.SlopePlot_seedvsplug.png", width = 8, height=16)
#Add SD?

##Is there a linear relationship, sure as expected. But are there outliers?
lm(abund.dat.year$`2019`$mean_value_seed ~ abund.dat.year$`2019`$mean_value_plug) -> fit1
summary(fit1)
plot(abund.dat.year$`2019`$mean_value_seed ~ abund.dat.year$`2019`$mean_value_plug)
abline(fit1)

abund.dat.year$`2019`$spEdit <- gsub('_', ' ', abund.dat.year$`2019`$Species, fixed = T)
abund.dat.year$`2019`$spEdit[abund.dat.year$`2019`$mean_value_seed < 20 & abund.dat.year$`2019`$mean_value_plug < 20] <- ''
#abund.dat.year$`2019.09`$spEdit[abund.dat.year$`2019.09`$mean_value_seed > 20 & abund.dat.year$`2019.09`$mean_value_plug < 40] <- ''
abund.dat.year$`2019`$`Relative difference` <- ifelse(abund.dat.year$`2019`$mean_value_seed < 20, 'High', 'Low')

p.diff <- ggplot(abund.dat.year$`2019`,
                  aes(x = mean_value_plug,
                      y = mean_value_seed,
                      label = spEdit)) +
  geom_point() 
p.diff <- p.diff + geom_smooth(method = "lm", show.legend = FALSE) + theme(legend.position="right")
p.diff <- p.diff + geom_text_repel(aes(fontface='italic', color = `Relative difference`),
                                     size = 2.5,
                                     segment.size = 0.15, show.legend = FALSE) + theme_bw()


p.diff











#########################################
#RANK ORDER OVER TIME SERIES
#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#test on df.cover

#pick 10? species with the greatest changes in cover overtime in seed plots. Then plot 
#those species as time series with solid line seed and dashed lines plug - label spp.

theme_set(theme_bw())

abund.dat -> abund.dat.copy #make copy
abund.dat$year <- as.factor(abund.dat$year)
#abund.dat2$logMix <- log(abund.dat2$mean_value_mix) #Log tranform cover
#abund.dat2$mean_value_mix <- round(abund.dat2$mean_value_mix, 2)
abund.dat$logseed <- log(abund.dat$mean_value_seed) #Log tranform cover
abund.dat$logseed <- round(abund.dat$logseed, 2)

#get diff of mean from 2017 to 2019.09
#sort. those species with greatest change are to be plotted

dat.1 <- filter(abund.dat, year == "2017" | year=="2019.09") %>% 
  group_by(Species) %>% 
  mutate(Difference = mean_value_seed - lag(mean_value_seed))

datdiff <- filter(dat.1, year=="2019.09") 
datdiff[order(datdiff$Difference),] -> down#pick top ten reducers
#down$col <- "red"
datdiff[order(-datdiff$Difference),] -> up#pick top ten gainers
#up$col <- "green"

taxaList <- c( as.character(down$Species[1:10]), as.character(up$Species[1:10]) )

#take the species list above and subset the dataframe for plotting
abund.dat.reduced <- subset(abund.dat, Species %in% taxaList)

#table1$val2 <- table2$val2[match(table1$pid, table2$pid)]
abund.dat.reduced$col <- down$col[match(abund.dat.reduced$Species, down$Species[1:10])]
abund.dat.reduced$col[is.na(abund.dat.reduced$col)] <- "green" #replace NA with green



newggslopegraph(abund.dat.reduced, year, logseed, Species) +
  labs(title="logMean Cover by Species", 
       subtitle = "species with greatest abs. cover change",
       caption="2017-2019 fall surveys")

ggsave(filename = "../OUT/meanSeedCover.slopePlot.png", width = 8, height=16)

##REDO BASED ON LOG OR SCALED DIFFERENCES!!!

ggplot(abund.dat.reduced, aes(x=year)) +                                  #try facet_wrap_paginate
  geom_line(aes(y=mean_value_seed, group=Species, colour=col), size=1) + 
  scale_x_discrete(expand=c(0, 2)) +
 geom_dl(aes(label= Species, y=mean_value_seed), method=list(dl.combine("first.points", "last.points"), cex = 0.7)) +
  theme_bw() +
  theme(legend.position = "none") 
  #geom_label_repel(aes(label = Species),
  #                 nudge_x = 1,
  #                 na.rm = TRUE)










############################################################
#MAKE A SPLOE PLOT PER FAMILY ##############################
#using abund.dat2 could use df.mean.cover$Plug for including spring
#spp.lookup
copy2.Fam <- copy2
copy2.Fam$Family.sim <- spp.trans$Family.simplified [match(copy2.Fam$Species, spp.trans$treeName)]
copy2.Fam$Species <- as.character(copy2.Fam$Species)

df.mean.cover$Seed -> copy2
copy2$Family.sim <- spp.trans$Family.simplified [match(copy2$Species, spp.trans$treeName)]
copy2$logMean <- log(copy2$Mean) #Log tranform cover


pdf("../OUT/Family.slopeplots.Seed.pdf")
for(i in 1:12){
 print(ggplot(copy2, aes(x=year)) +
  geom_line(aes(y=logMean, group=Species, colour=Species), size=2) + facet_grid_paginate(.~Family.sim, ncol = 1, nrow = 1, page = i) + 
  labs(title="Time Series of Mean Cover - Seed Plots", 
       subtitle="colored by species", 
       caption="", 
       y="(log)Mean Cover", 
       color=NULL) +
  geom_dl(aes(label= Species, y=logMean), method=list(dl.combine("first.points", "last.points"), cex = 0.5)) +
  theme(legend.position = "none") +
  scale_colour_manual(values=rep(brewer.pal(5,"Greens"),times=130)) )
  #geom_label_repel(aes(label = Species, y=logMean),nudge_x = 1, na.rm = TRUE)
}
dev.off()

#n_pages(slplotFam)

##############################
#Per each 36 replicate, plot each species change overtime with SD or one line per plot
        # if we want to look at variance of species between different replicate mixes, add new.rep to glmm model
##############################
df.cover.Byreps <- df.coverlong %>%
  group_by(Species, year, new.rep) %>%
  summarise(Mean =mean(value), Sd= sd(value), logMean = round(log(Mean), 4))
df.cover.Byreps$logMean[df.cover.Byreps$logMean == "-Inf"] <- "0"
df.cover.Byreps$logMean <- as.numeric(df.cover.Byreps$logMean)

pdf("../OUT/slopeplots.by.rep.pdf")
for(i in 1:37){
print(ggplot(df.cover.Byreps, aes(x=year)) +
    geom_line(aes(y=logMean, group=Species, colour=Species), size=2) + facet_grid_paginate(.~new.rep, ncol = 1, nrow = 1, page = 1) + 
    labs(title="Time Series of Abs Cover", 
         subtitle="colored by phyD", 
         caption="", 
         y="Cover", 
         color=NULL) +
    geom_dl(aes(label= Species, y=logMean), method=list(dl.combine("first.points", "last.points"), cex = 0.5)) +
    theme(legend.position = "none") +
    scale_colour_manual(values=rep(brewer.pal(5,"BrBG"),times=130)) )
}
dev.off()



#plot absolute cover by species for each  plot, colored by PhyD plot and seed vs plug line type:
pdf("../OUT/slopeplots.perSpeciesv2.pdf")
for(i in 1:127){
  print(ggplot(df.coverlong, aes(x=year)) +                                  #try facet_wrap_paginate
  geom_line(aes(y=value, group=plot, colour=phyD, linetype=type), size=2) + facet_wrap_paginate(.~Species, ncol = 1, nrow = 1, page = i, scales = "free_y") + 
  labs(title="Vegetative Cover", 
       subtitle="colored by plot PD", 
       caption="", 
       y="Abs. Cover", 
       color=NULL) +
  geom_dl(aes(label= plot, y=value), method=list(dl.combine("first.points", "last.points"), cex = 0.5)) +
  #theme(legend.position = "none") +
  scale_colour_manual(values=rep(brewer.pal(5,"Dark2"),times=130)) )
}
dev.off()
















#make a list of species to plot (based on significance for somthing and then call that in ggplot)
test.specieslist <- c("Agastache_nepetoides", "Agastache_scrophulariifolia")

#ERRORS by plotting only 2 years not if call a species specifically as below
ggplot(copy[copy$Species==test.specieslist,], aes(x=year)) +
  geom_line(aes(y=logMean, col=SDMC, group=Species), size=2)+
  labs(title="Time Series of Mean Cover", 
       subtitle="Colored by SDMC", 
       caption="", 
       y="(log)Mean Cover by Species", 
       color=NULL) 

ggplot(copy[copy$Species==c("Agastache_nepetoides"),], aes(x=year)) +
  geom_line(aes(y=logMean, col=SDMC, group=Species), size=2)+
  labs(title="Time Series of Mean Cover", 
       subtitle="Colored by SDMC", 
       caption="", 
       y="(log)Mean Cover by Species", 
       color=NULL) 


