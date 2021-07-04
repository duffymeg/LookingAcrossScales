### Analysis of literature review for Looking Across Scales intro ms by Elderd, Mideo, and Duffy

# Set wd to source file location

# load libraries we'll need
library(tidyverse)
library(cowplot)

# Import data
scales <- read.csv("LookingAcrossScalesLitReview.csv", header = TRUE)

str(scales)
scales$Disease.scale <- as.factor(scales$Disease.scale)
levels(scales$Disease.scale)

excluded <- scales %>%
  filter(Disease.scale == "excluded")

scales <- scales %>%
  filter(Disease.scale != "excluded")

scales$num.scales <- str_count(scales$Disease.scale, ',')+1

# Put categories in order so that stacked bar plots come out with all single scale studies at the bottom
scales$Disease.scale <- ordered(scales$Disease.scale, levels = c("within host, between hosts, macro",
                                                                 "within host, between hosts, among patches",
                                                                 "within host, macro",
                                                                 "between hosts, among patches",
                                                                 "within host, between hosts",
                                                                 "macro",
                                                                 "among patches",
                                                                 "between hosts",
                                                                 "within host"))

# Initial line plot
lineplot <- ggplot(scales, aes(x = Year, y = num.scales)) +
  geom_jitter(shape = 16, position=position_jitter(width=0.2, height=0.2), alpha = 0.8, aes(color=Disease.scale)) +
  ylab("# of scales") +
  geom_smooth(method='lm') +
  theme_cowplot() +
  theme(legend.position="none") 

summary(timeanalysis <- glm(num.scales ~ Year, family = 'poisson', data = scales))


# Things are pretty sparse in many years. Let's create 5 year bins
scales$YearBin <- ifelse(scales$Year > 1991 & scales$Year < 1997, 1994,
                         ifelse(scales$Year > 1996 & scales$Year < 2002, 1999,
                         ifelse(scales$Year > 2001 & scales$Year < 2007, 2004,
                                ifelse(scales$Year > 2006 & scales$Year < 2012, 2009,
                                       ifelse(scales$Year > 2011 & scales$Year < 2017, 2014, 2019)))))


str(scales)

scales$YearBin <- as.factor(as.character(scales$YearBin))


ggplot(scales, aes(x = YearBin, color = Disease.scale, fill = Disease.scale)) +
  geom_bar(position='dodge', color='black') +
  theme_cowplot()

# Summarize results by year
scalessummary <- scales %>%
  group_by(YearBin, Disease.scale) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n), n = n)

scalessummary_numscale <- scales %>%
  group_by(YearBin) %>%
  summarise(num.scales=mean(num.scales)) 

summarise(scales, mean=mean(num.scales))

scalessummary_n <- scalessummary %>%
  group_by(YearBin) %>%
  summarise(total = sum(n))

scalessummary_numsinglescale <- scales %>%
  group_by(YearBin) %>%
  summarise(count = sum(num.scales == 1))

propsinglescale <- scalessummary_numsinglescale$count/scalessummary_n$total

# Create stacked bar plot showing disease scales over time (using 5 year binned data)
barplot <- ggplot(scalessummary, aes(x = YearBin, y = freq, color = Disease.scale, fill = Disease.scale)) +
  geom_col(color='black') +
  ylab('Proportion') +
  xlab('Middle year of five year bin') +
  theme(legend.title = element_blank()) +
  theme_cowplot() +
  annotate('text', x = 1, y = 1.05, label = 'n = 36') +
  annotate('text', x = 2, y = 1.05, label = 'n = 30') +
  annotate('text', x = 3, y = 1.05, label = 'n = 43') +
  annotate('text', x = 4, y = 1.05, label = 'n = 41') +
  annotate('text', x = 5, y = 1.05, label = 'n = 51') +
  annotate('text', x = 6, y = 1.05, label = 'n = 36')

barplot


panelc <- ggplot(MaxBrood, aes(x=Species2, y=max.percbrood)) + 
  geom_boxplot() + theme_bw() + 
  stat_summary(fun.data = give.n, geom = "text") +

# extract the legend
legend <- get_legend(
  # create some space to the left of the legend
  barplot + theme(legend.box.margin = margin(0, 0, 0, 12))
)


yearplot <- plot_grid(lineplot, barplot, labels = "auto", ncol = 1, align = "h", rel_widths = c(1, 2))
yearplot

ggsave("yearplot.jpg", yearplot, units = "in", width = 10, height = 7, dpi = 300)

taxonplot <- ggplot(scales, aes(x = Host.Habitat)) +
  geom_bar(aes(fill=Disease.scale), color = 'black', show.legend = FALSE) +
  xlab('Host habitat') + ylab('# studies') +
  theme_cowplot() 
taxonplot

micromacroplot <- ggplot(scales, aes(x = Macro.v.micro)) +
  geom_bar(aes(fill=Disease.scale), color = 'black', show.legend = FALSE) +
  xlab('Microparasite v. macroparasite') + ylab('# studies') +
  theme_cowplot()
micromacroplot

# Let's collapse/combine the small/weird parasite classes
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "Alveolates/Alga", "misc"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "helminths", "nematode"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "Social parasite", "misc"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "waterborne pathogen", "misc"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "vector-borne", "misc"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "sterilizing pathogens", "misc"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "proxy", "misc"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "generic microparasites", "general"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "invertebrate", "invert"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "apicomplexan", "malaria"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "microsporidian", "misc"))
scales <- scales %>%
  mutate(Parasite.Class = replace(Parasite.Class, Parasite.Class == "myxozoan", "misc"))


scalessummarybyparasiteandscale <- scales %>%
  group_by(Parasite.Class, Disease.scale) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n), n = n)


scalessummarybyparasiteandscale_n <- scalessummarybyparasiteandscale %>%
  group_by(Parasite.Class) %>%
  summarise(total = sum(n))

barplotparasiteandscale <- ggplot(scalessummarybyparasiteandscale, aes(x = Parasite.Class, y = freq, color = Disease.scale, fill = Disease.scale)) +
  geom_col(color='black') +
  ylab('Proportion') +
  xlab('Type of parasite') +
  theme_cowplot() +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(legend.position="none") +
  annotate('text', x = 1, y = 1.05, label = 'n = 16') +
  annotate('text', x = 2, y = 1.05, label = 'n = 24') +
  annotate('text', x = 3, y = 1.05, label = 'n = 76') +
  annotate('text', x = 4, y = 1.05, label = 'n = 63') +
  annotate('text', x = 5, y = 1.05, label = 'n = 6') +
  annotate('text', x = 6, y = 1.05, label = 'n = 10') +
  annotate('text', x = 7, y = 1.05, label = 'n = 12') +
  annotate('text', x = 8, y = 1.05, label = 'n = 5') +
  annotate('text', x = 9, y = 1.05, label = 'n = 4') +
  annotate('text', x = 10, y = 1.05, label = 'n = 2') +
  annotate('text', x = 11, y = 1.05, label = 'n = 19') 
  
barplotparasiteandscale

bottomrow <- plot_grid(taxonplot, micromacroplot, labels = c('c', 'd'), ncol=2)
fourpanelplot <- plot_grid(yearplot, bottomrow, labels = c('a',''), ncol = 1, rel_heights = c(2,1)) 
fourpanelplot

fivepanelplot <- plot_grid(fourpanelplot, barplotparasiteandscale, labels = c('','e'), ncol = 1, rel_heights = c(2.5,1)) 
fivepanelplot

ggsave("fivepanelplot.jpg", fivepanelplot, units = "in", width = 10, height = 12, dpi = 300)

# Now moving on to create figure 2 for the manuscript
scalessummarybyparasite <- scales %>%
  group_by(YearBin, Parasite.Class) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n), n = n)

scalessummarybyparasite_n <- scalessummarybyparasite %>%
  group_by(YearBin) %>%
  summarise(total = sum(n))

barplotparasitetype <- ggplot(scalessummarybyparasite, aes(x = YearBin, y = freq, color = Parasite.Class, fill = Parasite.Class)) +
  geom_col(color='black') +
  ylab('Proportion') +
  xlab('Middle year of five year bin') +
  scale_fill_manual(values=c("#ff6e4a", "#ffa343", '#ffcf48', "#45cea2", "#17806d", "#1dacd6", "#1f75fe", "#7442c8",
                             "#c364c5", "#c0448f", "#ff43a4")) +
  theme_cowplot() +
  guides(fill=guide_legend(ncol=2)) +
  annotate('text', x = 1, y = 1.05, label = 'n = 36') +
  annotate('text', x = 2, y = 1.05, label = 'n = 30') +
  annotate('text', x = 3, y = 1.05, label = 'n = 43') +
  annotate('text', x = 4, y = 1.05, label = 'n = 41') +
  annotate('text', x = 5, y = 1.05, label = 'n = 51') +
  annotate('text', x = 6, y = 1.05, label = 'n = 36')

barplotparasitetype

scales <- scales %>%
  mutate(Host.Classification = replace(Host.Classification, Host.Classification == "Animals", "MultAnim"))

scales$Host.Classification <- as.factor(scales$Host.Classification)
levels(scales$Host.Classification)


scalessummarybyhost <- scales %>%
  group_by(YearBin, Host.Classification) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n), n = n)

scalessummarybyhost_n <- scalessummarybyhost %>%
  group_by(YearBin) %>%
  summarise(total = sum(n))

barplothosttype <- ggplot(scalessummarybyhost, aes(x = YearBin, y = freq, color = Host.Classification, fill = Host.Classification)) +
  geom_col(color='black') +
  ylab('Proportion') +
  xlab('Middle year of five year bin') +
  scale_fill_manual(values=c("#ff6e4a", '#ffcf48', "#45cea2", "#1dacd6", "#1f75fe", "#7442c8",
                             "#c364c5", "#ff43a4")) +
  theme_cowplot() +
  guides(fill=guide_legend(ncol=2)) +
  annotate('text', x = 1, y = 1.05, label = 'n = 36') +
  annotate('text', x = 2, y = 1.05, label = 'n = 30') +
  annotate('text', x = 3, y = 1.05, label = 'n = 43') +
  annotate('text', x = 4, y = 1.05, label = 'n = 41') +
  annotate('text', x = 5, y = 1.05, label = 'n = 51') +
  annotate('text', x = 6, y = 1.05, label = 'n = 36')

barplothosttype

figure2 <- plot_grid(barplotparasitetype, barplothosttype, labels = c('a','b'), ncol = 1, align = 'v')
figure2

ggsave("figure2.jpg", figure2, units = "in", width = 8, height = 5, dpi = 300)

