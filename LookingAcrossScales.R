### Analysis of literature review for Looking Across Scales intro ms by Elderd, Mideo, and Duffy

# Set wd to source file location

# load libraries we'll need
library(tidyverse)
library(cowplot)

# Import data
scales <- read.csv("LookingAcrossScalesLitReview.csv", header = TRUE)

str(scales)
scales$Excluded <- as.factor(scales$Excluded)
levels(scales$Excluded)

scales <- scales %>%
  filter(Excluded != "excluded")


# Things are pretty sparse in many years. Let's create 5 year bins
scales$YearBin <- ifelse(scales$Year > 1991 & scales$Year < 1997, 1994,
                         ifelse(scales$Year > 1996 & scales$Year < 2002, 1999,
                         ifelse(scales$Year > 2001 & scales$Year < 2007, 2004,
                                ifelse(scales$Year > 2006 & scales$Year < 2012, 2009,
                                       ifelse(scales$Year > 2011 & scales$Year < 2017, 2014, 2019)))))


str(scales)

scales$YearBin <- as.factor(as.character(scales$YearBin))

# Summarize results by year
scalessummary_n <- scalessummary %>%
  group_by(YearBin) %>%
  summarise(total = sum(n))


habitatplot <- ggplot(scales, aes(x = Host.Habitat)) +
  geom_bar(show.legend = FALSE) +
  xlab('Host habitat') + ylab('# studies') +
  theme_cowplot() 
habitatplot

micromacroplot <- ggplot(scales, aes(x = Macro.v.micro)) +
  geom_bar(show.legend = FALSE) +
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



toprow <- plot_grid(habitatplot, micromacroplot, labels = c('a', 'b'), ncol=2)
toprow

# Now moving on to create the next two panels
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

panelscd <- plot_grid(barplotparasitetype, barplothosttype, labels = c('c','d'), ncol = 1, align = 'v')
panelscd

figure <- plot_grid(toprow, panelscd, labels = c('', ''), ncol = 1, align = 'h', rel_heights = c(1, 2))
figure

ggsave("figure.jpg", figure, units = "in", width = 11, height = 11, dpi = 300)

