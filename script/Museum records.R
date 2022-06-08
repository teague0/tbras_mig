#Museum records


mus <- read.delim("./data/TABR-a50b0c9ca7de490d998c9b6c903d198f.tsv", sep = "\t", header = TRUE)
names(mus)


hist(mus$year, xlim = c(1900,2022), breaks="FD")

mus %>% filter(hastissue == 1) %>%
  filter(country %in% c("UNITED STATES", "U S A", "USA", "United States", "MEXICO", "Mexico")) %>% 
  ggplot()+
  geom_histogram(aes(x = year))+
  xlim(1900,2022)+
  facet_wrap(~stateprovince)


