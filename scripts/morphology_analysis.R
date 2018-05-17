library(tidyverse)

#read in morphology data file for basic comparison analysis
morph <- read.csv("data/cleaned_data.csv")
head(morph)

#see a histogram of all branch data
ggplot(data = morph) +
  geom_histogram(mapping = aes(x=branches)) +
  xlab("Number of branches on 4th pelvic ray") 

#make a subset of data with just information from palau and png
Palau_PNG <- morph %>%
  filter(locality == "Papua New Guinea" | locality == "Palau")

#4th pelvic branches histogram*** # everything looks the same
ggplot(data = Palau_PNG) +
  geom_histogram(binwidth = 0.5, mapping = aes(x = branches, fill = locality), show.legend = FALSE) +
  facet_wrap(~locality) +
  xlab("No. branches on 4th pelvic ray") +
  theme_classic(base_family = 'Arial', base_size = 10)

#extract east & western haplotype data
west <- filter(morph, haplotype == "west")
east <- filter(morph, haplotype == "east")

#compare characters from eastern and western haplotypes with a t tests - suggests no difference between populations
t.test(west$branches, east$branches)
t.test(west$fifth_fourth_ratio, east$fifth_fourth_ratio)
t.test(west$membrane_length, east$membrane_length)


#read in morphology data file where stuff has been deleted
file <- read.csv("data/morphology_numerical_data3.csv", header = TRUE, sep = ",")
head(file)

#define 'traits' as the numerical columns that the PCA will read
traits <- data.matrix(file[,5:8]), rownames.force = NA)



