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

#4th pelvic branches histogram*** palau vs png
# everything looks the same
ggplot(data = Palau_PNG) +
  geom_histogram(binwidth = 0.5, mapping = aes(x = branches, fill = locality), show.legend = FALSE) +
  facet_wrap(~locality) +
  xlab("No. branches on 4th pelvic ray") +
  theme_classic(base_family = 'Arial', base_size = 10)

#4th pelvic branches histogram*** east vs west instead 
# everything looks the same
ggplot(data = morph) +
  geom_histogram(binwidth = 0.5, mapping = aes(x = branches, fill = haplotype), show.legend = FALSE) +
  facet_wrap(~haplotype) +
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
some_data <- read.csv("data/morphology_numerical_data3.csv", header = TRUE, sep = ",")
head(some_data)

#define 'traits' as the numerical columns that the PCA will read

traits <- some_data %>% 
  select(fifth_fourth_ratio, pelvic_ratio, fin_membrane, branches)
head(traits)
View(some_data)

pca2 <- princomp(traits, cor=T)
summary(pca2)
loadings (pca2) #this is the important one - defines PCA axes
plot(pca2)

# plot
plot(pca2$scores[,1], pca2$scores[,2],
     cex = 1.5, 
     col = ifelse(some_data$haplotype == 1, 'tomato', 'maroon4'), 
     pch=20, 
     xlab="PC 1", ylab="PC 2", xlim=c(-2.5,5), ylim= c(-2.5,3),
     cex.axis = 0.7,
     xaxs ="i", yaxs = "i", 
     pin=c(16, 9), 
     bty="l") #take away borders

legend(x="top", 
       cex=0.8, xpd = T , horiz=T, pch=c(20), 
       col=c("tomato", "maroon4"), 
       legend = c("Eastern white-bellied morph", "Western black-bellied morph"),
       bty = "n")

#western haplotype = 1 = maroon4
#eastern haplotype = 2 = tomato






