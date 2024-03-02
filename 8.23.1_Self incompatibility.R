Solution of 8.23.1, Self incompatibility in plants --- Golberg et al. 2010

1. Write a program that counts how many species are in each category of `Status`. The output is a `data.frame`
# Read the table (use stringsAsFactors == FALSE, otherwise species names
# will become factors!)
g2010 <- read.csv("~/Desktop/Goldberg2010_data.csv", stringsAsFactors = FALSE)
# Check the dimensions
dim(g2010)
# Print the first few lines
head(g2010)

Now, we want to count how many species are associated with each `Status`, and store the results in a `data.frame`. 

# Get unique values in Status
possible_status <- sort(unique(g2010$Status))
# Create empty data.frame
results <- data.frame()
# Cycle through the possible values:
for (status in possible_status) {
  # Add to the data.frame
  results <- rbind(results, 
                   data.frame(Status = status, count = sum(g2010$Status == status)))
}
results

2. Write a program that builds a `data.frame` specifying how many species are in each `Status` for each genus (note that each species name starts with the genus, followed by an underscore).

#  the first species
sp1 <- g2010$Species[1]
#  the species name
sp1
# Split 
strsplit(sp1, "_")
# access the genus 
# [[1]] -> First element of the list; 
# [1] -> First element of the vector
strsplit(sp1, "_")[[1]][1]

With this function at hand, we can for example add a new `Genus` column to `g2010`:

# Create empty vector of strings
genera <- character(0)
# For each species, extract genus
for (sp in g2010$Species){
  genus <- strsplit(sp, "_")[[1]][1]
  # updated genus list
  genera <- c(genera, genus)
}
# Finally, append to g2010
g2010$Genus <- genera
#  the first few
head(g2010)
table(g2010$Genus, g2010$Status)
