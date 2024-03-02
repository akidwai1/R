# Solution of 8.23.4, Titles and citations --- Letchford et al. 2015

1. Write a program that performs the test described above using all the papers published in 2010. The program should do the following: 1) read the data; 2) extract all the papers published in 2010; 3) rank the articles by citations, and by title length; 4) compute the Kendall's tau expressing the correlation between the two rankings. For this dataset, the Authors got a tau of about -0.07 with a significant p-value.

#read data
# Read the data
 l2015 <- read.csv("Desktop/Letchford2015_data.csv", stringsAsFactors = FALSE)
# Check dimensions
dim(l2015)
# Print first few lines
head(l2015)

#extracted only the papers published in 2010:
p2010 <- l2015[l2015$year == 2010, ]

# Example of use of rank
rank(c(1, 2, 3, 2, 1, 5, 3, 4))

# Now, storing the ranking of title lengths and citations separately:
rank_titlelength <- rank(p2010$title_length)
rank_citations <- rank(p2010$cites)
cor(rank_citations, rank_titlelength, method = "kendall", use = "pairwise")
cor.test(rank_citations, rank_titlelength, method = "kendall", use = "pairwise")

2.  Write a function that repeats the analysis for a particular journal-year combination. Try to run the function for the top scientific publications `Nature` and `Science`, and for the top medical journals `The Lancet` and `New Eng J Med`, for all years in the data (2007-2013). Do you always find a negative, significant correlation (i.e., negative tau with low *p*-value)?

compute_tau_journal_year <- function(my_data, my_journal, my_year) {
  # First, filter the data 
  my_subset <- my_data[my_data$journal == my_journal & my_data$year == my_year, ]
  print(c(my_journal, my_year, "Articles:", dim(my_subset)[1]))
}
compute_tau_journal_year(l2015, "Nature", 2010)

# Next, we write the analysis:
compute_tau_journal_year <- function(my_data, my_journal, my_year) {
  # First, filter the data 
  my_subset <- my_data[my_data$journal == my_journal & my_data$year == my_year, ]
  # Rank by title length and citations
  rank_titlelength <- rank(my_subset$title_length)
  rank_citations <- rank(my_subset$cites)
  # Return the value of tau
  return(data.frame(Journal = my_journal,
                    Year = my_year,
                    tau = cor(rank_citations, rank_titlelength, 
                              method = "kendall", use = "pairwise")))
}

# running it
compute_tau_journal_year(l2015, "Nature", 2010)

#Alternatively,

compute_tau_journal_year <- function(my_data, my_journal, my_year) {
  # First, filter the data 
  my_subset <- my_data[my_data$journal == my_journal & my_data$year == my_year, ]
  if (dim(my_subset)[1] < 2) {
    tau <- NA
    p.value <- NA
  } else {
    # Rank by title length and citations
    rank_titlelength <- rank(my_subset$title_length)
    rank_citations <- rank(my_subset$cites)
    # Run the test
    my_test <- cor.test(rank_citations, rank_titlelength, 
                        method = "kendall", use = "pairwise")  
    tau <- as.numeric(my_test$estimate)
    p.value <- as.numeric(my_test$p.value)
  }
  return(data.frame(Journal = my_journal,
                    Year = my_year,
                    tau = tau,
                    p.value = p.value))
}
compute_tau_journal_year(l2015, "Nature", 2010)

# Running it for all years and a few journals:
results <- data.frame()

for (year in 2007:2013){
  for (jr in c("Nature", "Science", "The Lancet", "New Eng J Med")) {
   results <- rbind(results, compute_tau_journal_year(l2015, jr, year))
  }
}
# Noticed both positive and negative tau(s):
results

