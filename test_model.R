## Shiny RCI Pre-loaded Test Models 

#library(devtools)
#install_github("philchalmers/mirt")
#packageVersion("mirt")


# So that it's using the CRAN version not the GitHub version 
#remove.packages("SimDesign")
#install.packages("SimDesign") 
# Check version 
#packageDescription("SimDesign")[c("RemoteType","RemoteRef","RemoteRepo","RemoteUsername","Version")]


library(mirt)
library(SimDesign)




##### Simulate "Test Models" 

### BDI-2 
# From (de Sá Junior et al., 2019)'s paper 
#

# Define true item parameters from the paper
#   There are 21 items, each with 4 response categories (3 thresholds)

# Item names 
BDI2item_names <- c(
  "1. Sadness", "2. Pessimism", "3. Past failure", "4. Loss of pleasure",
  "5. Guilty feelings", "6. Punishment feelings", "7. Self-dislike",
  "8. Self-criticalness", "9. Suicidal thoughts", "10. Crying",
  "11. Agitation", "12. Loss of interest", "13. Indecisiveness",
  "14. Worthlessness", "15. Loss of energy", "16. Changes in sleep",
  "17. Irritability", "18. Changes in appetite", "19. Concentration difficulty",
  "20. Tiredness or fatigue", "21. Loss of interest in sex"
)

# Discrimination parameters (a)
a <- c(
  2.38, 2.20, 2.04, 2.27, 1.72, 1.74, 2.86, 1.62, 2.12, 1.52,
  1.59, 2.73, 2.03, 3.31, 2.35, 1.58, 2.13, 1.40, 2.07, 2.24, 1.32
)

# Threshold parameters (b1, b2, b3)
b1 <- c(
  1.10, 1.12, 1.32, 0.95, 0.75, 1.46, 1.24, 0.25, 1.89, 1.16,
  0.83, 0.89, 0.78, 1.31, 0.40, 0.03, 0.65, 0.48, 0.57, 0.32, 1.91
)
b2 <- c(
  2.75, 2.57, 1.98, 2.34, 2.96, 2.39, 1.60, 1.78, 3.22, 1.93,
  2.90, 2.26, 1.88, 1.80, 1.96, 1.69, 2.10, 2.31, 1.60, 1.75, 3.26
)
b3 <- c(
  3.13, 3.35, 3.41, 3.25, 3.78, 2.63, 2.54, 3.24, 3.60, 2.73,
  3.19, 2.87, 2.12, 2.72, 3.05, 3.04, 2.93, 3.28, 2.84, 2.52, 4.44
)

# Combine b para into a single matrix
b <- cbind(b1, b2, b3)


# Simulate the item response data
set.seed(1234) 
N <- 5000
d <- -a * b
sim_data <- simdata(a = a, d = d, N = N, itemtype = 'graded') 

# Name the columns of the simulated data for easier reading
colnames(sim_data) <- BDI2item_names
print(head(sim_data))

# Fit GRM --> NOW I HAVE A MODEL (BDI2mod)
BDI2mod <- mirt(
  data = sim_data,
  model = 1,     # Or model_spec <- 'F1 = 1-21' if want to specify the factor loadings
  itemtype = 'graded',
  verbose = FALSE)

# A hypothetical two-factor model
# two_factor_spec <-
#   'Affective = 1-8
#   Somatic = 15-21'


## How well items recovered 
# Extract estimated parameters in the standard IRT a/b format
# cfs <- coef(BDI2mod, IRTpars = TRUE, simplify = TRUE)

# Compare it to true item parameters above
# itempar <- as.data.frame(cfs$items)   # make it a dataframe first, otherwise it's a vector and somehow it errored

# a.RMSD <- SimDesign::RMSD(itempar$a, a)     # RMSD(ahat, truea)
# b1.RMSD <- RMSD(itempar$b1, b1)
# b2.RMSD <- RMSD(itempar$b2, b2)
# b3.RMSD <- RMSD(itempar$b3, b3)

# Just presented in a cleaner data frame
# itemRMSD <- data.frame(
#   Parameter = c("a", "b1", "b2", "b3"),
#   RMSD_Value = c(a.RMSD, b1.RMSD, b2.RMSD, b3.RMSD))
# itemRMSD




### PHQ-9 
# From (LaLonde et al., 2025)
#
#' Q. Should the item parametrs names be the same or not for Shiny?
#' Q. Should the seed b set the same?


# Define true item parameters from the paper
# There are 9 items, each with 4 response categories (3 thresholds)

# Item names 
PHQ9item_names <- c("1. Little interest", "2. Feeling down", "3. Trouble sleeping",
                "4. Feeling tired or little energy", "5. Poor appetite or overeating",
                "6. Feeling bad aboyt self", "7. Trouble concentrating", 
                "8. Motor disturbance", "9. Suicidal ideation")

# Discrimination parameters (a)
a <- c(2.21, 3.53, 1.94, 2.20, 2.10, 3.20, 1.94, 1.90, 3.24)

# Threshold parameters (b1, b2, b3)
b1 <- c(0.83, 0.89, 0.33, 0.16, 0.94, 1.00, 1.04, 1.58, 1.83)
b2 <- c(1.75, 1.71, 1.30, 1.33, 1.74, 1.66, 1.88, 2.35, 2.43)
b3 <- c(2.50, 2.23, 1.97, 2.00, 2.42, 2.19, 2.46, 2.97, 2.79)

# Combine b para into a single matrix
b <- cbind(b1, b2, b3)

# Simulate the item response data
set.seed(12345) 
N <- 5000
d <- -a * b
sim_data <- simdata(a = a, d = d, N = N, itemtype = 'graded') 

# Name the columns of the simulated data for easier reading
colnames(sim_data) <- PHQ9item_names
print(head(sim_data))

# Fit GRM --> PHQ9mod
PHQ9mod <- mirt(
  data = sim_data,
  model = 1,     
  itemtype = 'graded',
  verbose = FALSE)

## How well items recovered 
# Extract estimated parameters in the standard IRT a/b format
# cfs <- coef(PHQ9mod, IRTpars = TRUE, simplify = TRUE)

# Compare it to true item parameters above
# itempar <- as.data.frame(cfs$items) 

# a.RMSD <- SimDesign::RMSD(itempar$a, a)     # RMSD(ahat, truea)
# b1.RMSD <- RMSD(itempar$b1, b1)
# b2.RMSD <- RMSD(itempar$b2, b2)
# b3.RMSD <- RMSD(itempar$b3, b3)

# Just presented in a cleaner data frame
# itemRMSD <- data.frame(
#   Parameter = c("a", "b1", "b2", "b3"),
#   RMSD_Value = c(a.RMSD, b1.RMSD, b2.RMSD, b3.RMSD))
# itemRMSD




### GAD-7
# From (Schalet et al., 2015)
#

# Item names 
GAD7item_names <- c("1. Feeling nervous anxiety", "2. Not able to stop worrying",
                "3. Worrying too much", "4. Trouble relaxing", "5. Hard to sit still", 
                "6. Becoming easily annoyed", "7. Something might happen")

# Discrimination parameters (a)
a <- c(2.38, 2.62, 2.53, 2.21, 1.98, 1.66, 2.26)
  
# Threshold parameters (b1, b2, b3)
b1 <- c(0.19, 0.27, 0.04, 0.11, 0.78, 0.19, 0.68)
b2 <- c(1.55, 1.44, 1.35, 1.26, 1.91, 1.70, 1.81)
b3 <- c(2.30, 2.08, 1.99, 1.95, 2.73, 2.59, 2.39)

# Combine b para into a single matrix
b <- cbind(b1, b2, b3)

# Simulate the item response data
set.seed(123) 
N <- 5000
d <- -a * b
sim_data <- simdata(a = a, d = d, N = N, itemtype = 'graded') 

# Name the columns of the simulated data for easier reading
colnames(sim_data) <- GAD7item_names
print(head(sim_data))

# Fit GRM --> GAD7mod
GAD7mod <- mirt(
  data = sim_data,
  model = 1,     
  itemtype = 'graded',
  verbose = FALSE)


## How well items recovered 
# Extract estimated parameters in the standard IRT a/b format (w/ IRTpars = TRUE)
# cfs <- coef(GAD7mod, IRTpars = TRUE, simplify = TRUE)

# Compare it to true item parameters above
# itempar <- as.data.frame(cfs$items) 

# a.RMSD <- SimDesign::RMSD(itempar$a, a)     # RMSD(ahat, truea)
# b1.RMSD <- RMSD(itempar$b1, b1)
# b2.RMSD <- RMSD(itempar$b2, b2)
# b3.RMSD <- RMSD(itempar$b3, b3)

# Just presented in a cleaner data frame
# itemRMSD <- data.frame(
#   Parameter = c("a", "b1", "b2", "b3"),
#   RMSD_Value = c(a.RMSD, b1.RMSD, b2.RMSD, b3.RMSD))
# itemRMSD






















