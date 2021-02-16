# D206 DATA CLEANING PA
options(max.print=10000)

# Import data into RStudio
churn <- read.csv("C:/Users/bakhr_000/Documents/churn_raw_data.csv")

# Display number of columns and rows
dim(churn)

# Display variable type
str(churn)

# Display number of missing values in a column
sapply(churn, function(x) sum(is.na(x)))
# Children, Age, Income, Techie, Phone, TechSupport
# and Tenure, Bandwidth_GB_Year columns have about
# or over 1,000 missing values

# Removing rows containing missing values
library(dplyr)
churn <- churn %>% filter(!is.na(Children))
churn <- churn %>% filter(!is.na(Phone))
churn <- churn %>% filter(!is.na(TechSupport))
# Removing columns containing missing values
churn$Techie <- NULL

# Imputing missing values with columns median value
churn <- churn %>% mutate(Age = ifelse(is.na(Age), median(Age,na.rm=TRUE), Age))
churn <- churn %>% mutate(Income = if_else(is.na(Income), median(Income,na.rm=TRUE), Income))
churn <- churn %>% mutate(Bandwidth_GB_Year = if_else(is.na(Bandwidth_GB_Year), median(Bandwidth_GB_Year,na.rm=TRUE), Bandwidth_GB_Year))
churn <- churn %>% mutate(Tenure = if_else(is.na(Tenure), median(Tenure,na.rm=TRUE), Tenure))

# Display duplicated rows
churn[duplicated(churn), ]
# There are no duplicated rows

# Drop unnecessary columns
churn[1] <- NULL
churn$Customer_id <- NULL
churn$Interaction <- NULL
dim(churn)

# Summary for churn dataset
summary(churn)
# After exploring summary we see that:
# Outage_sec_perweek has negative value as Min
# Population column has 0 value row
# Zip col has 3 digit row whereas US Zip is 5 digits

# Changing Outage_second_perweek - values to +
churn$Outage_sec_perweek <- abs(churn$Outage_sec_perweek)
summary(churn$Outage_sec_perweek)

# Replace population 0 with median
count(churn[churn$Population == 0, ])

# Replace 0 with population average
churn$Population <- replace(churn$Population,
                            churn$Population == 0, NA)
churn <- churn %>% mutate(Population =
                            if_else(is.na(Population),
                                    median(Population,na.rm=TRUE),
                                    Population))
summary(churn$Population)

count(churn[churn$Zip < 10000, ])
# As we can see 471 rows are incorrect
# We can either drop column or clear rows
# For now we skip this part as for PCA analysis
# It is not important; however for cluster analysis
# We may need it

# Define outliers with z-score
# Z-score is calculated by using scale() in R
# Define outliers and create df without outliers
child_z <- scale(churn$Children)
churn <- churn[(child_z <= 3) & (child_z >= -3),]
inc_z <- scale(churn$Income)
churn <- churn[(inc_z <= 3) & (inc_z >= -3),]
pop_z <- scale(churn$Population)
churn <- churn[(pop_z <= 3) & (pop_z >= -3),]
dim(churn)

# Boxplot for Population, Children, Age, Income
library(ggplot2)
library(gridExtra)
library(grid)
Pop <- ggplot(churn, aes(x=Population))+geom_boxplot() + coord_flip()
Child <- ggplot(churn, aes(x=Children))+geom_boxplot() + coord_flip()
Age <- ggplot(churn %>% filter(!is.na(Age)), aes(x=Age))+geom_boxplot() + coord_flip()
Inc <- ggplot(churn %>% filter(!is.na(Income)), aes(x=Income))+geom_boxplot() + coord_flip()
grid.arrange(Pop,Child,Age,Inc)

# Plotting Histogram for Population, Children, Age
# and Income attributes
Pop <- ggplot(churn, aes(x=Population))+geom_histogram(bins=10)
Child <- ggplot(churn, aes(x=Children))+geom_histogram(bins=10)
Age <- ggplot(churn, aes(x=Age))+geom_histogram(bins=10)
Inc <- ggplot(churn, aes(x=Income))+geom_histogram(bins=10)
grid.arrange(Pop,Child,Age,Inc)


# PCA ANALYSIS
# Creating Numeric value columns 
churn_numeric <- churn %>% select(Population, Children, Age, Income, Outage_sec_perweek,
                                  Email, Contacts, Yearly_equip_failure,
                                  Tenure, MonthlyCharge, Bandwidth_GB_Year)

# Normalizing data so that all columns have mean value 0 and std 1
churn_norm <- as.data.frame(scale(churn_numeric))

# Creating PCA. Transforming the normalized, numeric dataset is easy in R.
# The result can be accessed by using pca$x.
pca <- prcomp(churn_norm)
head(pca$x)

# Scree Plot; x-axis for component y-axis for variance
library(factoextra)
fviz_eig(pca)
# Above function elbow bend shows PC1 has more variation

# Selecting fewer components
cumsum((pca$sdev)^2*.1)
# Above function displays 8 components will give
# 90.67% of original datasets variance.

# Which variables are most important in
# explaining the original dataset's total variance:
# The highest loadings in component 1 are age, email.
rotation <- pca$rotation
rotation

# To complete analysis we extract reduced dataset
# We use 8 components as it gives 90.67% of variance
churn_reduced <- as.data.frame(pca$x)[,1:8]
head(churn_reduced)
