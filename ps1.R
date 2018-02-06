#############################################################
#     File Name           :     Documents/ARE/ARE212/PS1/Code/ps1.R
#     Created By          :     MBlundell
#     Creation Date       :     [2018-01-27 14:36]
#     Last Modified       :     [2018-02-06 10:49]
#     Description         :      
#     
#############################################################

# 
# Set up.
#
library(pacman)
p_load(haven, dplyr, readr, tidyr, haven)

dir <- "C:/Users/mblundell/Documents/ARE/ARE212/PS1/"
originaldata <- paste0(dir, "OriginalData/")
output <- paste0(dir, "Output/")

source(paste0(dir, "Code/ps1functions.R"))

# 
# Load data and clean it up a bit.
#
data <- read_csv(paste0(originaldata,
                        "be0eb9fc-f1b9-4f50-bd91-379b8978c377_Data.csv"),
                 col_types = "ccccn")

# Remove missing values at the end of dataset.
data <- data[complete.cases(data),]

# Drop a couple of variables we don't need.
data[,c("Series Code")] <- NULL

# Data are in long format so we reshape to wide.
data <- spread(data, c("Series Name"), c("2010 [YR2010]"))

# Remove any obs with missing values.
data <- data[complete.cases(data),]

# Rename columns.
names(data) <- c("country", "code", "CO2", "GDP", "POP")

# Confirm we have 194 countries with complete data.
dim(data)

# 
# 3. Calculate a table showing the sample mean, standard deviation, minimum
# and maximum for each series.
#
sapply(data[, c("CO2", "GDP", "POP")],
       function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))

# 
# Make some plots here.
#

# 4. Create a histogram for CO2 with 15 buckets
buckets <- 15
hist(data$CO2, breaks = buckets, 
     main = "Histogram of CO2 Emissions (kt) for 2010.",
     xlab  = "CO2 Emissions (kt)")

# Now do the same for GDP
hist(data$GDP, breaks = buckets, 
     main = "Histogram of GDP (constant 2010 US $)",
     xlab  = "GDP (constant 2010 US $)")

# 5. Plot CO2 against GDP.
plot(data$GDP, data$CO2,
     xlab = "GDP (constant 2010 US $)",
     ylab = "CO2 Emissions (kt)",
     main = "Scatterplot of CO2 Emissions against GDP for 2010")

# 6. Create a new variable "Per capita CO2 emissions," CO2pc.
data$CO2pc <- data$CO2 / data$POP

# 7. Create a new variable "Per capita GDP," GDPpc.
data$GDPpc <- data$GDP / data$POP

# 8. Plot CO2pc against GDPpc
plot(data$GDPpc, data$CO2pc,
     xlab = "GDP per capita (constant 2010 US $)",
     ylab = "CO2 emissions (kt) per capita",
     main = "Scatterplot of CO2 per capita against GDP per capita for 2010")

# 9. Create demeaned variables of CO2pc and GDPpc
data$CO2pcdev <- demean(data$CO2pc)
data$GDPpcdev <- demean(data$GDPpc)

# 10. Plot CO2pc against GDPpc
plot(data$GDPpcdev, data$CO2pcdev,
     xlab = "GDP p.c., demeaned (constant 2010 US $)",
     ylab = "CO2 Emissions p.c., demeaned (kt)",
     main = "Scatterplot of demeaned CO2 p.c. against demeaned GDP p.c.")

# 11 Take natural log of CO2pc and GDPpc
data$CO2pcln <- log(data$CO2pc)
data$GDPpcln <- log(data$GDPpc)

# 12 Plot CO2pcln
plot(data$GDPpcln, data$CO2pcln,
     xlab = "ln(GDP p.c. (constant 2010 US $))",
     ylab = "ln(CO2 Emissions PC (kt))",
     main = "Scatterplot of ln(CO2 Emission p.c.) against ln(GDP p.c.)")

# 13 Export data as a comma delimited ascii file
write.table(data, file = paste0(output, "data_q13.csv"), row.names = F, sep = ",")

#
# 14. Now run some regressions and calculate stuff.
#

# Regress CO2pc on GDPpc without an intercept
beta <- reg(data[, c("CO2pc"), drop = T], as.matrix(data[, c("GDPpc")]))
beta

# Multiply CO2pc by 1,000, changing its units to tons.
data$CO2pc.tons <- data$CO2pc * 1000
# Run the regression again
# New coefficient is 2.233e-04, which is the same as before but multiplied by a factor of 1000.
beta.tons <- reg(data[, c("CO2pc.tons"), drop = T], as.matrix(data[, c("GDPpc")]))
beta.tons

# Now divide GDPpc by 1,000, changing the units to thousands of $.
data$GDPpc.thous <- data$GDPpc / 1000
# Run the regression again
# New coefficient is 0.223, which is the same as beta.tons but multiplied by a factor of 1000.
y <- data[, c("CO2pc.tons"), drop = T]
X  <- as.matrix(data[, c("GDPpc.thous")])
beta.tons.thousands <- reg(y, X)
beta.tons.thousands

# 15. For the regression in the previous part calculate and report a bunch
# of summary stats
report.stats(y, X, beta.tons.thousands)

# Plot predicted values against actual CO2pc
y_hat <- pred(X, beta.tons.thousands)
plot(y, y_hat,
     xlab = "Actual CO2 Emissions p.c. (tons)",
     ylab = "Predicted CO2 Emissions p.c.",
     main = "Scatterplot of Predicted vs Actual CO2 Emissions p.c.")
abline(0, 1)

# Plot residuals against GDPpc
e <- y - y_hat
plot(X, e,
     ylab = "Residuals",
     xlab = "GDP per capita (thousands of 2010 US $)",
     main = "Scatterplot of Residuals versus GDP p.c.")

# 
# 16. Repeat 15 but with an intercept
#
y <- data[, c("CO2pc.tons"), drop = T]
X  <- cbind(1, as.matrix(data[, c("GDPpc.thous")]))
beta.tons.thousands.int <- reg(y, X)
rownames(beta.tons.thousands.int)[1] <- "Intercept"

# Summary stats
report.stats(y, X, beta.tons.thousands.int)

# Plot predicted values against actual CO2pc
y_hat <- pred(X, beta.tons.thousands.int)
plot(y, y_hat,
     xlab = "Actual CO2 Emissions p.c. (tons)",
     ylab = "Predicted CO2 Emissions p.c. (tons)", 
     main = "Scatterplot of Predicted vs Actual CO2 Emissions p.c. (w. Intercept)")
abline(0, 1)

# Plot residuals against GDPpc
e <- y - y_hat
plot(X[,2], e,
     xlab = "GDP p.c. (thousands of 2010 US $)",
     ylab = "Residuals",
     main = "Scatterplot of Residuals versus GDP p.c. (w. Intercept)")

# 
# 17. Regress CO2pc on an intercept, GDPpc, and GDPpc^2
#
data$GDPpc.thous.sq <- data$GDPpc.thous^2
y <- data[, c("CO2pc.tons"), drop = T]
X  <- cbind(1, as.matrix(data[, c("GDPpc.thous", "GDPpc.thous.sq")]))
beta.3 <- reg(y, X)
rownames(beta.3)[1] <- "Intercept"

# calculate summary stats
report.stats(y, X, beta.3)

# Plot predicted values against actual CO2pc
y_hat <- pred(X, beta.3)
plot(y, y_hat,
     xlab = "Actual CO2 Emissions p.c. (tons)",
     ylab = "Predicted CO2 Emissions p.c.",
     main = "Scatterplot of Predicted vs Actual CO2 Emissions p.c. (w. Intercept, sq. term)")
abline(0, 1)

# Plot residuals against GDPpc
e <- y - y_hat
plot(X[,2], e, 
     xlab = "GDP p.c. (thousands of 2010 US $)",
     ylab = "Residuals",
     main = "Scatterplot of Residuals versus GDP p.c. (w. Intercept, sq. term)")


# 
# 18. Regress demeaned CO2pc (tons) on demeaned GDPpc (thousands) and GDPpc^2
#
data$CO2pc.tons.dev <- demean(data$CO2pc.tons)
data$GDPpc.thous.dev <- demean(data$GDPpc.thous)
data$GDPpc.thous.sq.dev <- demean(data$GDPpc.thous.sq)
y <- data[, c("CO2pc.tons.dev"), drop = T]
X  <- as.matrix(data[, c("GDPpc.thous.dev", "GDPpc.thous.sq.dev")])
beta.4 <- reg(y, X)
beta.4

#
# 19. 
#

# Regress CO2pc (tons) on GDPpc (thousands) and save residuals
y <- data[, c("CO2pc.tons"), drop = T]
X  <- as.matrix(data[, c("GDPpc.thous")])
beta.tons.thousands <- reg(y, X)
y_hat <- pred(X, beta.tons.thousands)
e <- y - y_hat

# Regress [i GDPpc.thous.sq] on GDPpc.thous
Y <- cbind(1, as.matrix(data[, c("GDPpc.thous.sq")]))
X <- as.matrix(data[, c("GDPpc.thous")])
beta.5 <- reg(Y,X)
E <- Y - X %*% beta.5

resid.betas <- reg(e, E)
resid.betas
