# Libraries
library(plm) # for Panel Granger test
library(tseries) # for ADF Test
library(lmtest) # BP test


# Read the datasets
asset_causality_min_9 <- read.csv("/Users/Melike/Desktop/Causality/asset_causality_min_9.csv", stringsAsFactors = FALSE)
asset_causality_min_10 <- read.csv("/Users/Melike/Desktop/Causality/asset_causality_min_10.csv", stringsAsFactors = FALSE)
asset_causality_min_9_50 <- read.csv("/Users/Melike/Desktop/Causality/asset_causality_min_9_50.csv", stringsAsFactors = FALSE)
asset_causality_min_10_50 <- read.csv("/Users/Melike/Desktop/Causality/asset_causality_min_10_50.csv", stringsAsFactors = FALSE)

rev_causality_min_9 <- read.csv("/Users/Melike/Desktop/Causality/rev_causality_min_9.csv", stringsAsFactors = FALSE)
rev_causality_min_10 <- read.csv("/Users/Melike/Desktop/Causality/rev_causality_min_10.csv", stringsAsFactors = FALSE)
rev_causality_min_9_50 <- read.csv("/Users/Melike/Desktop/Causality/rev_causality_min_9_50.csv", stringsAsFactors = FALSE)
rev_causality_min_10_50 <- read.csv("/Users/Melike/Desktop/Causality/rev_causality_min_10_50.csv", stringsAsFactors = FALSE)


# Panel data conversion
panel_data_asset <- pdata.frame(asset_causality_min_10_50, index = c("company", "year")) # Setting as panel data
panel_data_rev <- pdata.frame(rev_causality_min_10_50, index = c("company", "year"))

# Augmented Dickey Fuller Unit Root Test 
# No unit roots if p-value is smaller than 0.05 
adf.test(panel_data_asset$total_assets, k=2) # The series is stationary.
adf.test(panel_data_asset$num_patents, k=2) # The series is stationary.

adf.test(panel_data_rev$total_rev, k=2) # The series is stationary.
adf.test(panel_data_rev$num_patents, k=2) # The series is stationary.


# Breusch-Pagan Test for Heteroskedasticity 
# H0 is for the Breusch-Pagan test is homoskedasticity
bptest(total_assets ~ num_patents + factor(company), data = panel_data_asset, studentize=F) # There is no homoskedasticity
bptest(total_rev ~ num_patents + factor(company), data = panel_data_rev, studentize=F) # There is no homoskedasticity


# Panel Granger Causality - ASSETS
pgrangertest(total_assets ~ num_patents, data = asset_causality_min_9, test = "Ztilde") # gives the standardised statistic recommended by Dumitrescu/Hurlin (2012) for fixed T samples. 
pgrangertest(total_assets ~ num_patents, data = asset_causality_min_9_50, test = "Ztilde")

pgrangertest(total_assets ~ num_patents, data = asset_causality_min_10, test = "Ztilde") 
pgrangertest(total_assets ~ num_patents, data = asset_causality_min_10_50, test = "Ztilde")
pgrangertest(total_assets ~ num_patents, data = asset_causality_min_10_50, test = "Zbar")
pgrangertest(total_assets ~ num_patents, data = asset_causality_min_10_50, test = "Wbar")


# Panel Granger Causality - REVENUES
pgrangertest(total_rev ~ num_patents, data = rev_causality_min_9, test = "Ztilde")
pgrangertest(total_rev ~ num_patents, data = rev_causality_min_9_50, test = "Ztilde")

pgrangertest(total_rev ~ num_patents, data = rev_causality_min_10, test = "Ztilde")
pgrangertest(total_rev ~ num_patents, data = rev_causality_min_10_50, test = "Ztilde")
pgrangertest(total_rev ~ num_patents, data = rev_causality_min_10_50, test = "Zbar")
pgrangertest(total_rev ~ num_patents, data = rev_causality_min_10_50, test = "Wbar")


