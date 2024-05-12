#' Purpose: script to get sections of the complaint data
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: Aug 14, 2024
#'

# Load the necessary library
library(data.table)

# Increase timeout
options(timeout = 120)

# Define the URL for the zip file
cfpbComplaints <- 'https://files.consumerfinance.gov/ccdb/complaints.csv.zip'

# Download the zip file
temp <- tempfile()
download.file(cfpbComplaints, temp)

# Unzip the downloaded zip file
unzip(temp)

# Load the unzipped CSV file
data <- fread("complaints.csv")

# Preview the data
head(data)

# Remember to delete the temporary file after you're done
file.remove(temp)


# Selecting some sections of the data
data$`Date received` <- as.Date(data$`Date received`)
data$yr              <- lubridate::year(data$`Date received`)
lastYr               <- subset(data, data$yr==2024)

mortgageLoan <- subset(lastYr, lastYr$Product=='Mortgage')
studentLoan  <- subset(lastYr, lastYr$Product=='Student loan')

# Save
write.csv(mortgageLoan,'~/Desktop/ICPSR/ICPSR_Day2/data/mortgageLoan_2024.csv', row.names = F)
write.csv(studentLoan,'~/Desktop/ICPSR/ICPSR_Day2/data/studentLoan_2024.csv', row.names = F)

# End