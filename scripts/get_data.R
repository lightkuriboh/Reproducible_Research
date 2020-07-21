
data_url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
file_path <- 'raw_data/compressed.csv.bz2'

raw_data_description <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf'

download.file(data_url, file_path)
download.file(raw_data_description, 'raw_data/data_description.pdf')

storm_data <- read.csv(file_path)
