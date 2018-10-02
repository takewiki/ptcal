library(readxl)
mydata <- read_excel("data-raw/8月DC_处理后.xlsx")
mydata;
library(ptcal);
mydata2 <-writePTCIntoTable(mydata)
mydata2$个税
mydata2;
