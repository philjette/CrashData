#Grab data on plane crashes from planecrashinfo.com and load to data frame for analysis
#Phil Jette
#March 2015
#define year you want to go back to for data
startYear<-1950

library(XML)
library(stringr)

getData <-function(year){
#url
url<-paste("http://www.planecrashinfo.com/", year, "/", year, ".htm", sep="")  
  
# Read and parse HTML file
html.raw <- htmlTreeParse(url,useInternal = TRUE)
html.parse<-unlist(xpathApply(html.raw, '//td', function(x)
  xpathSApply(x,".//text()", xmlValue)))

#get rid of field names
html.parse<-html.parse[5:length(html.parse)]

#Get fields into vectors to prepare for data frame
crashDates <- html.parse[seq(1, length(html.parse), 6)]
crashLocation <- html.parse[seq(2, length(html.parse), 6)]
crashOperator <- html.parse[seq(3, length(html.parse), 6)]
crashType <- html.parse[seq(4, length(html.parse), 6)]
crashOutcome <- html.parse[seq(6, length(html.parse), 6)]

#compile into data.frame
data<-data.frame(cbind(crashDates,crashLocation,crashOperator,crashType,crashOutcome))

return(data)

}

#initialize data table
compiledData <-data.frame()

#loop through years and get data
for (i in startYear:2015) {
  compiledData <- rbind(compiledData, getData(i))
}

#split the crash outcome into passengers and fatalities
compiledData$crashF <- unlist(str_split(compiledData$crashOutcome, "\\/", n=2))[seq(1, length(compiledData$crashOutcome)*2, 2)]
compiledData$crashP <- unlist(str_split(compiledData$crashOutcome, "\\/", n=2))[seq(2, length(compiledData$crashOutcome)*2, 2)]
compiledData$crashP <- unlist(str_split(compiledData$crashP, "\\(", n=2))[seq(1, length(compiledData$crashP)*2, 2)]

#coerce fields to appropriate data types
compiledData$crashF <-as.numeric(compiledData$crashF)
compiledData$crashP <-as.numeric(compiledData$crashP)
compiledData$Prop<-round(compiledData$crashF/compiledData$crashP,2)
compiledData$crashDates<-as.Date(compiledData$crashDates,"%d %b %Y")
compiledData$crashYear <- format(compiledData$crashDates, format="%Y")

#Fix certain locations to allow for mapping
compiledData$crashLocation<-str_replace(compiledData$crashLoc, "Near ", "")
compiledData$crashLococation<-str_replace(compiledData$crashLoc, "Off ", "")

#write this out if you want
write.csv(compiledData, "CompiledCrashData.csv")
