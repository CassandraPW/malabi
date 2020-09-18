# Each time a new test is done by a reader,
# use this script to run through the responses and check that we have recorded all of the
# types of errors made in our 'coded_response_error.csv' database. Missing errors should be added by hand.

#Libraries
library(readr)
source("Malabi_functions3.R")
cre <- read_csv("data/raw/coded_response_errors.csv")
outDir <- "data/clean/afterErrorCoding"



test<-"wp" # choose the test you are interested in.
### Choose one below, merge and code for paper or tablet ...
# Paper
allsubj<-multmerge("data/raw/beforeErrorCoding_paperTest", paste0("?",test,"_screener") )
a<-merge(allsubj,cre, by.x = c("test_id","order","word","response"),
         by.y = c("test_id","order","word","response"), all.x = TRUE)

#### Tablet
#allsubj<-multmerge("data/beforeErrorCoding_tabletTest", paste0("?",test,"_screener") )
#a<-merge(allsubj,cre, by.x = c("test_id","trial_index","word","button_pressed_text"),
         #by.y = c("test_id","order","word","response"), all.x = TRUE)

#a<-a[,c("test_id","trial_index","word","button_pressed_text","subj_id","wp_location","error_code")]
#colnames(a)<-c("test_id","order","word","response","subj_id","wp_location","error_code")
### Did Marie add subject ?


# Are there responses that have not been coded for ?
a[!is.na(a$response)&is.na(a$error_code),] # If empty, than there are no new errors. STOP HERE.
# If neccessary, make a file of errors to be coded for.
#write.csv(x, "data/working/new_errors.csv", row.names = FALSE)


# Now add the error coded files to the After Error Coding folder !
for (subj in a$subj_id) {
  d<-a[a$subj_id==subj, ]
  write.csv(d, paste0(outDir,"/",test,"_screener_",subj,".csv"), row.names = FALSE)
}
