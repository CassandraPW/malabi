multmerge = function(mypath,mypattern){
  filenames=list.files(path=mypath, pattern=mypattern, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x, y) merge(x, y, all = TRUE), datalist)
}

standarderror <- function(x) {sd(x, na.rm=T)/sqrt(sum(!is.na(x)))}
#sw_allErrorNorms


Malabi<-function(test,student_id,grade){
  d <- read_csv(paste0("data/clean/afterErrorCoding/",test,"_screener_",student_id,".csv"))
  norms <- read_csv(paste0("data/clean/",test,"_stat_norms_craw.csv"))
  norms<-norms[norms$grade==grade,]
  if (test=="sw"){
    ## Attention Errors
    d$Attention <- ifelse(grepl(pattern="ad", d$error_code), 1, 0)
    d$Attention_C <- ifelse(grepl(pattern="cad", d$error_code), 1, 0)
    d$Attention_V <- ifelse(grepl(pattern="vad", d$error_code), 1, 0)
    #d$adom <- ifelse(grepl(pattern="adom", d$error_code), 1, 0)
    #d$adod <- ifelse(grepl(pattern="adod", d$error_code), 1, 0)

    ## Letter Position
    d$LetterPosition <- ifelse(grepl(pattern="lp", d$error_code), 1, 0)
    d$LetterPosition_C <- ifelse(grepl(pattern="clp", d$error_code), 1, 0)
    d$LetterPosition_V <- ifelse(grepl(pattern="vlp", d$error_code), 1, 0)

    LetterDoubling <- c("bsub","bom","bod","bvod","bvsub","bvom")
    d$LetterDoubling <- ifelse(grepl(pattern=paste(LetterDoubling, collapse = "|"), d$error_code), 1, 0)

    ## Surface
    Surface<-c("sur","gr","gem","sbs","sml")
    d$Surface <- ifelse(grepl(pattern=paste(Surface, collapse = "|"), d$error_code), 1, 0)

    ## Visual Dyslexia
    Visual<-c("sub","om","od")
    d$Visual <- ifelse(grepl(pattern=paste(Visual, collapse = "|"), d$error_code), 1, 0)

    ## Deep
    Deep<-c("mor","sem","fun")
    d$Deep <- ifelse(grepl(pattern=paste(Deep, collapse = "|"), d$error_code), 1, 0)
    #d$mor <- ifelse(grepl(pattern="mor", d$error_code), 1, 0)
    #d$sem <- ifelse(grepl(pattern="sem", d$error_code), 1, 0)
    #d$fun <- ifelse(grepl(pattern="fun", d$error_code), 1, 0)
    #phonBuffer <- c("mor","sem","fun")
    #d$phonBuffer <- ifelse(grepl(pattern=paste(phonBuffer, collapse = "|"), d$error_code), 1, 0)

    ## Vowel
    d$Vowel <- ifelse(grepl(pattern="v", d$error_code), 1, 0)

    ## Neglect
    d$Neglect <- ifelse(grepl(pattern="neg", d$error_code), 1, 0)

    d<-d[ ,c("word","Attention","Attention_C","Attention_V",
             "LetterPosition","LetterPosition_C","LetterPosition_V",
             "LetterDoubling",
             "Surface",
             "Visual",
             "Deep",
             "Vowel",
             "Neglect")]

    #ndf<-melt(d, id = c("word"), measured = c("ad","cad","vad","adom","adod","lp","clp","vlp","doubles","surface","visual","mor","sem","fun","phonBuffer","vowel","negation"))

    ndf<- d %>% pivot_longer(cols = -c(word), names_to = "variable", values_to = "value")
  }
  else if (test=="pw"){
    ## Attention Errors
    d$Attention <- ifelse(grepl(pattern="ad", d$error_code), 1, 0)
    d$Attention_C <- ifelse(grepl(pattern="cad", d$error_code), 1, 0)
    d$Attention_V <- ifelse(grepl(pattern="vad", d$error_code), 1, 0)
    #d$adom <- ifelse(grepl(pattern="adom", d$error_code), 1, 0)
    #d$adod <- ifelse(grepl(pattern="adod", d$error_code), 1, 0)

    ## Letter Position
    d$LetterPosition <- ifelse(grepl(pattern="lp", d$error_code), 1, 0)
    d$LetterPosition_C <- ifelse(grepl(pattern="clp", d$error_code), 1, 0)
    d$LetterPosition_V <- ifelse(grepl(pattern="vlp", d$error_code), 1, 0)
    LetterDoubling <- c("bsub","bom","bod","bvod","bvsub","bvom")
    d$LetterDoubling <- ifelse(grepl(pattern=paste(LetterDoubling, collapse = "|"), d$error_code), 1, 0)

    ## Surface
    Surface<-c("sur","gr","gem","sbs","sml")
    d$Surface <- ifelse(grepl(pattern=paste(Surface, collapse = "|"), d$error_code), 1, 0)

    ## Visual Dyslexia
    Visual<-c("sub","om","od")
    d$Visual <- ifelse(grepl(pattern=paste(Visual, collapse = "|"), d$error_code), 1, 0)

    ## Deep
    Deep<-c("mor","sem","fun")
    d$Deep <- ifelse(grepl(pattern=paste(Deep, collapse = "|"), d$error_code), 1, 0)
    #d$mor <- ifelse(grepl(pattern="mor", d$error_code), 1, 0)
    #d$sem <- ifelse(grepl(pattern="sem", d$error_code), 1, 0)
    #d$fun <- ifelse(grepl(pattern="fun", d$error_code), 1, 0)
    #phonBuffer <- c("mor","sem","fun")
    #d$phonBuffer <- ifelse(grepl(pattern=paste(phonBuffer, collapse = "|"), d$error_code), 1, 0)

    ## Vowel
    d$Vowel <- ifelse(grepl(pattern="v", d$error_code), 1, 0)

    ## Neglect
    d$Neglect <- ifelse(grepl(pattern="neg", d$error_code), 1, 0)

    d<-d[ ,c("word","Attention","Attention_C","Attention_V",
             "LetterPosition","LetterPosition_C","LetterPosition_V",
             "LetterDoubling",
             "Surface",
             "Visual",
             "Deep",
             "Vowel",
             "Neglect")]

    #ndf<-melt(d, id = c("word"), measured = c("ad","cad","vad","adom","adod","lp","clp","vlp","doubles","surface","visual","mor","sem","fun","phonBuffer","vowel","negation"))

    ndf<- d %>% pivot_longer(cols = -c(word), names_to = "variable", values_to = "value")
  }
  else if (test=="wp"){
    ## Attention Errors
    d$Attention <- ifelse(grepl(pattern="ad", d$error_code), 1, 0)
    d$Attention_C <- ifelse(grepl(pattern="cad", d$error_code), 1, 0)
    d$Attention_V <- ifelse(grepl(pattern="vad", d$error_code), 1, 0)
    #d$adom <- ifelse(grepl(pattern="adom", d$error_code), 1, 0)
    #d$adod <- ifelse(grepl(pattern="adod", d$error_code), 1, 0)

    ## Letter Position
    d$LetterPosition <- ifelse(grepl(pattern="lp", d$error_code), 1, 0)
    d$LetterPosition_C <- ifelse(grepl(pattern="clp", d$error_code), 1, 0)
    d$LetterPosition_V <- ifelse(grepl(pattern="vlp", d$error_code), 1, 0)
    LetterDoubling <- c("bsub","bom","bod","bvod","bvsub","bvom")
    d$LetterDoubling <- ifelse(grepl(pattern=paste(LetterDoubling, collapse = "|"), d$error_code), 1, 0)

    ## Surface
    Surface<-c("sur","gr","gem","sbs","sml")
    d$Surface <- ifelse(grepl(pattern=paste(Surface, collapse = "|"), d$error_code), 1, 0)

    ## Visual Dyslexia
    Visual<-c("sub","om","od")
    d$Visual <- ifelse(grepl(pattern=paste(Visual, collapse = "|"), d$error_code), 1, 0)

    ## Deep
    Deep<-c("mor","sem","fun")
    d$Deep <- ifelse(grepl(pattern=paste(Deep, collapse = "|"), d$error_code), 1, 0)
    #d$mor <- ifelse(grepl(pattern="mor", d$error_code), 1, 0)
    #d$sem <- ifelse(grepl(pattern="sem", d$error_code), 1, 0)
    #d$fun <- ifelse(grepl(pattern="fun", d$error_code), 1, 0)
    #phonBuffer <- c("mor","sem","fun")
    #d$phonBuffer <- ifelse(grepl(pattern=paste(phonBuffer, collapse = "|"), d$error_code), 1, 0)

    ## Vowel
    d$Vowel <- ifelse(grepl(pattern="v", d$error_code), 1, 0)

    ## Neglect
    d$Neglect <- ifelse(grepl(pattern="neg", d$error_code), 1, 0)

    d<-d[ ,c("word","Attention","Attention_C","Attention_V",
             "LetterPosition","LetterPosition_C","LetterPosition_V",
             "LetterDoubling",
             "Surface",
             "Visual",
             "Deep",
             "Vowel",
             "Neglect")]

    #ndf<-melt(d, id = c("word"), measured = c("ad","cad","vad","adom","adod","lp","clp","vlp","doubles","surface","visual","mor","sem","fun","phonBuffer","vowel","negation"))

    ndf<- d %>% pivot_longer(cols = -c(word), names_to = "variable", values_to = "value")
    }

  x<-aggregate(value~variable, ndf, sum)

  wup<-merge(x,norms, by.x = "variable", by.y = "error_type")
  print(wup$variable[wup$value >= wup$CRAW])
  return(wup)
}

#Crawford and Howell single subject Dys test.
CrawfordHowell <- function(case, control){
  tval <- (case - mean(control)) / (sd(control)*sqrt((length(control)+1) / length(control)))
  degfree <- length(control)-1
  pval <- 2*(1-pt(abs(tval), df=degfree)) #two-tailed p-value
  result <- data.frame(t = tval, df = degfree, p=pval)
  return(result)
}
