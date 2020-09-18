################################################
#### Set Up ##############################
################################################
setwd("/Users/cpotier/Malabi/R_malabi")
source('Malabi_functions3.R')  

# libraries
library(readr); library(ggplot2) ;  library(arsenal); library(plotly) ; library(ez) ; library(reshape)
library(pander) 
panderOptions('round', 2)
panderOptions('keep.trailing.zeros', TRUE)
subj <- read_csv("data/clean/subjects_for_norms.csv")
subj$grade[subj$grade==5]<- 7 # Change to American school grades
subj$grade <- factor(subj$grade, ordered=TRUE)
subj <- subj[subj$include_in_norms==1, ]


################################################
#### Single Words ##############################
################################################
sw<-multmerge("data/afterErrorCoding", "?sw_screener") #Coded responses from the clean file.
#Check that I have data for all subjects that should be included in the norms for 6th and 7th(5)grade.
subjCheck<-subj$subj_id[(subj$include_in_norms)&!(subj$subj_id%in%sw$subj_id)]
# Yes!
sw<-merge(sw, subj[,c("subj_id","grade","sex","school_id","age_in_months")])
# Total percentage error for the test.
sw$IR<-ifelse(is.na(sw$response), 0,1)
total_error <-aggregate(IR~ subj_id + grade + sex + school_id + age_in_months + test_id, data=sw, FUN=sum)

pander(ezStats(data = total_error, dv=IR, wid = subj_id, between = .(grade)))
pander(ezANOVA(data = total_error, dv=IR, wid = subj_id, between = .(grade)))

# Make columns for error types
col_list <- list('ad', 'cad', 'vad', 'adom', 'adod', #Attention
                 'lp', 'clp', 'vlp', #Letter Position
                 'sur', 'gr', 'gem', 'sbs','sml', #Surface
                 'v', #Vowel
                 'sub', 'om', 'od', 'sim', 'cl', #Visual
                 'sem', 'mor', 'fun', #Deep
                 'neg', #Neglect
                 'bsub', 'bom', 'bod', 'bvod', 'bvsub', 'bvom') #Doubling (linked to LP)
errorDF <- sapply(col_list, function(i){ x<-ifelse(grepl(pattern=i, sw$error_code), 1, 0)})
colnames(errorDF)<- col_list
sw<-cbind(sw, errorDF)

##########
########## Attentions Errors #################
##########
# remove the outliers
subj_error <-aggregate(ad~ subj_id + grade + test_id, data=sw, FUN=sum)
x<-ezStats(data=subj_error, dv=ad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$ad > x[[6]][1] | subj_error$grade==7 & subj_error$ad > x[[6]][2]]
# Stats w/out outliers
subj_error_rmout<-aggregate(ad~ subj_id + grade, data=sw[!sw$subj_id%in%rmv, ], FUN=sum)
sw_stats <- ezStats(data=subj_error_rmout, dv=ad, wid=subj_id, between=.(grade))

ezPlot(data=subj_error_rmout, dv=ad, wid=subj_id, between=.(grade), x=grade)
sw_stats$error_type <-"Attention"
sw_stats

###############@
### CAD
subj_error <-aggregate(cad~ subj_id + grade + test_id, data=sw, FUN=sum)
x<-ezStats(data=subj_error, dv=cad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$cad > x[[6]][1] | subj_error$grade==7 & subj_error$cad > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=cad, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Attention_C"
sw_stats<-rbind(sw_stats,apnd)
pander(sw_stats)

###############@
### VAD
subj_error <-aggregate(vad~ subj_id + grade + test_id, data=sw, FUN=sum)
x<-ezStats(data=subj_error, dv=vad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$vad > x[[6]][1] | subj_error$grade==7 & subj_error$vad > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=vad, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Attention_V"
sw_stats<-rbind(sw_stats,apnd)

  
##########
########## Letter Position Errors #################
##########
subj_error <-aggregate(lp~ subj_id + grade + test_id, data=sw, FUN=sum)
subj_error <-aggregate(lp~ subj_id + grade + test_id, data=sw, FUN=sum)
ggplot(subj_error, aes(grade, lp)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=lp, wid=subj_id, between=.(grade))
ezPlot(data=subj_error, dv=lp, wid=subj_id, between=.(grade), x=grade)
pander(ezANOVA(data=subj_error, dv=lp, wid=subj_id, between=.(grade)))

# Stats w/out outliers (+3SD errors)
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$lp > x[[6]][1] | subj_error$grade==7 & subj_error$lp > x[[6]][2]]
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=lp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition"
sw_stats<-rbind(sw_stats,apnd)

###############@
### CLP
subj_error <-aggregate(clp~ subj_id + grade + test_id, data=sw, FUN=sum)
x<-ezStats(data=subj_error, dv=clp, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$clp > x[[6]][1] | subj_error$grade==7 & subj_error$clp > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=clp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition_C"
sw_stats<-rbind(sw_stats,apnd)

###############@
### VLP
subj_error <-aggregate(vlp~ subj_id + grade + test_id, data=sw, FUN=sum)
x<-ezStats(data=subj_error, dv=vlp, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$vlp > x[[6]][1] | subj_error$grade==7 & subj_error$vlp > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=vlp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition_V"
sw_stats<-rbind(sw_stats,apnd)

### LetterDoubling
sw$Doubling<-sw$bsub+sw$bom+sw$bod+sw$bvod+sw$bvsub+sw$bvom 
subj_error <-aggregate(Doubling~ subj_id + grade + test_id, data=sw, FUN=sum)
x<-ezStats(data=subj_error, dv=Doubling, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Doubling > x[[6]][1] | subj_error$grade==7 & subj_error$Doubling > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Doubling, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterDoubling"
sw_stats<-rbind(sw_stats,apnd)

###############
### Neglect
subj_error <-aggregate(neg~ subj_id + grade + test_id, data=sw, FUN=sum)
ggplot(subj_error, aes(grade, neg)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=neg, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$neg > x[[6]][1] | subj_error$grade==7 & subj_error$neg > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=neg, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Neglect"
sw_stats<-rbind(sw_stats,apnd)


###############@
### Vowel Errors
subj_error <-aggregate(v~ subj_id + grade + test_id, data=sw, FUN=sum)
ggplot(subj_error, aes(grade, v)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=v, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$v > x[[6]][1] | subj_error$grade==7 & subj_error$v > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=v, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"VowelErrors"
sw_stats<-rbind(sw_stats,apnd)

##################
### Visual
sw$Visual<-sw$sub+sw$om+sw$od+sw$sim+sw$cl
subj_error <-aggregate(Visual~ subj_id + grade + test_id, data=sw, FUN=sum)
ggplot(subj_error, aes(grade, Visual)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=Visual, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Visual > x[[6]][1] | subj_error$grade==7 & subj_error$Visual > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Visual, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Visual"
sw_stats<-rbind(sw_stats,apnd)


###################
### Surface
sw$Surface<-sw$sur+sw$gr+sw$gem+sw$sbs+sw$sml
subj_error <-aggregate(Surface~ subj_id + grade + test_id, data=sw, FUN=sum)
x<-ezStats(data=subj_error, dv=Surface, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Surface > x[[6]][1] | subj_error$grade==7 & subj_error$Surface > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Surface, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Surface"
sw_stats<-rbind(sw_stats,apnd)


### Deep
sw$Deep<-sw$sem+sw$mor+sw$fun
subj_error <-aggregate(Deep~ subj_id + grade + test_id, data=sw, FUN=sum)
x<-ezStats(data=subj_error, dv=Deep, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Deep > x[[6]][1] | subj_error$grade==7 & subj_error$Deep > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Deep, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Deep"
sw_stats<-rbind(sw_stats,apnd)


sw_stats$test_id<-"sw"



################################################
#### Pseudo Words ##############################
################################################
pw<-multmerge("data/afterErrorCoding", "?pw_screener") #Coded responses from the clean file.
#Check that I have data for all subjects that should be included in the norms for 6th and 7th(5)grade.
subjCheck<-subj$subj_id[(subj$include_in_norms)&!(subj$subj_id%in%pw$subj_id)]
# Yes!
pw<-merge(pw, subj[,c("subj_id","grade","sex","school_id","age_in_months")])
# Total percentage error for the test.
pw$IR<-ifelse(is.na(pw$response), 0,1)
total_error <-aggregate(IR~ subj_id + grade + sex + school_id + age_in_months + test_id, data=pw, FUN=sum)

pander(ezStats(data = total_error, dv=IR, wid = subj_id, between = .(grade)))
pander(ezANOVA(data = total_error, dv=IR, wid = subj_id, between = .(grade)))

# Make columns for error types
col_list <- list('ad', 'cad', 'vad', 'adom', 'adod', #Attention
                 'lp', 'clp', 'vlp', #Letter Position
                 'sur', 'gr', 'gem', 'sbs','sml', #Surface
                 'v', #Vowel
                 'sub', 'om', 'od', 'sim', 'cl', #Visual
                 'sem', 'mor', 'fun', #Deep
                 'neg', #Neglect
                 'bsub', 'bom', 'bod', 'bvod', 'bvsub', 'bvom') #Doubling (linked to LP)

errorDF <- sapply(col_list, function(i){ x<-ifelse(grepl(pattern=i, pw$error_code), 1, 0)})
colnames(errorDF)<- col_list
pw<-cbind(pw, errorDF)

##########
########## Attentions Errors #################
##########
# remove the outliers
subj_error <-aggregate(ad~ subj_id + grade + test_id, data=pw, FUN=sum)
x<-ezStats(data=subj_error, dv=ad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$ad > x[[6]][1] | subj_error$grade==7 & subj_error$ad > x[[6]][2]]
# Stats w/out outliers
subj_error_rmout<-aggregate(ad~ subj_id + grade, data=pw[!pw$subj_id%in%rmv, ], FUN=sum)
pw_stats <- ezStats(data=subj_error_rmout, dv=ad, wid=subj_id, between=.(grade))

ezPlot(data=subj_error_rmout, dv=ad, wid=subj_id, between=.(grade), x=grade)
pw_stats$error_type <-"Attention"
pw_stats

###############@
### CAD
subj_error <-aggregate(cad~ subj_id + grade + test_id, data=pw, FUN=sum)
x<-ezStats(data=subj_error, dv=cad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$cad > x[[6]][1] | subj_error$grade==7 & subj_error$cad > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=cad, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Attention_C"
pw_stats<-rbind(pw_stats,apnd)
pander(pw_stats)

###############@
### VAD
subj_error <-aggregate(vad~ subj_id + grade + test_id, data=pw, FUN=sum)
x<-ezStats(data=subj_error, dv=vad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$vad > x[[6]][1] | subj_error$grade==7 & subj_error$vad > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=vad, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Attention_V"
pw_stats<-rbind(pw_stats,apnd)


##########
########## Letter Position Errors #################
##########
subj_error <-aggregate(lp~ subj_id + grade + test_id, data=pw, FUN=sum)
subj_error <-aggregate(lp~ subj_id + grade + test_id, data=pw, FUN=sum)
ggplot(subj_error, aes(grade, lp)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=lp, wid=subj_id, between=.(grade))
ezPlot(data=subj_error, dv=lp, wid=subj_id, between=.(grade), x=grade)
pander(ezANOVA(data=subj_error, dv=lp, wid=subj_id, between=.(grade)))

# Stats w/out outliers (+3SD errors)
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$lp > x[[6]][1] | subj_error$grade==7 & subj_error$lp > x[[6]][2]]
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=lp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition"
pw_stats<-rbind(pw_stats,apnd)

###############@
### CLP
subj_error <-aggregate(clp~ subj_id + grade + test_id, data=pw, FUN=sum)
x<-ezStats(data=subj_error, dv=clp, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$clp > x[[6]][1] | subj_error$grade==7 & subj_error$clp > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=clp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition_C"
pw_stats<-rbind(pw_stats,apnd)

###############@
### VLP
subj_error <-aggregate(vlp~ subj_id + grade + test_id, data=pw, FUN=sum)
x<-ezStats(data=subj_error, dv=vlp, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$vlp > x[[6]][1] | subj_error$grade==7 & subj_error$vlp > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=vlp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition_V"
pw_stats<-rbind(pw_stats,apnd)

### LetterDoubling
pw$Doubling<-pw$bsub+pw$bom+pw$bod+pw$bvod+pw$bvsub+pw$bvom 
subj_error <-aggregate(Doubling~ subj_id + grade + test_id, data=pw, FUN=sum)
x<-ezStats(data=subj_error, dv=Doubling, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Doubling > x[[6]][1] | subj_error$grade==7 & subj_error$Doubling > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Doubling, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterDoubling"
pw_stats<-rbind(pw_stats,apnd)


###############@
### Vowel Errors
subj_error <-aggregate(v~ subj_id + grade + test_id, data=pw, FUN=sum)
ggplot(subj_error, aes(grade, v)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=v, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$v > x[[6]][1] | subj_error$grade==7 & subj_error$v > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=v, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"VowelErrors"
pw_stats<-rbind(pw_stats,apnd)

###############
### Neglect
subj_error <-aggregate(neg~ subj_id + grade + test_id, data=pw, FUN=sum)
ggplot(subj_error, aes(grade, neg)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=neg, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$neg > x[[6]][1] | subj_error$grade==7 & subj_error$neg > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=neg, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Neglect"
pw_stats<-rbind(pw_stats,apnd)

##################
### Visual
pw$Visual<-pw$sub+pw$om+pw$od+pw$sim+pw$cl
subj_error <-aggregate(Visual~ subj_id + grade + test_id, data=pw, FUN=sum)
ggplot(subj_error, aes(grade, Visual)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=Visual, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Visual > x[[6]][1] | subj_error$grade==7 & subj_error$Visual > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Visual, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Visual"
pw_stats<-rbind(pw_stats,apnd)

### Surface
pw$Surface<-pw$sur+pw$gr+pw$gem+pw$sbs+pw$sml
subj_error <-aggregate(Surface~ subj_id + grade + test_id, data=pw, FUN=sum)
x<-ezStats(data=subj_error, dv=Surface, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Surface > x[[6]][1] | subj_error$grade==7 & subj_error$Surface > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Surface, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Surface"
pw_stats<-rbind(pw_stats,apnd)


### Deep
pw$Deep<-pw$sem+pw$mor+pw$fun
subj_error <-aggregate(Deep~ subj_id + grade + test_id, data=pw, FUN=sum)
x<-ezStats(data=subj_error, dv=Deep, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Deep > x[[6]][1] | subj_error$grade==7 & subj_error$Deep > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Deep, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Deep"
pw_stats<-rbind(pw_stats,apnd)


pw_stats$test_id<-"pw"


################################################
#### Word Pairs ##############################
################################################
wp<-multmerge("data/afterErrorCoding", "?wp_screener") #Coded responses from the clean file.
#Check that I have data for all subjects that should be included in the norms for 6th and 7th(5)grade.
subjCheck<-subj$subj_id[(subj$include_in_norms)&!(subj$subj_id%in%wp$subj_id)]
# Yes!
wp<-merge(wp, subj[,c("subj_id","grade","sex","school_id","age_in_months")])
# Total percentage error for the test.
wp$IR<-ifelse(is.na(wp$response), 0,1)
total_error <-aggregate(IR~ subj_id + grade + sex + school_id + age_in_months + test_id, data=wp, FUN=sum)

pander(ezStats(data = total_error, dv=IR, wid = subj_id, between = .(grade)))
pander(ezANOVA(data = total_error, dv=IR, wid = subj_id, between = .(grade)))

# Make columns for error types
col_list <- list('ad', 'cad', 'vad', 'adom', 'adod', #Attention
                 'lp', 'clp', 'vlp', #Letter Position
                 'sur', 'gr', 'gem', 'sbs','sml', #Surface
                 'v', #Vowel
                 'sub', 'om', 'od', 'sim', 'cl', #Visual
                 'sem', 'mor', 'fun',
                 'neg', #Neglect
                 'bsub', 'bom', 'bod', 'bvod', 'bvsub', 'bvom') #Doubling (linked to LP)

errorDF <- sapply(col_list, function(i){ x<-ifelse(grepl(pattern=i, wp$error_code), 1, 0)})
colnames(errorDF)<- col_list
wp<-cbind(wp, errorDF)

##########
########## Attentions Errors #################
##########
# remove the outliers
subj_error <-aggregate(ad~ subj_id + grade + test_id, data=wp, FUN=sum)
x<-ezStats(data=subj_error, dv=ad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$ad > x[[6]][1] | subj_error$grade==7 & subj_error$ad > x[[6]][2]]
# Stats w/out outliers
subj_error_rmout<-aggregate(ad~ subj_id + grade, data=wp[!wp$subj_id%in%rmv, ], FUN=sum)
wp_stats <- ezStats(data=subj_error_rmout, dv=ad, wid=subj_id, between=.(grade))

ezPlot(data=subj_error_rmout, dv=ad, wid=subj_id, between=.(grade), x=grade)
wp_stats$error_type <-"Attention"
wp_stats

###############@
### CAD
subj_error <-aggregate(cad~ subj_id + grade + test_id, data=wp, FUN=sum)
x<-ezStats(data=subj_error, dv=cad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$cad > x[[6]][1] | subj_error$grade==7 & subj_error$cad > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=cad, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Attention_C"
wp_stats<-rbind(wp_stats,apnd)
pander(wp_stats)

###############@
### VAD
subj_error <-aggregate(vad~ subj_id + grade + test_id, data=wp, FUN=sum)
x<-ezStats(data=subj_error, dv=vad, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$vad > x[[6]][1] | subj_error$grade==7 & subj_error$vad > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=vad, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Attention_V"
wp_stats<-rbind(wp_stats,apnd)


##########
########## Letter Position Errors #################
##########
subj_error <-aggregate(lp~ subj_id + grade + test_id, data=wp, FUN=sum)
subj_error <-aggregate(lp~ subj_id + grade + test_id, data=wp, FUN=sum)
ggplot(subj_error, aes(grade, lp)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=lp, wid=subj_id, between=.(grade))
ezPlot(data=subj_error, dv=lp, wid=subj_id, between=.(grade), x=grade)
pander(ezANOVA(data=subj_error, dv=lp, wid=subj_id, between=.(grade)))

# Stats w/out outliers (+3SD errors)
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$lp > x[[6]][1] | subj_error$grade==7 & subj_error$lp > x[[6]][2]]
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=lp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition"
wp_stats<-rbind(wp_stats,apnd)

###############@
### CLP
subj_error <-aggregate(clp~ subj_id + grade + test_id, data=wp, FUN=sum)
x<-ezStats(data=subj_error, dv=clp, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$clp > x[[6]][1] | subj_error$grade==7 & subj_error$clp > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=clp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition_C"
wp_stats<-rbind(wp_stats,apnd)

###############@
### VLP
subj_error <-aggregate(vlp~ subj_id + grade + test_id, data=wp, FUN=sum)
x<-ezStats(data=subj_error, dv=vlp, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$vlp > x[[6]][1] | subj_error$grade==7 & subj_error$vlp > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=vlp, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterPosition_V"
wp_stats<-rbind(wp_stats,apnd)

### LetterDoubling
wp$Doubling<-wp$bsub+wp$bom+wp$bod+wp$bvod+wp$bvsub+wp$bvom 
subj_error <-aggregate(Doubling~ subj_id + grade + test_id, data=wp, FUN=sum)
x<-ezStats(data=subj_error, dv=Doubling, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Doubling > x[[6]][1] | subj_error$grade==7 & subj_error$Doubling > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Doubling, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"LetterDoubling"
wp_stats<-rbind(wp_stats,apnd)


###############@
### Vowel Errors
subj_error <-aggregate(v~ subj_id + grade + test_id, data=wp, FUN=sum)
ggplot(subj_error, aes(grade, v)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=v, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$v > x[[6]][1] | subj_error$grade==7 & subj_error$v > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=v, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"VowelErrors"
wp_stats<-rbind(wp_stats,apnd)

###############
### Neglect
subj_error <-aggregate(neg~ subj_id + grade + test_id, data=wp, FUN=sum)
ggplot(subj_error, aes(grade, neg)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=neg, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$neg > x[[6]][1] | subj_error$grade==7 & subj_error$neg > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=neg, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Neglect"
wp_stats<-rbind(wp_stats,apnd)

##################
### Visual
wp$Visual<-wp$sub+wp$om+wp$od+wp$sim+wp$cl
subj_error <-aggregate(Visual~ subj_id + grade + test_id, data=wp, FUN=sum)
ggplot(subj_error, aes(grade, Visual)) +
  geom_point(position = "jitter")

x<-ezStats(data=subj_error, dv=Visual, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Visual > x[[6]][1] | subj_error$grade==7 & subj_error$Visual > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Visual, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Visual"
wp_stats<-rbind(wp_stats,apnd)


### Surface
wp$Surface<-wp$sur+wp$gr+wp$gem+wp$sbs+wp$sml
subj_error <-aggregate(Surface~ subj_id + grade + test_id, data=wp, FUN=sum)
x<-ezStats(data=subj_error, dv=Surface, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Surface > x[[6]][1] | subj_error$grade==7 & subj_error$Surface > x[[6]][2]]

# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Surface, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Surface"
wp_stats<-rbind(wp_stats,apnd)


### Deep
wp$Deep<-wp$sem+wp$mor+wp$fun
subj_error <-aggregate(Deep~ subj_id + grade + test_id, data=wp, FUN=sum)
x<-ezStats(data=subj_error, dv=Deep, wid=subj_id, between=.(grade))
x$outlier <- (3 * x$SD) + x$Mean
rmv<-subj_error$subj[subj_error$grade==6 & subj_error$Deep > x[[6]][1] | subj_error$grade==7 & subj_error$Deep > x[[6]][2]]
# Stats w/out outliers
apnd <- ezStats(data=subj_error[!subj_error$subj%in%rmv,], dv=Deep, wid=subj_id, between=.(grade))
#stats$Se <- stats$SD/sqrt(stats$N)
apnd$error_type <-"Deep"
wp_stats<-rbind(wp_stats,apnd)


wp_stats$test_id<-"wp"

##########################
# Write .csv of the error ttype norms.
#write.csv(sw_stats, 'data/clean/sw_allErrorNorms.csv', row.names = FALSE )
#write.csv(pw_stats, 'data/clean/pw_allErrorNorms.csv', row.names = FALSE )
#write.csv(wp_stats, 'data/clean/wp_allErrorNorms.csv', row.names = FALSE )
##########################



##########################
# Combine the three different test datasets into one to see what other differences there may be.
x<-rbind(sw, wp)
x<-rbind(x,pw)

total_error <-aggregate(IR~ subj_id + grade + sex + school_id + age_in_months + test_id, data=x, FUN=sum)

pander(ezStats(data = total_error, dv=IR, wid = subj_id, between = .(sex, test_id)))
pander(ezANOVA(data = total_error, dv=IR, wid = subj_id, between = .(sex, test_id)))
# No sex differences or interactions with test

##################
library(tidyr)
a<-x %>% gather(error_code, value, ad:Deep) #Put the data in long format.
a$len<-nchar(gsub("[^a-z]","",a$word)) # And count the number of letters in the word

pander(aggregate(len~grade+test_id, a, mean))
pander(aggregate(len~grade+test_id, a, sd))

ggplot(a, aes(len, IR)) +
  geom_point()

# Frequency
manulexFreq <- read_csv("ManulexFreqPerMillion.csv")
a$FREQ<-manulexFreq$FREQ[match(a$word, manulexFreq$WORD)]
aggregate(FREQ~grade+test_id, a, mean)
sw<-a[a$test_id=="sw",]

unique(a$word[is.na(a$FREQ) & a$test_id!="pw"])
write.csv(a, "freq.csv", row.names = FALSE)
#Missing Freq
#[1] frime OK   marcherionsOK pinceraOK  voilentOK     magner      glisseraiOK   rebondirons
#[8] rogner      clamer      fiableOK      lange

WEF<-aggregate(value~word+FREQ, data=subset(a, test_id!="pw"), sum)
plot(WEF$FREQ,WEF$value)
