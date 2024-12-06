################################################################################
# Author: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)

################################################################################
# Description of script and Instructions
################################################################################

# This script imports the data extracted regarding code- and data-sharing policy
# for journals in ecology without code-sharing policies. This script's main
# purpose is to clean and prepare the data for further analyses, as well as to
# calculate percentages and raw numbers to be reported.

################################################################################
# Packages needed
################################################################################

pacman::p_load(stringr,readxl,dplyr)

# Clear memory
rm(list=ls())

################################################################################
# Functions needed
################################################################################

# function obtained from: https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
# this function makes the first letter of a word capital to keep everything tidy 
# and consistent
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

################################################################################
# Importing data
################################################################################

# importing data for 2015-2016
db.2015.16 <- read_excel("data/raw_databases/code_availability_without_policies_raw_data_2015-2016.xlsx")

# importing data for 2018-2019
db.2018.19 <- read_excel("data/raw_databases/code_availability_without_policies_raw_data_2018-2019.xlsx")

################################################################################
# Standardizing databases
################################################################################

# are the variable names the same? yes
setdiff(names(db.2015.16),names(db.2018.19))
setdiff(names(db.2018.19),names(db.2015.16))

# putting both databases together
db.full <- rbind(db.2015.16,db.2018.19)

# deleting the text id and the authors names 
#db.full <- db.full[, -c(1, 2)]
db.full <- as.data.frame(dplyr::select(db.full, -fulltextID, -Authors))

# exploring database
summary(db.full)

# transforming some variables to factor
cols.factor <- c("Title", "Journal","statistical.analysis.or/and.simulations",
                 "Publication_year","bioinformatic.analysis", 
                 "additional.comment.on.analysis","analysis_software_names",
                 "#software_with_version", "#packages","#packages_with_version",
                 "CodeShared","LinktoCode","LocationShared","Repository", 
                 "FreeSoftware","DataUsed","DataShared","ExtraComments",
                 "Screener","CommentonComments")

db.full[cols.factor] <- lapply(db.full[cols.factor], factor)

# # exploring the database further
# summary(db.full)
# names(db.full)
# table(db.full$Journal)
# table(db.full$analysis_software_names)
# table(db.full$'#software_with_version')
# table(db.full$'#packages')
# table(db.full$'#packages_with_version')
# table(db.full$CodeShared)
# table(db.full$LocationShared)
# table(db.full$Repository)
# table(db.full$FreeSoftware)
# table(db.full$Screener)

# Convert factor columns to character type
factor_cols <- sapply(db.full, is.factor)
db.full[factor_cols] <- lapply(db.full[factor_cols], as.character)

# Replace NA values with "NA" in the entire data frame
db.full <- replace(db.full, is.na(db.full), "NA")

# fixing a typo in a title
db.full$Title <- str_replace_all(db.full$Title, 
                                 "1 Habitat choice and complex decision making in a trap-building predator", 
                                 "Habitat choice and complex decision making in a trap-building predator") 


################################################################################
# exploring each variable and cleaning and standardizing it if necessary
################################################################################

################################################################################
# Journal
sort(table(db.full$Journal))
sum(table(db.full$Journal))


# visualizing where the NA's are, if any: none, all fixed now
db.full[is.na(db.full$Journal),]


################################################################################
# Publication_year
table(db.full$Publication_year)


# check if there are NA's: none, all fixed now
db.full[is.na(db.full$Publication_year),]


# we are creating a variable with two levels to properly label the two 
# period of times that we sampled (we did not conceived it as 4 years, but 
# rather as 2 time points)
db.full$Publication_year <- as.numeric(as.character(db.full$Publication_year))
db.full$Publication_year.2 <- ifelse(db.full$Publication_year < 2017, 
                                     "2015-2016", "2018-2019")
db.full$Publication_year.2 <- as.factor(db.full$Publication_year.2)
table(db.full$Publication_year.2)


################################################################################
# statistical.analysis.or/and.simulations

# first name needs to be modified to make it easy to handle: get rid of the "/"
db.full <- rename(db.full, "statistical.analysis.and.or.simulations" = 
                    "statistical.analysis.or/and.simulations")

table(db.full$statistical.analysis.and.or.simulations)


################################################################################
# bioinformatic.analysis:
table(db.full$bioinformatic.analysis)


################################################################################
# additional.comment.on.analysis

# adding info for two entries
db.full[db.full$Title == "Past and predicted future effects of housing growth on open space conservation opportunity areas and habitat connectivity around National Wildlife Refuges
","additional.comment.on.analysis"] <- "geospatial analysis"
db.full[db.full$Title == "Quantifying spatiotemporal pattern of urban greenspace: new insights from high resolution data",
        "additional.comment.on.analysis"] <- "geospatial analysis"


################################################################################
# analysis_software_names:
table(db.full$analysis_software_names)


# standardizing terminology: first, substituting ',' and ';' by 'and'
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   ",", " and")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   ";", " and")

# making the first letter of a word capital to keep everything tidy and consistent
db.full$analysis_software_names <- sapply(as.character(db.full$analysis_software_names), 
                                          CapStr)


table(db.full$analysis_software_names)


# a bunch of manual formatting for standardization
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "And", "and")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "Stata", "STATA")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "Matlab", "MATLAB")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "STATISTICA", "Statistica")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "MaxEnt", "Maxent")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "PRIMER", "Primer")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "PCORD", "PC-ORD")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "SPSS Statistics", "SPSS")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "CONFEOR and R and LINKAGE MAPPER Software and CIRCUITSCAPE Software and PINCH POINT MAPPER Software and BARRIER MAPPER and", 
                                                   "CONFEOR and R and LINKAGE MAPPER Software and CIRCUITSCAPE Software and PINCH POINT MAPPER Software and BARRIER MAPPER")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "IBM SPSS Amos", "SPSS Amos")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "SPSS AMOS", "SPSS Amos")
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "IBM SPSS", "SPSS")
db.full$analysis_software_names <- ifelse(db.full$analysis_software_names=="Amos",
                                          "SPSS Amos",
                                          db.full$analysis_software_names)
db.full$analysis_software_names <- str_replace_all(db.full$analysis_software_names, 
                                                   "Genstat For Windows", "Genstat")

# correcting some entries that should have been assigned with "Not Stated" 
# instead of "NA"
table(is.na(db.full$analysis_software_names))
db.full$analysis_software_names <- ifelse(db.full$statistical.analysis.and.or.simulations=="yes" & 
                                            db.full$analysis_software_names=="NA",
                                          "Not Stated",
                                          db.full$analysis_software_names)


#db.full$analysis_software_names <- factor(db.full$analysis_software_names)
table(db.full$analysis_software_names)


# # counting articles using R and other software
# table(str_detect(db.full$analysis_software_names, "R "))
# table(str_detect(db.full$analysis_software_names, " R"))
# counting articles using exclusively R
# nrow(db.full[db.full$analysis_software_names=="R" & 
#                !(is.na(db.full$analysis_software_names)),])



################################################################################
# FreeSoftware
table(db.full$FreeSoftware)

# fixing one entry after we collectively made a decision
db.full$FreeSoftware <- str_replace_all(db.full$FreeSoftware, 
                                        "can't tell", 
                                        "yes") # just one case using MARK

# visual exploration
db.full[order(db.full$analysis_software_names),c("analysis_software_names",
                                                 "FreeSoftware")] 


# fixing some entries
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "MATLAB", "no", 
                               db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "NA", "NA", 
                               db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "Not Stated", 
                               "NA", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "PC-ORD and R", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "Primer and R", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "Primer and PRESENCE and R", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "R", 
                               "yes", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "R and IBM SPSS Amos", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "R and SAS", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "R and Statistica", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "Canoco and SAS", 
                               "no", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "JMP and R", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "SAS and R", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "SPSS and R", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "SPSS and R and Primer and PERMDISP", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "SPSS and SMATR", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "Statistica and SAS and R", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "SYSTAT and Primer and R", 
                               "partially", db.full$FreeSoftware)
db.full$FreeSoftware <- ifelse(db.full$analysis_software_names == "UVAFME", 
                               "yes", db.full$FreeSoftware)


# creating new variable where partially is counted as yes
db.full$FreeSoftware.2 <- recode(db.full$FreeSoftware,
                                 "partially" = "yes",
                                 .default = levels(db.full$FreeSoftware))

table(db.full$FreeSoftware)
table(db.full$FreeSoftware.2)
sum(table(db.full$FreeSoftware.2))


# visual exploration: software and freeness consistency
db.full[order(db.full$analysis_software_names),c("analysis_software_names",
                                                 "FreeSoftware.2")] # all good


################################################################################
# #software_with_version
table(db.full$'#software_with_version')


# visual exploration
db.full[db.full$'#software_with_version'=="NA",]


# fixing some entries that were assigned with "NA" instead of 0
table(db.full[db.full$'#software_with_version'=="NA","analysis_software_names"])
db.full[(db.full$'#software_with_version'=="NA" & 
           db.full$analysis_software_names!="Not Stated") & 
          db.full$analysis_software_names!="NA",]


db.full$'#software_with_version' <- ifelse((db.full$'#software_with_version'=="NA" & 
                                              db.full$analysis_software_names!="Not Stated") & 
                                             db.full$analysis_software_names!="NA",
                                           0,
                                           db.full$'#software_with_version')

# all fixed now
db.full[(db.full$'#software_with_version'=="NA" & 
           db.full$analysis_software_names!="Not Stated") & 
          db.full$analysis_software_names!="NA",]


################################################################################
# #packages
table(db.full$'#packages')
table(is.na(db.full$'#packages'))

# some entries for #packages are 0 despite analysis_software_names=="Not Stated"
db.full[db.full$'#packages' == 0 & 
          db.full$analysis_software_names=="Not Stated",]

# fixing those now after extra checks and deciding to use NA for #packages
# whenever the authors do not report the software used as it could be that 
# no package is applicable to the software they used, which we do not know
db.full$'#packages' <- ifelse(db.full$'#packages' == 0 & 
                                db.full$analysis_software_names=="Not Stated",
                              "NA",
                              db.full$'#packages')

# all fixed now
db.full[db.full$'#packages' == 0 & 
          db.full$analysis_software_names=="Not Stated",]


# in addition, there are 0's in #packages_with_version assigned to entries not 
# using any package 
db.full[db.full$'#packages'==0,] # there are 0's in #packages_with_version that should not be there
db.full[db.full$'#packages'==0,'#packages_with_version']
db.full[db.full$'#packages'=="NA",'#packages_with_version']
table(is.na(db.full$'#packages_with_version'))


# changing those to NA's
db.full$'#packages_with_version' <- ifelse(db.full$'#packages'==0,
                                           "NA",
                                           db.full$'#packages_with_version')

# all good now
table(db.full[db.full$'#packages'==0,'#packages_with_version'])

is.numeric(db.full$'#packages')

################################################################################
# #packages_with_version
table(db.full$'#packages_with_version')
is.numeric(db.full$'#packages_with_version')


################################################################################
# DataUsed
table(db.full$DataUsed)
table(db.full[db.full$statistical.analysis.and.or.simulations=="yes","DataUsed"])


################################################################################
# DataShared
table(db.full$DataShared)


# fixing a mistake
db.full[db.full$Title == "How phylogeny shapes the taxonomic and functional structure of plant-insect networks",
        "DataUsed"] <- "yes"

table(db.full$DataUsed)
table(db.full$DataShared)

# there are entries assigned with values other than NA despite no data being used 
# Fixing those typos here.
db.full[,c("DataUsed","DataShared")]
db.full[db.full$DataUsed=="no",c("DataShared")]
db.full[db.full$DataUsed=="NA",c("DataShared")]

db.full$DataShared <- ifelse(db.full$DataUsed=="no",
                             "NA",
                             db.full$DataShared)

db.full$DataShared <- ifelse(db.full$DataUsed=="NA",
                             "NA",
                             db.full$DataShared)

db.full[,c("DataUsed","DataShared")]


# creating new variable where partially is counted as simply yes
db.full$DataShared.2 <- recode(db.full$DataShared,
                               "partially" = "yes",
                               .default = levels(db.full$FreeSoftware))
table(db.full$DataShared)
table(db.full$DataShared.2)


################################################################################
# CodeShared
table(db.full$CodeShared)

# The following 5 typos appeared due to our reassignment of stats/simulation
# from yes to no
db.full[db.full$CodeShared=="no" & db.full$statistical.analysis.and.or.simulations=="no", ]
db.full[db.full$statistical.analysis.and.or.simulations=="no", ]

# whenever statistical.analysis.and.or.simulations=="no", all variables regarding
# data and code should be NA for the sake of consistency and clean data
db.full[db.full$statistical.analysis.and.or.simulations=="no", 
        c("CodeShared","analysis_software_names",'#software_with_version',
          '#packages','#packages_with_version',"FreeSoftware",
          "DataUsed","DataShared","FreeSoftware.2","DataShared.2",
          "LinktoCode","LocationShared","Repository")] 

db.full[db.full$statistical.analysis.and.or.simulations=="no", 
        c("CodeShared","analysis_software_names",'#software_with_version',
          '#packages','#packages_with_version',"FreeSoftware",
          "DataUsed","DataShared","FreeSoftware.2","DataShared.2",
          "LinktoCode","LocationShared","Repository")] <- "NA"

db.full[db.full$statistical.analysis.and.or.simulations=="no", 
        c("CodeShared","analysis_software_names",'#software_with_version',
          '#packages','#packages_with_version',"FreeSoftware",
          "DataUsed","DataShared","FreeSoftware.2","DataShared.2",
          "LinktoCode","LocationShared","Repository")] 

# all good now
db.full[db.full$CodeShared=="no" & db.full$statistical.analysis.and.or.simulations=="no", ]


# creating new variable where partially is counted as simply yes
db.full$CodeShared.2 <- recode(db.full$CodeShared,
                               "partially" = "yes",
                               .default = levels(db.full$CodeShared))

# db.full$CodeShared.2 <- factor(db.full$CodeShared.2)
table(db.full$CodeShared.2)
table(db.full$CodeShared)



################################################################################
# LinktoCode
table(db.full$LinktoCode)

# fixing a typo where LinktoCode was assigned to "no" instead of "NA"
db.full[db.full$LinktoCode=="no",]
db.full$LinktoCode <- ifelse(db.full$LinktoCode=="no",
                             "NA",
                             db.full$LinktoCode)

# the following code is
# no longer needed, but keeping it in case it needs to be double-checked again
# due to different versioning between observers
# # there is one entry with a true NA and the rest NA's are character "NA"
# table(is.na(db.full$LinktoCode))
# 
# # fixing this for the sake of clean data.
# db.full$LinktoCode <- ifelse(is.na(db.full$LinktoCode),
#                              "NA",
#                              db.full$LinktoCode)
#table(is.na(db.full$LinktoCode))


################################################################################
# LocationShared
table(db.full$LocationShared)


# standardizing names
db.full$LocationShared <- str_replace_all(db.full$LocationShared, 
                                          "supplementary file", 
                                          "supplementary material")
db.full$LocationShared <- str_replace_all(db.full$LocationShared, 
                                          "Supplementary material", 
                                          "supplementary material")
db.full$LocationShared <- str_replace_all(db.full$LocationShared, 
                                          "supplementary material\n Appendix 3 for R script", 
                                          "supplementary material")

table(db.full$LocationShared)

# table(db.full[db.full$Publication_year.2=="2015-2016","LocationShared"])
# table(db.full[db.full$Publication_year.2=="2018-2019","LocationShared"])


################################################################################
# Repository
table(db.full$Repository)

# there is an entry that needs changing "no" to NA. Typo fixing.
db.full$Repository <- ifelse(db.full$Repository=="no",
                             "NA",
                             db.full$Repository)

table(db.full$Repository)

# column_names <- colnames(db.full)


################################################################################
# reorganizing dataset for further checks, tests and variable additions
################################################################################
db.full.organized <- db.full[,c("Title","Journal",
                                
                                "Publication_year",
                                "Publication_year.2", 
                                
                                "statistical.analysis.and.or.simulations"
                                ,"bioinformatic.analysis", 
                                "additional.comment.on.analysis",
                                
                                "analysis_software_names",
                                "#software_with_version",
                                
                                "#packages" ,
                                "#packages_with_version",
                                
                                "CodeShared" ,
                                "CodeShared.2",
                                
                                "LinktoCode", 
                                "LocationShared",
                                "Repository",
                                
                                "FreeSoftware",
                                "FreeSoftware.2",
                                
                                "DataUsed",
                                "DataShared",
                                "DataShared.2",
                                
                                "ExtraComments","CommentonComments","Screener")]


################################################################################
# Calculating further variables
################################################################################

################################################################################
# #software.calculated
################################################################################

# counting the number of articles that used 2 or more software, so that we can
# easily calculate the number of softwares used by each article

db.full.organized$'#software.calculated' <- str_count(db.full.organized$analysis_software_names, 
                                                      pattern = "and")

table(db.full.organized$'#software.calculated')
table(is.na(db.full.organized$'#software.calculated'))

# NA for the subset not doing stats and/or simulations
db.full.organized$'#software.calculated' <- ifelse(db.full.organized$statistical.analysis.and.or.simulations=="no",
                                                   NA,
                                                   db.full.organized$'#software.calculated')

table(db.full.organized$'#software.calculated')
table(is.na(db.full.organized$'#software.calculated'))

# for those reporting software, the total number of software use would then be
# db.full.organized$'#packages' + 1 (i.e. if an "and" is detected that means 2 
# software, if non is detected but software was stated that means 1 software, 
# etc) 
db.full.organized$'#software.calculated' <- ifelse(db.full.organized$analysis_software_names!="Not Stated",
                                                   db.full.organized$'#software.calculated'+1,
                                                   NA)

table(db.full.organized$'#software.calculated')
summary(db.full.organized$'#software.calculated')
table(is.na(db.full.organized$'#software.calculated'))

# some further checks, which are passed: good news
table(db.full.organized$'#software.calculated' < db.full.organized$'#software_with_version')
db.full.organized[!(is.na(db.full.organized$'#software.calculated')) & 
                    (db.full.organized$'#software.calculated' < db.full.organized$'#software_with_version'),
                  c("analysis_software_names",'#software.calculated','#software_with_version')]


################################################################################
################################################################################

################################################################################
# Exploration of journals without policies: quick & dirty numbers for the time
# being
################################################################################

################################################################################
################################################################################

# First, some quick number checking
table(db.full.organized[db.full.organized$Publication_year.2=="2015-2016","CodeShared.2"])
table(db.full.organized[db.full.organized$Publication_year.2=="2018-2019","CodeShared.2"])

# number of eligible articles
nrow(db.full.organized[db.full.organized$statistical.analysis.and.or.simulations=="yes",])


# performing some countings for the results

# number of journals covered each year
db.full.eligible <- as.data.frame(
  db.full.organized %>% 
    filter(statistical.analysis.and.or.simulations=="yes") %>% 
    group_by(Publication_year.2,Journal) %>% 
    summarise(count = n_distinct(CodeShared.2)) %>% 
    summarise(n = n())
)
db.full.eligible

# counting number of eligible articles per journal for code
articles.per.journal <- as.data.frame(db.full.organized %>% 
                                        filter(statistical.analysis.and.or.simulations=="yes") %>%
                                        group_by(Journal) %>% 
                                        summarise(total = n()))
articles.per.journal

# counting number of articles with code per journal
code.shared.per.journal <- as.data.frame(db.full.organized %>% 
                                           filter(statistical.analysis.and.or.simulations=="yes" & 
                                                    CodeShared.2=="yes") %>%
                                           group_by(Journal) %>% 
                                           summarise(with.code = n()))

code.shared.per.journal

sum(code.shared.per.journal$with.code)
round(sum(code.shared.per.journal$with.code)/sum(articles.per.journal$total)*100,1)


# counting number of eligible articles per journal for data
articles.per.journal.data <- as.data.frame(db.full.organized %>% 
                                             filter(statistical.analysis.and.or.simulations=="yes" &
                                                      (DataUsed=="yes")) %>%
                                             group_by(Journal) %>% 
                                             summarise(total.data = n()))
articles.per.journal.data

# counting number of articles with data per journal
data.shared.per.journal <- as.data.frame(db.full.organized %>% 
                                           filter(statistical.analysis.and.or.simulations=="yes" &
                                                    (DataUsed=="yes") &
                                                    DataShared.2=="yes")
                                         %>% group_by(Journal) %>% 
                                           summarise(with.data = n()))
data.shared.per.journal

# merging datasets (to obtain all the information for the table in the ms)
articles.per.journal.code <- merge(articles.per.journal,
                                   code.shared.per.journal,
                                   all.x=T,
                                   by="Journal")

articles.per.journal.code.data <- merge(articles.per.journal.code,
                                        articles.per.journal.data,
                                        all.x=T,
                                        by="Journal")

articles.per.journal.code.data <- merge(articles.per.journal.code.data,
                                        data.shared.per.journal,
                                        all.x=T,
                                        by="Journal")
articles.per.journal.code.data


articles.per.journal.code.data$with.code <- ifelse(is.na(articles.per.journal.code.data$with.code),
                                                   0,
                                                   articles.per.journal.code.data$with.code)


# adding the % to the table
articles.per.journal.code.data$percentage.with.code <- round((articles.per.journal.code.data$with.code/
                                                                articles.per.journal.code.data$total)
                                                             *100,1)
articles.per.journal.code.data$percentage.with.data <- round((articles.per.journal.code.data$with.data/
                                                                articles.per.journal.code.data$total.data)
                                                             *100,1)

# as.data.frame(cbind(as.character(articles.per.journal$Journal),
#                     as.integer(round((code.shared.per.journal$total/articles.per.journal$n)*100,0))))

articles.per.journal.code.data
summary(articles.per.journal.code.data)


################################################################################
# Then, calculating percentages in a more detailed way. 
################################################################################

########################################
# databases

# subset of molecular articles to be excluded
nostats <- db.full.organized[db.full.organized$statistical.analysis.and.or.simulations=="no",]
nrow(nostats)

# subset of nonmolecular articles to use for code calculations
stats <- db.full.organized[db.full.organized$statistical.analysis.and.or.simulations=="yes",]


########################################
# estimates for the articles usable for answering code-related questions

# number of usable articles for code
nrow(stats)

# articles per year
table(stats$Publication_year.2)

# and only yes and no
table(stats$CodeShared.2)

# percentages
round((table(stats$CodeShared.2)/nrow(stats))*100,1)

# code shared including partially
table(stats$CodeShared)

# percentages
round((table(stats$CodeShared)/nrow(stats))*100,1)


# estimating year changes in overall code shared
code.shared.per.period <- as.data.frame(stats %>%
                                          group_by(Publication_year.2,CodeShared.2) %>% 
                                          summarise(with.code = n()))

code.per.period <- as.data.frame(stats %>%
                                   group_by(Publication_year.2) %>% 
                                   summarise(total.per.year = n()))

code.shared.per.period.perc <- merge(code.shared.per.period,
                                     code.per.period,
                                     all.x=T,
                                     by="Publication_year.2")

code.shared.per.period.perc$percentage.code.shared <- round((code.shared.per.period.perc$with.code/code.shared.per.period.perc$total.per.year)*100,1)
#7/2.5

code.shared.per.period.perc


########################################
# estimates for the articles usable for answering data-related questions
# i.e. those using data

# subset of nonmolecular articles to use for data calculations
stats.data <- db.full.organized[db.full.organized$statistical.analysis.and.or.simulations=="yes" &
                                  (db.full.organized$DataUsed=="yes"),]


# and only yes and no
table(stats.data$DataShared.2)

# percentages
round((table(stats.data$DataShared.2)/nrow(stats.data))*100,1)

# data shared including partially
table(stats.data$DataShared)

# percentages
round((table(stats.data$DataShared)/nrow(stats.data))*100,1)


# estimating year changes in overall data shared
data.shared.per.period <- as.data.frame(stats.data %>%
                                          group_by(Publication_year.2,DataShared.2) %>% 
                                          summarise(with.data = n()))

data.per.period <- as.data.frame(stats.data %>%
                                   group_by(Publication_year.2) %>% 
                                   summarise(total.per.year = n()))

data.shared.per.period.perc <- merge(data.shared.per.period,
                                     data.per.period,
                                     all.x=T,
                                     by="Publication_year.2")


data.shared.per.period.perc$percentage.data.shared <- round((data.shared.per.period.perc$with.data/
                                                               data.shared.per.period.perc$total.per.year)
                                                            *100,1)
#43.3/31.0
data.shared.per.period.perc


########################################
# the computational reproducibility potential

# articles with both full code and full data
nrow(stats.data[stats.data$CodeShared=="yes" & stats.data$DataShared=="yes",])
round((nrow(stats.data[stats.data$CodeShared=="yes" & stats.data$DataShared=="yes",
                       ])/nrow(stats))*100,1) #using stats instead of stats.data to integrate those studies that did not use data but would be reproducible as long as the provided the code


################################################################################
# Features for long-term reproducibility

# articles not providing the software used
table(stats$analysis_software_names=="Not Stated")
round((table(stats$analysis_software_names=="Not Stated")/nrow(stats))*100,1)


########################################
# exploring the reporting of versions: software
table(stats$'#software_with_version')
stats[stats$'#software_with_version'=="NA",] # originally some NA's should not be there, but that has now been fixed by reviewing those entries above


# cbind(stats$'#software.calculated',stats$'#software_with_version',
# stats$'#software.calculated' > stats$'#software_with_version')

# percentage of articles for which not all software.calculated version was provided
round((table(stats$'#software.calculated' > stats$'#software_with_version')/
         sum(table(stats$'#software.calculated' > stats$'#software_with_version')))
      *100,1)

# raw numbers
table(stats$'#software.calculated' > stats$'#software_with_version')
table(as.numeric(stats$'#software.calculated') > as.numeric(stats$'#software_with_version'))
summary(as.numeric(stats$'#software.calculated'))

# calculating the mean percentage of software for which software was provided
summary(as.numeric(stats$'#software_with_version')/as.numeric(stats$'#software.calculated'))
# cbind(stats$'#software.calculated',stats$'#software_with_version',
#       as.numeric(stats$'#software_with_version')/as.numeric(stats$'#software.calculated'))

########################################
# exploring the reporting of versions: package
table(stats$'#packages')
table(stats$'#packages_with_version')
stats[stats$'#packages'=="NA",] # clean
stats[stats$'#packages_with_version'=="NA",] # clean
stats[stats$'#packages'==0,c('#packages','#packages_with_version')]# clean
stats[stats$'#packages_with_version'=="NA",
      c('#packages','#packages_with_version')]# clean


########################################
# exploring the reporting of versions: packages

# is the variable numeric? NO, make it.
is.numeric(stats$'#packages')
stats$'#packages' <- as.numeric(stats$'#packages')

# number of articles using packages
table(stats$'#packages'>0)
table(is.na(stats$'#packages'))
# table(stats$'#packages'==0)
# nrow(stats[stats$'#packages'>0 & !(is.na(stats$'#packages')),])


# cbind(stats$'#packages',stats$'#packages_with_version',
#       stats$'#packages' > stats$'#packages_with_version')


# percentage of articles for which not all package version was provided
round((nrow(stats[stats$'#packages' > 0 & !(is.na(stats$'#packages')) &
                    (stats$'#packages' > stats$'#packages_with_version'),])/
         nrow(stats[stats$'#packages' > 0 & !(is.na(stats$'#packages')),]))*100,1)

# raw number
nrow(stats[stats$'#packages' > 0 &
             !(is.na(stats$'#packages')) &
             (stats$'#packages' > stats$'#packages_with_version'),])

summary(stats[stats$'#packages' > 0 & !(is.na(stats$'#packages')),'#packages'])

# calculating the mean percentage of packages for which software was provided
summary(as.numeric(stats$'#packages_with_version')/as.numeric(stats$'#packages'))
# cbind(stats$'#packages',stats$'#packages_with_version',
#       as.numeric(stats$'#packages_with_version')/as.numeric(stats$'#packages'))


########################################
# exploring software used, free vs. non-free
table(stats$FreeSoftware)
table(stats$FreeSoftware.2)

#stats[stats$analysis_software_names=="Not Stated",] # clean
stats[stats$analysis_software_names!="Not Stated",c("analysis_software_names",
                                                    "FreeSoftware",
                                                    "FreeSoftware.2")]
# stats[stats$analysis_software_names=="NA",c("statistical.analysis.and.or.simulations",
#                                             "analysis_software_names",
#                                             "FreeSoftware",
#                                             "FreeSoftware.2")]

# raw and percentage including partially
table(stats[stats$analysis_software_names!="Not Stated","FreeSoftware"])
round((table(stats[stats$analysis_software_names!="Not Stated","FreeSoftware"])/
         nrow(stats[stats$analysis_software_names!="Not Stated",]))
      *100,1)


# raw and percentage only fully free (yes) or non-fully free (at least one
# software is not free)
table(stats[stats$analysis_software_names!="Not Stated","FreeSoftware.2"])
round((table(stats[stats$analysis_software_names!="Not Stated","FreeSoftware.2"])/
         nrow(stats[stats$analysis_software_names!="Not Stated",]))
      *100,1)


# # checks that show a few things that might needed cleaning, but are now fixed
# stats.data[stats.data$FreeSoftware.2=="NA",]
# stats.data[stats.data$analysis_software_names=="NA",]
# db.full.organized[db.full.organized$FreeSoftware.2=="NA",]
# db.full.organized[db.full.organized$analysis_software_names=="NA",]



########################################
# exploring where code-sharing information was shared

# exploring software used, free vs. non-free
table(stats$LocationShared)


# raw and percentage numbers
table(stats[stats$LocationShared!="NA","LocationShared"])
round((table(stats[stats$LocationShared!="NA","LocationShared"])/
         nrow(stats[stats$LocationShared!="NA",]))
      *100,1)
table(stats[stats$LocationShared!="NA","Repository"])
#stats[stats$LocationShared!="NA",c("LocationShared","Repository")] # remove "no"
#stats[stats$Repository=="no",]


################################################################################
# exporting full and clean dataset
################################################################################

# duplicating variables to change their name
db.full.organized$num.software.calculated <- db.full.organized$'#software.calculated' 
db.full.organized$num.software_with_version <- db.full.organized$'#software_with_version' 
db.full.organized$num.packages <- db.full.organized$'#packages' 
db.full.organized$num.packages_with_version <- db.full.organized$'#packages_with_version' 

################################################################################
# reorganizing and selecting dataset for saving
################################################################################
db.full.organized <- db.full.organized[db.full.organized$statistical.analysis.and.or.simulations=="yes",
                                       
                                       c("Title","Journal",
                                         
                                         "Publication_year",
                                         "Publication_year.2", 
                                         
                                         "statistical.analysis.and.or.simulations",
                                         "bioinformatic.analysis", 
                                         "additional.comment.on.analysis",
                                         
                                         "analysis_software_names",
                                         "num.software.calculated",
                                         "num.software_with_version",
                                         
                                         "num.packages" ,
                                         "num.packages_with_version",
                                         
                                         "CodeShared" ,
                                         "CodeShared.2",
                                         
                                         "LinktoCode", 
                                         "LocationShared",
                                         "Repository",
                                         
                                         "FreeSoftware",
                                         "FreeSoftware.2",
                                         
                                         "DataUsed",
                                         "DataShared",
                                         "DataShared.2",
                                         
                                         "ExtraComments","CommentonComments","Screener")]


write.csv(db.full.organized,
          "data/clean_databases/code_availability_without_policies_full_and_clean.csv",
          row.names=FALSE)
