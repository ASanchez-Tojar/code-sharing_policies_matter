################################################################################
# Author: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)

################################################################################
# Description of script and Instructions
################################################################################

# This script imports the data extracted regarding code- and data-sharing policy
# for journals in ecology with code-sharing policies. The dataset corresponds to
# an updated version of the dataset used in Culina et al. 2020. This script's 
# main purpose is to clean and prepare the data for further analyses, as well as
# to calculate percentages and raw numbers to be reported.

################################################################################
# Packages needed
################################################################################

pacman::p_load(stringr,openxlsx,dplyr,readxl)

# Clear memory
rm(list=ls())

################################################################################
# Functions needed
################################################################################

# function obtained from: https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
# this function makes the first letter of a word capital to keep everything tidy and consistent
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

################################################################################
# Importing data
################################################################################

Culina.et.al.2020 <- as.data.frame(
  read_excel("../data/raw_databases/Culina_et_al_2020_clean_data_AB_final.xlsx")
)

nrow(Culina.et.al.2020)


################################################################################
# Features for long-term reproducibility for Culina et al. 2020

# subset of nonmolecular articles to use for code calculations, which in for
# this dataset coincides with the entire data, so this is just a double-check
stats.Culina.et.al.2020 <- 
  Culina.et.al.2020[Culina.et.al.2020$statistical.analysis.and.or.simulations.2=="yes",]


################################################################################
# estimates for the articles usable for answering code-related questions

# number of usable articles for code
nrow(stats.Culina.et.al.2020)

# articles per year
table(stats.Culina.et.al.2020$Publication_year.2)


################################################################################
# standardizing the variables
################################################################################

################################################################################
# software_names:
table(stats.Culina.et.al.2020$software_names)
summary(stats.Culina.et.al.2020$software_names)
table(is.na(stats.Culina.et.al.2020$software_names))

# transforming NA's to "Not Stated"
stats.Culina.et.al.2020$software_names <- ifelse(is.na(stats.Culina.et.al.2020$software_names),
                                                 "Not Stated",
                                                 stats.Culina.et.al.2020$software_names)
table(is.na(stats.Culina.et.al.2020$software_names))

table(stats.Culina.et.al.2020$software_names=="na")
stats.Culina.et.al.2020$software_names <- ifelse(stats.Culina.et.al.2020$software_names=="na",
                                                 "Not Stated",
                                                 stats.Culina.et.al.2020$software_names)

table(stats.Culina.et.al.2020$software_names=="Not Stated")

table(stats.Culina.et.al.2020$software_names=="NA")


# a bunch of manual formatting for standardization
stats.Culina.et.al.2020$software_names <- 
  str_replace_all(stats.Culina.et.al.2020$software_names, 
                  "r;genemarker;colony", "r; genemarker;colony")
stats.Culina.et.al.2020$software_names <- 
  str_replace_all(stats.Culina.et.al.2020$software_names, 
                  "r;jmp", "r; jmp")
stats.Culina.et.al.2020$software_names <- 
  str_replace_all(stats.Culina.et.al.2020$software_names, 
                  "r;  arcgis", "r; arcgis")
stats.Culina.et.al.2020$software_names <- 
  str_replace_all(stats.Culina.et.al.2020$software_names, 
                  "jmp; tpsdig;  stacs", "jmp; tpsdig;stacs")
stats.Culina.et.al.2020$software_names <- 
  str_replace_all(stats.Culina.et.al.2020$software_names, 
                  "amos;  spss", "amos; spss")


# making the first letter of a word capital to keep everything tidy and consistent
stats.Culina.et.al.2020$software_names <- sapply(as.character(stats.Culina.et.al.2020$software_names), 
                                                 CapStr)

table(stats.Culina.et.al.2020$software_names)


# adding extra editing mirroring what we did for the no policy dataset. This is
# not really important and it is simply to clean up the data a bit more. It will
# not affect any results and chances are it could still be polished up a bit more
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Spss", "SPSS")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Jmp", "JMP")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Jags", "JAGS")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Pc-ord", "PC-ORD")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Pc Ord", "PC-ORD")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "R; Genemarker;colony", 
                                                          "R; Genemarker; Colony")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Sas", "SAS")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Imagej", "ImageJ")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "JMP; Tpsdig;stacs", 
                                                          "JMP; Tpsdig; Stacs")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Stata", "STATA")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Matlab", "MATLAB")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "STATISTICA", "Statistica")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "MaxEnt", "Maxent")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "PRIMER", "Primer")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "PCORD", "PC-ORD")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "SPSS Statistics", "SPSS")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "IBM SPSS Amos", "SPSS Amos")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "SPSS AMOS", "SPSS Amos")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "IBM SPSS", "SPSS")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Amos; SPSS", "SPSS Amos")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "STATA 12se", "STATA")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          "Genstat For Windows", "Genstat")


# standardizing terminology: first, substituting ',' and ';' by 'and'
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          ";", " and")
stats.Culina.et.al.2020$software_names <- str_replace_all(stats.Culina.et.al.2020$software_names, 
                                                          ",", " and")


table(stats.Culina.et.al.2020$software_names)

# articles not providing the software used
table(stats.Culina.et.al.2020$software_names=="Not Stated")
table(stats.Culina.et.al.2020$software_names=="NA")
round((table(stats.Culina.et.al.2020$software_names=="Not Stated")/nrow(stats.Culina.et.al.2020))*100,1)


################################################################################
# #software
table(stats.Culina.et.al.2020$'#software')
table(is.na(stats.Culina.et.al.2020$'#software'))

# fixing a value with NA instead of 0
stats.Culina.et.al.2020$'#software' <- ifelse(is.na(stats.Culina.et.al.2020$'#software'),
                                              0,
                                              stats.Culina.et.al.2020$'#software')
table(is.na(stats.Culina.et.al.2020$'#software'))


# and converting the 0 associated with Not Stated to NA
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#software'==0 & stats.Culina.et.al.2020$software_names=="Not Stated",
                        c('#software',"software_names")]

stats.Culina.et.al.2020$'#software' <- ifelse(stats.Culina.et.al.2020$'#software'==0
                                              & stats.Culina.et.al.2020$software_names=="Not Stated",
                                              "NA",
                                              stats.Culina.et.al.2020$'#software')
table(stats.Culina.et.al.2020$'#software')
stats.Culina.et.al.2020$'#software' <- as.numeric(stats.Culina.et.al.2020$'#software')
summary(stats.Culina.et.al.2020$'#software')

# fixing two typos in #software
stats.Culina.et.al.2020$'#software' <- ifelse(stats.Culina.et.al.2020$doi %in% 
                                                c("10.1098/rspb.2018.1224",
                                                  "10.1098/rspb.2015.1921"),
                                              2,
                                              stats.Culina.et.al.2020$'#software')
summary(stats.Culina.et.al.2020$'#software')



################################################################################
# exploring the reporting of versions: software
table(stats.Culina.et.al.2020$'#software_with_version')
table(is.na(stats.Culina.et.al.2020$'#software_with_version'))

# converting NA's to characters
stats.Culina.et.al.2020$'#software_with_version' <- ifelse(is.na(stats.Culina.et.al.2020$'#software_with_version'),
                                                           "NA",
                                                           stats.Culina.et.al.2020$'#software_with_version')

stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#software_with_version'=="NA",
                        c("software_names",'#software_with_version','#software')] #one NA should not be there (R and Fortran)


# fixing two typos that were assigned as yes rather than 1 and 0
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1111/1365-2435.13040",c('#software_with_version')] <- 1
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1111/mec.14517",c('#software_with_version')] <- 0


# fixing a few typos from the entry with "R and Fortran"
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1098/rspb.2015.0837",c('#software_with_version')] <- 2
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1098/rspb.2015.0837",c('#packages')] <- 2
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1098/rspb.2015.0837",c('#packages_with_version')] <- 2
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1098/rspb.2015.0837",c('#packages_version_location')] <- "references"

# and for
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1098/rspb.2016.0172",c('#software_with_version')] <- 1
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1098/rspb.2016.0172",c('#packages_with_version')] <- 0
stats.Culina.et.al.2020[stats.Culina.et.al.2020$doi=="10.1098/rspb.2016.0172",c('#software_version_location')] <- "materials&methods"

# all good
table(stats.Culina.et.al.2020$'#software_with_version')
table(is.na(stats.Culina.et.al.2020$'#software_with_version'))

stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#software_with_version'=="NA",
                        c("software_names",'#software_with_version','#software')]

# making the variable numeric
stats.Culina.et.al.2020$'#software_with_version' <- as.numeric(stats.Culina.et.al.2020$'#software_with_version')
summary(stats.Culina.et.al.2020$'#software_with_version')
table(stats.Culina.et.al.2020$'#software_with_version')

# all good
stats.Culina.et.al.2020[(stats.Culina.et.al.2020$'#software_with_version'=="NA" & 
                           stats.Culina.et.al.2020$software_names!="Not Stated") & 
                          stats.Culina.et.al.2020$software_names!="NA",]

# good
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#software_with_version'=="NA",
                        c("software_names")]



################################################################################
# Calculating software.calculated
################################################################################

# counting the number of articles that used 2 or more software, so that we can
# easily calculate the number of softwares used by each article, and to compare
# it with our original counting
stats.Culina.et.al.2020$'#software.calculated' <- str_count(stats.Culina.et.al.2020$software_names, pattern = "and")
table(stats.Culina.et.al.2020$'#software.calculated')
table(is.na(stats.Culina.et.al.2020$'#software.calculated'))
is.numeric(stats.Culina.et.al.2020$'#software.calculated')
summary(stats.Culina.et.al.2020$'#software.calculated')


# for those reporting software, the total number of software use would then be
# stats$'#packages' + 1 (i.e. if an "and" is detected that means 2 software,
# if non is detected but software was stated that means 1 software, etc) 
stats.Culina.et.al.2020$'#software.calculated' <- ifelse(stats.Culina.et.al.2020$software_names!="Not Stated",
                                                         stats.Culina.et.al.2020$'#software.calculated'+1,
                                                         NA)
summary(stats.Culina.et.al.2020$'#software.calculated')
table(stats.Culina.et.al.2020$'#software.calculated')
table(is.na(stats.Culina.et.al.2020$'#software.calculated'))

# some further checks, which are passed: good news
table(stats.Culina.et.al.2020$'#software.calculated' < stats.Culina.et.al.2020$'#software_with_version')
stats.Culina.et.al.2020[!(is.na(stats.Culina.et.al.2020$'#software.calculated')) & 
                          (stats.Culina.et.al.2020$'#software.calculated' < stats.Culina.et.al.2020$'#software_with_version'),
                        c("software_names",'#software.calculated','#software_with_version')]


# found some errors '#software' so will be using '#software.calculated' throughout
table(stats.Culina.et.al.2020$'#software.calculated')
table(stats.Culina.et.al.2020$'#software')


################################################################################
# exploring the reporting of versions: package
table(stats.Culina.et.al.2020$'#packages')
table(stats.Culina.et.al.2020$packages_names)
table(stats.Culina.et.al.2020$'#packages_with_version')


# fixing some small typos and standardizing across package_names, this is only
# to keep it clean, and it is a variable we do not have for the journals without
# policies
table(stats.Culina.et.al.2020$packages_names)


# transforming na's to "NA"
table(is.na(stats.Culina.et.al.2020$packages_names))
table(stats.Culina.et.al.2020$packages_names=="na")
table(stats.Culina.et.al.2020$packages_names=="Na")
stats.Culina.et.al.2020$packages_names <- ifelse(stats.Culina.et.al.2020$packages_names=="na",
                                                 "NA",
                                                 stats.Culina.et.al.2020$packages_names)

table(stats.Culina.et.al.2020$packages_names=="NA")
table(stats.Culina.et.al.2020$packages_names=="na")
table(stats.Culina.et.al.2020$packages_names=="Na")

# a bunch of manual formatting for standardization
stats.Culina.et.al.2020$packages_names <- 
  str_replace_all(stats.Culina.et.al.2020$packages_names, 
                  "adegenet; gstudio ; mmod", "adegenet; gstudio; mmod")
stats.Culina.et.al.2020$packages_names <- 
  str_replace_all(stats.Culina.et.al.2020$packages_names, 
                  "dplr ; vegan", "dplyr; vegan")
stats.Culina.et.al.2020$packages_names <- 
  str_replace_all(stats.Culina.et.al.2020$packages_names, 
                  "lme4; boot;  visreg", "lme4; boot; visreg")
stats.Culina.et.al.2020$packages_names <- 
  str_replace_all(stats.Culina.et.al.2020$packages_names, 
                  "lme4; mumin;multcomp", "lme4; mumin; multcomp")
stats.Culina.et.al.2020$packages_names <- 
  str_replace_all(stats.Culina.et.al.2020$packages_names, 
                  "masterbayes; pedantics ; survival ; glmmadmb; inbreedr", 
                  "masterbayes; pedantics; survival; glmmadmb; inbreedr")
stats.Culina.et.al.2020$packages_names <- 
  str_replace_all(stats.Culina.et.al.2020$packages_names, 
                  "multcomp;  lsmean; vegan", "multcomp; lsmean; vegan")
stats.Culina.et.al.2020$packages_names <- 
  str_replace_all(stats.Culina.et.al.2020$packages_names, 
                  "nlme;  adehabitathr", "nlme; adehabitathr")
stats.Culina.et.al.2020$packages_names <- 
  str_replace_all(stats.Culina.et.al.2020$packages_names, 
                  "mumln", "mumin")

# typo where n should have been na
# stats.Culina.et.al.2020[stats.Culina.et.al.2020$packages_names=="n",]
# stats.Culina.et.al.2020$packages_names <- 
#   str_replace_all(stats.Culina.et.al.2020$packages_names, 
#                   "n", "NA")

stats.Culina.et.al.2020[stats.Culina.et.al.2020$packages_names=="n","packages_names"]<-"NA"

table(stats.Culina.et.al.2020$packages_names)


# making the first letter of a word capital to keep everything tidy and consistent
stats.Culina.et.al.2020$packages_names <- sapply(as.character(stats.Culina.et.al.2020$packages_names), 
                                                 CapStr)

# standardizing terminology: first, substituting ',' and ';' by 'and'
stats.Culina.et.al.2020$packages_names <- str_replace_all(stats.Culina.et.al.2020$packages_names, 
                                                          ";", " and")
stats.Culina.et.al.2020$packages_names <- str_replace_all(stats.Culina.et.al.2020$packages_names, 
                                                          ",", " and")

table(stats.Culina.et.al.2020$packages_names)


################################################################################
# exploring #packages
table(stats.Culina.et.al.2020$'#packages')
table(is.na(stats.Culina.et.al.2020$'#packages'))

# some values for entries with no packages need to be changed to NA
stats.Culina.et.al.2020[stats.Culina.et.al.2020$packages_names=="NA",
                        c("packages_names",'#packages','#packages_with_version')]

stats.Culina.et.al.2020[stats.Culina.et.al.2020$packages_names=="NA",
                        '#packages_with_version'] <- "NA"

stats.Culina.et.al.2020[stats.Culina.et.al.2020$packages_names=="NA",
                        c("packages_names",'#packages','#packages_with_version')]

# all good here
table(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages'==0,'#packages_with_version'])

# also here despite the software Not stated and package = 1, not an issue though
# even though we will assign NA for those with software Not stated and package = 0
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages'==1 & 
                          stats.Culina.et.al.2020$'#packages_with_version'==0,
                        c("packages_names",'#packages','#packages_with_version',
                          '#software',"software_names")]

# assign NA for those with software Not stated and package = 0
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages' == 0 & 
                          stats.Culina.et.al.2020$software_names=="Not Stated",]
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages' == 0 & 
                          stats.Culina.et.al.2020$software_names=="Not Stated",'#packages'] <- "NA"
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages' == 0 & 
                          stats.Culina.et.al.2020$software_names=="Not Stated",]


# making sure that when #packages == 0 or NA, #packages_with_version should be NA
table(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages'==0,'#packages_with_version'])
table(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages'=="NA",'#packages_with_version'])

# counting the number of articles that used 2 or more packages, so that we can
# easily calculate the number of packages used by each article, and to compare
# it with our original counting
stats.Culina.et.al.2020$'#packages.calculated' <- str_count(stats.Culina.et.al.2020$packages_names, 
                                                            pattern = "and")
table(stats.Culina.et.al.2020$'#packages.calculated')
is.numeric(stats.Culina.et.al.2020$'#packages.calculated')
summary(stats.Culina.et.al.2020$'#packages.calculated')

# for those reporting packages 
stats.Culina.et.al.2020$'#packages.calculated' <- ifelse(stats.Culina.et.al.2020$packages_names!="NA",
                                                         stats.Culina.et.al.2020$'#packages.calculated'+1,
                                                         0)

# assign NA for those with software Not stated and package = 0
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated' == 0 & 
                          stats.Culina.et.al.2020$software_names=="Not Stated",
                        c('#packages.calculated',"software_names")]
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated' == 0 & 
                          stats.Culina.et.al.2020$software_names=="Not Stated",
                        '#packages.calculated'] <- "NA"
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated' == 0 & 
                          stats.Culina.et.al.2020$software_names=="Not Stated",]

# found some errors '#packages' so will be using '#packages.calculated' throughout
table(stats.Culina.et.al.2020$'#packages.calculated')
table(stats.Culina.et.al.2020$'#packages')


# further checks and cleaning '#packages_with_version'
table(stats.Culina.et.al.2020$'#packages_with_version')

# # changing Na to NA and no to 0 #no longer needed
# stats.Culina.et.al.2020$'#packages_with_version' <- 
#   ifelse(stats.Culina.et.al.2020$'#packages_with_version'=="Na",
#          "NA",
#          stats.Culina.et.al.2020$'#packages_with_version')
# 
# stats.Culina.et.al.2020$'#packages_with_version' <- 
#   ifelse(stats.Culina.et.al.2020$'#packages_with_version'=="no",
#          0,
#          stats.Culina.et.al.2020$'#packages_with_version')


# some NA associated to entries with packages. Needs fixing. #fixed now
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages_with_version'=="NA",
                        c("doi","packages_names",
                          '#packages.calculated',
                          '#packages',
                          '#packages_with_version')]

table(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages_with_version'=="NA",
                              '#packages.calculated'])
table(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages_with_version'=="NA",
                              '#packages'])


# exploring the data further and finding some 0's that should not be assigned to
# #packages_with_version # fixed now
stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated'==0,
                        c("packages_names",
                          '#packages.calculated',
                          '#packages',
                          '#packages_with_version')]# clean

table(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated'==0,
                              '#packages_with_version'])

# fixing those minor typos # no longer needed but code kept since it does not hurt
stats.Culina.et.al.2020$'#packages_with_version' <- ifelse(stats.Culina.et.al.2020$'#packages.calculated'==0,
                                                           "NA",
                                                           stats.Culina.et.al.2020$'#packages_with_version')


################################################################################
# FreeSoftware

# fixing some entries using the same mirror code from database without policies
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "MATLAB", 
                                               "no", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "NA", 
                                               "NA", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "Not Stated", 
                                               "NA", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "R", 
                                               "yes", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "R and SAS", 
                                               "partially", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "JMP and R", 
                                               "partially", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "SAS and R", 
                                               "partially", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "SPSS and R", 
                                               "partially", stats.Culina.et.al.2020$FreeSoftware)

# exploring some NA's
stats.Culina.et.al.2020[stats.Culina.et.al.2020$FreeSoftware == "NA",
                        c("software_names","FreeSoftware")]

stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "Genemapper", 
                                               "yes", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "Agilent Feature Extraction and Partek Genomics Suite and Expander", 
                                               "no", stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware <- ifelse(stats.Culina.et.al.2020$software_names == "Sequencher and Geneious", 
                                               "no", stats.Culina.et.al.2020$FreeSoftware)


################################################################################
# Here is an attempt at further cleaning FreeSoftware
################################################################################

# to complete the cleaning and standardization of the variable FreeSoftware,
# we are going to use the information from the database without policies, which
# we have polished consistently
stats.no.policies <- read.table("../data/clean_databases/code_availability_without_policies_full_and_clean.csv",
                                header=T,sep=",")

# generating a vector containing free software
stats.no.policies.software <- stats.no.policies[!is.na(stats.no.policies$FreeSoftware),
                                                c("analysis_software_names",
                                                  "FreeSoftware",
                                                  "FreeSoftware.2")]
# generating vectors
stats.no.policies.software.free <- tolower(unique(unlist(strsplit(
  stats.no.policies.software[stats.no.policies.software$FreeSoftware=="yes",
                             "analysis_software_names"]," and "))))

stats.no.policies.software.nonfree <- tolower(unique(unlist(strsplit(
  stats.no.policies.software[stats.no.policies.software$FreeSoftware=="no",
                             "analysis_software_names"]," and "))))


stats.Culina.et.al.2020$FreeSoftware.revised <- "NA"

for (i in 1:nrow(stats.Culina.et.al.2020)){
  
  # full mach free
  num.matches <- length(names(table(
    tolower(unique(unlist(strsplit(stats.Culina.et.al.2020[i,"software_names"],
                                   " and "))))
    %in% stats.no.policies.software.free)))
  
  type.of.match <- names(table(
    tolower(unique(unlist(strsplit(stats.Culina.et.al.2020[i,"software_names"],
                                   " and "))))
    %in% stats.no.policies.software.free))
  
  stats.Culina.et.al.2020$FreeSoftware.revised[i] <- 
    unique(ifelse(num.matches==1 & type.of.match==TRUE,
                  "yes",
                  stats.Culina.et.al.2020$FreeSoftware[i]))
  
  # full mach nonfree
  num.matches.nonfree <- length(names(table(
    tolower(unique(unlist(strsplit(stats.Culina.et.al.2020[i,"software_names"],
                                   " and "))))
    %in% stats.no.policies.software.nonfree)))
  
  type.of.match.nonfree <- names(table(
    tolower(unique(unlist(strsplit(stats.Culina.et.al.2020[i,"software_names"],
                                   " and "))))
    %in% stats.no.policies.software.nonfree))
  
  stats.Culina.et.al.2020$FreeSoftware.revised[i] <- 
    unique(ifelse(num.matches==1 & type.of.match==TRUE,
                  "yes",
                  stats.Culina.et.al.2020$FreeSoftware.revised[i]))
  
}

stats.Culina.et.al.2020[,c("software_names","FreeSoftware.revised","FreeSoftware")]
table(stats.Culina.et.al.2020$FreeSoftware.revised==stats.Culina.et.al.2020$FreeSoftware)

# assuming the function is working well, there were no disagreements between the
# dataset from journals without policies and the dataset from journals with
# policies

# stats.Culina.et.al.2020$software_names.to.lower <- tolower(stats.Culina.et.al.2020$software_names)
# stats.no.policies.software$software_names.to.lower <- tolower(stats.no.policies.software$analysis_software_names)
# 
# stats.Culina.et.al.2020.revised <- merge(stats.Culina.et.al.2020,
#                                          stats.no.policies.software[,c("software_names.to.lower","FreeSoftware")],
#                                          by="software_names.to.lower",
#                                          all=F)
#        
# stats.Culina.et.al.2020.revised[,c("software_names",
#                                    "FreeSoftware.revised",
#                                    "FreeSoftware.y")]
# 
# table(stats.Culina.et.al.2020.revised$FreeSoftware.revised==stats.Culina.et.al.2020.revised$FreeSoftware.y)


# for (i in 1:nrow(stats.Culina.et.al.2020)){
#     num.matches <- length(names(table(
#       tolower(unique(unlist(strsplit(stats.Culina.et.al.2020[i,"software_names"],
#                                    " and "))))
#     %in% stats.no.policies.software.free
#   )))
#     type.of.match <- names(table(
#       tolower(unique(unlist(strsplit(stats.Culina.et.al.2020[i,"software_names"],
#                                      " and "))))
#       %in% stats.no.policies.software.free
#     ))
#     print(num.matches)
#     print(type.of.match)
#     
#     print(unique(ifelse(num.matches==1 & type.of.match==TRUE,
#            "yes",
#            "no")))
#     print(tolower(unique(unlist(strsplit(stats.Culina.et.al.2020[i,"software_names"],
#                                          " and ")))))
#     
# }



# creating new variable for FreeSoftware where partially is counted as yes
# stats.Culina.et.al.2020$FreeSoftware <- as.factor(stats.Culina.et.al.2020$FreeSoftware)
stats.Culina.et.al.2020$FreeSoftware.2 <- ifelse(stats.Culina.et.al.2020$FreeSoftware=="partially",
                                                 "yes",
                                                 stats.Culina.et.al.2020$FreeSoftware)

# stats.Culina.et.al.2020$FreeSoftware.2.revised <- ifelse(stats.Culina.et.al.2020$FreeSoftware.revised=="partially",
#                                                          "yes",
#                                                          stats.Culina.et.al.2020$FreeSoftware.revised)

stats.Culina.et.al.2020[stats.Culina.et.al.2020$FreeSoftware == "NA",
                        c("software_names","FreeSoftware")]

stats.Culina.et.al.2020[order(stats.Culina.et.al.2020$software_names),
                        c("software_names",
                          "FreeSoftware")] 

table(stats.Culina.et.al.2020$FreeSoftware)
table(stats.Culina.et.al.2020$FreeSoftware.2)
sum(table(stats.Culina.et.al.2020$FreeSoftware.2))


# visual exploration: software and freeness consistency
stats.Culina.et.al.2020[order(stats.Culina.et.al.2020$software_names),c("software_names",
                                                                        "FreeSoftware.2")] # all good


################################################################################
# LocationShared.2

# standardizing between datasets
table(stats.Culina.et.al.2020$LocationShared.2)
stats.Culina.et.al.2020$LocationShared.2 <- ifelse(stats.Culina.et.al.2020$LocationShared.2=="supplementary file",
                                                   "supplementary material",
                                                   stats.Culina.et.al.2020$LocationShared.2)

table(stats.Culina.et.al.2020$LocationShared.2)


################################################################################
# Repository

# standardizing between datasets
table(stats.Culina.et.al.2020$Repository)


################################################################################
# recoding and standardizing variables between dataset
################################################################################

stats.Culina.et.al.2020$CodePublished.2 <- 
  str_replace_all(stats.Culina.et.al.2020$CodePublished.2, 
                  "some", "partially")

# renaming by duplicating
stats.Culina.et.al.2020$analysis_software_names <- stats.Culina.et.al.2020$software_names
stats.Culina.et.al.2020$CodeShared <- stats.Culina.et.al.2020$CodePublished.2
stats.Culina.et.al.2020$CodeShared.2 <- stats.Culina.et.al.2020$CodePublished.3
stats.Culina.et.al.2020$DataShared <- stats.Culina.et.al.2020$DataShared.2
stats.Culina.et.al.2020$DataShared.2 <- stats.Culina.et.al.2020$DataShared.3


################################################################################
# reorganizing dataset for further checks, tests and variable additions
################################################################################
stats.Culina.et.al.2020.organized <- 
  stats.Culina.et.al.2020[,c("doi","Journal",
                             
                             "Publication_year",
                             "Publication_year.2", 
                             
                             "statistical.analysis.and.or.simulations.2",
                             "bioinformatic.analysis", 
                             "additional.comment.on.analysis",
                             
                             "analysis_software_names",
                             "#software.calculated",
                             "#software_with_version",
                             
                             "#packages" ,
                             "#packages.calculated",
                             "#packages_with_version",
                             
                             "CodeShared",
                             "CodeShared.2",
                             
                             "LinktoCode", 
                             "LocationShared",
                             "Repository",
                             
                             "FreeSoftware",
                             "FreeSoftware.2",
                             
                             "DataUsed",
                             "DataShared",
                             "DataShared.2",
                             
                             "ExtraComments","comments_AB","comments_on_AB")]


names(stats.Culina.et.al.2020.organized)


################################################################################
################################################################################

################################################################################
# Exploration of journals without policies: quick & dirty numbers for the time
# being
################################################################################

################################################################################
################################################################################


################################################################################
# Features for long-term reproducibility

# articles not providing the software used for Culina et al. 2020
table(stats.Culina.et.al.2020.organized$analysis_software_names=="Not Stated")
round((table(stats.Culina.et.al.2020.organized$analysis_software_names=="Not Stated")/nrow(stats.Culina.et.al.2020.organized))*100,1)


########################################
# exploring the reporting of versions: software

# percentage of articles for which not all software version was provided
round((table(stats.Culina.et.al.2020.organized$'#software.calculated' > 
               stats.Culina.et.al.2020.organized$'#software_with_version')/
         sum(table(stats.Culina.et.al.2020.organized$'#software.calculated' > 
                     stats.Culina.et.al.2020.organized$'#software_with_version')))*100,1)

# raw numbers
table(stats.Culina.et.al.2020.organized$'#software.calculated' > 
        stats.Culina.et.al.2020.organized$'#software_with_version')

summary(stats.Culina.et.al.2020.organized$'#software.calculated')

# calculating the mean percentage of software for which software was provided
summary(as.numeric(stats.Culina.et.al.2020.organized$'#software_with_version')/
          as.numeric(stats.Culina.et.al.2020.organized$'#software.calculated'))


################################################################################
# exploring the reporting of versions: package
table(stats.Culina.et.al.2020.organized$'#packages.calculated')
table(stats.Culina.et.al.2020.organized$'#packages_with_version')
stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$'#packages_with_version'=="NA",] # clean
stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$'#packages.calculated'==0,c('#packages.calculated','#packages_with_version')]# clean
stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$'#packages_with_version'=="NA",
                                  c('#packages.calculated','#packages_with_version')]# clean


# is the variable numeric? NO
is.numeric(stats.Culina.et.al.2020.organized$'#packages.calculated')
stats.Culina.et.al.2020.organized$'#packages.calculated' <- as.numeric(stats.Culina.et.al.2020.organized$'#packages.calculated')


# number of articles using packages
table(stats.Culina.et.al.2020.organized$'#packages.calculated'>0)

# percentage of articles for which not all package version was provided
round((nrow(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$'#packages.calculated'
                                              > 0 & !(is.na(stats.Culina.et.al.2020.organized$'#packages.calculated')) &
                                                (stats.Culina.et.al.2020.organized$'#packages.calculated'
                                                 > stats.Culina.et.al.2020.organized$'#packages_with_version'),])/
         nrow(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$'#packages.calculated'
                                                > 0 & !(is.na(stats.Culina.et.al.2020.organized$'#packages.calculated')),]))*100,1)

# raw number
nrow(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$'#packages.calculated' > 0 &
                                            !(is.na(stats.Culina.et.al.2020.organized$'#packages.calculated')) &
                                            (stats.Culina.et.al.2020.organized$'#packages.calculated'
                                             > stats.Culina.et.al.2020.organized$'#packages_with_version'),])


summary(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$'#packages.calculated'> 0 &
          !(is.na(stats.Culina.et.al.2020.organized$'#packages.calculated')),
        '#packages.calculated'])

# calculating the mean percentage of packages for which software was provided
summary(as.numeric(stats.Culina.et.al.2020.organized$'#packages_with_version')/
          as.numeric(stats.Culina.et.al.2020.organized$'#packages'))

################################################################################
# exploring Freeness
table(stats.Culina.et.al.2020.organized$FreeSoftware)
table(stats.Culina.et.al.2020.organized$FreeSoftware.2)

# raw and percentage including partially
table(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$analysis_software_names!="Not Stated","FreeSoftware"])
round((table(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$analysis_software_names!="Not Stated","FreeSoftware"])/
         nrow(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$analysis_software_names!="Not Stated",]))*100,1)

# raw and percentage only fully free (yes) or non-fully free (at least one
# software is not free)
table(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$analysis_software_names!="Not Stated","FreeSoftware.2"])
round((table(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$analysis_software_names!="Not Stated","FreeSoftware.2"])/
         nrow(stats.Culina.et.al.2020.organized[stats.Culina.et.al.2020.organized$analysis_software_names!="Not Stated",]))*100,1)


unique(stats.Culina.et.al.2020.organized$analysis_software_names)

################################################################################
# exporting full and clean dataset
################################################################################

# renaming by duplicating
stats.Culina.et.al.2020$num.software.calculated <- stats.Culina.et.al.2020$'#software.calculated' 
stats.Culina.et.al.2020$num.software_with_version <- stats.Culina.et.al.2020$'#software_with_version' 
stats.Culina.et.al.2020$num.packages <- stats.Culina.et.al.2020$'#packages' 
stats.Culina.et.al.2020$num.packages.calculated <- stats.Culina.et.al.2020$'#packages.calculated' 
stats.Culina.et.al.2020$num.packages_with_version <- stats.Culina.et.al.2020$'#packages_with_version' 


################################################################################
# reorganizing dataset for further checks, tests and variable additions
################################################################################
stats.Culina.et.al.2020.organized <- 
  stats.Culina.et.al.2020[,c("doi","Journal",
                             
                             "Publication_year",
                             "Publication_year.2", 
                             
                             "statistical.analysis.and.or.simulations.2",
                             "bioinformatic.analysis", 
                             "additional.comment.on.analysis",
                             
                             "analysis_software_names",
                             "num.software.calculated",
                             "num.software_with_version",
                             
                             "num.packages" ,
                             "num.packages.calculated",
                             "num.packages_with_version",
                             
                             "CodeShared",
                             "CodeShared.2",
                             
                             "LinktoCode", 
                             "LocationShared",
                             "Repository",
                             
                             "FreeSoftware",
                             "FreeSoftware.2",
                             
                             "DataUsed",
                             "DataShared",
                             "DataShared.2",
                             
                             "ExtraComments","comments_AB","comments_on_AB")]


names(stats.Culina.et.al.2020.organized)


write.csv(stats.Culina.et.al.2020.organized,
          "../data/clean_databases/code_availability_with_policies_full_and_clean.csv",
          row.names=FALSE)

# ################################################################################
# # exploring the reporting of versions: packages # wrong includes NA's
# 
# # is the variable numeric? Yes
# is.numeric(stats.Culina.et.al.2020$'#packages.calculated')
# 
# # number of articles using packages
# table(stats.Culina.et.al.2020$'#packages.calculated'>0)
# 
# # percentage of articles for which not all package version was provided
# round((nrow(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated' > 0 &
#                                           (stats.Culina.et.al.2020$'#packages.calculated' > 
#                                              stats.Culina.et.al.2020$'#packages_with_version'),
# ])/
#   nrow(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated' > 0,
#   ]))*100,1)
# 
# # raw number
# nrow(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated' > 0,]) - 
#   nrow(stats.Culina.et.al.2020[stats.Culina.et.al.2020$'#packages.calculated' > 0 &
#                                  (stats.Culina.et.al.2020$'#packages.calculated' > 
#                                     stats.Culina.et.al.2020$'#packages_with_version'),
#   ])

