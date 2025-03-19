################################################################################
# Author: 
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com)

################################################################################
# Description of script and Instructions
################################################################################

# This script imports the data prepared in previous scripts containing the
# data from journals with and without a code-sharing policy to generate the 
# figures used in the manuscript

################################################################################
# Packages needed
################################################################################

# if not already installed, run:
#install.packages("pacman")
pacman::p_load(stringr,readxl,dplyr,ggplot2,patchwork,ggpattern)

# Clear memory
rm(list=ls())


################################################################################
# Importing data
################################################################################

################################################################################
# importing all clean data for journals without a code-sharing policy
db.full <- read.table("data/clean_databases/code_availability_without_policies_full_and_clean.csv",
                      header=T,sep=",")


# standardizing NA's
table(is.na(db.full$CodeShared))
db.full$CodeShared <- ifelse(is.na(db.full$CodeShared), 
                             "NA", 
                             as.character(db.full$CodeShared))

table(is.na(db.full$DataShared))
db.full$DataShared <- ifelse(is.na(db.full$DataShared), 
                             "NA", 
                             as.character(db.full$DataShared))


table(db.full$CodeShared)
table(db.full$DataShared)

table(db.full$statistical.analysis.and.or.simulations)
table(db.full$LocationShared)
table(db.full$Publication_year.2)


# perecentages
round(table(db.full$CodeShared)/nrow(db.full)*100,1)
db.full.no.data.NA <- db.full[db.full$DataShared!="NA",]
round(table(db.full.no.data.NA$DataShared)/nrow(db.full.no.data.NA)*100,1)


################################################################################
# importing all clean data for journals with a code-sharing policy
db.full.Culina <- read.table("data/clean_databases/code_availability_with_policies_full_and_clean.csv",
                             header=T,sep=",")

# standardizing NA's
table(is.na(db.full.Culina$CodeShared))
db.full.Culina$CodeShared <- ifelse(is.na(db.full.Culina$CodeShared), 
                                    "NA", 
                                    as.character(db.full.Culina$CodeShared))

table(is.na(db.full.Culina$DataShared))
db.full.Culina$DataShared <- ifelse(is.na(db.full.Culina$DataShared), 
                                    "NA", 
                                    as.character(db.full.Culina$DataShared))


table(db.full.Culina$CodeShared)
table(db.full.Culina$DataShared)

table(db.full.Culina$statistical.analysis.and.or.simulations)
table(db.full.Culina$LocationShared)


# perecentages
round(table(db.full.Culina$CodeShared)/nrow(db.full.Culina)*100,1)
db.full.Culina.no.data.NA <- db.full.Culina[db.full.Culina$DataShared!="NA",]
round(table(db.full.Culina.no.data.NA$DataShared)/nrow(db.full.Culina.no.data.NA)*100,1)


# quick comparison

# how much times lower for journals without a code-sharing policy
round(((table(db.full.Culina$CodeShared.2)/nrow(db.full.Culina)*100)/
         (table(db.full$CodeShared.2)/nrow(db.full)*100)),1)

# how much times lower
round(((table(db.full.Culina.no.data.NA$DataShared.2)/nrow(db.full.Culina.no.data.NA)*100)/
         (table(db.full.no.data.NA$DataShared.2)/nrow(db.full.no.data.NA)*100)),1)


################################################################################
# Figure 1
################################################################################

################################################################################
# Code availability
################################################################################

# calculating number of studies that shared  at least some code
partially.shared <- db.full %>% 
  group_by(Publication_year.2) %>% 
  summarise(CodeShared = sum(CodeShared == "partially"))

# calculating number of studies that shared seemingly all code
all.shared <- db.full %>% 
  group_by(Publication_year.2) %>% 
  summarise(CodeShared = sum(CodeShared == "yes"))


# calculating number of studies that did not share any code
none.shared <- db.full %>% 
  group_by(Publication_year.2) %>% 
  summarise(CodeShared = sum(CodeShared == "no"))


# calculating number of eligible articles (after title-and-abstract and fulltext screening)
total <- db.full %>% 
  group_by(Publication_year.2) %>% 
  summarise(n = n())


# making them all data frames
partially.shared <- as.data.frame(partially.shared)
all.shared <- as.data.frame(all.shared)
none.shared <- as.data.frame(none.shared)
total <- as.data.frame(total)


# adding total sample size
full.summary.1 <- merge(partially.shared,total)
full.summary.2 <- merge(all.shared,total)
full.summary.3 <- merge(none.shared,total)


# creating new variable to identify each value
full.summary.1$type <- "Partially"
full.summary.2$type <- "All"
full.summary.3$type <- "None"

# stacking them all: for real
full.summary <- rbind(full.summary.1,full.summary.2,full.summary.3)

# estimating percentages
full.summary$percentage <- (full.summary$CodeShared/full.summary$n)*100
full.summary


#fill <- c("grey98", "grey35", "grey5")
fill <- c("darkslategray2", "darkslategray4", "darkslategray")

# Stacked barplot with multiple groups
figure1a <- full.summary %>% 
  mutate(type = factor(type, levels = c("None", "Partially", "All"))) %>% 
  ggplot() + 
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  labs(y = "Percentage (%) of articles", fill = "Published code") +
  scale_fill_manual(values = fill) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank()
  )

figure1a


################################################################################
# Data availability
################################################################################

# calculating number of studies that published at least some data
partially.data.published <- db.full %>% 
  group_by(Publication_year.2) %>% 
  summarise(datapublished = sum(DataShared == "partially"))


# calculating number of studies that published seemingly all data
all.data.published <- db.full %>% 
  group_by(Publication_year.2) %>% 
  summarise(datapublished = sum(DataShared == "yes"))


# calculating number of studies that did not publish any data
none.data.published <- db.full %>% 
  group_by(Publication_year.2) %>% 
  summarise(datapublished = sum(DataShared == "no"))


# calculating number of eligible articles (after title-and-abstract and fulltext screening)
total.data <- db.full %>%
  filter(DataShared!="NA") %>% 
  group_by(Publication_year.2) %>%
  summarise(n = n())


# making them all data frames
partially.data.published <- as.data.frame(partially.data.published)
all.data.published <- as.data.frame(all.data.published)
none.data.published <- as.data.frame(none.data.published)
total.data <- as.data.frame(total.data)


# adding total sample size
full.data.summary.1 <- merge(partially.data.published,total.data)
full.data.summary.2 <- merge(all.data.published,total.data)
full.data.summary.3 <- merge(none.data.published,total.data)


# creating new variable to identify each value
full.data.summary.1$type <- "Partially"
full.data.summary.2$type <- "All"
full.data.summary.3$type <- "None"

# stacking them all: for real
full.data.summary <- rbind(full.data.summary.1,full.data.summary.2,full.data.summary.3)

# estimating percentages
full.data.summary$percentage <- (full.data.summary$datapublished/full.data.summary$n)*100
full.data.summary

#write.xlsx(full.data.summary, "results/full.data.summary.xlsx")

# choosing colours manually
#fill <- c("grey98", "grey35","grey5")
fill <- c("darkslategray2", "darkslategray4", "darkslategray")

# Stacked barplot with multiple groups
figure1b <- full.data.summary %>% 
  mutate(type = factor(type, levels = c("None",
                                        "Partially",
                                        "All"))) %>% 
  ggplot() + 
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat="identity",colour="black") +
  labs(y="Percentage (%) of articles", fill="Published data ") +
  scale_fill_manual(values=fill) +
  scale_y_continuous(breaks = seq(0,100,20),expand = expand_scale(mult = c(0, 0.05))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_blank())
figure1b


################################################################################
# Exporting Figure 1
################################################################################

tiff("figures/Figure1.tiff",
     height = 12, width = 28,
     units = 'cm', compression = "lzw", res = 600)

figure1 <- figure1a + 
  labs(title = c("a")) +
  figure1b +
  labs(title = c("b")) +
  plot_layout(ncol = 2, nrow = 1)

print(figure1)

dev.off()


################################################################################
# Figure 3
################################################################################

################################################################################
# Analytic Software
################################################################################

################################################################################
# Journals without a code-sharing policy

# categorizing studies on whether they stated or not the software used
table(db.full$analysis_software_names=="Not Stated")
db.full$software_stated <- ifelse(db.full$analysis_software_names=="Not Stated",
                                  "no",
                                  "yes")

table(db.full$software_stated)


# calculating number of studies that reported the software used
software.reported <- as.data.frame(db.full %>% 
                                     #filter(statistical.analysis.and.or.simulations=="yes") %>% 
                                     group_by(Publication_year.2) %>% 
                                     summarise(software_stated = sum(software_stated == "yes")))


# calculating number of studies that did not report the software used
no.software.reported <- as.data.frame(db.full %>% 
                                        #filter(statistical.analysis.and.or.simulations=="yes") %>% 
                                        group_by(Publication_year.2) %>% 
                                        summarise(software_stated = sum(software_stated == "no")))


# calculating number of eligible articles
software.reported.total <- as.data.frame(db.full %>% 
                                           #filter(statistical.analysis.and.or.simulations=="yes") %>% 
                                           group_by(Publication_year.2) %>% 
                                           summarise(n = n()))


# adding total sample size
full.summary.software.1 <- merge(software.reported,software.reported.total)
full.summary.software.2 <- merge(no.software.reported,software.reported.total)



# creating new variable to identify each value
full.summary.software.1$type <- "Yes"
full.summary.software.2$type <- "No"

# stacking them all: for real
full.summary.software <- rbind(full.summary.software.1,full.summary.software.2)
full.summary.software

# estimating percentages
full.summary.software$percentage <- (full.summary.software$software_stated/full.summary.software$n)*100
full.summary.software$code.sharing.policy <- "No"

full.summary.software


#fill <- c("grey98", "grey5")
fill <- c("cadetblue2", "cadetblue4")

mean.stated <- mean(full.summary.software[full.summary.software$type=="Yes","percentage"])


# Stacked barplot with multiple groups
figure3a <- full.summary.software %>%
  mutate(type = factor(type, levels = c("No","Yes"))) %>%
  ggplot() +
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  geom_hline(yintercept = mean.stated, color = "red", linetype = "dashed") +
  labs(y = "Percentage (%) of articles", fill = "Reported software") +
  scale_fill_manual(values = fill) +
  #scale_fill_viridis(discrete=T, direction = -1) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.position="none"
  )

figure3a

# # extracting colours
# g <- ggplot_build(figure3a)
# unique(g$data[[1]]["fill"])

################################################################################
# Journals with a code-sharing policy

# categorizing studies on whether they stated or not the software used
table(db.full.Culina$analysis_software_names=="Not Stated")
db.full.Culina$software_stated <- ifelse(db.full.Culina$analysis_software_names=="Not Stated",
                                         "no",
                                         "yes")

table(db.full.Culina$software_stated)


# calculating number of studies that reported the software used
software.reported.Culina <- as.data.frame(db.full.Culina %>% 
                                            #filter(statistical.analysis.and.or.simulations=="yes") %>% 
                                            group_by(Publication_year.2) %>% 
                                            summarise(software_stated = sum(software_stated == "yes")))


# calculating number of studies that did not report the software used
no.software.reported.Culina <- as.data.frame(db.full.Culina %>% 
                                               #filter(statistical.analysis.and.or.simulations=="yes") %>% 
                                               group_by(Publication_year.2) %>% 
                                               summarise(software_stated = sum(software_stated == "no")))


# calculating number of eligible articles
software.reported.Culina.total <- as.data.frame(db.full.Culina %>% 
                                                  #filter(statistical.analysis.and.or.simulations=="yes") %>% 
                                                  group_by(Publication_year.2) %>% 
                                                  summarise(n = n()))


# adding total sample size
full.summary.software.1.Culina <- merge(software.reported.Culina,software.reported.Culina.total)
full.summary.software.2.Culina <- merge(no.software.reported.Culina,software.reported.Culina.total)



# creating new variable to identify each value
full.summary.software.1.Culina$type <- "Yes"
full.summary.software.2.Culina$type <- "No"

# stacking them all: for real
full.summary.software.Culina <- rbind(full.summary.software.1.Culina,full.summary.software.2.Culina)
full.summary.software.Culina

# estimating percentages
full.summary.software.Culina$percentage <- (full.summary.software.Culina$software_stated/full.summary.software.Culina$n)*100
full.summary.software.Culina$code.sharing.policy <- "Yes"

full.summary.software.Culina

#fill <- c("grey98", "grey5")
fill <- c("lightblue1", "lightblue4")


mean.stated.Culina <- mean(full.summary.software.Culina[full.summary.software.Culina$type=="Yes","percentage"])

# Stacked barplot with multiple groups
figure3b <- full.summary.software.Culina %>%
  mutate(type = factor(type, levels = c("No","Yes")),
         code.sharing.policy = factor(code.sharing.policy, levels = c("Yes","No"))) %>%
  ggplot() +
  # geom_bar_pattern(aes(y = percentage, x = Publication_year.2, fill = type,), 
  #                  stat = "identity", colour = "black", pattern = "circle") +
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  geom_hline(yintercept = mean.stated.Culina, color = "red", linetype = "dashed") +
  labs(y = "Percentage (%) of articles", fill = "Reported software") +
  scale_fill_manual(values = fill) +
  #scale_fill_viridis(discrete=TRUE) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

figure3b


# # ################################################################################
# # combining both and plotting
# 
# summary.software <- rbind(full.summary.software,full.summary.software.Culina)
# summary.software


################################################################################
# Software version reporting
################################################################################

################################################################################
# Journals without a code-sharing policy

# subsetting the dataset to those reporting software
db.full.software <- as.data.frame(db.full %>% 
                                    filter(software_stated=="yes"))

db.full.software$software.version.provided <- ifelse((db.full.software$num.software.calculated > 
                                                        db.full.software$num.software_with_version),
                                                     "no",
                                                     "yes")
table(db.full.software$software.version.provided)
table(db.full.software$software.version.provided)/nrow(db.full.software)

# calculating number of studies that reported the software version used
software.version.reported <- as.data.frame(db.full.software %>% 
                                             group_by(Publication_year.2) %>% 
                                             summarise(software.version.provided = sum(software.version.provided == "yes")))


# calculating number of studies that did not report the software version used
no.software.version.reported <- as.data.frame(db.full.software %>% 
                                                group_by(Publication_year.2) %>% 
                                                summarise(software.version.provided = sum(software.version.provided == "no")))


# calculating number of eligible articles
software.version.reported.total <- as.data.frame(db.full.software %>% 
                                                   group_by(Publication_year.2) %>% 
                                                   summarise(n = n()))


# adding total sample size
full.summary.software.version.1 <- merge(software.version.reported,software.version.reported.total)
full.summary.software.version.2 <- merge(no.software.version.reported,software.version.reported.total)



# creating new variable to identify each value
full.summary.software.version.1$type <- "Yes"
full.summary.software.version.2$type <- "No"

# stacking them all: for real
full.summary.software.version <- rbind(full.summary.software.version.1,full.summary.software.version.2)
full.summary.software.version

# estimating percentages
full.summary.software.version$percentage <- (full.summary.software.version$software.version.provided/full.summary.software.version$n)*100
full.summary.software.version$code.sharing.policy <- "No"

full.summary.software.version

# # Write full.summary to an Excel file
# write.xlsx(full.summary, "results/full_Code_summary.xlsx")


#fill <- c("grey98", "grey5")
fill <- c("cadetblue2", "cadetblue4")

mean.stated.version <- mean(full.summary.software.version[full.summary.software.version$type=="Yes","percentage"])


# Stacked barplot with multiple groups
figure3c <- full.summary.software.version %>%
  mutate(type = factor(type, levels = c("No","Yes"))) %>%
  ggplot() +
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  geom_hline(yintercept = mean.stated.version, color = "red", linetype = "dashed") +
  labs(y = "Percentage (%) of articles", fill = "Reported software \nversions") +
  scale_fill_manual(values = fill) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.position="none"
  )

figure3c


################################################################################
# Journals with a code-sharing policy

# subsetting the dataset to those reporting software
db.full.software.Culina <- as.data.frame(db.full.Culina %>% 
                                           filter(software_stated=="yes"))

db.full.software.Culina$software.version.provided <- ifelse((db.full.software.Culina$num.software.calculated > 
                                                               db.full.software.Culina$num.software_with_version),
                                                            "no",
                                                            "yes")
table(db.full.software.Culina$software.version.provided)
table(db.full.software.Culina$software.version.provided)/nrow(db.full.software.Culina)


# calculating number of studies that reported the software version used
software.version.reported.Culina <- as.data.frame(db.full.software.Culina %>% 
                                                    group_by(Publication_year.2) %>% 
                                                    summarise(software.version.provided = sum(software.version.provided == "yes")))


# calculating number of studies that did not report the software version used
no.software.version.reported.Culina <- as.data.frame(db.full.software.Culina %>% 
                                                       group_by(Publication_year.2) %>% 
                                                       summarise(software.version.provided = sum(software.version.provided == "no")))


# calculating number of eligible articles
software.version.reported.total.Culina <- as.data.frame(db.full.software.Culina %>% 
                                                          group_by(Publication_year.2) %>% 
                                                          summarise(n = n()))


# adding total sample size
full.summary.software.version.Culina.1 <- merge(software.version.reported.Culina,software.version.reported.total.Culina)
full.summary.software.version.Culina.2 <- merge(no.software.version.reported.Culina,software.version.reported.total.Culina)



# creating new variable to identify each value
full.summary.software.version.Culina.1$type <- "Yes"
full.summary.software.version.Culina.2$type <- "No"

# stacking them all: for real
full.summary.software.version.Culina <- rbind(full.summary.software.version.Culina.1,full.summary.software.version.Culina.2)
full.summary.software.version.Culina

# estimating percentages
full.summary.software.version.Culina$percentage <- (full.summary.software.version.Culina$software.version.provided/
                                                      full.summary.software.version.Culina$n)*100
full.summary.software.version.Culina$code.sharing.policy <- "Yes"

full.summary.software.version.Culina


#fill <- c("grey98", "grey5")
fill <- c("lightblue1", "lightblue4")

mean.stated.version.Culina <- mean(full.summary.software.version.Culina[full.summary.software.version.Culina$type=="Yes","percentage"])


# Stacked barplot with multiple groups
figure3d <- full.summary.software.version.Culina %>%
  mutate(type = factor(type, levels = c("No","Yes"))) %>%
  ggplot() +
  # geom_bar_pattern(aes(y = percentage, x = Publication_year.2, fill = type), 
  #                  stat = "identity", colour = "black", pattern = "circle") +
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  geom_hline(yintercept = mean.stated.version.Culina, color = "red", linetype = "dashed") +
  labs(y = "Percentage (%) of articles", fill = "Reported software \nversions") +
  scale_fill_manual(values = fill) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

figure3d


################################################################################
# Package version reporting
################################################################################

################################################################################
# Journals without a code-sharing policy

# subsetting the dataset to those reporting at least 1 package
db.full.package <- as.data.frame(db.full %>% 
                                   filter(num.packages>0))

db.full.package$package.version.provided <- ifelse((db.full.package$num.packages > 
                                                      db.full.package$num.packages_with_version),
                                                   "no",
                                                   "yes")

table(db.full.package$package.version.provided)
table(db.full.package$package.version.provided)/nrow(db.full.package)

# calculating number of studies that reported the package version used
package.version.reported <- as.data.frame(db.full.package %>% 
                                            group_by(Publication_year.2) %>% 
                                            summarise(package.version.provided = sum(package.version.provided == "yes")))


# calculating number of studies that did not report the package version used
no.package.version.reported <- as.data.frame(db.full.package %>% 
                                               group_by(Publication_year.2) %>% 
                                               summarise(package.version.provided = sum(package.version.provided == "no")))


# calculating number of eligible articles
package.version.reported.total <- as.data.frame(db.full.package %>% 
                                                  group_by(Publication_year.2) %>% 
                                                  summarise(n = n()))


# adding total sample size
full.summary.package.version.1 <- merge(package.version.reported,package.version.reported.total)
full.summary.package.version.2 <- merge(no.package.version.reported,package.version.reported.total)



# creating new variable to identify each value
full.summary.package.version.1$type <- "Yes"
full.summary.package.version.2$type <- "No"

# stacking them all: for real
full.summary.package.version <- rbind(full.summary.package.version.1,full.summary.package.version.2)
full.summary.package.version

# estimating percentages
full.summary.package.version$percentage <- (full.summary.package.version$package.version.provided/full.summary.package.version$n)*100
full.summary.package.version$code.sharing.policy <- "No"

full.summary.package.version


#fill <- c("grey98", "grey5")
fill <- c("cadetblue2", "cadetblue4")

mean.stated.package.version <- mean(full.summary.package.version[full.summary.package.version$type=="Yes","percentage"])


# Stacked barplot with multiple groups
figure3e <- full.summary.package.version %>%
  mutate(type = factor(type, levels = c("No","Yes"))) %>%
  ggplot() +
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  geom_hline(yintercept = mean.stated.package.version, color = "red", linetype = "dashed") +
  labs(y = "Percentage (%) of articles", fill = "Reported package \nversions") +
  scale_fill_manual(values = fill) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.position="none"
  )

figure3e


################################################################################
# Journals with a code-sharing policy

# subsetting the dataset to those reporting at least 1 package
db.full.package.Culina <- as.data.frame(db.full.Culina %>% 
                                          filter(num.packages.calculated>0))

db.full.package.Culina$package.version.provided <- ifelse((db.full.package.Culina$num.packages.calculated > 
                                                             db.full.package.Culina$num.packages_with_version),
                                                          "no",
                                                          "yes")

table(db.full.package.Culina$package.version.provided)
table(db.full.package.Culina$package.version.provided)/nrow(db.full.package.Culina)

# calculating number of studies that reported the package versions used
package.version.reported.Culina <- as.data.frame(db.full.package.Culina %>% 
                                                   group_by(Publication_year.2) %>% 
                                                   summarise(package.version.provided = sum(package.version.provided == "yes")))


# calculating number of studies that did not report the package versions used
no.package.version.reported.Culina <- as.data.frame(db.full.package.Culina %>% 
                                                      group_by(Publication_year.2) %>% 
                                                      summarise(package.version.provided = sum(package.version.provided == "no")))


# calculating number of eligible articles
package.version.reported.total.Culina <- as.data.frame(db.full.package.Culina %>% 
                                                         group_by(Publication_year.2) %>% 
                                                         summarise(n = n()))


# adding total sample size
full.summary.package.version.Culina.1 <- merge(package.version.reported.Culina,package.version.reported.total.Culina)
full.summary.package.version.Culina.2 <- merge(no.package.version.reported.Culina,package.version.reported.total.Culina)



# creating new variable to identify each value
full.summary.package.version.Culina.1$type <- "Yes"
full.summary.package.version.Culina.2$type <- "No"

# stacking them all: for real
full.summary.package.version.Culina <- rbind(full.summary.package.version.Culina.1,full.summary.package.version.Culina.2)
full.summary.package.version.Culina

# estimating percentages
full.summary.package.version.Culina$percentage <- (full.summary.package.version.Culina$package.version.provided/
                                                     full.summary.package.version.Culina$n)*100
full.summary.package.version.Culina$code.sharing.policy <- "Yes"

full.summary.package.version.Culina

# # Write full.summary to an Excel file
# write.xlsx(full.summary, "results/full_Code_summary.xlsx")


#fill <- c("grey98", "grey5")
fill <- c("lightblue1", "lightblue4")

mean.stated.package.version.Culina <- mean(full.summary.package.version.Culina[full.summary.package.version.Culina$type=="Yes","percentage"])


# Stacked barplot with multiple groups
figure3f <- full.summary.package.version.Culina %>%
  mutate(type = factor(type, levels = c("No","Yes"))) %>%
  ggplot() +
  # geom_bar_pattern(aes(y = percentage, x = Publication_year.2, fill = type), 
  #                  stat = "identity", colour = "black", pattern = "circle") +
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  geom_hline(yintercept = mean.stated.package.version.Culina, color = "red", linetype = "dashed") +
  labs(y = "Percentage (%) of articles", fill = "Reported package \nversions") +
  scale_fill_manual(values = fill) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

figure3f


################################################################################
# Free vs Non-free software
################################################################################

################################################################################
# Journals without a code-sharing policy

# calculating number of studies that provided free and non-free software
partially.free <- as.data.frame(db.full.software %>% 
                                  group_by(Publication_year.2) %>% 
                                  summarise(FreeSoftware = sum(FreeSoftware == "partially")))

# calculating number of studies that provided only free software
all.free <- as.data.frame(db.full.software %>% 
                            group_by(Publication_year.2) %>% 
                            summarise(FreeSoftware = sum(FreeSoftware == "yes")))


# calculating number of studies that provided only non-free software
none.free <- as.data.frame(db.full.software %>% 
                             group_by(Publication_year.2) %>% 
                             summarise(FreeSoftware = sum(FreeSoftware == "no")))


# calculating number of eligible articles
total.free <- as.data.frame(db.full.software %>% 
                              group_by(Publication_year.2) %>% 
                              summarise(n = n()))


# adding total sample size
full.summary.free.1 <- merge(partially.free,total.free)
full.summary.free.2 <- merge(all.free,total.free)
full.summary.free.3 <- merge(none.free,total.free)


# creating new variable to identify each value
full.summary.free.1$type <- "Partially free"
full.summary.free.2$type <- "All free"
full.summary.free.3$type <- "None free"

# stacking them all: for real
full.summary.free <- rbind(full.summary.free.1,full.summary.free.2,full.summary.free.3)

# estimating percentages
full.summary.free$percentage <- (full.summary.free$FreeSoftware/full.summary.free$n)*100
full.summary.free

full.summary.free$code.sharing.policy <- "No"

full.summary.free


#fill <- c("grey98", "grey35", "grey5")
fill <- c("cadetblue2", "cadetblue3", "cadetblue4")

mean.free <- mean(full.summary.free[full.summary.free$type=="All free","percentage"])


# Stacked barplot with multiple groups
figure3g <- full.summary.free %>%
  mutate(type = factor(type, levels = c("None free", "Partially free", "All free"))) %>%
  ggplot() +
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  geom_hline(yintercept = mean.free, color = "red", linetype = "dashed") +
  labs(y = "Percentage (%) of articles", fill = "Software type") +
  scale_fill_manual(values = fill) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.position="none"
  )

figure3g


################################################################################
# Journals with a code-sharing policy

# calculating number of studies that provided free and non-free software
partially.free.Culina <- as.data.frame(db.full.software.Culina %>% 
                                  group_by(Publication_year.2) %>% 
                                  summarise(FreeSoftware = sum(FreeSoftware == "partially")))

# calculating number of studies that provided only free software
all.free.Culina <- as.data.frame(db.full.software.Culina %>% 
                            group_by(Publication_year.2) %>% 
                            summarise(FreeSoftware = sum(FreeSoftware == "yes")))


# calculating number of studies that provided only non-free software
none.free.Culina <- as.data.frame(db.full.software.Culina %>% 
                             group_by(Publication_year.2) %>% 
                             summarise(FreeSoftware = sum(FreeSoftware == "no")))


# calculating number of eligible articles
total.free.Culina <- as.data.frame(db.full.software.Culina %>% 
                              group_by(Publication_year.2) %>% 
                              summarise(n = n()))


# adding total sample size
full.summary.free.Culina.1 <- merge(partially.free.Culina,total.free.Culina)
full.summary.free.Culina.2 <- merge(all.free.Culina,total.free.Culina)
full.summary.free.Culina.3 <- merge(none.free.Culina,total.free.Culina)


# creating new variable to identify each value
full.summary.free.Culina.1$type <- "Partially free"
full.summary.free.Culina.2$type <- "All free"
full.summary.free.Culina.3$type <- "None free"

# stacking them all: for real
full.summary.free.Culina <- rbind(full.summary.free.Culina.1,full.summary.free.Culina.2,full.summary.free.Culina.3)

# estimating percentages
full.summary.free.Culina$percentage <- (full.summary.free.Culina$FreeSoftware/full.summary.free.Culina$n)*100
full.summary.free.Culina

full.summary.free.Culina$code.sharing.policy <- "Yes"
full.summary.free.Culina


#fill <- c("grey98", "grey35", "grey5")
fill <- c("lightblue1", "lightblue3", "lightblue4")

mean.free.Culina <- mean(full.summary.free.Culina[full.summary.free.Culina$type=="All free","percentage"])


# Stacked barplot with multiple groups
figure3h <- full.summary.free.Culina %>%
  mutate(type = factor(type, levels = c("None free", "Partially free", "All free"))) %>%
  ggplot() +
  # geom_bar_pattern(aes(y = percentage, x = Publication_year.2, fill = type), 
  #                  stat = "identity", colour = "black", pattern = "circle") +
  geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat = "identity", colour = "black") +
  geom_hline(yintercept = mean.free.Culina, color = "red", linetype = "dashed") +
  labs(y = "Percentage (%) of articles", fill = "Software type") +
  scale_fill_manual(values = fill) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

figure3h


################################################################################
# Exporting Figure 3
################################################################################

tiff("figures/Figure3.tiff",
     height = 27, width = 21,
     units = 'cm', compression = "lzw", res = 600)

figure3 <- figure3a + 
  labs(title = c("a: No code-sharing policy")) +
  figure3b +
  labs(title = c("b: Code-sharing policy")) +
  figure3c + 
  labs(title = c("c: No code-sharing policy")) +
  figure3d +
  labs(title = c("d: Code-sharing policy")) +
  figure3e + 
  labs(title = c("e: No code-sharing policy")) +
  figure3f +
  labs(title = c("f: Code-sharing policy")) +
  figure3g + 
  labs(title = c("g: No code-sharing policy")) +
  figure3h +
  labs(title = c("h: Code-sharing policy")) +
  plot_layout(ncol = 2, nrow = 4)

print(figure3)

dev.off()



# ################################################################################
# # Location code figure
# ################################################################################
# 
# # calculating number of studies that published code in a repository
# repository <- db.full %>% 
#   filter(!(is.na(Publication_year.2)), !(is.na(LocationShared))) %>% 
#   group_by(Publication_year.2) %>% 
#   summarise(location = sum(LocationShared == "repository"))
# 
# 
# # calculating number of studies that published code as supplementary file
# supplementary <- db.full %>% 
#   filter(!(is.na(Publication_year.2)), !(is.na(LocationShared))) %>% 
#   group_by(Publication_year.2) %>% 
#   summarise(location = sum(LocationShared == "supplementary material"))
# 
# 
# # calculating number of studies that published code in a webpage
# webpage <- db.full %>% 
#   filter(!(is.na(Publication_year.2)), !(is.na(LocationShared))) %>% 
#   group_by(Publication_year.2) %>% 
#   summarise(location = sum(LocationShared == "website"))
# 
# 
# # calculating number of eligible articles, i.e. those with code
# total.location <- db.full %>% 
#   filter(!(is.na(Publication_year.2)), !(is.na(LocationShared)), statistical.analysis.and.or.simulations=="yes") %>% 
#   group_by(Publication_year.2) %>% 
#   summarise(n = n())
# 
# 
# # making them all data frames
# repository <- as.data.frame(repository)
# supplementary <- as.data.frame(supplementary)
# webpage <- as.data.frame(webpage)
# total.location <- as.data.frame(total.location)
# 
# 
# # adding total sample size
# full.location.summary.1 <- merge(repository,total.location)
# full.location.summary.2 <- merge(supplementary,total.location)
# full.location.summary.3 <- merge(webpage,total.location)
# 
# 
# # creating new variable to identify each value
# full.location.summary.1$type <- "Repository"
# full.location.summary.2$type <- "Supplements"
# full.location.summary.3$type <- "Webpage"
# 
# # stacking them all: for real
# full.location.summary <- rbind(full.location.summary.1,full.location.summary.2,full.location.summary.3)
# 
# # estimating percentages
# full.location.summary$percentage <- (full.location.summary$location/full.location.summary$n)*100
# full.location.summary
# 
# # choosing colours manually
# fill <- c("grey98", "grey35","grey5")
# 
# 
# # Stacked barplot with multiple groups
# figure1c <- full.location.summary %>% 
#   mutate(type = factor(type, levels = c("Supplements",
#                                         "Webpage",
#                                         "Repository"))) %>% 
#   ggplot() + 
#   geom_bar(aes(y = percentage, x = Publication_year.2, fill = type), stat="identity",colour="black") +
#   labs(y="Percentage (%) of articles", fill="Code location") +
#   scale_fill_manual(values=fill) +
#   scale_y_continuous(breaks = seq(0,100,20),expand = expand_scale(mult = c(0, 0.05))) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10),
#         axis.title.y = element_text(size = 14),
#         axis.title.x=element_blank())
# figure1c


# ################################################################################
# # time trend figure
# ################################################################################
# subset_code_shared <- db.full %>%
#   filter(CodeShared == "yes" & Publication_year %in% c(2015,2016, 2018,2019))
# #ussing CodeShare.2
# 
# subset_data_shared <- db.full %>%
#   filter(DataShared == "yes" & Publication_year %in% c(2015,2016, 2018,2019))
# 
# code_shared_counts <- subset_code_shared %>%
#   group_by(Publication_year) %>%
#   summarise(count = n())
# 
# data_shared_counts <- subset_data_shared %>%
#   group_by(Publication_year) %>%
#   summarise(count = n())
# 
# merged_counts <- merge(code_shared_counts, data_shared_counts, by = "Publication_year", all = TRUE)
# merged_counts
# 
# plot_object <- ggplot(merged_counts, aes(x = Publication_year.2)) +
#   geom_line(aes(y = count.x, color = "CodeShared"), size = 1.5, group = 1) +
#   geom_line(aes(y = count.y, color = "DataShared"), size = 1.5, group = 1) +
#   labs(x = "Year", y = "Count", title = "Time Series Plot of Code Sharing and Data Sharing") +
#   scale_color_manual(values = c("CodeShared" = "blue", "DataShared" = "red"), 
#                      labels = c("Code Shared", "Data Shared"),
#                      guide = guide_legend(title = "Legend")) +
#   theme_minimal()
# 
# print(plot_object)
# 
# 
# ggsave("results/plot_time.png", plot = plot_object, width = 10, height = 6, dpi = 300)
# 


# ################################################################################
# # Location freesoftware
# ################################################################################
# table(db.full$FreeSoftware)
# 
# 
# # Filter the dataset
# subset_free <- db.full %>% filter(FreeSoftware == "yes")
# subset_not_free <- db.full %>% filter(FreeSoftware == "no")
# subset_partially <- db.full %>% filter(FreeSoftware == "partially")
# db.full$FreeSoftware <- ifelse(is.na(db.full$FreeSoftware), "NA", as.character(db.full$FreeSoftware))
# 
# # Calculate the counts
# count_free <- nrow(subset_free)
# count_not_free <- nrow(subset_not_free)
# count_partially <- nrow(subset_partially)
# # Create a data frame
# data <- data.frame(
#   Software = c("Free Software", "Not Free Software", "Partially Free"),
#   Count = c(count_free, count_not_free,count_partially)
# )
# 
# data




# # Export Figure 1 as TIFF file
# tiff("results/Figure1.tiff",
#      height = 12, width = 28,
#      units = 'cm', compression = "lzw", res = 600)
# 
# # Multi-panel plot using patchwork
# figure1 <- figure1a +
#   labs(title = ) +
#   plot_layout(ncol = 2, nrow = 1)
# 
# print(figure1)
# 
# dev.off()
# 
# 
# # Export Figure 1 as TIFF file
# tiff("results/Figure1b.tiff",
#      height = 12, width = 28,
#      units = 'cm', compression = "lzw", res = 600)
# 
# # Multi-panel plot using patchwork
# figure1b <- figure1b +
#   labs(title = "b)") +
#   plot_layout(ncol = 2, nrow = 1)
# 
# print(figure1b)
# 
# dev.off()

# # Export Figure 1 as TIFF file
# tiff("results/Figure1c.tiff",
#      height = 12, width = 28,
#      units = 'cm', compression = "lzw", res = 600)
# 
# # Multi-panel plot using patchwork
# figure1c <- figure1c +
#   labs(title = "b)") +
#   plot_layout(ncol = 3, nrow = 1)
# 
# print(figure1c)
# 
# dev.off()



