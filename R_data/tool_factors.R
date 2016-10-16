library(ggplot2)

plot.Likert <- function(values, scale, title) {
  if (length(scale) != length(values)) stop("scales be the same length as values")
  percents <- sapply(values, function(x) (x/sum(values)*100))
  bplot <- barplot(percents, main=title, ylim=c(0,100), names.arg=scale)
  text(x = bplot, y = percents, label = round(percents, 2), pos = 3, cex = 0.8)
}

plotter <- list()
# GENDER
plotter.Gender <- function(target) {
  plot.Likert(target$everybody, target$scale, "Full Population")
  plot.Likert(target$male, target$scale, "Male")
  plot.Likert(target$female, target$scale, "Female")
}
# SOURCE DISTRIBUTION MODEL
plotter.Source_Model <- function(target) {
  plot.Likert(target$everybody, target$scale, "Full Population")
  plot.Likert(target$open, target$scale, "Open-Source")
  plot.Likert(target$closed, target$scale, "Closed-Source")
  plot.Likert(target$both, target$scale, "Split Evenly")
}
# ROLES
plotter.Roles <- function(target) {
  plot.Likert(target$everybody, target$scale, "Full Population")
  plot.Likert(target$open, target$scale, "Open-Source")
  plot.Likert(target$soft_eng, target$scale, "Software Engineers")
  plot.Likert(target$sys_eng, target$scale, "System Engineers")
  plot.Likert(target$sys_arc, target$scale, "System Architects")
  plot.Likert(target$sys_admin, target$scale, "Systems Administrators")
  plot.Likert(target$devops, target$scale, "DevOps")
  plot.Likert(target$proj_maint, target$scale, "Project Maintainers")
  plot.Likert(target$proj_manag, target$scale, "Project Managers")  
}
# EXPERIENCE
plotter.Experience <- function(target) {
  plot.Likert(target$everybody, target$scale, "Full Population")
  plot.Likert(target$exp_1, target$scale, "1-5 Years")
  plot.Likert(target$exp_2, target$scale, "6-10 Years")
  plot.Likert(target$exp_3, target$scale, "11-15 Years")
  plot.Likert(target$exp_4, target$scale, "16-20 Years")
  plot.Likert(target$exp_5, target$scale, "21-25 Years")
  plot.Likert(target$exp_6, target$scale, "26+ Years")
}
# PROJECT SIZE
plotter.Project_Size <- function(target) {
  plot.Likert(target$everybody, target$scale, "Full Population")
  plot.Likert(target$psize_1, target$scale, "1 Developer")
  plot.Likert(target$psize_2, target$scale, "2-5 Developers")
  plot.Likert(target$psize_3, target$scale, "6-10 Developers")
  plot.Likert(target$psize_4, target$scale, "11-50 Developers")
  plot.Likert(target$psize_5, target$scale, "51+ Developers")
}

############################################################################################
# Q50_2: Howuseful would the following changes be in your merge conflict resolution tools? #
############################################################################################
# (1) better_transparency                                                                  #
# (2) better_usability                                                                     #
# (3) filtering_data                                                                       #
# (4) graphical_presentation                                                               #
# (5) exploring_history                                                                    #
# (6) consistent_terminology                                                               #
############################################################################################

better_transparency <- list()
better_transparency$scale <- c("Not useful", "Slightly useful", "Moderately useful", "Very useful", "Essential")
better_transparency$everybody  <- c(16, 36, 24, 40, 3)
# GENDER
better_transparency$male       <- c(14, 32, 22, 37, 2)
better_transparency$female     <- c(1, 3, 2, 0, 1)
# SOURCE DISTRIBUTION MODEL
better_transparency$open       <- c(4, 5, 6, 2, 0)
better_transparency$closed     <- c(10, 25, 14, 23, 3)
better_transparency$both       <- c(2, 6, 4, 15, 0)
# ROLES
better_transparency$soft_eng   <- c(16, 35, 23, 36, 3)
better_transparency$sys_eng    <- c(5, 13, 5, 5, 0)
better_transparency$sys_arc    <- c(4, 14, 9, 13, 1)
better_transparency$sys_admin  <- c(1, 10, 3, 5, 0)
better_transparency$devops     <- c(8, 13, 6, 13, 1)
better_transparency$proj_maint <- c(4, 11, 5, 10, 0)
better_transparency$proj_manag <- c(6, 13, 7, 9, 0)
# EXPERIENCE
better_transparency$exp_1      <- c(6, 7, 10, 6, 2)
better_transparency$exp_2      <- c(6, 7, 5, 11, 1)
better_transparency$exp_3      <- c(2, 5, 2, 10, 0)
better_transparency$exp_4      <- c(0, 4, 0, 7, 0)
better_transparency$exp_5      <- c(1, 0, 1, 2, 0)
better_transparency$exp_6      <- c(1, 3, 6, 4, 0)
# PROJECT SIZE
better_transparency$psize_1    <- c(1, 1, 0, 3, 0)
better_transparency$psize_2    <- c(7, 20, 9, 18, 1)
better_transparency$psize_3    <- c(6, 6, 7, 11, 2)
better_transparency$psize_4    <- c(1, 4, 4, 8, 0)
better_transparency$psize_5    <- c(1, 5, 4, 0, 0)

better_usability <- list()
better_usability$scale <- c("Not useful", "Slightly useful", "Moderately useful", "Very useful", "Essential")
better_usability$everybody  <- c(6, 17, 32, 48, 16)
# GENDER
better_usability$male       <- c(5, 14, 30, 43, 15)
better_usability$female     <- c(1, 2, 1, 2, 1)
# SOURCE DISTRIBUTION MODEL
better_usability$open       <- c(1, 4, 8, 3, 1)
better_usability$closed     <- c(5, 9, 17, 36, 8)
better_usability$both       <- c(0, 4, 7, 9, 7)
ROLES
better_usability$soft_eng   <- c(6, 17, 30, 44, 16)
better_usability$sys_eng    <- c(1, 6, 6, 9, 6)
better_usability$sys_arc    <- c(0, 6, 11, 18, 6)
better_usability$sys_admin  <- c(0, 4, 5, 8, 2)
better_usability$devops     <- c(2, 7, 6, 20, 6)
better_usability$proj_maint <- c(1, 4, 9, 9, 7)
better_usability$proj_manag <- c(2, 5, 9, 11, 8)
# EXPERIENCE
better_usability$exp_1      <- c(2, 4, 9, 15, 1)
better_usability$exp_2      <- c(4, 6, 12, 11, 7)
better_usability$exp_3      <- c(0, 3, 3, 10, 3)
better_usability$exp_4      <- c(0, 2, 0, 7, 2)
better_usability$exp_5      <- c(0, 0, 2, 0, 2)
better_usability$exp_6      <- c(0, 2, 6, 5, 1)
# PROJECT SIZE
better_usability$psize_1    <- c(0, 0, 3, 2, 0)
better_usability$psize_2    <- c(2, 8, 12, 23, 10)
better_usability$psize_3    <- c(3, 3, 7, 15, 4)
better_usability$psize_4    <- c(1, 2, 7, 5, 2)
better_usability$psize_5    <- c(0, 4, 3, 3, 0)

filtering_data <- list()
filtering_data$scale <- c("Not useful", "Slightly useful", "Moderately useful", "Very useful", "Essential")
filtering_data$everybody  <- c(8, 15, 32, 48, 16)
# GENDER
filtering_data$male       <- c(6, 12, 32, 41, 16)
filtering_data$female     <- c(2, 1, 0, 4, 0)
# SOURCE DISTRIBUTION MODEL
filtering_data$open       <- c(2, 3, 6, 5, 1)
filtering_data$closed     <- c(6, 9, 18, 31, 11)
filtering_data$both       <- c(0, 3, 8, 12, 4)
# ROLES
filtering_data$soft_eng   <- c(8, 15, 31, 43, 16)
filtering_data$sys_eng    <- c(0, 5, 7, 11, 5)
filtering_data$sys_arc    <- c(1, 4, 11, 18, 7)
filtering_data$sys_admin  <- c(0, 3, 7, 5, 4)
filtering_data$devops     <- c(0, 5, 10, 19, 7)
filtering_data$proj_maint <- c(0, 4, 9, 10, 7)
filtering_data$proj_manag <- c(2, 5, 10, 12, 6)
# EXPERIENCE
filtering_data$exp_1      <- c(5, 2, 9, 13, 2)
filtering_data$exp_2      <- c(3, 7, 10, 14, 6)
filtering_data$exp_3      <- c(0, 5, 6, 6, 2)
filtering_data$exp_4      <- c(0, 0, 3, 5, 3)
filtering_data$exp_5      <- c(0, 0, 0, 3, 1)
filtering_data$exp_6      <- c(0, 1, 4, 7, 2)
# PROJECT SIZE
filtering_data$psize_1    <- c(0, 1, 3, 1, 0)
filtering_data$psize_2    <- c(4, 6, 13, 23, 9)
filtering_data$psize_3    <- c(2, 4, 10, 13, 3)
filtering_data$psize_4    <- c(1, 3, 4, 7, 2)
filtering_data$psize_5    <- c(1, 1, 2, 4, 2)

graphical_presentation <- list()
graphical_presentation$scale <- c("Not useful", "Slightly useful", "Moderately useful", "Very useful", "Essential")
graphical_presentation$everybody  <- c(13, 26, 26, 37, 16)
# GENDER
graphical_presentation$male       <- c(12, 23, 22, 34, 15)
graphical_presentation$female     <- c(1, 2, 2, 2, 0)
# SOURCE DISTRIBUTION MODEL
graphical_presentation$open       <- c(4, 6, 2, 2, 3)
graphical_presentation$closed     <- c(5, 15, 16, 30, 8)
graphical_presentation$both       <- c(4, 5, 8, 5, 5)
# ROLES
graphical_presentation$soft_eng   <- c(13, 25, 25, 35, 14)
graphical_presentation$sys_eng    <- c(4, 5, 7, 7, 5)
graphical_presentation$sys_arc    <- c(4, 8, 9, 10, 9)
graphical_presentation$sys_admin  <- c(3, 4, 3, 6, 3)
graphical_presentation$devops     <- c(3, 7, 9, 12, 10)
graphical_presentation$proj_maint <- c(4, 9, 7, 6, 4)
graphical_presentation$proj_manag <- c(4, 7, 10, 7, 6)
# EXPERIENCE
graphical_presentation$exp_1      <- c(2, 7, 9, 10, 3)
graphical_presentation$exp_2      <- c(6, 10, 10, 8, 5)
graphical_presentation$exp_3      <- c(1, 2, 4, 8, 4)
graphical_presentation$exp_4      <- c(2, 2, 1, 3, 3)
graphical_presentation$exp_5      <- c(1, 0, 1, 2, 0)
graphical_presentation$exp_6      <- c(1, 5, 1, 6, 1)
# PROJECT SIZE
graphical_presentation$psize_1    <- c(1, 2, 0, 2, 0)
graphical_presentation$psize_2    <- c(4, 12, 15, 15, 8)
graphical_presentation$psize_3    <- c(4, 4, 8, 12, 4)
graphical_presentation$psize_4    <- c(2, 6, 2, 4, 3)
graphical_presentation$psize_5    <- c(2, 2, 1, 4, 1)

exploring_history <- list()
exploring_history$scale <- c("Not useful", "Slightly useful", "Moderately useful", "Very useful", "Essential")
exploring_history$everybody   <- c(7, 21, 36, 39, 16)
# GENDER
exploring_history$male       <- c(6, 18, 31, 36, 16)
exploring_history$female     <- c(1, 2, 3, 1, 0)
# SOURCE DISTRIBUTION MODEL
exploring_history$open        <- c(1, 3, 5, 6, 2)
exploring_history$closed      <- c(4, 13, 27, 25, 6)
exploring_history$both        <- c(2, 5, 4, 8, 8)
# ROLES
exploring_history$soft_eng   <- c(7, 21, 34, 37, 14)
exploring_history$sys_eng    <- c(1, 7, 11, 6, 3)
exploring_history$sys_arc    <- c(2, 7, 11, 12, 9)
exploring_history$sys_admin  <- c(1, 3, 8, 7, 0)
exploring_history$devops     <- c(2, 7, 15, 9, 8)
exploring_history$proj_maint <- c(1, 4, 10, 10, 5)
exploring_history$proj_manag <- c(2, 8, 12, 9, 4)
# EXPERIENCE
exploring_history$exp_1      <- c(3, 4, 10, 11, 3)
exploring_history$exp_2      <- c(3, 8, 13, 11, 5)
exploring_history$exp_3      <- c(0, 3, 5, 7, 4)
exploring_history$exp_4      <- c(0, 2, 3, 4, 2)
exploring_history$exp_5      <- c(0, 1, 1, 1, 1)
exploring_history$exp_6      <- c(1, 3, 4, 5, 1)
# PROJECT SIZE
exploring_history$psize_1    <- c(0, 0, 3, 2, 0)
exploring_history$psize_2    <- c(4, 10, 16, 19, 6)
exploring_history$psize_3    <- c(2, 7, 9, 9, 5)
exploring_history$psize_4    <- c(1, 3, 4, 6, 3)
exploring_history$psize_5    <- c(0, 1, 4, 3, 2)

consistent_terminology <- list()
consistent_terminology$scale <- c("Not useful", "Slightly useful", "Moderately useful", "Very useful", "Essential")
consistent_terminology$everybody  <- c(23, 41, 32, 15, 8)
# GENDER
consistent_terminology$male       <- c(21, 37, 27, 14, 8)
consistent_terminology$female     <- c(1, 2, 3, 1, 0)
# SOURCE DISTRIBUTION MODEL
consistent_terminology$open       <- c(5, 8, 2, 2, 0)
consistent_terminology$closed     <- c(14, 24, 19, 11, 7)
consistent_terminology$both       <- c(4, 9, 11, 2, 1)
# ROLES
consistent_terminology$soft_eng   <- c(23, 41, 27, 15, 7)
consistent_terminology$sys_eng    <- c(7, 11, 6, 3, 1)
consistent_terminology$sys_arc    <- c(6, 15, 12, 5, 3)
consistent_terminology$sys_admin  <- c(1, 6, 9, 1, 2)
consistent_terminology$devops     <- c(12, 13, 8, 5, 3)
consistent_terminology$proj_maint <- c(7, 12, 9, 2, 0)
consistent_terminology$proj_manag <- c(8, 13, 10, 2, 2)
# EXPERIENCE
consistent_terminology$exp_1      <- c(9, 11, 9, 2, 0)
consistent_terminology$exp_2      <- c(7, 15, 9, 5, 4)
consistent_terminology$exp_3      <- c(4, 6, 4, 4, 1)
consistent_terminology$exp_4      <- c(0, 3, 3, 3, 2)
consistent_terminology$exp_5      <- c(1, 0, 3, 0, 0)
consistent_terminology$exp_6      <- c(2, 6, 4, 1, 1)
# PROJECT SIZE
consistent_terminology$psize_1    <- c(2, 0, 1, 2, 0)
consistent_terminology$psize_2    <- c(10, 23, 13, 7, 2)
consistent_terminology$psize_3    <- c(7, 8, 10, 4, 3)
consistent_terminology$psize_4    <- c(2, 4, 8, 1, 2)
consistent_terminology$psize_5    <- c(2, 6, 0, 1, 1)

#############################################################################################
# Q50_1: Please rate how much each of these factor into the difficulty of a merge conflict? #
#############################################################################################
# (1) time_to_resolve                                                                       #
# (2) conflicting_lines                                                                     #
# (3) complexity_lines                                                                      #
# (4) conflicting_files                                                                     #
# (5) complexity_files                                                                      #
# (6) nonfunctional_changes                                                                 #
# (7) component_dependencies                                                                #
# (8) atomic_changesets                                                                     #
# (9) expertise_in_code                                                                     #
#############################################################################################
time_to_resolve <- list()
time_to_resolve$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
time_to_resolve$everybody  <- c(14, 56, 51, 25, 15)
# GENDER
time_to_resolve$male       <- c(11, 50, 47, 25, 13)
time_to_resolve$female     <- c(2, 2, 2, 0, 2)
# SOURCE DISTRIBUTION MODEL
time_to_resolve$open       <- c(3, 8, 5, 3, 3)
time_to_resolve$closed     <- c(9, 36, 31, 16, 8)
time_to_resolve$both       <- c(2, 12, 15, 6, 4)
# ROLES
time_to_resolve$soft_eng   <- c(14, 55, 45, 24, 15)
time_to_resolve$sys_eng    <- c(4, 11, 10, 4, 4)
time_to_resolve$sys_arc    <- c(5, 14, 19, 9, 6)
time_to_resolve$sys_admin  <- c(1, 7, 6, 6, 2)
time_to_resolve$devops     <- c(5, 17, 17, 9, 4)
time_to_resolve$proj_maint <- c(3, 11, 9, 10, 7)
time_to_resolve$proj_manag <- c(6, 11, 15, 6, 6)
# EXPERIENCE
time_to_resolve$exp_1      <- c()
time_to_resolve$exp_2      <- c()
time_to_resolve$exp_3      <- c()
time_to_resolve$exp_4      <- c()
time_to_resolve$exp_5      <- c()
time_to_resolve$exp_6      <- c()
# PROJECT SIZE
time_to_resolve$psize_1    <- c()
time_to_resolve$psize_2    <- c()
time_to_resolve$psize_3    <- c()
time_to_resolve$psize_4    <- c()
time_to_resolve$psize_5    <- c()

conflicting_lines <- list()
conflicting_lines$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
conflicting_lines$everybody  <- c(2, 40, 64, 45, 11)
# GENDER
conflicting_lines$male       <- c(1, 34, 56, 45, 11)
conflicting_lines$female     <- c(1, 2, 5, 0, 0)
# SOURCE DISTRIBUTION MODEL
conflicting_lines$open       <- c(0, 5, 13, 4, 1)
conflicting_lines$closed     <- c(2, 28, 37, 28, 5)
conflicting_lines$both       <- c(0, 7, 14, 13, 5)
# ROLES
conflicting_lines$soft_eng   <- c(2, 40, 57, 44, 11)
conflicting_lines$sys_eng    <- c(0, 9, 15, 7, 3)
conflicting_lines$sys_arc    <- c(0, 10, 25, 14, 5)
conflicting_lines$sys_admin  <- c(0, 6, 11, 4, 2)
conflicting_lines$devops     <- c(0, 13, 20, 16, 4)
conflicting_lines$proj_maint <- c(0, 9, 12, 14, 5)
conflicting_lines$proj_manag <- c(0, 16, 18, 8, 2)
# EXPERIENCE
conflicting_lines$exp_1      <- c()
conflicting_lines$exp_2      <- c()
conflicting_lines$exp_3      <- c()
conflicting_lines$exp_4      <- c()
conflicting_lines$exp_5      <- c()
conflicting_lines$exp_6      <- c()
# PROJECT SIZE
conflicting_lines$psize_1    <- c()
conflicting_lines$psize_2    <- c()
conflicting_lines$psize_3    <- c()
conflicting_lines$psize_4    <- c()
conflicting_lines$psize_5    <- c()

complexity_lines <- list()
complexity_lines$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
complexity_lines$everybody  <- c(5, 29, 38, 56, 34)
# GENDER
complexity_lines$male       <- c(5, 27, 35, 49, 31)
complexity_lines$female     <- c(0, 0, 2, 5, 1)
# SOURCE DISTRIBUTION MODEL
complexity_lines$open       <- c(2, 4, 5, 8, 4)
complexity_lines$closed     <- c(3, 18, 20, 36, 23)
complexity_lines$both       <- c(0, 7, 13, 12, 7)
# ROLES
complexity_lines$soft_eng   <- c(5, 29, 32, 56, 32)
complexity_lines$sys_eng    <- c(0, 3, 7, 16, 8)
complexity_lines$sys_arc    <- c(0, 5, 8, 26, 15)
complexity_lines$sys_admin  <- c(0, 6, 4, 8, 5)
complexity_lines$devops     <- c(0, 7, 8, 21, 17)
complexity_lines$proj_maint <- c(0, 4, 9, 18, 9)
complexity_lines$proj_manag <- c(1, 9, 11, 13, 10)
# EXPERIENCE
complexity_lines$exp_1      <- c()
complexity_lines$exp_2      <- c()
complexity_lines$exp_3      <- c()
complexity_lines$exp_4      <- c()
complexity_lines$exp_5      <- c()
complexity_lines$exp_6      <- c()
# PROJECT SIZE
complexity_lines$psize_1    <- c()
complexity_lines$psize_2    <- c()
complexity_lines$psize_3    <- c()
complexity_lines$psize_4    <- c()
complexity_lines$psize_5    <- c()

conflicting_files <- list()
conflicting_files$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
conflicting_files$everybody  <- c(10, 69, 50, 26, 6)
# GENDER
conflicting_files$male       <- c(7, 64, 44, 25, 6)
conflicting_files$female     <- c(1, 2, 4, 1, 0)
# SOURCE DISTRIBUTION MODEL
conflicting_files$open       <- c(3, 8, 9, 2, 1)
conflicting_files$closed     <- c(5, 43, 31, 18, 3)
conflicting_files$both       <- c(2, 18, 10, 6, 2)
# ROLES
conflicting_files$soft_eng   <- c(10, 66, 48, 24, 6)
conflicting_files$sys_eng    <- c(2, 15, 12, 4, 1)
conflicting_files$sys_arc    <- c(3, 17, 19, 14, 1)
conflicting_files$sys_admin  <- c(0, 12, 6, 5, 0)
conflicting_files$devops     <- c(2, 19, 21, 11, 0)
conflicting_files$proj_maint <- c(3, 16, 12, 8, 1)
conflicting_files$proj_manag <- c(5, 20, 13, 5, 1)
# EXPERIENCE
conflicting_files$exp_1      <- c()
conflicting_files$exp_2      <- c()
conflicting_files$exp_3      <- c()
conflicting_files$exp_4      <- c()
conflicting_files$exp_5      <- c()
conflicting_files$exp_6      <- c()
# PROJECT SIZE
conflicting_files$psize_1    <- c()
conflicting_files$psize_2    <- c()
conflicting_files$psize_3    <- c()
conflicting_files$psize_4    <- c()
conflicting_files$psize_5    <- c()

complexity_files <- list()
complexity_files$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
complexity_files$everybody  <- c(8, 34, 49, 51, 18)
# GENDER
complexity_files$male       <- c(8, 31, 44, 45, 18)
complexity_files$female     <- c(0, 0, 3, 5, 0)
# SOURCE DISTRIBUTION MODEL
complexity_files$open       <- c(1, 5, 4, 9, 3)
complexity_files$closed     <- c(3, 18, 35, 34, 10)
complexity_files$both       <- c(4, 11, 10, 8, 5)
# ROLES
complexity_files$soft_eng   <- c(8, 34, 45, 50, 16)
complexity_files$sys_eng    <- c(1, 6, 7, 13, 5)
complexity_files$sys_arc    <- c(2, 9, 13, 21, 8)
complexity_files$sys_admin  <- c(0, 4, 6, 10, 3)
complexity_files$devops     <- c(2, 6, 18, 17, 9)
complexity_files$proj_maint <- c(3, 7, 13, 13, 4)
complexity_files$proj_manag <- c(4, 10, 14, 12, 4)
# EXPERIENCE
complexity_files$exp_1      <- c()
complexity_files$exp_2      <- c()
complexity_files$exp_3      <- c()
complexity_files$exp_4      <- c()
complexity_files$exp_5      <- c()
complexity_files$exp_6      <- c()
# PROJECT SIZE
complexity_files$psize_1    <- c()
complexity_files$psize_2    <- c()
complexity_files$psize_3    <- c()
complexity_files$psize_4    <- c()
complexity_files$psize_5    <- c()

nonfunctional_changes <- list()
nonfunctional_changes$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
nonfunctional_changes$everybody  <- c(47, 63, 31, 15, 4)
# GENDER
nonfunctional_changes$male       <- c(41, 57, 30, 14, 3)
nonfunctional_changes$female     <- c(3, 3, 1, 0, 1)
# SOURCE DISTRIBUTION MODEL
nonfunctional_changes$open       <- c(6, 10, 6, 0, 0)
nonfunctional_changes$closed     <- c(32, 35, 20, 8, 4)
nonfunctional_changes$both       <- c(9, 18, 5, 7, 0)
# ROLES
nonfunctional_changes$soft_eng   <- c(47, 58, 31, 12, 4)
nonfunctional_changes$sys_eng    <- c(11, 15, 6, 1, 0)
nonfunctional_changes$sys_arc    <- c(12, 19, 15, 6, 1)
nonfunctional_changes$sys_admin  <- c(7, 7, 8, 0, 0)
nonfunctional_changes$devops     <- c(13, 16, 17, 4, 2)
nonfunctional_changes$proj_maint <- c(12, 16, 9, 2, 0)
nonfunctional_changes$proj_manag <- c(17, 14, 8, 3, 1)
# EXPERIENCE
nonfunctional_changes$exp_1      <- c()
nonfunctional_changes$exp_2      <- c()
nonfunctional_changes$exp_3      <- c()
nonfunctional_changes$exp_4      <- c()
nonfunctional_changes$exp_5      <- c()
nonfunctional_changes$exp_6      <- c()
# PROJECT SIZE
nonfunctional_changes$psize_1    <- c()
nonfunctional_changes$psize_2    <- c()
nonfunctional_changes$psize_3    <- c()
nonfunctional_changes$psize_4    <- c()
nonfunctional_changes$psize_5    <- c()

components_dependencies <- list()
components_dependencies$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
components_dependencies$everybody  <- c(20, 56, 39, 33, 14)
# GENDER
components_dependencies$male       <- c(19, 51, 32, 32, 13)
components_dependencies$female     <- c(0, 2, 5, 1, 0)
# SOURCE DISTRIBUTION MODEL
components_dependencies$open       <- c(3, 10, 7, 2, 1)
components_dependencies$closed     <- c(12, 35, 22, 21, 10)
components_dependencies$both       <- c(5, 11, 10, 10, 3)
# ROLES
components_dependencies$soft_eng   <- c(20, 53, 37, 30, 14)
components_dependencies$sys_eng    <- c(5, 5, 12, 10, 2)
components_dependencies$sys_arc    <- c(4, 14, 15, 15, 6)
components_dependencies$sys_admin  <- c(2, 8, 5, 6, 2)
components_dependencies$devops     <- c(7, 12, 17, 12, 5)
components_dependencies$proj_maint <- c(8, 11, 10, 9, 2)
components_dependencies$proj_manag <- c(10, 14, 10, 7, 3)
# EXPERIENCE
components_dependencies$exp_1      <- c()
components_dependencies$exp_2      <- c()
components_dependencies$exp_3      <- c()
components_dependencies$exp_4      <- c()
components_dependencies$exp_5      <- c()
components_dependencies$exp_6      <- c()
# PROJECT SIZE
components_dependencies$psize_1    <- c()
components_dependencies$psize_2    <- c()
components_dependencies$psize_3    <- c()
components_dependencies$psize_4    <- c()
components_dependencies$psize_5    <- c()

atomic_changesets <- list()
atomic_changesets$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
atomic_changesets$everybody  <- c(20, 48, 51, 29, 13)
# GENDER
atomic_changesets$male       <- c(20, 43, 44, 27, 12)
atomic_changesets$female     <- c(0, 3, 4, 1, 0)
# SOURCE DISTRIBUTION MODEL
atomic_changesets$open       <- c(0, 5, 9, 6, 2)
atomic_changesets$closed     <- c(14, 32, 34, 12, 8)
atomic_changesets$both       <- c(6, 11, 8, 11, 3)
# ROLES
atomic_changesets$soft_eng   <- c(20, 46, 48, 27, 12)
atomic_changesets$sys_eng    <- c(3, 10, 11, 8, 2)
atomic_changesets$sys_arc    <- c(3, 12, 21, 15, 3)
atomic_changesets$sys_admin  <- c(2, 6, 11, 4, 0)
atomic_changesets$devops     <- c(6, 16, 17, 10, 4)
atomic_changesets$proj_maint <- c(6, 10, 11, 12, 1)
atomic_changesets$proj_manag <- c(4, 18, 14, 7, 1)
# EXPERIENCE
atomic_changesets$exp_1      <- c()
atomic_changesets$exp_2      <- c()
atomic_changesets$exp_3      <- c()
atomic_changesets$exp_4      <- c()
atomic_changesets$exp_5      <- c()
atomic_changesets$exp_6      <- c()
# PROJECT SIZE
atomic_changesets$psize_1    <- c()
atomic_changesets$psize_2    <- c()
atomic_changesets$psize_3    <- c()
atomic_changesets$psize_4    <- c()
atomic_changesets$psize_5    <- c()

expertise_in_code <- list()
expertise_in_code$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
expertise_in_code$everybody  <- c(5, 23, 50, 54, 30)
# GENDER
expertise_in_code$male       <- c(5, 21, 43, 51, 27)
expertise_in_code$female     <- c(0, 1, 5, 0, 2)
# SOURCE DISTRIBUTION MODEL
expertise_in_code$open       <- c(0, 3, 4, 12, 4)
expertise_in_code$closed     <- c(3, 11, 34, 31, 21)
expertise_in_code$both       <- c(2, 9, 12, 11, 5)
# ROLES
expertise_in_code$soft_eng   <- c(5, 21, 46, 53, 29)
expertise_in_code$sys_eng    <- c(1, 1, 7, 16, 9)
expertise_in_code$sys_arc    <- c(0, 5, 14, 20, 15)
expertise_in_code$sys_admin  <- c(0, 1, 8, 9, 5)
expertise_in_code$devops     <- c(0, 4, 13, 20, 16)
expertise_in_code$proj_maint <- c(0, 3, 11, 18, 8)
expertise_in_code$proj_manag <- c(1, 6, 10, 18, 9)
# EXPERIENCE
expertise_in_code$exp_1      <- c()
expertise_in_code$exp_2      <- c()
expertise_in_code$exp_3      <- c()
expertise_in_code$exp_4      <- c()
expertise_in_code$exp_5      <- c()
expertise_in_code$exp_6      <- c()
# PROJECT SIZE
expertise_in_code$psize_1    <- c()
expertise_in_code$psize_2    <- c()
expertise_in_code$psize_3    <- c()
expertise_in_code$psize_4    <- c()
expertise_in_code$psize_5    <- c()