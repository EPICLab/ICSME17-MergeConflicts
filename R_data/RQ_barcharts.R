library(ggplot2)

plot.Likert <- function(values, scales, title, GROUPING = "none") {
  vmean         <- sum(values * seq_along(values))/sum(values)
  veffect       <- if (vmean<2.9) {"No Effect"} else if (vmean>3.1) {"Effect"} else {"50/50"}
  if (length(scales) != length(values)) stop("scales must be the same length as values")
  if (GROUPING == "aggressive") {
    if (length(values) != 5) stop("values must be length 5 for GROUPING=aggressive")
    lower_grp   <- sum(sapply(values[1:2], function(x) (x/sum(values)*100)))
    higher_grp  <- sum(sapply(values[3:5], function(x) (x/sum(values)*100)))
    values      <- c(lower_grp, higher_grp)
    scales      <- c(scales[1], scales[5])
  } else if (GROUPING == "conservative") {
    if (length(values) != 5) stop("values must be length 5 for GROUPING=conservative")
    lower_grp   <- sum(sapply(values[1:3], function(x) (x/sum(values)*100)))
    higher_grp  <- sum(sapply(values[4:5], function(x) (x/sum(values)*100)))
    values      <- c(lower_grp, higher_grp)
    scales      <- c(scales[1], scales[5])
  } else if (GROUPING == "donut") {
    if (length(values) != 5) stop("values must be length 5 for GROUPING=donut")
    lower_grp   <- sum(sapply(values[1:2], function(x) (x/sum(values)*100)))
    higher_grp  <- sum(sapply(values[4:5], function(x) (x/sum(values)*100)))
    values      <- c(lower_grp, higher_grp)
    scales      <- c(scales[1], scales[5])
  }
  percents <- sapply(values, function(x) (x/sum(values)*100))
  bplot <- barplot(percents, main=title, ylim=c(0,100), names.arg=scales)
  text(x = bplot, y = percents, label = paste0(round(percents, 2),"%"), pos = 3, cex = 0.8)
  mtext(text= paste0("mean: ",round(vmean, 2)," (",veffect,")"), side=1, line=3)
}

plotter <- list()
# GENDER
plotter.Gender <- function(target, GROUP = "none") {
  plot.Likert(target$female, target$scale, "Female", GROUPING=GROUP)
  plot.Likert(target$male, target$scale, "Male", GROUPING=GROUP)
  plot.Likert(target$everybody, target$scale, "Full Population", GROUPING=GROUP)
}
# SOURCE DISTRIBUTION MODEL
plotter.Source_Model <- function(target, GROUP = "none") {
  plot.Likert(target$both, target$scale, "Split Evenly", GROUPING=GROUP)
  plot.Likert(target$closed, target$scale, "Closed-Source", GROUPING=GROUP)
  plot.Likert(target$open, target$scale, "Open-Source", GROUPING=GROUP)
  plot.Likert(target$everybody, target$scale, "Full Population", GROUPING=GROUP)
}
# ROLES
plotter.Roles <- function(target, GROUP = "none") {
  plot.Likert(target$proj_manag, target$scale, "Project Managers", GROUPING=GROUP)
  plot.Likert(target$proj_maint, target$scale, "Project Maintainers", GROUPING=GROUP)
  plot.Likert(target$devops, target$scale, "DevOps", GROUPING=GROUP)
  plot.Likert(target$sys_admin, target$scale, "Systems Administrators", GROUPING=GROUP)
  plot.Likert(target$sys_arc, target$scale, "System Architects", GROUPING=GROUP)
  plot.Likert(target$sys_eng, target$scale, "System Engineers", GROUPING=GROUP)
  plot.Likert(target$soft_eng, target$scale, "Software Engineers", GROUPING=GROUP)
  plot.Likert(target$everybody, target$scale, "Full Population", GROUPING=GROUP)
}
# EXPERIENCE
plotter.Experience <- function(target, GROUP = "none") {
  plot.Likert(target$exp_6, target$scale, "26+ Years", GROUPING=GROUP)
  plot.Likert(target$exp_5, target$scale, "21-25 Years", GROUPING=GROUP)
  plot.Likert(target$exp_4, target$scale, "16-20 Years", GROUPING=GROUP)
  plot.Likert(target$exp_3, target$scale, "11-15 Years", GROUPING=GROUP)
  plot.Likert(target$exp_2, target$scale, "6-10 Years", GROUPING=GROUP)
  plot.Likert(target$exp_1, target$scale, "1-5 Years", GROUPING=GROUP)
  plot.Likert(target$everybody, target$scale, "Full Population", GROUPING=GROUP)
}
# PROJECT SIZE
plotter.Project_Size <- function(target, GROUP = "none") {
  plot.Likert(target$psize_5, target$scale, "51+ Developers", GROUPING=GROUP)
  plot.Likert(target$psize_4, target$scale, "11-50 Developers", GROUPING=GROUP)
  plot.Likert(target$psize_3, target$scale, "6-10 Developers", GROUPING=GROUP)
  plot.Likert(target$psize_2, target$scale, "2-5 Developers", GROUPING=GROUP)
  plot.Likert(target$psize_1, target$scale, "1 Developer", GROUPING=GROUP)
  plot.Likert(target$everybody, target$scale, "Full Population", GROUPING=GROUP)
}

#############################################################################################
# Q50_2: How useful would the following changes be in your merge conflict resolution tools? #
#############################################################################################
# (1) better_transparency                                                                   #
# (2) better_usability                                                                      #
# (3) filtering_data                                                                        #
# (4) graphical_presentation                                                                #
# (5) exploring_history                                                                     #
# (6) consistent_terminology                                                                #
#############################################################################################

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
# ROLES
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
time_to_resolve$exp_1      <- c(5, 17, 13, 3, 4)
time_to_resolve$exp_2      <- c(4, 21, 15, 15, 4)
time_to_resolve$exp_3      <- c(3, 8, 9, 3, 0)
time_to_resolve$exp_4      <- c(1, 6, 4, 1, 4)
time_to_resolve$exp_5      <- c(1, 2, 1, 0, 1)
time_to_resolve$exp_6      <- c(0, 2, 9, 3, 2)
# PROJECT SIZE
time_to_resolve$psize_1    <- c(0, 3, 3, 1, 0)
time_to_resolve$psize_2    <- c(6, 31, 20, 12, 10)
time_to_resolve$psize_3    <- c(7, 6, 15, 7, 3)
time_to_resolve$psize_4    <- c(0, 9, 7, 4, 1)
time_to_resolve$psize_5    <- c(1, 7, 6, 1, 1)

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
conflicting_lines$exp_1      <- c(2, 10, 14, 14, 2)
conflicting_lines$exp_2      <- c(0, 17, 21, 15, 6)
conflicting_lines$exp_3      <- c(0, 5, 14, 4, 1)
conflicting_lines$exp_4      <- c(0, 4, 6, 6, 0)
conflicting_lines$exp_5      <- c(0, 3, 1, 1, 0)
conflicting_lines$exp_6      <- c(0, 1, 8, 5, 2)
# PROJECT SIZE
conflicting_lines$psize_1    <- c(0, 2, 4, 2, 0)
conflicting_lines$psize_2    <- c(1, 20, 29, 21, 8)
conflicting_lines$psize_3    <- c(0, 12, 15, 10, 1)
conflicting_lines$psize_4    <- c(1, 3, 9, 6, 2)
conflicting_lines$psize_5    <- c(0, 3, 7, 6, 0)

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
complexity_lines$exp_1      <- c(3, 10, 9, 11, 9)
complexity_lines$exp_2      <- c(1, 15, 15, 17, 11)
complexity_lines$exp_3      <- c(0, 3, 4, 11, 6)
complexity_lines$exp_4      <- c(0, 1, 4, 9, 2)
complexity_lines$exp_5      <- c(1, 0, 2, 1, 1)
complexity_lines$exp_6      <- c(0, 0, 4, 7, 5)
# PROJECT SIZE
complexity_lines$psize_1    <- c(0, 2, 2, 3, 1)
complexity_lines$psize_2    <- c(2, 18, 21, 21, 17)
complexity_lines$psize_3    <- c(2, 4, 7, 15, 10)
complexity_lines$psize_4    <- c(0, 2, 6, 9, 4)
complexity_lines$psize_5    <- c(1, 3, 2, 8, 2)

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
conflicting_files$exp_1      <- c(2, 16, 12, 7, 4)
conflicting_files$exp_2      <- c(3, 32, 16, 6, 2)
conflicting_files$exp_3      <- c(2, 9, 10, 3, 0)
conflicting_files$exp_4      <- c(1, 4, 6, 5, 0)
conflicting_files$exp_5      <- c(1, 2, 1, 1, 0)
conflicting_files$exp_6      <- c(1, 6, 5, 4, 0)
# PROJECT SIZE
conflicting_files$psize_1    <- c(0, 2, 4, 0, 1)
conflicting_files$psize_2    <- c(4, 35, 22, 15, 3)
conflicting_files$psize_3    <- c(1, 19, 13, 4, 1)
conflicting_files$psize_4    <- c(1, 6, 8, 5, 1)
conflicting_files$psize_5    <- c(4, 7, 3, 2, 0)

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
complexity_files$exp_1      <- c(2, 9, 12, 12, 6)
complexity_files$exp_2      <- c(4, 15, 20, 17, 2)
complexity_files$exp_3      <- c(0, 4, 7, 8, 5)
complexity_files$exp_4      <- c(0, 4, 4, 5, 3)
complexity_files$exp_5      <- c(2, 1, 0, 1, 1)
complexity_files$exp_6      <- c(0, 1, 6, 8, 1)
# PROJECT SIZE
complexity_files$psize_1    <- c(0, 1, 2, 4, 1)
complexity_files$psize_2    <- c(5, 18, 25, 21, 9)
complexity_files$psize_3    <- c(1, 8, 10, 15, 4)
complexity_files$psize_4    <- c(0, 2, 9, 6, 3)
complexity_files$psize_5    <- c(2, 5, 3, 5, 1)

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
nonfunctional_changes$exp_1      <- c(14, 16, 8, 1, 2)
nonfunctional_changes$exp_2      <- c(14, 23, 13, 7, 1)
nonfunctional_changes$exp_3      <- c(9, 8, 4, 2, 1)
nonfunctional_changes$exp_4      <- c(5, 3, 4, 4, 0)
nonfunctional_changes$exp_5      <- c(4, 1, 0, 0, 0)
nonfunctional_changes$exp_6      <- c(1, 12, 2, 1, 0)
# PROJECT SIZE
nonfunctional_changes$psize_1    <- c(1, 2, 2, 2, 0)
nonfunctional_changes$psize_2    <- c(27, 28, 15, 6, 2)
nonfunctional_changes$psize_3    <- c(11, 16, 5, 4, 2)
nonfunctional_changes$psize_4    <- c(4, 7, 8, 2, 0)
nonfunctional_changes$psize_5    <- c(4, 10, 1, 1, 0)

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
components_dependencies$exp_1      <- c(4, 14, 11, 10, 3)
components_dependencies$exp_2      <- c(6, 25, 13, 10, 5)
components_dependencies$exp_3      <- c(5, 7, 5, 3, 4)
components_dependencies$exp_4      <- c(2, 4, 4, 5, 1)
components_dependencies$exp_5      <- c(1, 2, 1, 1, 0)
components_dependencies$exp_6      <- c(2, 4, 5, 4, 1)
# PROJECT SIZE
components_dependencies$psize_1    <- c(0, 1, 4, 3, 0)
components_dependencies$psize_2    <- c(15, 27, 10, 18, 9)
components_dependencies$psize_3    <- c(5, 11, 13, 7, 2)
components_dependencies$psize_4    <- c(0, 10, 8, 1, 2)
components_dependencies$psize_5    <- c(0, 7, 4, 4, 1)

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
atomic_changesets$exp_1      <- c(6, 13, 13, 5, 4)
atomic_changesets$exp_2      <- c(6, 19, 22, 8, 4)
atomic_changesets$exp_3      <- c(3, 7, 6, 6, 2)
atomic_changesets$exp_4      <- c(2, 5, 2, 5, 2)
atomic_changesets$exp_5      <- c(1, 2, 1, 1, 0)
atomic_changesets$exp_6      <- c(2, 2, 7, 4, 1)
# PROJECT SIZE
atomic_changesets$psize_1    <- c(0, 0, 5, 1, 1)
atomic_changesets$psize_2    <- c(14, 23, 24, 12, 6)
atomic_changesets$psize_3    <- c(6, 16, 9, 6, 1)
atomic_changesets$psize_4    <- c(0, 7, 7, 5, 2)
atomic_changesets$psize_5    <- c(0, 2, 6, 5, 3)

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
expertise_in_code$exp_1      <- c(1, 10, 16, 9, 6)
expertise_in_code$exp_2      <- c(3, 7, 17, 26, 6)
expertise_in_code$exp_3      <- c(0, 4, 6, 5, 9)
expertise_in_code$exp_4      <- c(1, 1, 2, 6, 6)
expertise_in_code$exp_5      <- c(0, 0, 3, 2, 0)
expertise_in_code$exp_6      <- c(0, 1, 6, 6, 3)
# PROJECT SIZE
expertise_in_code$psize_1    <- c(0, 3, 2, 2, 1)
expertise_in_code$psize_2    <- c(3, 13, 25, 27, 11)
expertise_in_code$psize_3    <- c(2, 5, 12, 11, 8)
expertise_in_code$psize_4    <- c(0, 0, 8, 5, 8)
expertise_in_code$psize_5    <- c(0, 2, 3, 9, 2)

######################################################################################################
# Q36: Please rate how much each of these factor into the difficulty of resolving a merge conflicts? #
######################################################################################################
#  (1) code_info                                                                                     #
#  (2) code_expertise                                                                                #
#  (3) ease_understand                                                                               #
#  (4) info_presentation                                                                             #
#  (5) trustworthiness                                                                               #
#  (6) tools_history                                                                                 #
#  (7) proj_complexity                                                                               #
#  (8) commit_messages                                                                               #
#  (9) changing_assumptions                                                                          #
# (10) proj_culture                                                                                  #
######################################################################################################
code_info <- list()
code_info$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
code_info$everybody  <- c(2, 21, 38, 48, 32)
# GENDER
code_info$male       <- c(2, 19, 35, 42, 31)
code_info$female     <- c(0, 2, 0, 4, 1)
# SOURCE DISTRIBUTION MODEL
code_info$open       <- c(1, 1, 8, 7, 3)
code_info$closed     <- c(0, 15, 23, 33, 19)
code_info$both       <- c(1, 5, 7, 8, 10)
# ROLES
code_info$soft_eng   <- c(2, 18, 36, 47, 31)
code_info$sys_eng    <- c(0, 3, 9, 11, 6)
code_info$sys_arc    <- c(0, 4, 13, 19, 11)
code_info$sys_admin  <- c(0, 3, 10, 8, 0)
code_info$devops     <- c(0, 4, 10, 18, 15)
code_info$proj_maint <- c(0, 4, 7, 12, 10)
code_info$proj_manag <- c(1, 5, 11, 11, 10)
# EXPERIENCE
code_info$exp_1      <- c(0, 8, 7, 11, 10)
code_info$exp_2      <- c(2, 6, 14, 15, 12)
code_info$exp_3      <- c(0, 5, 6, 8, 4)
code_info$exp_4      <- c(0, 2, 1, 5, 4)
code_info$exp_5      <- c(0, 0, 2, 2, 1)
code_info$exp_6      <- c(0, 0, 8, 7, 1)
# PROJECT SIZE
code_info$psize_1    <- c(0, 2, 2, 3, 0)
code_info$psize_2    <- c(1, 7, 20, 21, 17)
code_info$psize_3    <- c(1, 10, 5, 12, 6)
code_info$psize_4    <- c(0, 1, 5, 8, 6)
code_info$psize_5    <- c(0, 1, 6, 4, 3)

code_expertise <- list()
code_expertise$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
code_expertise$everybody  <- c(1, 17, 38, 49, 36)
# GENDER
code_expertise$male       <- c(1, 15, 33, 46, 34)
code_expertise$female     <- c(0, 1, 4, 2, 0)
# SOURCE DISTRIBUTION MODEL
code_expertise$open       <- c(0, 0, 8, 8, 4)
code_expertise$closed     <- c(0, 13, 22, 29, 26)
code_expertise$both       <- c(1, 4, 8, 12, 6)
# ROLES
code_expertise$soft_eng   <- c(1, 15, 35, 47, 36)
code_expertise$sys_eng    <- c(0, 2, 9, 9, 9)
code_expertise$sys_arc    <- c(0, 4, 9, 18, 16)
code_expertise$sys_admin  <- c(0, 3, 7, 7, 4)
code_expertise$devops     <- c(0, 4, 9, 19, 15)
code_expertise$proj_maint <- c(0, 2, 12, 8, 11)
code_expertise$proj_manag <- c(0, 5, 12, 10, 11)
# EXPERIENCE
code_expertise$exp_1      <- c(0, 6, 10, 10, 10)
code_expertise$exp_2      <- c(1, 4, 16, 19, 9)
code_expertise$exp_3      <- c(0, 4, 2, 7, 10)
code_expertise$exp_4      <- c(0, 2, 1, 5, 4)
code_expertise$exp_5      <- c(0, 1, 3, 0, 1)
code_expertise$exp_6      <- c(0, 0, 6, 8, 2)
# PROJECT SIZE
code_expertise$psize_1    <- c(0, 1, 4, 2, 0)
code_expertise$psize_2    <- c(1, 6, 21, 20, 18)
code_expertise$psize_3    <- c(0, 7, 9, 12, 6)
code_expertise$psize_4    <- c(0, 1, 2, 9, 8)
code_expertise$psize_5    <- c(0, 2, 2, 6, 4)

ease_understand <- list()
ease_understand$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
ease_understand$everybody  <- c(0, 14, 25, 65, 37)
# GENDER
ease_understand$male       <- c(0, 11, 23, 63, 32)
ease_understand$female     <- c(0, 2, 1, 2, 2)
# SOURCE DISTRIBUTION MODEL
ease_understand$open       <- c(0, 1, 1, 10, 8)
ease_understand$closed     <- c(0, 10, 16, 43, 21)
ease_understand$both       <- c(0, 3, 8, 12, 8)
# ROLES
ease_understand$soft_eng   <- c(0, 13, 23, 64, 34)
ease_understand$sys_eng    <- c(0, 3, 2, 13, 11)
ease_understand$sys_arc    <- c(0, 1, 4, 24, 18)
ease_understand$sys_admin  <- c(0, 1, 2, 11, 7)
ease_understand$devops     <- c(0, 3, 7, 20, 17)
ease_understand$proj_maint <- c(0, 3, 4, 19, 7)
ease_understand$proj_manag <- c(0, 4, 5, 14, 15)
# EXPERIENCE
ease_understand$exp_1      <- c(0, 7, 9, 14, 6)
ease_understand$exp_2      <- c(0, 3, 4, 32, 10)
ease_understand$exp_3      <- c(0, 1, 6, 7, 9)
ease_understand$exp_4      <- c(0, 1, 0, 4, 7)
ease_understand$exp_5      <- c(0, 2, 1, 0, 2)
ease_understand$exp_6      <- c(0, 0, 5, 8, 3)
# PROJECT SIZE
ease_understand$psize_1    <- c(0, 2, 1, 2, 2)
ease_understand$psize_2    <- c(0, 5, 13, 31, 17)
ease_understand$psize_3    <- c(0, 6, 4, 18, 6)
ease_understand$psize_4    <- c(0, 0, 3, 9, 8)
ease_understand$psize_5    <- c(0, 1, 4, 5, 4)

info_presentation <- list()
info_presentation$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
info_presentation$everybody  <- c(4, 24, 47, 32, 34)
# GENDER
info_presentation$male       <- c(3, 22, 43, 29, 32)
info_presentation$female     <- c(1, 2, 1, 2, 1)
# SOURCE DISTRIBUTION MODEL
info_presentation$open       <- c(0, 5, 5, 5, 5)
info_presentation$closed     <- c(3, 16, 28, 21, 22)
info_presentation$both       <- c(1, 3, 14, 6, 7)
# ROLES
info_presentation$soft_eng   <- c(4, 23, 45, 29, 33)
info_presentation$sys_eng    <- c(0, 8, 7, 9, 5)
info_presentation$sys_arc    <- c(0, 7, 11, 12, 17)
info_presentation$sys_admin  <- c(0, 3, 7, 8, 3)
info_presentation$devops     <- c(0, 9, 13, 14, 11)
info_presentation$proj_maint <- c(1, 7, 8, 11, 6)
info_presentation$proj_manag <- c(1, 10, 8, 8, 11)
# EXPERIENCE
info_presentation$exp_1      <- c(3, 7, 13, 6, 7)
info_presentation$exp_2      <- c(1, 12, 16, 12, 8)
info_presentation$exp_3      <- c(0, 2, 9, 6, 6)
info_presentation$exp_4      <- c(0, 1, 2, 3, 6)
info_presentation$exp_5      <- c(0, 2, 1, 0, 2)
info_presentation$exp_6      <- c(0, 0, 6, 5, 5)
# PROJECT SIZE
info_presentation$psize_1    <- c(0, 4, 2, 1, 0)
info_presentation$psize_2    <- c(1, 8, 21, 17, 19)
info_presentation$psize_3    <- c(2, 10, 8, 7, 7)
info_presentation$psize_4    <- c(1, 1, 8, 5, 5)
info_presentation$psize_5    <- c(0, 1, 8, 2, 3)

trustworthiness <- list()
trustworthiness$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
trustworthiness$everybody  <- c(17, 29, 39, 32, 24)
# GENDER
trustworthiness$male       <- c(14, 26, 35, 31, 23)
trustworthiness$female     <- c(3, 0, 3, 0, 1)
# SOURCE DISTRIBUTION MODEL
trustworthiness$open       <- c(1, 6, 5, 3, 5)
trustworthiness$closed     <- c(14, 14, 25, 24, 13)
trustworthiness$both       <- c(2, 9, 9, 5, 6)
# ROLES
trustworthiness$soft_eng   <- c(17, 28, 37, 31, 21)
trustworthiness$sys_eng    <- c(5, 9, 6, 3, 6)
trustworthiness$sys_arc    <- c(2, 13, 12, 10, 10)
trustworthiness$sys_admin  <- c(1, 4, 8, 4, 4)
trustworthiness$devops     <- c(7, 10, 11, 12, 7)
trustworthiness$proj_maint <- c(7, 7, 8, 7, 4)
trustworthiness$proj_manag <- c(4, 11, 5, 8, 10)
# EXPERIENCE
trustworthiness$exp_1      <- c(10, 2, 12, 7, 5)
trustworthiness$exp_2      <- c(5, 14, 11, 13, 6)
trustworthiness$exp_3      <- c(0, 7, 7, 5, 4)
trustworthiness$exp_4      <- c(0, 2, 5, 3, 2)
trustworthiness$exp_5      <- c(1, 2, 0, 0, 2)
trustworthiness$exp_6      <- c(1, 2, 4, 4, 5)
# PROJECT SIZE
trustworthiness$psize_1    <- c(2, 1, 4, 0, 0)
trustworthiness$psize_2    <- c(8, 10, 18, 17, 13)
trustworthiness$psize_3    <- c(6, 10, 7, 5, 6)
trustworthiness$psize_4    <- c(1, 5, 4, 8, 2)
trustworthiness$psize_5    <- c(0, 3, 6, 2, 3)

tools_history <- list()
tools_history$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
tools_history$everybody  <- c(16, 40, 31, 32, 22)
# GENDER
tools_history$male       <- c(14, 35, 28, 30, 22)
tools_history$female     <- c(2, 1, 3, 1, 0)
# SOURCE DISTRIBUTION MODEL
tools_history$open       <- c(0, 5, 4, 5, 6)
tools_history$closed     <- c(13, 28, 18, 21, 10)
tools_history$both       <- c(3, 7, 9, 6, 6)
# ROLES
tools_history$soft_eng   <- c(16, 39, 28, 31, 20)
tools_history$sys_eng    <- c(3, 10, 6, 6, 4)
tools_history$sys_arc    <- c(4, 9, 12, 11, 11)
tools_history$sys_admin  <- c(2, 7, 5, 5, 2)
tools_history$devops     <- c(4, 14, 10, 10, 9)
tools_history$proj_maint <- c(3, 7, 10, 6, 7)
tools_history$proj_manag <- c(4, 10, 8, 7, 9)
# EXPERIENCE
tools_history$exp_1      <- c(10, 10, 6, 5, 5)
tools_history$exp_2      <- c(4, 19, 11, 12, 3)
tools_history$exp_3      <- c(0, 8, 5, 4, 6)
tools_history$exp_4      <- c(2, 0, 4, 4, 2)
tools_history$exp_5      <- c(0, 3, 1, 0, 1)
tools_history$exp_6      <- c(0, 0, 4, 7, 5)
# PROJECT SIZE
tools_history$psize_1    <- c(0, 2, 4, 1, 0)
tools_history$psize_2    <- c(8, 20, 9, 17, 12)
tools_history$psize_3    <- c(6, 10, 9, 6, 3)
tools_history$psize_4    <- c(2, 4, 6, 3, 5)
tools_history$psize_5    <- c(0, 4, 3, 5, 2)

proj_complexity <- list()
proj_complexity$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
proj_complexity$everybody  <- c(6, 38, 39, 41, 17)
# GENDER
proj_complexity$male       <- c(6, 34, 36, 36, 17)
proj_complexity$female     <- c(0, 1, 2, 4, 0)
# SOURCE DISTRIBUTION MODEL
proj_complexity$open       <- c(1, 4, 7, 7, 1)
proj_complexity$closed     <- c(3, 28, 23, 24, 12)
proj_complexity$both       <- c(2, 6, 9, 10, 4)
# ROLES
proj_complexity$soft_eng   <- c(5, 37, 37, 39, 16)
proj_complexity$sys_eng    <- c(2, 8, 6, 10, 3)
proj_complexity$sys_arc    <- c(2, 8, 9, 17, 11)
proj_complexity$sys_admin  <- c(1, 2, 6, 7, 5)
proj_complexity$devops     <- c(3, 10, 11, 14, 9)
proj_complexity$proj_maint <- c(3, 7, 7, 11, 5)
proj_complexity$proj_manag <- c(2, 10, 9, 11, 6)
# EXPERIENCE
proj_complexity$exp_1      <- c(1, 14, 10, 8, 3)
proj_complexity$exp_2      <- c(2, 11, 15, 16, 5)
proj_complexity$exp_3      <- c(1, 6, 6, 9, 1)
proj_complexity$exp_4      <- c(1, 3, 1, 3, 4)
proj_complexity$exp_5      <- c(0, 2, 2, 1, 0)
proj_complexity$exp_6      <- c(1, 2, 5, 4, 4)
# PROJECT SIZE
proj_complexity$psize_1    <- c(0, 2, 2, 2, 1)
proj_complexity$psize_2    <- c(5, 17, 19, 17, 8)
proj_complexity$psize_3    <- c(1, 8, 9, 13, 3)
proj_complexity$psize_4    <- c(0, 7, 4, 4, 5)
proj_complexity$psize_5    <- c(0, 4, 5, 5, 0)

commit_messages <- list()
commit_messages$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
commit_messages$everybody  <- c(18, 32, 30, 44, 17)
# GENDER
commit_messages$male       <- c(17, 30, 27, 40, 15)
commit_messages$female     <- c(1, 0, 1, 4, 1)
# SOURCE DISTRIBUTION MODEL
commit_messages$open       <- c(2, 4, 5, 7, 2)
commit_messages$closed     <- c(13, 22, 19, 26, 10)
commit_messages$both       <- c(3, 6, 6, 11, 5)
# ROLES
commit_messages$soft_eng   <- c(18, 30, 29, 41, 16)
commit_messages$sys_eng    <- c(4, 5, 7, 12, 1)
commit_messages$sys_arc    <- c(3, 5, 15, 16, 8)
commit_messages$sys_admin  <- c(0, 5, 6, 7, 3)
commit_messages$devops     <- c(6, 8, 11, 15, 7)
commit_messages$proj_maint <- c(2, 5, 8, 13, 5)
commit_messages$proj_manag <- c(4, 4, 13, 12, 5)
# EXPERIENCE
commit_messages$exp_1      <- c(7, 10, 6, 9, 4)
commit_messages$exp_2      <- c(4, 12, 10, 17, 6)
commit_messages$exp_3      <- c(5, 5, 3, 6, 4)
commit_messages$exp_4      <- c(0, 3, 5, 3, 1)
commit_messages$exp_5      <- c(1, 1, 1, 2, 0)
commit_messages$exp_6      <- c(1, 1, 5, 7, 2)
# PROJECT SIZE
commit_messages$psize_1    <- c(1, 2, 1, 2, 1)
commit_messages$psize_2    <- c(7, 18, 11, 19, 11)
commit_messages$psize_3    <- c(6, 7, 10, 10, 1)
commit_messages$psize_4    <- c(3, 0, 5, 8, 4)
commit_messages$psize_5    <- c(1, 5, 3, 5, 0)

changing_assumptions <- list()
changing_assumptions$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
changing_assumptions$everybody  <- c(8, 27, 45, 36, 25)
# GENDER
changing_assumptions$male       <- c(8, 24, 39, 35, 23)
changing_assumptions$female     <- c(0, 2, 4, 1, 0)
# SOURCE DISTRIBUTION MODEL
changing_assumptions$open       <- c(2, 3, 7, 5, 3)
changing_assumptions$closed     <- c(3, 21, 27, 22, 17)
changing_assumptions$both       <- c(3, 3, 11, 9, 5)
# ROLES
changing_assumptions$soft_eng   <- c(8, 26, 42, 36, 22)
changing_assumptions$sys_eng    <- c(2, 5, 6, 10, 6)
changing_assumptions$sys_arc    <- c(2, 5, 15, 15, 10)
changing_assumptions$sys_admin  <- c(1, 4, 6, 6, 4)
changing_assumptions$devops     <- c(4, 5, 14, 14, 10)
changing_assumptions$proj_maint <- c(4, 5, 10, 8, 6)
changing_assumptions$proj_manag <- c(4, 8, 12, 9, 5)
# EXPERIENCE
changing_assumptions$exp_1      <- c(1, 10, 12, 8, 5)
changing_assumptions$exp_2      <- c(3, 8, 18, 11, 9)
changing_assumptions$exp_3      <- c(2, 5, 6, 5, 5)
changing_assumptions$exp_4      <- c(0, 2, 2, 5, 3)
changing_assumptions$exp_5      <- c(0, 2, 2, 1, 0)
changing_assumptions$exp_6      <- c(2, 0, 5, 6, 3)
# PROJECT SIZE
changing_assumptions$psize_1    <- c(0, 0, 2, 4, 1)
changing_assumptions$psize_2    <- c(5, 12, 22, 17, 10)
changing_assumptions$psize_3    <- c(3, 9, 10, 6, 6)
changing_assumptions$psize_4    <- c(0, 3, 7, 5, 5)
changing_assumptions$psize_5    <- c(0, 3, 4, 4, 3)

proj_culture <- list()
proj_culture$scale <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
proj_culture$everybody  <- c(13, 37, 43, 27, 21)
# GENDER
proj_culture$male       <- c(11, 35, 40, 25, 18)
proj_culture$female     <- c(1, 1, 3, 1, 1)
# SOURCE DISTRIBUTION MODEL
proj_culture$open       <- c(3, 4, 7, 3, 3)
proj_culture$closed     <- c(8, 25, 23, 19, 15)
proj_culture$both       <- c(2, 8, 13, 5, 3)
# ROLES
proj_culture$soft_eng   <- c(13, 35, 39, 26, 21)
proj_culture$sys_eng    <- c(5, 6, 6, 9, 3)
proj_culture$sys_arc    <- c(5, 8, 11, 13, 10)
proj_culture$sys_admin  <- c(1, 6, 3, 6, 5)
proj_culture$devops     <- c(8, 10, 7, 12, 10)
proj_culture$proj_maint <- c(7, 8, 7, 8, 3)
proj_culture$proj_manag <- c(7, 9, 8, 9, 5)
# EXPERIENCE
proj_culture$exp_1      <- c(5, 10, 12, 6, 3)
proj_culture$exp_2      <- c(3, 13, 14, 9, 10)
proj_culture$exp_3      <- c(2, 6, 7, 5, 3)
proj_culture$exp_4      <- c(1, 4, 1, 4, 2)
proj_culture$exp_5      <- c(1, 1, 3, 0, 0)
proj_culture$exp_6      <- c(1, 3, 6, 3, 0)
# PROJECT SIZE
proj_culture$psize_1    <- c(1, 1, 3, 2, 0)
proj_culture$psize_2    <- c(6, 18, 21, 13, 8)
proj_culture$psize_3    <- c(5, 10, 8, 4, 7)
proj_culture$psize_4    <- c(1, 3, 7, 6, 3)
proj_culture$psize_5    <- c(0, 5, 4, 2, 3)