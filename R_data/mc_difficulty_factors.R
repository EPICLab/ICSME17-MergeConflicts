library(ggplot2)
# QUESTION 50 (v1): Factors that make merge conflicts difficult.

plotExtremes <- function(group, title) {
  percents <- c((group[1] + group[2] + group[3]/2)/sum(group)*100, (group[3]/2 + group[4] + group[5])/sum(group)*100)
  bplot <- barplot(percents, main=title, ylim=c(0,100), names.arg=c("No Effect", "Effect"))
  text(x = bplot, y = percents, label = percents, pos = 3, cex = 0.8)
}

plotGender <- function(target) {
  plotExtremes(target$everybody, "Everybody")
  plotExtremes(target$male, "Male")
  plotExtremes(target$female, "Female")
}

plotSourceModels <- function(target) {
  plotExtremes(target$everybody, "Everybody")
  plotExtremes(target$open, "Open")
  plotExtremes(target$closed, "Closed")
  plotExtremes(target$both, "Both")
}

plotRoles <- function(target) {
  plotExtremes(target$everybody, "Everybody")
  plotExtremes(target$soft_eng, "Software Engineers")
  plotExtremes(target$sys_eng, "System Engineers")
  plotExtremes(target$sys_arc, "System Architects")
  plotExtremes(target$sys_admin, "Systems Administrators")
  plotExtremes(target$devops, "DevOps")
  plotExtremes(target$proj_maint, "Project Maintainers")
  plotExtremes(target$proj_manag, "Project Managers")  
}

time_to_resolve <- list()
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

conflicting_lines <- list()
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

complexity_lines <- list()
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

conflicting_files <- list()
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

complexity_files <- list()
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

nonfunctional_changes <- list()
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

components_dependencies <- list()
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

atomic_changesets <- list()
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

expertise_in_code <- list()
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