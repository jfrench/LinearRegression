# packages related to books
books = c("faraway", "alr4", "car", "rms")
install.packages(books)
# packages related to tidy/tidying data
tidy = c("broom", "tidyr", "dplyr", "tidyverse")
install.packages(tidy)
# packages related to plotting
moreplots = c("ggplot2", "ggthemes", "lattice", "HH")
install.packages(moreplots)
# packages related to model diagnostics
diag = c("leaps", "lmtest", "gvlma", "caret")
install.packages(diag)
# packages related to workflow
workflow = c("remotes")
install.packages(workflow)

# install perturb package from GitHub
remotes::install_github(repo = "JohnHendrickx/Perturb")

