

# remove any prior installation of the package
remove.packages("mlts")

# install the latest version from github
## check if devtools package is available
if(!("devtools" %in% installed.packages())){
  install.packages("devtools")
  }

## install mlts from github
devtools::install_github("https://github.com/munchfab/mlts/tree/RDSEM")

# other packages that we will use
packages = c("dplyr", "tidyr", "ggplot2", "bayesplot", "osfr", "usethis")

for(i in packages){
  if(!(i %in% installed.packages())){
    install.packages(i)
  }  
}
