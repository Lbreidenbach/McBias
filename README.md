# McBias
McBias is an r package that utilizes Monte Carlo simulations to help estimate the effects of bias found in datasets of convenience.

## **<ins>NOTE: McBias uses rjags which requires the JAGS library to be installed separately *outside* of R!</ins>** 
*Please see below for JAGS library installation instructions.*

*JAGS library installation:*
Install JAGS at this link (https://sourceforge.net/projects/mcmc-jags/). Click the green "Download" button and follow the installation wizard's instructions.

**Linux** users should use the following command to install JAGS: 
`sudo apt install jags`

<ins>After JAGS is installed:</ins>, 
**Windows** users will need to install rtools from the following link: https://cran.r-project.org/bin/windows/Rtools/ 


McBias can now be installed in R via devtools with the following command:
```
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
    
devtools::install_github("Lbreidenbach/McBias")
```


=======


