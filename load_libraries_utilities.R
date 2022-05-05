####################################################################################################
###########     Practical Methods for Measuring Algorithmic Fairness with Proxy Data      ##########
####################################################################################################

## Set working directory to source file location
setwd("~/fairness-assessments-proxies")

## Load libraries
library(RColorBrewer)
library(caret)
library(ltm)
library(rcompanion)
library(readODS)
library(googlesheets4)
library(haven)
library(Hmisc)
library(wru)
library(readxl)
library(httr)
library(jsonlite)
library(ggplot2)
library(ggmosaic)
library(data.table)
library(zoo)
library(plyr)
library(tidyverse)
library(plotly)

## Make sure dplyr is loaded last
library(dplyr)
detach(package:dplyr)
library(dplyr)