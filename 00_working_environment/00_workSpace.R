# Author: Viet Nguyen, faculty of Forest and Environment, HNEE
# Email: Duc.Nguyen@hnee.de
# The Script was compiled with reference to materials from the course
# LiDAR data collection and analysis taught by Dr. Nikolai Knapp at
# Thuenen Institute of Forest Ecosystems


# Check the folder where R load packages
.libPaths()

# Set the folder where R load packages (if necessary)
getwd()
.libPaths(paste0(getwd(),"/renv/library/R-4.2/x86_64-w64-mingw32"))
.libPaths()
# or your system's default R installation
# .libPaths("C:/Users/PC/AppData/Local/R/win-library/4.2")


###############################
#### Packages installation ####
###############################

# Not necessary to install packages, because all packages were included in
# this project via renv folder.

# install.packages("raster")
# install.packages("rgdal")
# install.packages("sf")
# install.packages("rgl")
# install.packages("lidR")
# install.packages("rgeos")
# install.packages("RCSF")
# install.packages("BiocManager")
# BiocManager::install("EBImage")


###########################
#### Load the packages ####
###########################

# Geographic Data (raster) Analysis and Modeling
library(raster)
#Bindings for the 'Geospatial' Data Abstraction Library
library(rgdal)
# Simple Features (vector data) for R 
library(sf)
# 3D Visualization Using OpenGL
library(rgl)
# Airborne LiDAR Data Manipulation and Visualization for Forestry Applications
library(lidR)
# Geometry handling
library(rgeos)
# Fast processing of big dataframe
library(data.table)


# Parallel computation in lidR
get_lidr_threads()
set_lidr_threads(10) # recommended to use haft of total threads
get_lidr_threads()
