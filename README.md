# Oncology Dashboard
The purpose of this project is to build a system wide oncology dashboard that can be used to explore new and established KPIs.  Some of the dahsbaord are currently being run manually so we are working with MArcy Cohen and Thomas Waslh to automate existing dashboards that the Oncology department currently uses. 

# OAO Team Members
Hala Sweidan, So Youn Kweon, Armando Villegas, Asala Erekat

# R Version
This application uses R version 3.6.1.  Please use the link to downlaod the correct R version: https://mirror.las.iastate.edu/CRAN/bin/windows/base/old/3.6.1/
# Installation
To run this application download the Volume Anlayis branch in this repo and then run the following files in the order they are written:
  1. Future_Oncology_Global.R
  2. ui.R
  3. Oncology_Server.R<br/>
After all three files have finished running, use the RStudio console to run the command: shinyApp(ui,server)

