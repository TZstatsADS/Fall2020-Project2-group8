# Project 2: Shiny App Development

The App directory contains the app files for the Shiny App (ui.R, server.R and global.R).
 - ui.R and server.R are two key components for the Shiny App:     
       ui.R controls the UI of the Shiny App;
       server.R implemented plots and interactive functions
 - data_processing.R is used to preprocess the data
 - global.R reads the processed data and define functions that used in server.R. When using two R files to process data, the performance of the app would be accelarated.
 - output folder contains processed data used for deployment and it will be reproduced once data_processing.R runs.
