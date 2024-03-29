# shiny-QC-dashboard
QC dashboard for proteomics using R and shiny/shinydashboard. Created and tested with Orbitrap mass spectrometers in mind.

The dashboard makes use of the key QC values that are stored in an SQLite database. During the development of the dashboard, the QC runs were injections of 50 ng of a HeLa cell tryptic digest, and the database is filled with the output values from Proteome Discoverer 2.4 search that are summarized and saved into an SQLite database by [the integrated *QC_Script_PD2.4*](https://github.com/dev-ev/qc-script-PD24) script.

The app consists of one long page and is based on the R packages shiny and shinydashboard:

<img src="https://github.com/dev-ev/shinyQCDashboard/blob/master/shinyQC_screenshot_1.PNG" alt="drawing" width="400"/>

Select the instrument using the dropdown menu on the left:

<img src="https://github.com/dev-ev/shinyQCDashboard/blob/master/shinyQC_screenshot_5.PNG" alt="drawing" width="350"/>


To change the time interval that is displayed, use the date pickers on the left:

<img src="https://github.com/dev-ev/shinyQCDashboard/blob/master/shinyQC_screenshot_4.PNG" alt="drawing" width="350"/>



