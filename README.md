# shinyQCDashboard
QC dashboard for proteomics using R and shinydashboard

Created and tested by Egor Vorontsov. Questions and concerns by e-mail at gmail: yegor dot msu.

The dashboard makes use of the key QC values that are stored in an SQLite database. In our lab, the QC runs are injections of 50 ng of a Pierce HeLa digest, and the database is filled with the output values for Proteome Discoverer search that are processed via the "QC_Script_PD2.4" Python script.

The app consists of one long page and is based on the R packages shiny/shinydashboard:

<img src="https://github.com/dev-ev/shinyQCDashboard/blob/master/shinyQC_screenshot_1.PNG" alt="drawing" width="400"/>

To select the instrument that is displayed, use the date pickers on the left:

![Instrument dropdown](https://github.com/dev-ev/shinyQCDashboard/blob/master/shinyQC_screenshot_5rs.PNG)


To change the time interval that is displayed, use the date pickers on the left:

![Date select](https://github.com/dev-ev/shinyQCDashboard/blob/master/shinyQC_screenshot_4rs.PNG)


The main QC table contains the following columns:

search_id INTEGER PRIMARY KEY,
raw_file TEXT NOT NULL,
file_date TEXT,
search_date TEXT,
instrument TEXT,
protein_number INTEGER,
peptide_number INTEGER NOT NULL,
psm_number INTEGER NOT NULL,
msms_number INTEGER NOT NULL,
id_rate REAL,
mean_psm_it_ms REAL,
median_psm_it_ms REAL,
mean_msms_it_ms REAL,
median_msms_it_ms REAL,
mean_mz_err_ppm REAL,
median_mz_err_ppm REAL,
mz_err_ppm_stdev REAL,
total_prec_intensity REAL,
mean_prec_intensity REAL,
mean_sengine_score REAL,
mean_peak_width REAL,
peak_width_stdev REAL,
pept_416 REAL,
pept_425 REAL,
pept_488 REAL,
pept_495 REAL,
pept_567 REAL,
pept_652 REAL,
pept_655 REAL,
comment TEXT

The "service" table contains the following columns:

procedure_id INTEGER PRIMARY KEY,
date TEXT NOT NULL,
type TEXT,
is_pm TEXT,
comment TEXT
