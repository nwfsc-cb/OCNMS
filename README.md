# OCNMS


OK.  This is the OCNMS github repository.  It has all of the data, scripts, and other files associated with the OCNMS diving surveys and associated data.  It also has a bunch of pdfs and such for reference.  These make the repo kind of big but whatever.

I have maintained included all of the data and scripts that were used in the 2018 Shelton et al. paper as well as the stuff we have done more recently.  I am planning on doing a big clean out in the not too distant future.... but don't hold your breath.

Big picture, diving data is in /Data.  The flat files of most use are in /CSV_2015_on.  Markdown documents (in /Markdown Docs) typically call scripts the /R scripts file to do their dirty work.  

In general, I try and limit hard coding of paths to one line at the beginning of each document.  So if your code isn't working, look there first.

Otherwise, good hunting.


## ANNUAL UPDATES

The code to run an annual update is in the '_00_Annual-Update' folder (surprise). There are separate qmd files for fish, inverts and algae, and UPC (in progress). They are separate because the data are available at different times. 

MOST DATA is house in the ~Github/OCNMS/DATA folder under the appropriate year. Data files include raw and processed data. '*.rds' files with 'Transect' in the title are probably what most people want. 

# There are three main R files

(1) _01_Annual_update_FISH.qmd
(2) _02_Annual_update_KELP-INVERTS.qmd
(3) _03_Annual_update_UPC.qmd (placeholder, not yet)

## There are also multiple helper files: 

(1) 'settings.r' has multiple settings and other common info across main files like: minimum visibility, a common ggplot theme for figures, code to run some other source files, etc.
(2) R-functions-ocnms.R contains various helper functions for common tasks
(3) 'update-swath-process-pre-2015-data.r' cleans up and combines the 2015 and post-2015 data
(4) 'update-swath-fish.r' and 'update-swath-inverts' further process the data. However, these data contain size and count data, so there will be multiple observations per species per transect.  

Each Annual_update file downloads the data from google drive, sources the 'update' files to clean up the data. There is then additional code to summarize the data to transect level and other binning for plotting. These files also plot data. 

# The data files

## Starting files

(1) NWFSC_FISH_ALLYEARS_data_20**.xlsm (or csv) - raw data. Containes 2016+ data. Includes sizes for fish.
(2) NWFSC_SWATH_ALLYEARS_data_20**.xlsm (or csv) - raw data. Containes 2016+ data.
(3) NWFSC_UPC_ALLYEARS_data_20**.xlsm (or csv) - raw data. 

## Output files

(1) Fish_2015-20**.rds (or csv). Processed fish data. NOT by transect. Includes size class (large, small) to distinguish rockfish YOY but no actual sizes.

(2) Swath_2015-20**.rds (or csv). Processed swath data. NOT by transect. No sizes

## Summarized files

(1) Summarized_Data_Fish_by_Transect.csv.  Count by transect. Size class large or small.
(2) Summarized_Data_Kelp_by_Transect.csv.  Count by transect. 
(3) Summarized_Data_Inverts_by_Transect.csv.  Count by transect. No sizes

### Known problems. 

The code currently sets the home_dir using the "~/Github/..." approach, which works for PCs but not Apple computers. I am working to update this glitch.

I am working on adding a similar file for UPC.














