RunR
===========

**This no longer works as of Sept 28, 2016, since Nike+ has shut down their public API because they suck.  I'm switching to Strava.**

R scripts to interface with the Nike+ API and provide basic stats and visualization of your running data.

Modified from synergenz's original repo to work with the current Nike+ API, which seems to have changed.  I haven't updated any of the gps/geo scripts.

Example analysis in 'main.R' file.  
Add your Nike+ access token as the first line in the corresponding file in the Data folder. 
Looks for existing runstats file in the Data folder. 
I just use this code to update my existing runstats file.  
You might need to modify to pull some data for the first time.  
Makes some plots about running progress using ggplot xkcd style.
