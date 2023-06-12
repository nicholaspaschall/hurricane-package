# hurricane-package

R package for performing analysis related to the HURDAT Atlantic basin tropical cyclone data. 

This package includes the necessary dataset, **HURDAT**, along with five functions: **interp_track**, **storm_size**, **track_map**, **cyclone_energy**, and **landfall**. The description of each are as follows.


• *interp_track*: calculates the interpolated latitude and longitude coordinates of a storm's center for each 30 minutes throughout the storm's duration<br>
• *track_map*: generates a map displaying a selection of storm tracks or selective points in time for given storms.<br>
• *storm_size*: generates a map displaying the position and size of a storm along with the storm's track, if specified. Storm position determined by its latitude and longitude, and storm size determined by its wind raddi at 34kn, 50kn, and 64kn<br>
• *cyclone_energy*: calculates the accumulated cyclone energy of a given storm based on its max wind, location, and time. The output is in 10e-04 kn^2 units.<br>
• *landfall*: either determines if a storm made landfall, produces a storm's strongest landfall, or generates a data frame containing information regarding its track and when and where it was over land or sea.

Applications of these functions can be found in the **hurricane.vignette.pdf**.
