# arse
Area of Resilience to Stress Event

# Description

A method for quantifying resilience after a stress event. A set of functions
calculate the area of resilience that is created by the departure of baseline 
'y' (i.e., robustness) and the time taken 'x' to return to baseline 
(i.e., rapidity) after a stress event using the Cartesian coordinates of the 
data. The `arse` package has the capability to calculate areas of resilience,
growth, and cases in which resilience is not achieved 
(e.g., diminished performance without return to baseline).

To study the area of resilience to stress event (`arse`), three things must 
be in place: (a) a baseline value (before the stress event) of a variable 
of interest 'y' needs to be known, (b) an incursion of a stress event needs 
to occur on an entity (e.g., individual, group), and (c) the variable of 
interest 'y' needs to be measured repeatedly after the incursion of a stress 
event. Thus, arse is the function of how much the variable 'y' decreases 
from baseline levels after a stress event (i.e., robustness) and the time it 
takes 'y' to return to baseline levels (i.e., rapidity). The combination of 
robustness and rapidity form a series of points that can be connected into an 
irregular polygon from which an area can be derived. It is this area, arse, 
that is indicative of how much resilience is demonstrated to a stress event 
where smaller values of arse indicate better resilience and larger values 
indicate worse resilience.

# Installation

The current official (i.e., CRAN) release can be installed directly within R with:

install.packages('arse')


After installing the devtools package with install.packages("devtools"), the development 
version of the arse package can be installed with:

devtools::install_github("nr3xe/arse")


