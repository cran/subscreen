# subscreen 1.0.0

## Bug fixes subscreencalc

* Fixed the example to make it work properly. Added some data pre-processing and handling of NAs. 
* Fixed the problem with max_comb=1 in combination with nkernel=1. Function sapply needed the option simplify=FALSE to keep the data structure  
* No error anymore if nkernel>1 and par_functions=""

## Bug fixes subscreenshow

* Factor levels 6 to 8 will now be displayed. They had no color assigned before.
* Reference line for overall result is now exact on the right place. The slider input has been removed as this caused inappropriate rounding in some cases.


## Enhancements subscreenshow

* The slider for the y-range is now improved. It will use nice numbers for the range selection. Thanks to Tommy (662787) from StackOverflow for roundDownNice(). And you can actually give a set of numbers you think of being nice in the new parameter NiceNumbers.
* Background shading including marks can be set by the new parameter StripesBGplot. The program will aim for the given number of stripes/marks but the actual display may differ to have nice intervals
* On the x-axis percentages of the total are shown

## Editorial changes

* Updated the description
* Packages shiny and DT are now imports although only needed in subscreenshow 
* Spelling errors corrected
* Changed some internal function and variable names for better readability
* Translated the rest of the German comments into English
* Deleted unused functions and program code
* Removed NA from the subgroup filter drop-down selection

# subscreen 2.0.0

* New Layout
* Added Subscreen Comparer
* Added Subscreen Mosaic
* Added Variable Importance calculation
* Added Colour Options Panel
* Added Display Options Panel
* Added identification of parent subgroups 
* Added option to memorize subgroups