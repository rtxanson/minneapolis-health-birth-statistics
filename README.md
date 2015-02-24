# Minneapolis Birth Statistics

This is liberated data from the [Minneapolis Health Department reports][mhd], previously available only in almost machine readable PDF format.

  [mhd]: http://www.minneapolismn.gov/health/

To access the source data:

 1. Go here http://www.minneapolismn.gov/health/reports/index.htm 
 2. Scroll down to the dropdown and then and select a year. Download will begin.

![Sample map](https://raw.githubusercontent.com/rtxanson/minneapolis-health-birth-statistics/master/README.png "Sample map")

[See a sample map](http://goo.gl/yCJ7Sb) on highlighting adequacy of care during pregnancy, by neighborhood. Be sure to click the neighborhoods for an infobox containing more data.

## Cleanup

This is sort of documented by way of the `Makefile`.

1. [pdftotext][pdftotext] - to extract the text 

    pdftotext -enc UTF-8 -table path/to/ugly.pdf

2. I wrote a parser for the text data, in `parser/births/`

3. Imported the data into ~~Exc~~ Google ~~Do~~ Drive Spreadsheet, and
    connected it with a city neighborhood dataset to clean up the neighborhood
    names. I have added a couple columns to help: the original ordering in the
    data source, city_gis_neighborhood_id, and city_gis_corrected_name.

I've done some spotchecking, but if you notice inconsistencies, please tell me.

  [pdftotext]: http://www.foolabs.com/xpdf/download.html

## Notes

The field names are insanely long, because I didn't want to have to write
documentation. If something is unclear, refer to original PDFs.

The "Unknown" neighborhood is not a surprise conspiracy neighborhood that no
one knows about, it is neighborhoods listed as unknown in the survey.

### Specific file notes

* `2006-2008.csv` - Does not contain Unknown data, and Mid-City Industrial neighborhood is not present

## TODOs

There is still more data to release from the PDFs:

 * Community data
 * City data
 * More years


