# UK business analysis

Including bits for analysing the FAME dataset (not open, via university license), Companies House data (open! Lots of info!), ONS data.

## Useful bits and bobs that live here:

* [Report on ONS Business Demography](https://danolner.github.io/FirmAnalysis/ONS_business_demography.html) dataset (quarto doc [here](https://github.com/DanOlner/FirmAnalysis/blob/master/docs/QUARTO_ONS_businessdemography.qmd); explanation of harmonising horrors [blogged about here](https://danolner.github.io/posts/business_demography/)).
* Companies House UK data file geocoded in [companieshouse.R](https://github.com/DanOlner/FirmAnalysis/blob/master/companieshousedata.R). Currently filters down to South Yorkshire, but uses the latest [Codepoint Open](https://osdatahub.os.uk/downloads/open/CodePointOpen) (via [this script](https://github.com/DanOlner/utilities/blob/master/postcodes_to_localauthorities_makelookup.R) in the utilities repo) and so can be used to filter to anywhere.  
