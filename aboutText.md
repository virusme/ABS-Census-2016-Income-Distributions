ABS Census 2016 - Incomes
=========================
This web-app provides some insights into income distributions across postcodes. Incomes (Individual and Households) are a subset of [ABS Census 2016](http://www.abs.gov.au/websitedbs/censushome.nsf/home/2016) data. We were interested in exploring this data in order to find some insights into a few questions, e.g: what are the income distributions across age, industry per post code? how does the distribution of household incomes stack up against mortgage repayments, etc per postcode? Hope you find this web-app useful.

Extracting data from [Census TableBuilder](http://www.abs.gov.au/websitedbs/censushome.nsf/home/tablebuilder) is not a trivial task, it takes a lot of effort to scrape the data. Ideally, extracting data should not be that hard but for some reason, we found it tricky. Data is available free of cost, which was helpful.

We were able to get the postcode to latitude/longitude mapping from [Corra](http://www.corra.com.au/australian-postcode-location-data/), making it easier for us to build a web-app that can be navigated using the maps.

If you have any questions or suggestions to improve this web-app, please email us at info@theportfoliotrader.com

Contributors:

* [VirusMe](https://github.com/virusme)
* [The Portfolio Trader](https://www.theportfoliotrader.com)



### Note:

* We tried finding the sweet spot between displaying the maps vs distribution. Map had to be shown for navigation and small distribution chart are useless to read. We finally arrived at this particular UI interface, where distributions comes to foreground when required. But a small quirk remains, that the map can only be dragged around using the left portion of the screen.</li>
* If you want to explore the distribution for a particular postcode, make sure to zoom-in enough to display only that postcode marker on the screen.</li>
* GitHub: Most of data pre-processing is done using [R](https://www.r-project.org/) and the web-app is built using [Shiny](https://shiny.rstudio.com/). Source code will be available on GitHub soon.
* We have not tested this web-app on a mobile device, so don't be surprised if the UI gives you a lot of headache on a mobile-device.


