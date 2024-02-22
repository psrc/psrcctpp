# psrcctpp

Tools to utilize Census Transportation Planning Package (CTPP) data for geographies in the Central Puget Sound Region.

Install with the following command: `devtools::install_github("psrc/psrcctpp")`

If you are having problems installing the package, make sure you have all the latest dependency packages. You may be prompted to install some dependency packages after that; you can install all by selecting option 1, "Install all".

This package requires a valid CTPP API key; to obtain one, visit `https://ctppdata.transportation.dev/ -> Login -> Create an account`. Once successfully logged in the options associated with your account (at top right of screen, hover username) include "Manage API Keys". Create a key there, then make an .renviron entry named `CTPP_API_KEY` with this value, in single quotes (the command `usethis::edit_r_environ()` conveniently locates and opens the file for you).
