The R files allow for reproducing the results in the published article 

Daus, S., Nilsen, T., & Braeken, J. (2018). Exploring Content Knowledge: Country Profile of Science Strengths and Weaknesses in TIMSS. Possible Implications for Educational Professionals and Science Research. Scandinavian Journal of Educational Research. doi:10.1080/00313831.2018.1478882


The syntax is optimized for running the Stan model at a high-performance cluster at the University of Oslo (Abel cluster), with the folder structure for the project. Your setup will likely deviate from this configuration, and hence the authors take no responsibility for your actions nor can we guarantee to offer help in this regard. 

General steps to take:
1. Download all the TIMSS 2011 database to a folder. Syntax expects code to be organized in folders by type (SA, SG, TS, etc). Strictly speaking only SA is needed.
2. Configure the folder paths in 01_config_paths.R to your liking. Ensure that all folders exist and are writable. Unfortunately the folder-structure in this project was needlessly complicated.
3. Run the main files in alphabetical order. Note that the authors were forced to install rstan on a particular R version to get it to work on the Linux-based server. Your setup might be simpler.
4. C_prep_int.R will process the data for all countries and store to disk. This design was intended to easy parallel processing of multiple countries 
5. D_prep_country.R will grab a country and set up the data in memory for a given country. Hard-coded to run only Norway. Can also be used in distributed processing for taking a country in a list of countries.
6. E_analysis_rstan.R is intended to be run with about 60GB RAM across 4 cores (4 chains). Will in turn source t1-stan/t1-stan-1.R which contains the actual stan code and parameters.
7. F_analysis_post.R is intended to be run with 60GB on a single core. Parallell processing is not useful here. Will in turn run the remaining files in the t1-stan folder. #2 will compute parameters from the posterior distribution. #3 will summarize the distribution. #4 will produce diagnostic plots. #5 will output logit estimates to Excel workbook. #6 will compute posterior predictions of the item proportions, and outputs the results. #7 will produce Figure 2 in the article.




