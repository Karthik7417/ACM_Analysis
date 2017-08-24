# ACM_Analysis
Analysis of the ACM Digital Library to find emerging trends and technologies 

ACM Analysis:
For this analysis we have downloaded data from the ACM digital library website. A web scraper was built to aggregate data for a list of selected topics (Big Data, Cloud Computing, Data Analytics, Service oriented architecture, WAN, Wikis, LAN, Expert Systems, MapReduce, Blogs). 
The files for stage 1 are in the ACM_preprocessing directory. Files for stage 2 are in the ACM_Analysis directory. 
Stage 1: Pre-processing
There are two web-scraper scripts to download the data. 	
1.	Document_Author_scraper.R script is located in the ACM_Pre Processing directory. The list of topics to be scraped can be changed by adding new topics to the keywords variable. Data produced by this script contains download statistics for each individual paper. 

Here is a sample of the data that is being scraped with this script. 

Output files from this script are saved with topic name followed by a “_doc_links.csv”. This document contains data like title, authors, download count, publication date, link. 

2.	author_scraper.R  script uses the links generated by the previous script to scrape data for each specific author. The output files from this scraper are saved with topic name followed by a “_data.csv”. Data produced by this script contains publication count, citation count and other statistics for each individual author. 

Here is a sample of the data that is to be generated. 
 
There is a Shiny app that displays the data from the topics “Data Analytics”, “Big Data” and “Blogs” in the form of a table. You can use the ui.R script or server.R script to launch the application.
This concludes the data extraction phase. The next steps are the analysis and visualization phases. 

Stage 2: Analysis
 The Affiliation_Gephi_Analysis.R script imports the two files that we created in the precious stage for every topic and generates the nodelist and the edgelist required to generate Gephi visualizations. 

Data manipulations done in the script are:
1.	Breaking the date into Month and Year columns. Reorganizing the date structure. 
2.	Identifying whether the affiliation is a university 

Gephi:
The nodelist and edgelist files for each topic are loaded based on the year and the clusters are computed in Gephi and saved as Member data.
 