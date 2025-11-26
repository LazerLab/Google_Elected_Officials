## Introduction
This repository contains replication data and for "Searching for Elected Officials: Google’s Prioritization of Political Information."

## Directory Structure
- /scripts: for scripts to load the data and produce final figures metrics
- /data: for data

## Data
We provide all scripts required to reproduce our analyses. Due to large file sizes of the raw data, we publically provide cleaned and aggregated data (produced in Steps 1-4 described in the pipeline below) necessary to reproduce analyses, figures and tables in Step 5. The specific data we provide are: 

- `qry_info_house.csv` Search query level dataset with information about all members of the 116th House of Representatives including party, districts they represent, relevance score, and whether they are included in the data.
- `qry_domain_rank_joined.csv` Number of results at each domain-query-rank level. All domain level classifications (six main types, politician-controllled, partisan audience scores, local or national news, unreliable or reliable news are joined here).
- `qry_domain_rank_si.csv` Number of results at each domain-query-rank level using an alternative search result parsing method explored in the supplementary information of the paper.
- `rbo_result_r.parquet` All sampled location pairs with calculated rank-biased overlap, Jaccard Index, number of URLs in each search result, and set differences. The columns with the original lists of URLs are removed to reduce file sizes.
- `gtrends_final_20200109_20201231.csv` Google Trends data (relative search volume) of all members included in our analysis.
- `qry_day_url.parquet` Number of results at the query-day-URL level.

## Data Processing Pipeline
### 1. Code relevance score and create final query-level dataset
- Get all unique URLs (results) and some metadata for each search query. These query-URl pairs are sampled for handcoding.
  <br>File: `get_unique_results.py`
  
- Create the final search query level data set. Add a column with relevance score, and a column (`house_analysis_include`) that dummy codes whether the member is included as a search query. Members representing the 5 territories (American Samoa, Guam, Puerto Rico, Northern Mariana Islands, and the Virgin Islands), who have a relevence score less than 3, or were no longer in office by 9/1/2020 are excluded.
  <br>File: `create_final_qry_info.r`  

### 2. Parsing and Summarising Search Results
- Remove NA URLs and excluded component types. 
  <br>File: `drop_na_add_domain` folder containing Snakefile
  
- Group cleaned data from `drop_na_add_domain` into different levels and save. Makes subsequent analyses more efficient. 
  <br>File: `generate_analysis_summary` folder containing Snakefile

- Repeat with an alternative parsing method that counts grouped links as one result (explored in SI section B).
  <br>File: `si_preprocess` folder containing sub-level `drop_na_add_domain_si` and `generate_analysis_summary_si` folders

_Note: call `snakemake` from within the folder to run the Snakefiles_

### 3. Domain-level classification
- Categorize domains into six main types, Campaign/Personal, Education, Government, News, Other Third Party, or Social Media by matching to external data or using keyword classification
  <br>File: `label_domains.r`

- Pull all unique search query-URL pairs for all social media domains and house.gov for handcoding as politician controlled.
  <br>File: `prepare_poli_control_code.r`

- Merge all domain level classifications (six types, politican-controlled, partisan audience score, local or national news, reliable or unreliable news) into one dataset at the search query-domain-rank level. Create coverate rates table (SI Table S3).
  <br>File: `post_process_domains.r`

### 4. Analysis for variation by search result location  
- Sample location pairs
  <br>File: `get_rbo_sim_sample.r`
  
- Pull URLs from data generated in Step 2 and compute rank-biased overlap for each pair.
    <br>File: `get_rbo_sim.py`

- Calculate jaccard index and additional metadata for each pair.
    <br>File: `postprocess_rbo_sim.py`

### 5. Figures and Tables 
- Figure 2 and SI Table S2 (Concentration, Compostion, and Ranking)
  <br>File: `plots_fig2.r`

- Figure 3 (Distribution of search results by politician control, local or national news, or unreliable or reliable news)
  <br>File: `plots_fig3.r`

- Figure 4 (Partisan differences in search results)
  <br>File: `plots_fig4.r`

- Figure 5 and SI Figure S6 (Variation by locatioin)
    <br>File: `plots_rbo.r`

- SI Figure S1 and calculations for SI Section B (Comparison to Alternative Methods for Parsing Search Results)
<br>File: `plots_si_cmpt_rank.r`

- SI Figures S2 to S5, SI Tables S4 to S6 (Variation over time and by search query)
    <br>File: `plots_si.r`
  
