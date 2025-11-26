import pandas as pd
import os

OUTFILE = "data/unique_results.csv"

# raw data, unavailable with replication code
PATH = "/net/data-backedup/web_search/intermediate_files/preprocessing/parquet_version"

# orig, CHANGE: "/net/data-backedup/lab-lazer/web_search/intermediate_files/preprocessing/parquet_version"

results_list = os.listdir(PATH)

results_list = [x for x in results_list if "results-" in x]

for r in results_list:  
    print(r)
    results = pd.read_parquet(PATH +'/'+r)
    results_filter = results[results['url'].isna()==False]
    results_filter = results_filter[["type", "title", "url", "text","qry","subtitle"]]
    results_filter = results_filter[results_filter['title']!="View all"]
    results_filter = results_filter.drop_duplicates()
    
    if os.path.exists(outfile) == True:
        existing_file = pd.read_csv(OUTFILE)
        results_filter = pd.concat([existing_file, results_filter], ignore_index=True) 
        results_filter = results_filter.drop_duplicates()
    results_filter.to_csv(OUTFILE, index=False)