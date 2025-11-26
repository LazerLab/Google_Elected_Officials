'''
Take input from house_parquet_no_error, calculate similarity for search results.

Input: parquet version of results
Output: csv of similarity
Based on: /home/guozhen1/google_audit_reproduce/workflow/old/result_similarity/get_loc_rbo_sim.py 
'''

import sys
import pandas as pd
import rbo
import numpy as np
import re
import random
import os

os.chdir("/home/aywan/internal_websearch")

def get_new_day_df(date_file):
    input_file = os.path.join(RAW_PARQUET_PATH, date_file)
    
    df = pd.read_parquet(input_file)
    
    # More efficient string replacements using vectorized operations
    loc_replacements = {
        'Congressional District 1,Hawaii,United States': 'HI-1,Hawaii,United States',
        'Congressional District 6,California,United States': 'CA-6,California,United States', 
        'Congressional District 45,California,United States': 'CA-45,California,United States'
    }
    df["loc_id"] = df["loc_id"].replace(loc_replacements)
    df['loc_id'] = df['loc_id'].str.split(',').str[0]

    return df

def dedup_urls(urls):
    seen = set()
    out = []
    for url in urls:
        if url not in seen:
            seen.add(url)
            out.append(url)
    return out


def sample_different_date(row, date_pool):
    existing_dates = {value for col, value in row.items() if col.startswith('old_date')}
    
    # Filter out those dates from the date pool
    possible_dates = [d for d in date_pool if d not in existing_dates]
    
    return np.random.choice(possible_dates)
    
def get_results_by_date(rbo_sample_df, date):
    df = get_new_day_df(date)
    #df = pd.merge(df, house_domain_clean, on='domain', how='left')
    
    date_samples = rbo_sample_df[rbo_sample_df["date_file"] == date]
    date_samples = date_samples.loc[:, ~date_samples.columns.str.startswith('old_date')]
    
    date_samples_long1 = date_samples[['qry', 'loc_1']]
    date_samples_long1.columns = ['qry', 'loc_id']
    date_samples_long2 = date_samples[['qry', 'loc_2']]
    date_samples_long2.columns = ['qry', 'loc_id']
    date_samples_long= pd.concat([date_samples_long1, date_samples_long2], axis=0).drop_duplicates()
    
    df_filter =  df.merge(
        date_samples_long[['qry', 'loc_id']].drop_duplicates(),
        on=['qry', 'loc_id'],
        how='inner'
    )
    
    df_filter = df_filter[~df_filter["url"].str.startswith("/search?q=")]
    df_filter = df_filter.dropna(subset=['url'])
    
    df_sorted = df_filter.sort_values(by=['qry', 'loc_id', 'serp_rank'])
    
    df_group = (
        df_sorted
        .groupby(['qry', 'loc_id'])['url']
        .apply(dedup_urls)
        .reset_index(name='url_list')
    )
    
    df1 = df_group.rename(columns={'loc_id': 'loc_1', 'url_list': 'url_list1'})
    df2 = df_group.rename(columns={'loc_id': 'loc_2', 'url_list': 'url_list2'})
    df_pairs = (
            date_samples
            .merge(df1, on=['qry', 'loc_1'], how='left')
            .merge(df2, on=['qry', 'loc_2'], how='left')
        )
        
    df_pairs = df_pairs.dropna(subset=['url_list1', 'url_list2'])

    df_pairs_copy = df_pairs.copy()
    #print(df_pairs_copy.head())
    if len(df_pairs_copy) > 0:

        df_pairs_copy['rbo'] = df_pairs_copy.apply(
                lambda row: rbo.RankingSimilarity(row['url_list1'], row['url_list2']).rbo(),
                axis=1
            )
    
    merged = date_samples.merge(df_pairs_copy[['qry', 'loc_1', 'loc_2']], 
                       on=['qry','loc_1', 'loc_2'], how='left', indicator=True)
    
            # Filter where the pair was not found in df2
    resample_df = merged[merged['_merge'] == 'left_only'].drop(columns=['_merge'])

    return df_pairs_copy, resample_df


def get_results_df(rbo_sample_df, result_file_path):
    #house_domain_clean = house_domain[['domain', 'news']]

    date_list = rbo_sample_df['date_file'].drop_duplicates().to_list()

    resample = []
    result = []
    i = 1
    for date in date_list:
        print(str(i))
        result_df, resample_df = get_results_by_date(rbo_sample_df, date)
        
        resample.append(resample_df)
        result.append(result_df)
 
        #if len(result_df)>0:
            #if os.path.exists(result_file_path):
               # old_df = pd.read_parquet(result_file_path)
                #result_df =  pd.concat([old_df, result_df], axis=0, ignore_index=True)
            #result_df.to_parquet(result_file_path)
        i = i+1
    
    resample_df_big = pd.concat(resample, ignore_index=True)
    result_df_big = pd.concat(result, ignore_index=True)

    if os.path.exists(result_file_path):
        old_df = pd.read_parquet(result_file_path)
        result_df_big =  pd.concat([old_df, result_df_big], axis=0, ignore_index=True)
    result_df_big.to_parquet(result_file_path)
    
    return(resample_df_big)
    
if __name__ == "__main__":

    DATA_ROOT = "/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files"
    RAW_PARQUET_PATH = os.path.join(DATA_ROOT, "summary_by_day")

    #house_domain = pd.read_csv("data/house_domain_final_122024.csv")
    rbo_sample_df = pd.read_parquet("data/rbo_sim_sampling.parquet")
    #rbo_sample_done = pd.read_parquet("data/rbo_result.parquet")

    #merged = rbo_sample_df_orig.merge(rbo_sample_done[['qry', 'loc_1', 'loc_2', 'date_file']], 
                      #on=['qry','loc_1', 'loc_2', 'date_file'], how='left', indicator=True)
    
            # Filter where the pair was not found in df2
    #resample_df = merged[merged['_merge'] == 'left_only'].drop(columns=['_merge'])
    #resample_df = pd.read_parquet("data/resample_df.parquet")
    #print(len(resample_df))

    all_dates = rbo_sample_df['date_file'].drop_duplicates().to_list()

    #all_dates = rbo_sample_df_orig['date_file'].drop_duplicates().to_list()

    
    resample_df = get_results_df(rbo_sample_df, #date_list,#house_domain,
                                       "data/rbo_result.parquet")

    i = 0
    while len(resample_df) >0:
        # add new ls
        print("resample round " + str(i)) 
        new_col_name = 'old_date'+str(i)
        resample_df = resample_df.rename(columns={'date_file': new_col_name })

        resample_df['date_file'] = resample_df.apply(lambda row: sample_different_date(row, all_dates), axis=1)

        resample_df = get_results_df(resample_df,"data/rbo_result.parquet")
        i = i+1
        if i>4:
            break