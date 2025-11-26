import pandas as pd
import os

os.chdir("~/internal_websearch")

def jaccard_similarity(list1, list2):
    set1, set2 = set(list1), set(list2)
    intersection = set1 & set2
    union = set1 | set2
    if not union:
        return 0.0  # Avoid division by zero
    return len(intersection) / len(union)

def len_setdiff(list1, list2):
    set_diff = set(list1)- set(list2)
    return len(set_diff)

rbo_sample_done = pd.read_parquet("data/rbo_result.parquet")

rbo_sample_done['list_len1'] = rbo_sample_done['url_list1'].apply(len)
rbo_sample_done['list_len2'] = rbo_sample_done['url_list2'].apply(len)
rbo_sample_done['jaccard_similarity'] = rbo_sample_done.apply(
    lambda row: jaccard_similarity(row['url_list1'], row['url_list2']),
    axis=1
)
rbo_sample_done['setdiff_1_2'] = rbo_sample_done.apply(
    lambda row: len_setdiff(row['url_list1'], row['url_list2']),
    axis=1
)
rbo_sample_done['setdiff_2_1'] = rbo_sample_done.apply(
    lambda row: len_setdiff(row['url_list2'], row['url_list1']),
    axis=1
)

rbo_sample_done_r = rbo_sample_done.loc[:, ~rbo_sample_done.columns.str.startswith('url_list')]
rbo_sample_done_r.to_parquet("data/rbo_result_r.parquet")