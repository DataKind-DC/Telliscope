#!/usr/bin/env python

import pandas as pd
import os
import numpy as np

def parse_and_concat_files(write=False):
    """
    Get a list of the excel files within the `Export and Import Data` dir,
    convert them to dataframes, add some columns indicating import/export,
    the HS2 Code, and the HS2 Description from chapter.xls. Optionally
    write the data to your current working directory.

    Returns:
        data (pandas DataFrame):
    """

    col_types = {'CIF Value (ETB)':float,
             'CIF Value (USD)':float,
             'CPC':str,
             'Country (Consignment)':str,
             'Country (Origin)':str,
             'Destination':str,
             'FOB Value (ETB)':float,
             'FOB Value (USD)':float,
             'Gross Wt. (Kg)':float,
             'HS Code':str,
             'HS Description':str,
             'Month':str,
             'Net Wt. (Kg)':float,
             'Net.Wt. (Kg)':float,
             'Quantity':float,
             'Sup. Unit':str,
             'Total tax (ETB)':float,
             'Total tax (USD)':float,
             'Unit':str,
             'Year':str}

    files = []
    for dirpath, dirnames, filenames in os.walk(os.getcwd()):
        for file in filenames:
            if '.xls' in file and 'chapter' not in file and "~$" not in file:
                files.append(file)

    chapter = pd.read_excel(r'chapter.xls',dtype=str)
    dfs = []
    files_len = len(files)
    for i, file in enumerate(files):
        df = pd.read_excel(file,dtype=col_types)
        if 'import' in file:
            df['Destination'] = np.nan
        else:
            df['Country (Origin)'] = np.nan
            df['Country (Consignment)'] = np.nan

        df.rename({'Net.Wt. (Kg)':'Net Wt. (Kg)',
                   'CIF Value (USD)':'Value (USD)',
                   'CIF Value (ETB)':'Value (ETB)',
                   'FOB Value (ETB)':'Value (ETB)',
                   'FOB Value (USD)':'Value (USD)',
                   'Sup. Unit':'Unit'},axis=1,inplace=True)
        df['Direction'] = file[:file.index("_")]
        df['HS2'] = df['HS Code'].apply(lambda x: x[:2])
        df = pd.merge(df,chapter[['HS2','HS2_DSCRIPTION']],on='HS2')
        if i > 0:
            last_cols = list(dfs[i-1].columns)
            current_cols = sorted(df.columns)
            assert (last_cols == current_cols),"Column names weren't the same!"

        dfs.append(df[sorted(df.columns)])
        print("Done parsing {} of {} files".format(i+1,files_len))

    data = pd.concat(dfs)
    if write:
        data.to_csv('data.csv',index=False)

    return data


if __name__ == "__main__":
    data = parse_and_concat_files()
