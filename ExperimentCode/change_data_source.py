#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar  3 07:56:21 2023

@author: bbsrc

Load in the old datasource file and change the parameters.
"""

import pandas as pd

ds = pd.read_csv('../MonBin/datasource.csv')


ds['frequencyL'] = ds['frequencyL'].replace(1.6, .4)
ds['frequencyL'] = ds['frequencyL'].replace(2.0, .5)
ds['frequencyR'] = ds['frequencyR'].replace(1.6, .4)
ds['frequencyR'] = ds['frequencyR'].replace(2.0, .5)

ds['videoL'] = ds['videoL'].str.replace('_f1.6', '_f0.4')
ds['videoL'] = ds['videoL'].str.replace('_f2.0', '_f0.5')
ds['videoR'] = ds['videoR'].str.replace('_f1.6', '_f0.4')
ds['videoR'] = ds['videoR'].str.replace('_f2.0', '_f0.5')

ds.to_csv('./datasource.csv')