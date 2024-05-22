#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 13 15:14:08 2022

@author: bbsrc
"""

from itertools import product 

import pandas as pd



ocular_conditions = ['MonL', 'MonR', 'Bin', 'DichL', 'DichR', 
                     'MonXL', 'MonXR', 'BinX', 'DichXL', 'DichXR']
contrasts = [.06, .12, .24, .48, .96]
conditions = product(ocular_conditions, contrasts)
conditions = [(i+1, *con) 
              for i, con in enumerate(product(ocular_conditions, contrasts))]
# Double up on Bin conditions
conditions = conditions + [c for c in conditions if 'Bin' in c]
conditions = sorted(conditions)
conditions = pd.DataFrame(conditions, columns=[
    'condition_code',
    'condition',
    'contrast'
    ]
    )

