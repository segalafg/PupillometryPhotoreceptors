#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 19 13:06:03 2022

@author: bbsrc
"""
import os
import os.path as op
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import butter, filtfilt

from pyplr import utils, preproc

DATA_DIR ='/home/bbsrc/Desktop/Experiments/MonBinPupil/S-cone/results/jtm/000/exports/000/'

annotations = pd.read_csv(op.join(DATA_DIR, 'annotations.csv'))
pupil = pd.read_csv(op.join(DATA_DIR, 'pupil_positions.csv'))
left = pupil.loc[pupil.eye_id==0]
right = pupil.loc[pupil.eye_id==1]

for eye in [left, right]:
    eye.loc[eye.confidence<.1, 'diameter_3d'] = np.nan  
    eye.diameter_3d = eye.diameter_3d.interpolate()
    eye = preproc.butterworth_series(eye, ['diameter_3d'], 3, 4/(120/2))

events = annotations[annotations.label=='pre_scene']
events = events.set_index('timestamp')
left = left.set_index('pupil_timestamp')
right = right.set_index('pupil_timestamp')

borrow_attributes = [
'label', 'duration', 'condition_code',
'contrastL', 'contrastR', 'frequencyL', 
'frequencyR','ocular_condition']


left_trials = utils.extract(
    left, events, offset=0, duration = 1440, borrow_attributes=borrow_attributes
    )

right_trials = utils.extract(
    right, events, offset=0, duration = 1440, borrow_attributes=borrow_attributes
    )

t = np.linspace(0, 12, 1440) 
for ids, df in right_trials.groupby('event'):
    plt.plot(t, df.diameter_3d)


# Plot
fig, ax = plt.subplots(1, 1, figsize=(12, 4))

newL = (
       left_trials.groupby(
           ['contrastL', 'frequencyL', 'onset'])
       ['diameter_3d'].mean().unstack().T
)
    
newR = (
       right_trials.groupby(
           ['contrastR', 'frequencyR', 'onset'])
       ['diameter_3d'].mean().unstack().T
       )