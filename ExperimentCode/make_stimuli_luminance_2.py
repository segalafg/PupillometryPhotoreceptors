#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jun 11 16:37:42 2022

@author: jtm545

Script to make the stimuli for the MonBin1 experiment. 
"""

import os
import os.path as op
from itertools import product

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from pyplr import stlabhelp
from pysilsub.devices import StimulationDevice
from pysilsub.binocular import BinocularStimulationDevice
from pysilsub.CIE import get_CIE_1924_photopic_vl, get_CIES026_action_spectra


# Make stims folder if it doesn't exist
if not op.exists('./Luminance'):
    os.mkdir('./Luminance')
    os.mkdir('./Luminance/STLAB_1')
    os.mkdir('./Luminance/STLAB_2')
    print('> Created stims folder: "./Luminance"')

# Constants
MINTENSITY = 0
MAXTENSITY = 4095
BACKGROUND = MAXTENSITY/2
Fs = 100
vl = get_CIE_1924_photopic_vl()
lms = get_CIES026_action_spectra()

# Predictive models for each device
S1 = StimulationDevice.from_json('./calibration/STLAB_1_York.json')
S2 = StimulationDevice.from_json('./calibration/STLAB_2_York.json')

# Binocular helper to optimise settings 
Sbin = BinocularStimulationDevice(S1, S2)
Sbin.anchor = 'left'
Sbin.optim = 'right'

# Gamma correct the anchored device
S1.do_gamma(fit='polynomial')
S1.gamma[ S1.gamma<0] = 0
S1.plot_gamma(show_corrected=True)
S2.do_gamma(fit='polynomial')
S2.gamma[S2.gamma<0] = 0
S2.plot_gamma(show_corrected=True)

# Bounds for primaries
bounds = [(0., 1.,) for primary in range(10)]


# Plot for stimuli
plt.style.use('seaborn')
fig, axs = plt.subplots(2, 3, figsize=(12, 6))

# Stimulus parameters
frequencies = [.4, .5]
contrasts = [.06, .12, .24, .48, .96]
#contrasts = [.96]
seconds = 12

for f, c in product(frequencies, contrasts):
    
    # A complete cycle
    x = stlabhelp.sinusoid_modulation(f, 1/f, Fs)
    
    # Adjust intensity amplitudes to native resolution
    cycle_mod = (((x*c)*BACKGROUND)+BACKGROUND).astype('int')
    cycle_mod = cycle_mod / MAXTENSITY
    
    # Because the optimisation takes a bit of time, only solve for what we
    # need. We can piece the stimulus profile back together later.
    peak_idx = cycle_mod.argmax()
    trough_idx = cycle_mod.argmin()
    s1_settings = cycle_mod[peak_idx:trough_idx+1]
    s1_settings = [np.tile(s, 10) for s in s1_settings]
    t = np.linspace(0, 1/f, len(x))
    #t = t[peak_idx:trough_idx+1]

    # Plot ideal stimulus profile
    if f==.5:
        axs[0, 0].plot(t, x*c, c='k', alpha=c)
    elif f==.4:
        axs[1, 0].plot(t, x*c, c='k', alpha=c)

    # Optimise settings for one device against another. See 
    # pysilsub/binocular.py for more information.
    s2_settings = Sbin.optimise_settings(s1_settings)
    
    # Gamma correct the s1_settings
    s1_settings = [S1.s2w(
        S1.gamma_correct(
            S1.w2s(s)
            )) for s in s1_settings]

    # Gamma correct the s2_settings
    s2_settings = [S2.s2w(
        S2.gamma_correct(
            S2.w2s(s)
            )) for s in s2_settings]
    
    # Put the settings in lists
    spectra1 = []
    spectra2 = []
    for s1, s2 in zip(s1_settings, s2_settings):
        spectra1.append(S1.predict_multiprimary_spd(s1))
        spectra2.append(S2.predict_multiprimary_spd(s2))
    
    # Plot forward projection of photopic luminance for the settings
    spectra1 = pd.concat(spectra1, axis=1).T.reset_index(drop=True)
    spectra2 = pd.concat(spectra2, axis=1).T.reset_index(drop=True)
    spectra1_vl = spectra1.dot(vl)
    spectra2_vl = spectra2.dot(vl)
    
    vl1 = spectra1_vl.to_numpy()
    vl1 = np.hstack([vl1[0:13][::-1].T, vl1.T, vl1[13:][::-1].T])
    vl2 = spectra2_vl.to_numpy()
    vl2 = np.hstack([vl2[0:13][::-1].T, vl2.T, vl2[13:][::-1].T])
    
    # if f==.5:
    #     axs[0, 1].plot(t, vl1[0], label=c)
    #     axs[0, 2].plot(t, vl2[0], label=c)

    # elif f==.4:
    #     axs[1, 1].plot(t, vl1[0], label=c)
    #     axs[1, 2].plot(t, vl2[0], label=c)
    
    # Reconstruct the stimulus profile
    def reconstruct_full_cycle(settings):
        phase1 = (settings[0:16])[::-1]  # Reverse it
        phase2_3 = settings  # Peak to trough
        phase4 = (settings[16:])[::-1]
        return phase1 + phase2_3 + phase4
        
        
    s1_settings = reconstruct_full_cycle(s1_settings)
    s2_settings = reconstruct_full_cycle(s2_settings)
    
    s1_settings = [S1.w2s(w) for w in s1_settings]
    s2_settings = [S2.w2s(w) for w in s2_settings]
    
    
    # Make the STLAB video files
    time_points = np.linspace(0, 1000*seconds, seconds*Fs).astype('int')
    
    for device, settings in zip([S1, S2], [s1_settings, s2_settings]):

        settings = settings * 30  # More than we need
        settings = settings[0:len(time_points)]  # Trim it down to size
        led_cycles = np.array(settings)
        
        led_cycles = np.insert(led_cycles, 0, time_points, axis=1)
        
        # If frequency is not a round number, the device output will not 
        # return to the background after 12 seconds. So we add final spectrum 
        # to make sure the luminaire returns to background levels after a 
        # modulation. 
        final_spec = led_cycles[0].copy()
        final_spec[0] = 12050  # 50 ms afterwards, won't be skipped
        led_cycles = np.vstack([led_cycles, final_spec])
        
        led_cycles = pd.DataFrame(led_cycles)
        led_cycles.columns = ['time' if c==0 
                              else 'LED-' + str(c-1) 
                              for c in led_cycles.columns]
        metadata = {
            'title': f'{f} Hz luminance modulation',
            'seconds': seconds, 
            'contrast': c,
            'frequency': f,
            'device': device.name
            }
        fname = f'./Luminance/{device.name.split()[0]}/c{c}_f{f}.json'
        stlabhelp.make_video_file(
            led_cycles, repeats=1, fname=fname, **metadata)
    

    plt.legend(title='Contrast')
    
for ax in [axs[0, 1], axs[0, 2], axs[1, 1], axs[1, 2]]:
    #ax.set_ylim((0, 50))
    ax.set_ylabel('Photopic luminance')

axs[0, 0].set_ylabel('STLAB intensity (all channels)\n$f$ = 0.5')
axs[1, 0].set_ylabel('STLAB intensity (all channels)\n$f$ = 0.4')

axs[0, 0].set_title('Stimulus profile')
axs[0, 1].set_title('Forward prediction - Source 1')
axs[0, 2].set_title('Forward prediction - Source 2')

for ax in axs[1]:
    ax.set_xlabel('Time (s)')

plt.tight_layout()

fig.savefig('./STLAB_luminance_contrast_stims.svg')



