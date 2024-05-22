#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jun 11 16:37:42 2022

@author: jtm545

Script to make the stimuli for the MonBinPupil experiment. 
"""
import os
import os.path as op
from itertools import product
from pprint import pprint
from datetime import datetime
import pickle

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from pyplr import stlabhelp
from pysilsub.problems import SilentSubstitutionProblem as SSP
from pysilsub.observers import ColorimetricObserver as ICO

# %% ~~~ PLOT STYLE ~~~

plt.style.use('seaborn')
plt.rcParams['font.size'] = 14
plt.rcParams['font.family'] = 'Helvetica'

# %% Functions


def check_exists(folder: str) -> str:
    if not op.exists(folder):
        os.makedirs(folder)
    return folder


# %% ~~~ EXPERIMENTS ~~~
# These appear to be the maximum achieveable contrasts
SCONE = {
    'ignore': ['rh'],
    'target': ['sc'],
    'silence': ['mc', 'lc', 'mel'],
    'max_contrast': .45
}

MCONE = {
    'ignore': ['rh'],
    'target': ['mc'],
    'silence': ['sc', 'lc', 'mel'],
    'max_contrast': .13
}

LCONE = {
    'ignore': ['rh'],
    'target': ['lc'],
    'silence': ['sc', 'mc', 'mel'],
    'max_contrast': .15
}

MELANOPSIN = {
    'ignore': ['rh'],
    'target': ['mel'],
    'silence': ['sc', 'mc', 'lc'],
    'max_contrast': .22
}

LMINUSM = {
    'ignore': ['rh'],
    'target': ['mc', 'lc'],
    'silence': ['sc', 'mel'],
    'max_contrast': .1
}


EXPERIMENTS = {
    'S-cone': SCONE,
    'M-cone': MCONE,
    'L-cone': LCONE,
    'Melanopsin': MELANOPSIN,
    'L-M': LMINUSM
}



# %% ~~~ CONSTANTS ~~~


MINTENSITY = 0
MAXTENSITY = 4095
BACKGROUND = MAXTENSITY/2
Fs = 100  # STLAB switching time
MAX_S_CONE_CONTRAST = .45


# %% ~~~ MAIN SCRIPT ~~~

def main(out: str, experiment: str, observer_age: int):
    print('\n')
    print(f"{'~'*80 : ^80}")
    msg = f'Making {experiment} stimuli for {observer_age} year-old observer'
    print(f"{msg: ^80}")
    print(f"{'~'*80 : ^80}")
    print(f"Experiment: {experiment}")
    print(f"Observer age: {observer_age}")
    print('\n')

    ignore = EXPERIMENTS[experiment].get('ignore')
    target = EXPERIMENTS[experiment].get('target')
    silence = EXPERIMENTS[experiment].get('silence')
    max_contrast = EXPERIMENTS[experiment].get('max_contrast')
    if experiment=='L-M':
         stim_folder = check_exists(out)
         angle = float(input("Enter angle in radians: "))
    else:
         stim_folder = check_exists(f'./{experiment}/stims/{observer_age}')
    stlab_1_stim_folder = check_exists(op.join(stim_folder, 'STLAB_1/'))
    stlab_2_stim_folder = check_exists(op.join(stim_folder, 'STLAB_2/'))

    #%% ~~~ CALIBRATION ~~~ #

    # Create observer model for specified age and save it
    ico = ICO(age=observer_age, field_size=10)
    with open(op.join(stim_folder, 'observer_model.pkl'), 'wb') as fh:
        pickle.dump(ico, fh)

    # Load predictive models for each device and plug in the observer
    S1 = SSP.from_json('./calibration/STLAB_1_York.json')
    S2 = SSP.from_json('./calibration/STLAB_2_York.json')
    S1.observer, S2.observer = ico, ico

    # We know that the two devices differ slightly in output. Here we obtain
    # a calibration ratio for each LED that *may* be used to perform a simple
    # correction later.
    S1_S2_calibration_ratio = pd.read_csv(
        './calibration/S1_S2_calibration_ratio.csv',
        index_col='Primary'
        ).squeeze()

    # Plot the calibration spds, do the gamma corrections, save output, etc.
    for device in [S1, S2]:
        # Keep a log of which device / calibration was used to prepare the stims
        # and at what time
        with open(f'./{stim_folder}/{device.config["json_name"]}_device_log.txt', 'w') as fh:
            pprint(device.config, stream=fh)
            print(f'\n> Time created: {datetime.now()}', file=fh)

        # Perform gamma correction
        device.do_gamma(fit='polynomial')
        device.gamma[device.gamma < MINTENSITY] = MINTENSITY
        device.gamma[device.gamma > MAXTENSITY] = MAXTENSITY

    #%% ~~~ MAKE THE STIMS ~~~ #

    # Plot for stimuli
    fig, axs = plt.subplots(2, 3, figsize=(12, 6.5))

    # Stimulus parameters
    frequencies = [0.5, 0.4]
    contrasts = [.06, .12, .24, .48, .96]
    seconds = 12

    contrast_table = []
    for f, c in product(frequencies, contrasts):

        # A complete cycle
        x = stlabhelp.sinusoid_modulation(f, 1/f, Fs)

        # Adjust intensity amplitudes to native resolution
        target_contrasts = x * (c * max_contrast)

        # Define the problems
        for problem in [S1, S2]:
            problem.ignore = ignore
            problem.target = target
            problem.silence = silence                
            problem.target_contrast = 0.
            problem.background = [.5] * problem.nprimaries

        # Find the solutions
        s1_solutions = []
        s2_solutions = []
        for target_contrast in target_contrasts:
            if experiment=='L-M':                
                S1.target_contrast = [target_contrast * np.sin(angle), target_contrast * np.cos(angle)]
                s1_solutions.append(S1.linalg_solve())
                
                S2.target_contrast = [target_contrast * np.sin(angle), target_contrast * np.cos(angle)]
                s2_solutions.append(S2.linalg_solve())
            else:
                S1.target_contrast = target_contrast
                s1_solutions.append(S1.linalg_solve())
                S2.target_contrast = target_contrast
                s2_solutions.append(S2.linalg_solve())

        # Time vector
        t = np.linspace(0, 1/f, len(x))

        # Plot ideal stimulus profile
        if f == 0.5:
            axs[0, 0].plot(t, target_contrasts, c='k',
                           alpha=c, lw=2+c, label=c)
        elif f == 0.4:
            axs[1, 0].plot(t, target_contrasts, c='k',
                           alpha=c, lw=2+c, label=c)
        # breakpoint()
        # Plot solutions
        colors = S1.observer.photoreceptor_colors
        for ax, problem, solutions in zip(
                [axs[:, 1], axs[:, 2]],
                [S1, S2],
                [s1_solutions, s2_solutions]
                ):
            solutions = [problem.gamma_correct(
                (s*MAXTENSITY).astype('int')) for s in solutions]
            problem.background = solutions[0]  # !

            contrast_profiles = [
                problem.get_photoreceptor_contrasts(s) for s in solutions]
            contrast_profiles = pd.concat(contrast_profiles, axis=1).T

            if f == 0.5:
                contrast_profiles.plot(
                    ax=ax[0], lw=2+c, alpha=c, color=colors, legend=False)

            elif f == 0.4:
                contrast_profiles.plot(
                    ax=ax[1], lw=2+c, alpha=c, color=colors, legend=False)

        s1_settings = [S1.w2s(w) for w in s1_solutions]
        s2_settings = [S2.w2s(w) for w in s2_solutions]

        # ~~~ MAKE STLAB VIDEO FILES ~~~ #

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
            led_cycles.columns = ['time' if c == 0
                                  else 'LED-' + str(c-1)
                                  for c in led_cycles.columns]
            metadata = {
                'title': f'{f} Hz {experiment} modulation',
                'seconds': seconds,
                'contrast': c,
                'frequency': f,
                'device': device.name,
                'experiment': experiment,
                'contrast_profiles': contrast_profiles.to_numpy().tolist(),
                'device_configuration': device.config
            }
            fname = op.join(stim_folder, f'{device.name.split()[0]}/c{c}_f{f}.json')
            stlabhelp.make_video_file(
                led_cycles, repeats=1, fname=fname, **metadata)

            contrast_profiles.index.name = 'spectrum'
            contrast_profiles['f'] = f
            contrast_profiles['c'] = c
            contrast_table.append(contrast_profiles.reset_index())

    # Tweak the stim figure and save
    for ax in [axs[0, 1], axs[0, 2], axs[1, 1], axs[1, 2]]:
        ax.set_ylabel('Contrast')

    axs[0, 0].set_ylabel('Target S-cone contrast\n$f$ = 0.5')
    axs[1, 0].set_ylabel('Target S-cone contrast\n$f$ = 0.4')
    axs[0, 0].set_title('Ideal stimulus profile')
    axs[0, 1].set_title('Forward prediction - Source 1')
    axs[0, 2].set_title('Forward prediction - Source 2')
    axs[0, 0].legend(title='Contrast')
    handles, labels = axs[0, 2].get_legend_handles_labels()
    axs[0, 2].legend([handles[-1]], [labels[-1]])

    for ax in axs[1]:
        ax.set_xlabel('Time (s)')

    for ax in axs.flatten():
        ax.set_ylim((-max_contrast*1.05, max_contrast*1.05))

    plt.tight_layout()
    
    # Save the figs
    fig.savefig(
        op.join(stim_folder, 'STLAB_S_cone_contrast_stims.svg'))
    pd.concat(contrast_table).to_csv(
        op.join(stim_folder, 'contrast_profiles.csv'), index=None)


if __name__ == '__main__':
    main(out='./test', experiment='L-M', observer_age=34)
