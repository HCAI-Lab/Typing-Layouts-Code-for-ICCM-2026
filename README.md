# Typing-Layouts-ACTR-Code-for-ICCM-2026

This repository contains the ACT-R models used for the ICCM 2026 paper:

Cognitive Modeling of Typing Layouts: Comparing Representations with Motor Action and Visual Attention

The models compare four typing layout representations:

1. QWERTY Bimanual
2. FastType
3. Hunt and peck
4. Abacus gesture typing

Each layout has two model versions:

1. Manual model
2. Manual + Vision model

## Files

Manual model files:

    Baseline_manual.lisp
    FastType_manual.lisp
    HuntAndPeck_manual.lisp
    LinearAbacus_manual.lisp

Manual + Vision model files:

    Baseline_manual_vision.lisp
    FastType_manual_vision.lisp
    HuntAndPeck_manual_vision.lisp
    LinearAbacus_manual_vision.lisp

Support files for the Manual + Vision models:

    function_new.lisp
    TemplateMatching.py
    Utils.py
    VisiTor.py
    Window.png
    Images_v3/

## Requirements

The models were developed using ACT-R 7.26.

Before running any model, load ACT-R:

    (load "~/actr7.x/load-act-r.lisp")

Please update the path if ACT-R is installed in a different location.

## Running the Manual Models

The Manual models simulate typing random sentence words up to 10,000 words.

After loading ACT-R, load one Manual model file. For example:

    (load "Baseline_manual.lisp")

Other Manual model files can be loaded in the same way:

    (load "FastType_manual.lisp")
    (load "HuntAndPeck_manual.lisp")
    (load "LinearAbacus_manual.lisp")

The models print word markers and timestamps. These outputs were used to compute WPM by block.

## Running the Manual + Vision Models

The Manual + Vision models use the repeated sentence:

    buy fresh milk

Before running a Manual + Vision model:

1. Load ACT-R.
2. Open Window.png and keep it visible on the screen.
3. Keep the Images_v3 folder in the same directory as the model files.
4. Load the VisiTor support file.
5. Load one Manual + Vision model file.

Example:

    (load "~/actr7.x/load-act-r.lisp")
    (load "function_new.lisp")
    (load "Baseline_manual_vision.lisp")

Other Manual + Vision model files can be loaded in the same way:

    (load "FastType_manual_vision.lisp")
    (load "HuntAndPeck_manual_vision.lisp")
    (load "LinearAbacus_manual_vision.lisp")

The Manual + Vision models report WPM across the first 300 words.

## Notes

The Manual model and Manual + Vision model use different task settings and were calibrated separately. Within each model version, the same parameter set was applied across all four layouts.

The Manual + Vision models require function_new.lisp, TemplateMatching.py, Utils.py, VisiTor.py, Window.png, and the Images_v3 folder to remain in the same directory as the model files.

Abacus typing does not yet have direct human text entry benchmark data, so its result should be interpreted as a model prediction for future human testing.
