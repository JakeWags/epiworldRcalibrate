

```yaml
title: "epiworldRcalibrate: Deep Learning Calibration for epiworldR SIR Models"
tags:
  - R
  - infectious disease modeling
  - SIR models
  - epidemiology
  - calibration
  - machine learning
  - LSTM
authors:
  - name: Sima Najafzadehkhoei
    equal-contrib: true
    affiliation: 1
  - name: George G. Vega Yon
    equal-contrib: true
    affiliation: 1
  - name: Bernardo Modenesi
    equal-contrib: false
    affiliation: 1
affiliations:
 - name: University of Utah, USA
   index: 1
date: 15 November 2025
---

```

# Summary

`epiworldRcalibrate` is an R package that provides **fast and robust calibration** of SIR epidemic model parameters using a pretrained **Bidirectional Long Short-Term Memory (BiLSTM)** neural network. The package takes a 61-day incidence time series and returns estimates of the **contact rate**, **transmission rate**, and **basic reproduction number (R₀)**. These outputs allow users to convert epidemic curves—whether simulated through `epiworldR` or derived from empirical data—into interpretable mechanistic parameters.

A defining feature of the package is its **zero-configuration machine-learning workflow**. Although the underlying model runs in Python using modern scientific tools, `epiworldRcalibrate` automatically handles Python detection, virtual-environment creation, dependency installation, and model loading. Users interact entirely within R, enabling seamless integration into simulation studies, teaching environments, or modeling pipelines.

By embedding a pretrained deep learning model directly within the R ecosystem, `epiworldRcalibrate` provides a practical, high-throughput alternative to classical calibration methods, which often require computationally expensive optimization or Bayesian inference.

# Statement of need

Estimating epidemiological parameters from observed incidence is a longstanding challenge in infectious disease modeling. Standard approaches—including likelihood-based estimation, approximate Bayesian computation, and manual grid search—can be computationally intensive, sensitive to noise, and difficult to scale across thousands of simulation runs. They also require technical expertise in numerical optimization or Bayesian computation that may be inaccessible to many practitioners.

At the same time, interest in agent-based epidemic simulators has increased, with `epiworldR` providing a fast and flexible R interface for large-scale SIR simulations. However, these tools lack an efficient, standardized mechanism to map **from incidence curves back to model parameters**, creating a bottleneck for calibration, validation, and parameter recovery studies.

`epiworldRcalibrate` addresses this gap by offering:

* a pretrained neural network designed specifically for SIR dynamics,
* fully automatic preprocessing consistent with the training pipeline,
* a simple, one-function interface for parameter estimation, and
* frictionless integration with `epiworldR`.

To our knowledge, it is the first R package to embed a deep-learning-based inverse model for SIR calibration, greatly simplifying parameter recovery tasks for researchers and students.

# State of the field

Deep learning methods have recently gained prominence in epidemic forecasting, parameter regression, and surrogate modeling. Recurrent neural networks—and BiLSTMs in particular—perform well on temporally autocorrelated epidemic data, capturing stochasticity, nonlinear peak behavior, and complex infection dynamics.

Meanwhile, epidemic modeling in R is supported by a range of packages. Tools such as `EpiEstim` and `EpiNow2` enable estimation of time-varying reproduction numbers, while `pomp` provides a general framework for state-space model inference. The `epiworldR` package offers fast agent-based epidemic simulations but does not include mechanisms to infer parameters from observed trajectories.

Despite the proliferation of simulation and inference tools, **no existing R package provides a pretrained deep learning architecture dedicated to estimating SIR transmission parameters** from raw incidence data. Users who wish to incorporate deep learning approaches must ordinarily write custom Python code, manage environments, or build their own models—steps that create substantial barriers to adoption.

`epiworldRcalibrate` overcomes these limitations by embedding a fully trained model with standardized preprocessing, enabling reproducible, high-speed calibration entirely within R.

# Key features

`epiworldRcalibrate` combines deep learning with accessible R tooling. Its core capabilities include:

* **Automatic Python and model management**
  The package detects available Python installations, creates or reuses a virtual environment, installs necessary dependencies, and loads the pretrained BiLSTM model without user intervention.

* **One-step parameter estimation**
  Users supply a 61-day incidence series, population size, and recovery rate. The package preprocesses the data, performs model inference, and returns calibrated parameter estimates.

* **Transparent preprocessing**
  A dedicated function displays the percentage-change transformation applied to incidence data prior to prediction, promoting reproducibility and interpretability.

* **Model lifecycle utilities**
  Functions are available to initialize, inspect, and clean up the Python model, supporting advanced scripting, pipeline automation, and high-throughput calibration.

* **Seamless integration with `epiworldR`**
  Calibration outputs can be fed directly into agent-based simulations, enabling closed-loop workflows for parameter recovery, scenario exploration, and forward modeling.

# Software description

The BiLSTM model embedded in `epiworldRcalibrate` consists of three layers with 160 hidden units in each direction, including dropout regularization and conditioning on population size and recovery rate. The architecture was trained on large-scale synthetic SIR datasets containing noisy trajectories representative of realistic outbreak dynamics.

The R interface is structured around a clear initialization–inference–cleanup workflow. Internal routines handle Python discovery, dependency installation, and model loading. The main user-facing function performs validation of inputs, applies the appropriate temporal preprocessing, runs the deep learning model, and returns biologically plausible parameter estimates through constrained activation functions.

The package requires a 61-element incidence vector corresponding to days 0 through 60, along with positive population and recovery-rate inputs. Errors are informative and guide users toward correct data formats.

# Existing alternatives

Several R packages support epidemic modeling but do not offer pretrained deep learning calibration. The table below highlights key distinctions.

| Package              | DL-Based Calibration | SIR Support | Preprocessing | R Integration | Primary Use Case                     |
| -------------------- | -------------------- | ----------- | ------------- | ------------- | ------------------------------------ |
| `epiworldRcalibrate` | ✓                    | ✓           | ✓             | ✓             | Deep learning parameter calibration  |
| `EpiEstim`           | ✗                    | Partial     | ✗             | ✓             | Time-varying Rₜ estimation           |
| `pomp`               | ✗                    | ✓           | ✗             | ✓             | State-space modeling                 |
| `EpiNow2`            | ✗                    | ✗           | ✓             | ✓             | Forecasting and nowcasting           |
| `epiworldR`          | ✗                    | ✓           | ✗             | ✓             | Fast agent-based epidemic simulation |

No existing R tool combines SIR simulation, preprocessing, and pretrained neural network calibration in a unified workflow.

# Conclusion

`epiworldRcalibrate` introduces a novel deep-learning-assisted approach to estimating SIR parameters directly from incidence data. By combining a pretrained BiLSTM model, transparent preprocessing, and automatic Python management with a lightweight R interface, it enables fast, reproducible calibration that integrates seamlessly with the `epiworldR` ecosystem.

This design lowers technical barriers for parameter recovery, facilitates rapid exploratory modeling, supports teaching applications, and enhances reproducibility in simulation-based studies. The package advances the R ecosystem by making modern deep learning tools accessible to epidemiologists, data scientists, and public health researchers.

# Acknowledgements

We thank the developers of `epiworldR` and the contributors to the Python scientific ecosystem whose tools underpin the embedded deep learning model.

# References

(Uses `paper.bib` for citations.)

---


