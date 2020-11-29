---
title: "TRIAD Development Protocol"
author: "Bijan Seyednasrollah"
date: '2019-05-01'
---


This document explains the development protocol and style guide for the TRIAD project

### Style Guide
*"Good coding style is like correct punctuation: you can manage without it, butitsuremakesthingseasiertoread."*, Hadley Wickham.  We try to closely follow *The tidyverse style guide*. The main goal is clearity while being concise and stay consistent.
Details about  *The tidyverse style guide* can be found from [tidyverse](https://style.tidyverse.org/).


### Development
At the moment, we try to keep three alive main branches:

1. *master*: the master branch contains the most stable/tested but not necessarily released version
2. *devel*: the devel branch contains the most up-to-date development version
3. *test*: the test branch is a place to test small ideas before the full implementation

The *master* should be bug-free as much as possible. Ideally, new features and ideas should be tested prior implementation in the *devel* branch. Features are not merged into the *master* before a comprehensive test.


### Debug
- Discovered issues and bugs should be documented in the **working-issues.md** document as a *known issue* under *Known Issues*. 

- All know issues and bugs should be explained in a way that one can reproduce the issue.
- New ideas are documented in the **working-issues.md** document under *Suggested New Features*. 
- It is best practice to address the known issue as soon as possible
- It is best practice to perform a comprehensive test after any debug or adding any feature to identify potential side-effects as soon as possible.
- After addressing a know issue, it may be temporarily moved under **Addressed Issues**, with some extra note about how the issue was addressed. It can be completely removed after some time.
- After having the first stable version, all know issues should be submitted to the GitHub repo. Only known issues with no solution should be listed in **working-issue.md**.

### Test
While there are many testing approaches, we adopt two most popular testing approaches:

1. [White-Box Testing](https://en.wikipedia.org/wiki/White-box_testing)
2. [Black-Box Testing](https://en.wikipedia.org/wiki/Black-box_testing)

It is expected that all features of the application are "white-box" tested as soon as the development is completed. Ideally, a *test case* should be implemented for each test of all functions/methods/scripts.

It is expected that several comprehensive *black-box* test cases are designed and documented, in order to test the entire app for identifying potenital issues.


### Code Structure
At the moment, The code contains four major parts:

1. **_global.R_**: to initialize the application before calling ui.R
2. **_ui.R_**: the user interface is built in ui.R
3. **_server.R_**: the functionalities are developed in server.R
4. **_funcs.R_**: contains the small functions called in server.R or ui.R

It is expected that every function is responsible for a single task. While, we acknowledge *Shiny* apps are generally contain large chunks of code in server.R and ui.R as the debugging becomes a challenge for function calls from the *Shiny* shell, we still try to minimize the size of main code in server.R and ui.R.

### Comments
- It is expected that comments are used troughout the code to explain which part does what
- Comments in the code are reserved only to increase clarity of each parts.   
- Comments should not be used for raising question or adding notes about a know issue or bug. Those should be documented in *working-issues.md*.

### Versioning
We adopt a simple versioning approach with three digits: **_X.Y.Z.B_**, starting from 0.0.1.0. From the left:

- **_X_**: The first digit indicates major released version number, it should remain as **0** until the first complete fully tested version is released.
- **_Y_**: The second digit indicate its status: 0 for alpha, 1 for beta, 2 for candidate for release and 3 for released. 
- **_Z_**: The third digit indicates minor released, reserved for changes that add new minor functionalities or addressed serveral bugs
- **_B_**: The last digit indicates the build number, starting from 1. Each build addresses faily minor features or bugs or small appearance issues. 


