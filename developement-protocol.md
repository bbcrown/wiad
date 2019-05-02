---
title: "TRIAD Development Protocol"
author: Bijan Seyednasrollah
date: "2019-05-01"
---

## TRIAD Development Protocol

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


### Test
While there are many testing approaches, we adopt two most popular testing approaches:

1. [White-Box Testing](https://en.wikipedia.org/wiki/White-box_testing)
2. [Black-Box Testing](https://en.wikipedia.org/wiki/Black-box_testing)

It is expected that all features of the application are "white-box" tested as soon as the development is completed. Ideally, a *test case* should be implemented for each test of all functions/methods/scripts.

It is expected that several comprehensive *black-box* test case are designed and documented, in order to test the entire app for identifying potenital issues.


### TRIAD Code Structure
At the moment, The code contains four major parts:

1- global.R: to initialize the application before calling ui.R
2- ui.R: the user interface is built in ui.R
3- server.R: the functionalities are developed in server.R
4- funcs.R: contains the small functions called in server.R or ui.R

It is expected that every function is responsible for a single task. While, we acknowledge *Shiny* apps are generally contain large chunks of code in server.R and ui.R as the debugging becomes a challenge for function calls from the *Shiny* shell, we still try to minimize the size of main code in server.R and ui.R.
