---
title: "TRIAD Working Issues"
author: "Bijan Seyednasrollah"
date: '2019-05-01'
---

This is a place to document known issues, reproducible bugs and new ideas. This document lists the know issues identfied during the devlopement process.


## Known Issues
Known issues are listed here to be addressed as soon as possible.
- when reloading the page all markers disappear, which can be prohibitive to use the tool when dating 1000 year of cores with a poor internet connection.

## Addressed Issues
Known issues are temporarily moved here after being addressed. Addressed issues will be cleared after few weeks.

## Suggested New Features
- add a button (similar to linker button) that converts a normal point to the pith. If you have a core that goes from bark to bark, then we need the pith to make sure that years match up and do not just keep decreasing. They should increase after the pith (or closest ring to the pith). 
- add a feature to delete individual points. Let's say on second inspection, the user realises that a ring is a false ring and wants to delete the point, that should be possible.
- when setting a point inbetween two old points, maybe we could automatically slot it in the right place in the table. Let's say a ring was not marked because operator was thinking is was a false ring and wants to go back and mark it later.
- add double linker functionality to bridge gaps. So when a linker follows a linker you measure from penultimate marker to the first linker and the second linker to the last marker. This would allow to jump gaps in measurements without affecting the normal functionality.
- Maybe we also want to add a false ring marker.
- Potential improvement of the metadata include: changing location to latitude and longitude, introducing little question marks next to the meta-data title, which provide explanations when hovering over them.
- Message and contact us Tab
- Sliding for the image (iframe tag)
- Scalebar (spend more time, begin, end and distance), choice between DPI or scalebar not both. record how we got it.
- We could also fit a spline (cubic, etc.) through the graph and make a second plot with detrended ring width index.
- residual from spline (output)
- detrended
- change to button saying 'Not confimred' until to click on it and it says 'confirmed'. Maybe with a disk icon? SHOULD BE AN ACTION BUTTON!
- new feature for a comment marker (ignore for distance calculation but record the year, always smaller no., h, double click to be merged with false ring)

## Development Log
Newly achieved development milestones are reported here. 

