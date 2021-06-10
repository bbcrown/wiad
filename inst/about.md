
**WIAD** stands for Wood Image Analysis and Dataset. 

### Application and Use

In order to extract visual data from images of ecological samples, such as wood, one needs to: 

1. Collect and prepare samples
2. Digitise them
3. Measure visual information such as annual growth increment or earlywood width
4. Further process the data (i.e. cross-dating and detrending)

All four steps require meticulous detail and expertise, thus are painstaking and time intenstive. As a consequence studies relying on visual properties of wood have been limited to relatively small samples sizes (typically n < 100). Because all steps rely on the scientists subjective expertise and current norms in the field limit reporting to descriptions of used tools and methods instead of providing raw data, studies based on visual properties of wood and other ecological data lack transparency. As a consequence, reproducing a study based on visual properties would require repeating all four painstaking and time intensive steps. WIAD's vision is to create a platform that allows traceable automation of these steps (i.e., enabling data provenance) as much as possible and saves the underlying raw and derived data to facilitate sharing in the future.

In its first version, the WIAD provides an simple interactive web interface to facilitate the measurement, evaluation and correction of distance measurements, such as tree rings (or early- and latewood) widths from scanned images. The WIAD interface is available at https://wiad.science and can be installed and run locally as R package (available on CRAN and https://github.com/bnasr/wiad/). When using the online interace, resulting data and the images are saved in the WIAD repository. WIAD also provides simple data processing tools. Currently, the tools allow online ploting of the data in real-time and extraction of ring width indices based on several common detrending methods using the dlpR package (Bunn, 2008).

Samples must be adequately prepared to obtain quality data. Depending on the nature of the sample, this can involve various steps, such as sanding, polishing, and staining. We are working on providing additional resources for sample preparation and imaging. In the meantime, here are a few key considerations when using WIAD. Importantly, image resolution can strongly affect future uses of the provided image, but desired higher resolution scans also require more storage space and are processed more slowly. Due to storage limitations, we have to restrict the maximum file size to 30 MB when using the online interface for now, but the user can easily override this restriction, when running WIAD locally by setting the maxImageSize variable in the global.R file. For now, we recommend 3200 dpi as scan resolution, which balances the tradeoff between visual quality and file size for standard increment cores. Further, we suggest always cropping images to only the region of interest prior to upload to reduce file size and increase processing speed. Images can be cropped with open-source image manipulation programs like GNU Image Manipulation Program. Users can even consider segmenting images into multiple files for large samples, thus files (e.g., images of cross-sections). Importantly, care must be taken to avoid changing the resolution or other features when pre-processing images.

While images are accummulating in the WIAD database, novel techniques to automate data extraction and cross-dating are continuously being developed by the WIAD core development team. We work on integrating features such as semi-automatic tree ring detection and automatic cross-dating as soon as they have been thoroughly tested. The raw imagery, metadata, and derived data will also be made publicly available without restrictions for non-commercial uses (read specifics in the Fair-use policy tab). For this purpose, we will publish a curated version of the accumulated data (i.e., images, metadata, derived data) every few years. Curation of versions of the data set, will allow review, cleaning and prevent potential abuses of directly accessible public image archives (e.g., upload of pornograpihc material). When provided, contact details (e.g., names and email addresses) for specific datasets will be made publicly available with the data to enhance collaboration and data sharing. 

### Objective and Mission

The objective of WIAD is to serve as a repository for digital images of ecological samples (i.e. increment cores, thinsections, x-ray films, root and leaf scans) and derived data. The overarching aim is to provide teaching tools and share the imagery, data and derived data for non-commercial uses with a wide array of third-party data end-users, including researchers, educators and the general public. The raw imagery will be made publicly available without restrictions for non-commerical uses (read specifics under the fair-use policy). We strongly recommend the download of imagery and datasets for use in your own research and teaching, but please acknowledge all the work that went into building this tool and contact data owners.

The mission of the WIAD team is to advance eco-physiological sciences by providing a free tools and repository that will enable unforeseen analyses based on larger sample sizes, novel visual characteristics and new methods of analysis, while equally facilitating data sharing and access. Thereby, WIAD intends to make eco-physiological sciences more transparent, reproducible, low cost and engaging. From more information read our paper (Rademacher et al., in review).

## The WIAD team

### Core team
WIAD was the idea of Tim Rademacher, which was developed into a vision together with Bijan Seyednasrollah and David Basler. Bijan Seyednasroolah then led the development of the WIAD v1.0.0.0. 

### Extended collaborators 
Many people have contributed in the development and testing of the tools and software offered through WIAD. In particular, we want to thank Tessa Mandra, Elise Miller, David Orwig, Neil Pederson, Andrew D. Richardson, and Donglai Wei. 

### Acknowledgement
WIAD depends on multiple collaborators, including contributors and users, which are thanked for their efforts in support of WIAD. This project profited from support of the National Science Foundation (DEB-1741585, DEB-1237491 and DEB-1832210).  

### References

Bunn (2008) *A dendrochronology program library in R (dplR)*, Dendrochronologia, 26 (2), 115-124, doi: 10.1016/j.dendro.2008.01.002

Rademacher, Seyednasrollah, Basler, Cheng, Mandra, Miller, Lin, Orwig, Pederson, Pfister, Richardson, Wei, Yao
(in review) *The Wood Image Analysis and Dataset (WIAD): open-access visual analysis tools to advance the ecological data revolution*, bioRxiv
