
**WIAD** stands for Wood Image Analysis and Dataset. 

### Application and Use

In order to extract visual data from images of ecological samples, such as wood, one needs to: 

1. Collect and prepare samples
2. Digitise them
3. Measure visual information such as annual growth increment or earlywood width
4. Further process the data (i.e. cross-dating and detrending)

All four steps require meticulous detail and expertise, thus are painstaking and time intenstive. As a consequence studies relying on visual properties of wood have been limited to relatively small samples sizes (typically n < 100). Because all steps rely on the scientists subjective expertise and current norms in the field limit reporting to descriptions of used tools and methods instead of providing raw data, studies based on visual properties of wood and other ecological data lack transparency. As a consequence, reproducing a study based on visual properties would require repeating all four painstaking and time intensive steps. WIAD's vision is to create a platform that allows traceable automation of these steps (i.e., enabling data provenance) as much as possible and saves the underlying raw and derived data to facilitate sharing in the future.

In its first version, the WIAD provides an simple interactive web interface to facilitate the measurement, evaluation and correction of distance measurements, such as tree rings (or early- and latewood) widths from scanned images. Resulting data and the images are saved in the WIAD repository. The tools are freely available online and as an R package. WIAD also provides simple data processing tools. Currently, the tools allow online ploting of the data in real-time and extraction of ring width indices based on several common detrending methods using the dlpR package (Bunn, 2008).

While images are accummulating in the WIAD database, novel techniques to automate data extraction and cross-dating are continuously being developed by the WIAD core development team. Features such as semi-automatic tree ring detection and automatic cross-dating will be added to the free online tool as soon as they have been thoroughly tested. Finally, a front end to explore and extract data from the database is equally under development.

### Objective and Mission

The objective of WIAD is to serve as a repository for digital images of ecological samples (i.e. increment cores, thinsections, x-ray films, root and leaf scans) and derived data. The overarching aim is to provide teaching tools and share the imagery, data and derived data for non-commercial uses with a wide array of third-party data end-users, including researchers, educators and the general public. Contact details (e.g., names and email addresses) for specific datasets are made publicly available to enhance collaboration and data sharing. The raw imagery is also made publicly available without restrictions for non-commerical uses (read specifics under the fair-use policy). We strongly recommend the download of imagery and datasets for use in your own research and teaching, but please acknowledge all the work that went into building this tool and contact data owners.

The mission of the WIAD team is to advance eco-physiological sciences by providing a free tool and repository that will enable unforeseen analyses based on larger sample sizes, novel visual characteristics and new methods of analysis, while equally facilitating data sharing and access. Thereby, WIAD intends to make eco-physiological sciences more transparent, reproducible, low cost and engaging. From more information read our paper (Rademacher et al., in prep.).

## The WIAD team

### Core team
WIAD was the idea of Tim Rademacher, which was developed into a vision together with Bijan Seyednasrollah and David Basler. Bijan Seyednasroolah then led the development of the WIAD v0.1.1.1. 

### Extended collaborators 
Many people have contributed in the development and testing of the tools and software offered through WIAD. In particular, we want to thank Tessa Mandra, Elise Miller, David Orwig, Neil Pederson, Andrew Richardson, and Donglai Wei. 

### Acknowledgement
WIAD depends on multiple collaborators, including contributors and users, which are thanked for their efforts in support of WIAD. This project profited from support of the National Science Foundation (DEB-1741585, DEB-1237491 and DEB-1832210).  

### References

Bunn (2008) *A dendrochronology program library in R (dplR)*, Dendrochronologia, 26 (2), 115-124, doi: 10.1016/j.dendro.2008.01.002

Rademacher, Seyednasrollah, Basler, Cheng, Mandra, Miller, Lin, Orwig, Pederson, Pfister, Richardson, Wei, Yao
(in prep.) *The Wood Image Analysis and Dataset (WIAD): open-access visual analysis tools to advance the ecological data revolution*, bioRxiv
