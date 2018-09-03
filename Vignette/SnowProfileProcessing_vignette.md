Vignette: SnowProfileProcessing
================

Introduction
------------

The package was developed with the aim to create a dataset of SWE (Snow Water Equivalent) for the Euregio area ( [link](http://www.europaregion.info/it/default.asp)). The idea was to use the data collected by snow observer and shared with Local Avalanche Service in the 3 region, Trentino (I), South Tyrol (I) and Tyrol (O) to have timeseries of SWE for several points. The package was developed in the framework of project [Cryomon Sci-Pro](http://www.eurac.edu/en/research/projects/Pages/projectdetail4240.aspx). The partners are Eurac Research (South Tyrol), University of Trento (Trentino) and University of Innsbruck (Tyrol).

Description
-----------

We collect data of snowpit from MeteoTrentino, developing an R package to process the data and extract SWE timeseries.

Snowpit, done following mod.3-4, AINEVA (Cagnati, A., 2003), contain data of layers characteristic (grain type, size, ...) and physical parameter (liquid water content,density, ...).

These data are organized into 2 files, the first contatins the stratigraphy for each snowpit done during the season (possible more than one winter season), the second contain the data of density measured (horizontal sampling) for each layer for each snowpit. There are some cases that the density are not measured, for examples for thin layers the sampler is too large. With the following functions we merge the stratigraphy file with the density file, reconstruct missing density with a method based on grain type and layer hardness (Valt M. & Cagnati A., 2005) and aggregate each profile to extract information about snow height, mean snow density and snow water equivalent.

Package structure
-----------------

### *Function*

The package include these five functions:

1.  **file\_config\_xml\_parsing**: the function read and parse an external files that contain the relative path, and the key parameter used in the example scripts.

2.  **combine\_stratigraphy\_density**:the function merge stratigraphy and density data. The algorithm check data and station in the 2 input files and assign to the proper layer the density measurement.

3.  **calculate\_density\_matrix\_grain\_hardness**: the function create a coupling matrix to estimate snow density from grain type and layer hardness usign the method proposed by Valt (Valt M. & Cagnati A., 2005). The algorithm calculate the average, the standard deviation and the sample of density for each coupling of values of grain type and hardness. Only layers with density measured are used in this statistics.

4.  **calculator\_density\_and\_SWE**: the function reconstruct missing snow density from grain type and hardness usign a coupling matrix. This matrix could be calculated (and evaluated) with the function above or in can be found in literature (Valt M. & Cagnati A.). For each layer the function calculate snow water equivalent, starting from density and thickness.

5.  **aggregate\_profiles**: the function aggregate each profile calculating the snow height, the mean snow density and the snow water equivalent.

### *Folders structure*

Create the following folders structure:

-   folder

    -   **Data**

        -   **Input\_data** : Put inside the density and stratigraphy tables. The examples provide the data structure.
        -   **Output\_data** : Here scripts save results files
        -   **Support\_files**: Put here the density matrix of grain\_type and harndess (see the examples *Grain\_Hardness\_v0.csv*). Now inside there is a template based on Valt paper.

    -   **Plots**: here scripts save various plots (organized in subfolders created automatically)

    -   **File\_config**: Fill here the *file\_config\_v0.xml*.

The folders highlighted with bold character must be called as in the schema above.

### *Example\_scripts*

The functions above are used in the following scripts, given an example of usage and process of snow profiles.

Before runs each script set working directory. From Rstudio select Session --&gt; Set Working Directory --&gt; Choose Directory and select **File\_config** path. All the paths used in script and in file\_config.xml depending from **File\_config** path.

Here we provide a short description of examples.

-   **01\_Examples\_profile\_analysis.R**: This script read the 2 input files (stratigraphy + density), merge into one file (part 1), estimate density from grain type and hardness matrix (Valt & Cagnati, 2005) and calculate SWE for each layer (part 2) and aggregate each profile extracting snow height, mean snow density and snow water equivalent (part3). The input data are provided in *file\_config\_vo.xml*. This files contain the information that the script need to process data. For examples there are input path, names of variables and output file names. The file is an xml file and it works filling the tag. (Example:&lt;*tag*&gt;**fill\_here**&lt;*/tag*&gt;)
