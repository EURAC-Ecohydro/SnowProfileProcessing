SnowProfileProcessing
================

R package for the postprocessing of snow pit measurement.

Description
-----------

Snowpit, done following mod.3-4, AINEVA (Cagnati, A., 2003), contain data of layers characteristic (grain type, size, ...) and physical parameter (liquid water content,density, ...).

These data are organized into 2 files, the first contatins the stratigraphy for each snowpit done during the season (possible more than one winter season), the second contain the data of density measured (horizontal sampling) for each layer for each snowpit. There are some cases that the density are not measured, for examples for thin layers the sampler is too large. With the following functions we merge the stratigraphy file with the density file, reconstruct missing density with a method based on grain type and layer hardness (Valt M. & Cagnati A., 2005) and aggregate each profile to extract information about snow height, mean snow density and snow water equivalent.

#### Folder *R*

The package include these five function:

1.  **file\_config\_xml\_parsing**: the function read and parse an external files that contain the relative path, and the key parameter used in the example scripts.
2.  **combine\_stratigraphy\_density**: the function merge stratigraphy and density data. The algorithm check data and station in the 2 input files and assign to the proper layer the density measurement.
3.  **calculate\_density\_matrix\_grain\_hardness**: the function create a coupling matrix to estimate snow density from grain type and layer hardness usign the method proposed by Valt (Valt M. & Cagnati A., 2005). The algorithm calculate the average, the standard deviation and the sample of density for each coupling of values of grain type and hardness. Only layers with density measured are used in this statistics.
4.  **calculator\_density\_and\_SWE**: the function reconstruct missing snow density from grain type and hardness usign a coupling matrix. This matrix could be calculated (and evaluated) with the function above or in can be found in literature (Valt M. & Cagnati A.). For each layer the function calculate snow water equivalent, starting from density and thickness.
5.  **aggregate\_profiles**: the function aggregate each profile calculating the snow height, the mean snow density and the snow water equivalent.

These function are used in t

References
----------

-   Cagnati, A., 2003. *Sistemi di Misura e metodi di osservazio-ne nivometeorologici*, AINEVA, 14 Trento, p. 186
-   Valt M & Cagnati A., 2005 *Stima della Densità della neve conoscendo la forma dei grani e la durezza*, Neve e Valanghe, 55, p. 40-45
