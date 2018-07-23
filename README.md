The repository includes the NicheMapper code used in Levy et al. Ecological Monographs titled "Time and ecological resilience: Can diurnal animals compensate for climate change by shifting to nocturnal activity?"

### Prerequisites and input data:
The operating system should contain the following libraries:
1) NetCDF libraries that were compiled with ifort.
2) Intel's parallel libraries (must be the same version as your ifort). <br />

#### The execution folder should include the following files: <br />
3) A folder named "NetCDF_files" that includes the NetCDF input files for each coordinate. For example, for coordinate #10004, the folder should include the following files:
ALBEDO_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
GLW_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
SMOIS_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
Tsoil_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
EAH_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
PSFC_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
SWDOWN_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
Tsurface_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
EAIR_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
QAIR_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
TAH_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
TV_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
FVEG_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
RHOAIR_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
Tair_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
WIND10_10004_MIC_CLIM_36_past_25.572_-81.191.nc <br />
The input files can be downloaded from the "Knowledge Network for Biocomplexity" repository (https://knb.ecoinformatics.org/#view/doi:10.5063/F1Z899CZ).
4) A text file that contains the names of the input files for each coordinate in the input dataset, excluding the names of the variables. For example, for coordinate #10004, the file should include the line:
10004_MIC_CLIM_36_past_25.572_-81.191.nc
5) The file geo_em.d01.nc

### Execution:
To run the model, the user should run the following command: 
```
mpiexec -n [number of cores] ./endo.cpu [input text file] [name of output NetCDF file] [start year] [shade conditions] [sensitivity analysis variable] [sensitivity analysis change in variable] 
```
with the different parameters as: <br />
1) [number of cores] is the number of cores available for the model execution. 
2) [input text file] is the name of the text file that contains the names of the NetCDF input files (see prequisit no. 4).
3) [name of output NetCDF file] is the requested name of the output file.
4) [start year] is the first year of the model execution, usually 1980 for past climate and 2080 for future climate. 
5) [shade conditions] is the requested shade conditions and is either "shade" or "no_shade". 
6) [sensitivity analysis variable] is a sensitivity analysis variable and is either "mass", "min_q", and "fur_refl" for animal body mass, minimum energy expenditure, and fur reflectivity, respectively. Other values will be ignored 
7) [sensitivity analysis change in variable] is the requested change for the sensitivity analysis in percentage change from the original parameterization of the variable. For example, the value -50 will reduce the value of the variable by 50% compared to the original model. 

A sample running command can be:
```
mpiexec -n 3 ./endo.cpu past_input_files.txt past_output_fur_refl_-04.nc 1980 no_shade fur_refl -40
```
where the model will run on 3 cores, with "past_input_files.txt" as the input text file, "past_output_fur_refl_-04.nc" as the NetCDF output file, and 1980 as the first year in the model. The model will run under no shade conditions (open microhabitat), with a sensitivity analysis that simulates a fur reflectivity that is lower by 40% than the fur reflectivity used in the original model.
