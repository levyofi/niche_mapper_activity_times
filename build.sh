#!/bin/bash
#prequesites:
# 1) compile netcdf libraries with ifort
# 2) install Intel's parallel libraries for the same version as your ifort.

mpiifort -O3  -c -o"parameters_nc.o" "./parameters_nc.f90"
mpiifort -O3  -c -o"parameters_endo.o" "./parameters_endo.f90"
mpiifort -O3  -c -o"Object_module.o" "./Object_module.f90"
mpiifort -O3  -c -o"List_module.o" "./List_module.f90"
mpiifort -O3  -c -o"Factory_module.o" "./Factory_module.f90"
mpiifort -O3  -c -o"Observer_module.o" "./Observer_module.f90"
mpiifort -O3  -I${PATH_TO_NETCDF_INCLUDE_DIR} -c -o"climate_module.o" "./climate_module_nc.f90"
mpiifort -O3  -I${PATH_TO_NETCDF_INCLUDE_DIR}  -c -o"Summary_module.o" "./Summary_module.f90"
mpiifort -O3  -c -o"mouse_endo_module.o" "./mouse_endo_module.f90"
mpiifort -O3  -c -o"main.o" "./main.f90"

mpiifort -O3 -L${PATH_TO_NETCDF_LIB_DIR} -lnetcdff  -lnetcdf -lnetcdf -xhost -o"endo.cpu"  ./parameters_nc.o ./parameters_endo.o ./Factory_module.o ./List_module.o ./Summary_module.o ./mouse_endo_module.o ./Object_module.o ./Observer_module.o ./climate_module.o ./main.o  -lm


