################################################################################
### Makefile template for user ESMF application, leveraging esmf.mk mechanism ##
################################################################################

################################################################################
### Finding and including esmf.mk ##############################################

# Note: This fully portable Makefile template depends on finding environment
#       variable "ESMFMKFILE" set to point to the appropriate "esmf.mk" file,
#       as is discussed in the User's Guide.
#       However, you can still use this Makefile template even if the person
#       that installed ESMF on your system did not provide for a mechanism to
#       automatically set the environment variable "ESMFMKFILE". In this case
#       either manually set "ESMFMKFILE" in your environment or hard code the
#       location of "esmf.mk" into the include statement below.
#       Notice that the latter approach has negative impact on portability.

ifneq ($(origin ESMFMKFILE), environment)
  $(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)
ESMF_F90COMPILEOPTS=-g -O0
#ESMF_F90COMPILEOPTS=-O0 -g -qfullpath -qmaxmem=-1 -qalias=noaryovrlp  -qarch=auto -qtune=auto -qcache=auto -qsclk=micro  -O0 -g -qstrict
#ESMF_F90COMPILEOPTS=-warn all -WB -Winline -traceback -C -CB -CA -CU -ftrapuv -nozero -no-bss-init
#ESMF_F90LINKOPTS=-g -O0

################################################################################
### Compiler and linker rules using ESMF_ variables supplied by esmf.mk ########

.SUFFIXES: .f90 .F90 .c .C

.f90:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) \
	$(ESMF_F90COMPILEFREENOCPP) $<
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) \
	$(ESMF_F90LINKRPATHS) -o $@ $*.o $(ESMF_F90ESMFLINKLIBS)        

.F90:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) \
	$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) \
	$(ESMF_F90LINKRPATHS) -o $@ $*.o $(ESMF_F90ESMFLINKLIBS)        

.c:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) \
	$(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) \
	$(ESMF_CXXCOMPILECPPFLAGS) $<
	$(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) \
	$(ESMF_CXXLINKRPATHS) -o $@ $*.o $(ESMF_CXXESMFLINKLIBS)

.C:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) \
	$(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) \
	$(ESMF_CXXCOMPILECPPFLAGS) $<
	$(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) \
	$(ESMF_CXXLINKRPATHS) -o $@ $*.o $(ESMF_CXXESMFLINKLIBS)

################################################################################
### Sample targets for user ESMF applications ##################################

all: xgridEx1

clean:
	rm *.o xgridEx1

