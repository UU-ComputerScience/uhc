#!/bin/sh

../../bin/shuffle files-ag-s.dep --dep --depnameprefix=RULER3_ --depsrcvar=SRC_RULER3_PREFIX --depdstvar=RULER3_BLD_PREFIX --depmainvar=RULER3_AG_S_MAIN_SRC_AG --depdpdsvar=RULER3_AG_S_DPDS_SRC_AG > files-ag-s-dep.mk
../../bin/shuffle files-ag-d.dep --dep --depnameprefix=RULER3_ --depsrcvar=SRC_RULER3_PREFIX --depdstvar=RULER3_BLD_PREFIX --depmainvar=RULER3_AG_D_MAIN_SRC_AG --depdpdsvar=RULER3_AG_D_DPDS_SRC_AG > files-ag-d-dep.mk
