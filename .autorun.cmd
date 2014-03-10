@echo off

set TOP=d:\working\pangaea
set TPAM=c:\working\tpam

doskey home=cd /d %HOME%
doskey phome=pushd %HOME%
doskey top=cd /d %TOP%
doskey ptop=pushd %TOP%
doskey tpam=cd /d %TPAM% 
doskey ptpam=pushd %TPAM%

doskey l=dir $*
doskey lh="C:\Program Files\LockHunter\LockHunter" $*
doskey vmware-mount="C:\Program Files (x86)\VMware\VMware Virtual Disk Development Kit\bin\vmware-mount" $*
doskey z=cls
