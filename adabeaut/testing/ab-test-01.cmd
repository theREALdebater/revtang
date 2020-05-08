@echo off

rem This script may need to be modified to suit your environment. The target of the gprbuild 
rem program may need to be changed. The locations of the obj and bin directories may need to be 
rem changed. 

setlocal
set TARGET=x86_64-w64-mingw32
set OBJDIR=..\..\..\obj\
set BINDIR=..\..\..\bin\
set TESTORIG=ab-test-01-orig.ada
set TESTFILE=ab-test-01.ada

rem Build the adabeaut program
gprbuild -Padabeaut --target=%TARGET%
if errorlevel 1 goto :FailedBuild

rem Copy the resulting .exe file into a bin folder
copy %OBJDIR%*.exe %BINDIR%

rem Make a copy of the ab-test-01-orig.ada
if exist %TESTFILE% del %TESTFILE%
copy %TESTORIG% %TESTFILE%

rem Run the test
%BINDIR%adabeaut %TESTFILE%
if errorlevel 1 goto :FailedRun
goto :GoodRun

:FailedBuild
echo Build failed
goto :EOF

:FailedRun
echo Run failed
goto :EOF

:GoodRun
echo Success: Examine ab-test-01.ada to ensure it has been processed correctly. 

