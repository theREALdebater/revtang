gprbuild -Prevtang
if errorlevel 1 goto :Failed
adabeaut --margin=100 ab-test-01.ada
goto :EOF
:Failed
echo Build failed
