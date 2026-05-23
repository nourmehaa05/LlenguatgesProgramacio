@echo off
REM Establecer las rutas de Prolog
set SWIPL_BIN=C:\Program Files\swipl\bin
set SWIPL_LIB=C:\Program Files\swipl\lib
set PATH=%SWIPL_BIN%;%PATH%

REM Cambiar al directorio Pips
cd /d "C:\Users\nourm\OneDrive\Documentos\CARRERA\LlenguatgesProgramacio\Pips"

REM Ejecutar la aplicación Java
java -cp ".;%SWIPL_LIB%\jpl.jar" -Djava.library.path="%SWIPL_BIN%;%SWIPL_LIB%" PipsGUI

pause
