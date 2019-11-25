@echo on
@rem Required vars:
@rem    WIN_MSYS2_ROOT
@rem    WIN_OTP_PATH
@rem    ERTS_VERSION

SETLOCAL
@call:log Set the paths appropriately

:: Construct unix paths
FOR /f %%i IN ('cygpath -a %~dp0..\..') DO SET "PROJECT_ROOT=%%i"
:: remove trailing /
SET "PROJECT_ROOT=%PROJECT_ROOT:~0,-1%"

call "%~dp0vcvarsall" || exit /b %ERRORLEVEL%

@call:log Test
"%WIN_MSYS2_ROOT%\usr\bin\bash.exe" -lc "${PROJECT_ROOT}/.circleci/windows/test.sh" || exit /b %ERRORLEVEL%
@call:log Test done.

exit /b 0

:log :: Display a log message
@echo :: [1;33m %time% : %* [0m>con && exit /b
