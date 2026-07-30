@echo off
setlocal EnableExtensions
rem Windows environment names are case-insensitive. Expanding both spellings
rem duplicates the same value and can exceed cmd.exe's input-line limit.
set "RUZU_CMAKE_PATH=%PATH%"
set "Path="
set "PATH="
set "PATH=%RUZU_CMAKE_PATH%"
if not defined RUZU_CMAKE_EXE exit /b 9009
if /i "%~1"=="--build" (
    "%RUZU_CMAKE_EXE%" %* --parallel 1
) else (
    rem cubeb-sys forces the static debug CRT, while Rust's MSVC debug profile
    rem links the dynamic release CRT. Make the final cache definitions win
    rem and keep native C/C++ archives ABI-compatible with Rust test binaries.
    "%RUZU_CMAKE_EXE%" %* -DUSE_STATIC_MSVC_RUNTIME=OFF -DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreadedDLL "-DCMAKE_C_FLAGS_DEBUG=/D_ITERATOR_DEBUG_LEVEL=0" "-DCMAKE_CXX_FLAGS_DEBUG=/D_ITERATOR_DEBUG_LEVEL=0"
)
exit /b %ERRORLEVEL%
