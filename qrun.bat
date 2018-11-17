@echo off
for /f "tokens=*" %%a in (rons.txt) do (
call %%a
)
pause