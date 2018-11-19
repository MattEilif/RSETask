@echo off
for /f "tokens=*" %%a in (ascii2.dat) do (
call %%a
)
pause