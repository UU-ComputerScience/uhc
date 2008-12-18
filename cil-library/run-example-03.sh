# Mono should be installed and in PATH

runhaskell 03_IO_Locals.hs > 03_IO_Locals.il &&
ilasm2 03_IO_Locals.il &&
echo &&
mono 03_IO_Locals.exe

rm 03_IO_Locals.il
rm 03_IO_Locals.exe

