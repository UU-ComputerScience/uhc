# Mono should be installed and in PATH

runhaskell 01_Hello.hs > 01_Hello.il &&
ilasm2 01_Hello.il &&
echo &&
mono 01_Hello.exe

rm 01_Hello.il
rm 01_Hello.exe

