# Mono should be installed and in PATH

runhaskell 07_ByRef.hs > 07_ByRef.il &&
ilasm2 07_ByRef.il &&
echo &&
mono 07_ByRef.exe

rm 07_ByRef.il
rm 07_ByRef.exe

