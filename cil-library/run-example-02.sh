# Mono should be installed and in PATH

runhaskell 02_Arith.hs > 02_Arith.il &&
ilasm 02_Arith.il &&
echo &&
mono 02_Arith.exe

rm 02_Arith.il
rm 02_Arith.exe

