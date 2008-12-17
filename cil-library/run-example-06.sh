# Mono should be installed and in PATH

runhaskell 06_Types_Tuple.hs > 06_Types_Tuple.il &&
ilasm 06_Types_Tuple.il &&
echo &&
mono 06_Types_Tuple.exe

rm 06_Types_Tuple.il
rm 06_Types_Tuple.exe

