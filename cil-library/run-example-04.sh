# Mono should be installed and in PATH

runhaskell 04_Branches.hs > 04_Branches.il &&
ilasm 04_Branches.il &&
echo &&
mono 04_Branches.exe

rm 04_Branches.il
rm 04_Branches.exe

