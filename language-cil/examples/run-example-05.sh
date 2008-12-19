# Mono should be installs.d and in PATH

runhaskell 05_Tailcalls.hs > 05_Tailcalls.il &&
ilasm2 05_Tailcalls.il &&
echo &&
mono 05_Tailcalls.exe

rm 05_Tailcalls.il
rm 05_Tailcalls.exe

