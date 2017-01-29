#!/bin/sh

./git2hs.sh && 
runhaskell Setup configure && 
runhaskell Setup build && 
./rm1x-template --force --book kits/AnlgKit1.txt kits/AnlgKit2.txt kits/RhBoxKit.txt kits/SynthKit.txt kits/SEKit.txt kits/PsychKit.txt kits/AcidKit.txt kits/TeknoKit.txt kits/AmbntKit.txt kits/HardKit.txt kits/HouseKit.txt kits/BreakKit.txt kits/JunglKit.txt kits/DnBKit.txt kits/BigKit.txt kits/HipHpKit.txt kits/AcoKit.txt kits/JazzKit.txt kits/BrushKit.txt kits/PercsKit.txt kits/BDKit.txt kits/HHCyKit.txt kits/SDKit.txt kits/TomKit.txt kits/SFXKit1.txt kits/SFXKit2.txt kits/GMStandKit.txt kits/Notes.txt 

