#!/usr/bin/env bash

echo "Se compaiono delle funzioni non contenute in Prelude devi definirle in eval.hs"
echo ""
echo "Se compaiono delle funzioni contenute in Prelude MA con altro tipo devi definirle e rinominarle in eval.hs"
echo ""
echo "Inserisci l'espressione"

read str

str="${str//./-> }"

# copy to clipboard if which xclip return the path of an executable
path_to_xclip=$(which xclip)
if [ -x "$path_to_xclip" ] ; then
   echo "${str//λ/\\}" | xclip -selection clipboard
fi

str="${str//λ/\\\\}"

sed -i "s|expr\s=.*$|expr = $str|g" eval.hs

echo ""

echo "Vuoi editare eval.hs?"
read -p "$* [y/N]: " yn
case $yn in
    [yY]*) $EDITOR eval.hs ;;
    *) ;;
esac


echo "Vuoi lanciare ghci per verificare il tipo? (:t expr)"
read -p "$* [Y/n]: " yn
case $yn in
    [Nn]*) echo "Fine" ;;
    *) ghci eval.hs ;;
esac
