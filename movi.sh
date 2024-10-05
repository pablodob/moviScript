#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Debe ejecutar con la ruta del archivo: $0 <ruta_al_archivo>"
    exit 1
fi

FILE=$1

# Pasar comandos directamente a GHCi usando echo para cargar el archivo y ejecutar la función
echo ":l Main.hs" > temp_ghci_cmds.txt
echo "run \"$FILE\"" >> temp_ghci_cmds.txt
echo ":quit" >> temp_ghci_cmds.txt

# Ejecuta GHCi y pasa el archivo con los comandos
ghci < temp_ghci_cmds.txt

# Elimina el archivo temporal después de ejecutar
rm temp_ghci_cmds.txt