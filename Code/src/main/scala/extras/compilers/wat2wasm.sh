#!/bin/bash

# Check for input file
if [ -z "$1" ]; then
  echo "Bitte geben Sie eine .wat-Datei als Argument an."
  exit 1
fi

# Check if input is .wat file
if [ ! -f "$1" ] || [[ "$1" != *.wat ]]; then
  echo "Die angegebene Datei ist entweder nicht vorhanden oder keine .wat-Datei."
  exit 1
fi

# Check for wat2wasm
if ! command -v wat2wasm &> /dev/null; then
  echo "wat2wasm ist nicht installiert. Bitte installieren Sie es zuerst."
  exit 1
fi

# Compile .wat to .wasm
wat_file="$1"
wasm_file="${wat_file%.wat}.wasm"

echo "Kompiliere $wat_file zu $wasm_file ..."
wat2wasm "$wat_file" -o "$wasm_file"

if [ $? -ne 0 ]; then
  echo "Fehler bei der Kompilierung der .wat-Datei."
  exit 1
fi

echo "$wasm_file erfolgreich erstellt. Ausführung mittels wasmedge unter Angabe der erforderlichen Parameter."