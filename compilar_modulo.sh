#!/bin/bash
# ============================================================================
# AssumptionsLab - Script de compilación optimizado (sin sudo)
# ============================================================================

set -e

PROJECT_DIR="$HOME/ProyectosJamovi/AssumptionsLab"

echo "============================================================"
echo "  Compilación de AssumptionsLab"
echo "============================================================"
echo ""

cd "$PROJECT_DIR"

# Opción de limpieza con: ./compilar_modulo.sh --clean
if [ "$1" == "--clean" ]; then
    echo "[!] Limpiando carpeta build (reinstalación completa)..."
    rm -rf build
    echo "  ✓ Cache de build eliminada"
    echo ""
fi

# Eliminar .jmo anterior
if ls *.jmo 1> /dev/null 2>&1; then
    echo "[1/2] Eliminando .jmo anterior..."
    rm -f *.jmo
    echo "  ✓ .jmo anterior eliminado"
else
    echo "[1/2] No hay .jmo anterior que eliminar"
fi
echo ""

# Compilar reutilizando la caché de dependencias en build/
echo "[2/2] Compilando con jmvtools..."
Rscript -e "options(Ncpus = parallel::detectCores()); jmvtools::install()"

echo ""
echo "============================================================"
echo "  ✓ COMPILACIÓN COMPLETADA"
echo "============================================================"
echo ""
echo "Archivo generado:"
ls -lh *.jmo
echo ""
