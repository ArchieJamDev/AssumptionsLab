#!/bin/bash

# Colores para mensajes en la terminal
VERDE='\033[0;32m'
AZUL='\033[0;34m'
ROJO='\033[0;31m'
NC='\033[0m' # Sin color

echo -e "${AZUL}=== Iniciando actualización de GitHub ===${NC}"

# 1. Comprobar si estamos en un repositorio Git
if [ ! -d ".git" ]; then
    echo -e "${ROJO}Error: Este directorio no es un repositorio Git.${NC}"
    exit 1
fi

# 2. Mostrar el estado actual
echo -e "\n${AZUL}Comprobando estado del repositorio...${NC}"
git status -s

# 3. Preguntar si se desean añadir todos los cambios
echo -e "\n¿Deseas añadir todos los cambios pendientes? (s/n)"
read -r respuesta
if [[ "$respuesta" =~ ^([sS][iI]|[sS])$ ]]; then
    git add .
    echo -e "${VERDE}Cambios añadidos correctamente (git add .).${NC}"
else
    echo -e "${ROJO}Operación cancelada por el usuario.${NC}"
    exit 0
fi

# 4. Solicitar el mensaje del commit
echo -e "\nIntroduce el mensaje para el commit (deja en blanco para usar fecha/hora por defecto):"
read -r mensaje_commit

if [ -z "$mensaje_commit" ]; then
    mensaje_commit="Actualización automática: $(date '+%Y-%m-%d %H:%M:%S')"
fi

# 5. Realizar el commit
git commit -m "$mensaje_commit"
echo -e "${VERDE}Commit realizado con éxito: \"$mensaje_commit\"${NC}"

# 6. Detectar la rama actual (por defecto main o master)
rama_actual=$(git branch --show-current)
if [ -z "$rama_actual" ]; then
    rama_actual="main"
fi

echo -e "\n${AZUL}Subiendo cambios a GitHub (rama: $rama_actual)...${NC}"

# 7. Hacer el push
if git push origin "$rama_actual"; then
    echo -e "\n${VERDE}¡Repositorio actualizado y sincronizado con éxito en GitHub! 🚀${NC}"
else
    echo -e "\n${ROJO}Error al subir los cambios. Comprueba tu conexión o conflictos pendientes.${NC}"
    exit 1
fi
