#!/bin/bash
set -e

TOOLS_DIR="$HOME/tools"
REPO_URL="https://github.com/apohl79/tools.git"

echo "Installing development environment setup tools..."

# Check if tools directory already exists
if [ -d "$TOOLS_DIR" ]; then
    echo "Directory $TOOLS_DIR already exists."
    echo "Updating existing installation..."
    cd "$TOOLS_DIR"
    #git pull
else
    echo "Cloning repository to $TOOLS_DIR..."
    git clone "$REPO_URL" "$TOOLS_DIR"
    cd "$TOOLS_DIR"
fi

echo ""
echo "Running setup script..."
python3 setup.py

echo ""
echo "Installation complete!"
