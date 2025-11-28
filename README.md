# macOS Development Environment Setup

A comprehensive setup script for provisioning a macOS Emacs based development environment with Homebrew packages, npm tools, pip packages, and various configurations.

## Quick Install

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/apohl79/tools/master/install.sh)"
```

## Manual Installation

```bash
git clone https://github.com/apohl79/tools.git ~/tools
cd ~/tools
python3 setup.py
```

## Usage

The setup script supports various options:

```bash
# Run full setup (all layers)
python3 setup.py

# Run specific layer only
python3 setup.py -l2          # Run layer 2 only

# Check what would be installed (dry run)
python3 setup.py -c           # Check all layers
python3 setup.py -c -l3       # Check specific layer

# Sync installed packages to config
python3 setup.py -s           # Add installed packages to setup.toml
```

## Configuration

Edit `setup.toml` to customize the packages and tools to install. The setup is organized in layers:

- **Layer 0**: Core tools (Xcode CLI, Homebrew)
- **Layer 1**: Sudo setup
- **Layer 2**: Development tools and packages
- **Layer 3**: Emacs configuration
- **Layer 4**: Shell and environment setup
- **Layer 5**: Additional configurations (jenv, etc.)

## Requirements

- macOS
- Internet connection
- Admin privileges (for some installations)
