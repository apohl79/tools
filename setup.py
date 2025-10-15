#!/usr/bin/env python3
import os
import sys
import subprocess
import platform
import shutil
import glob
import yaml
import argparse
import json


def get_script_dir():
    """get the directory of the script, resolving symlinks."""
    script_path = os.path.realpath(sys.argv[0])
    return os.path.dirname(script_path)


def link(name, source, target):
    """create a symlink if it doesn't exist."""
    if not os.path.exists(target):
        print(f"linking {name} ({source} -> {target})")
        os.symlink(source, target)
    else:
        print(f"skipping {name} (exists)")


def link_force(name, source, target):
    """create a symlink, removing existing target if needed."""
    if os.path.exists(target):
        os.remove(target)
    link(name, source, target)


def run_command(cmd, shell=True, check=False, env=None):
    """run a shell command and return success status."""
    try:
        result = subprocess.run(cmd, shell=shell, check=check, capture_output=True, text=True, env=env)
        if result.stdout:
            print(result.stdout, end='')
        if result.stderr and result.returncode != 0:
            print(result.stderr, end='', file=sys.stderr)
        return result.returncode == 0
    except subprocess.CalledProcessError:
        return False


def command_exists(cmd):
    """check if a command exists in PATH."""
    return shutil.which(cmd) is not None


def get_installed_brew_packages():
    """get list of installed brew packages."""
    try:
        result = subprocess.run(['brew', 'list', '--formula'], capture_output=True, text=True)
        if result.returncode == 0:
            return set(result.stdout.strip().split('\n'))
    except (subprocess.SubprocessError, FileNotFoundError):
        pass
    return set()


def get_installed_brew_casks():
    """get list of installed brew casks."""
    try:
        result = subprocess.run(['brew', 'list', '--cask'], capture_output=True, text=True)
        if result.returncode == 0:
            return set(result.stdout.strip().split('\n'))
    except (subprocess.SubprocessError, FileNotFoundError):
        pass
    return set()


def get_installed_npm_packages():
    """get list of globally installed npm packages."""
    try:
        result = subprocess.run(['npm', 'list', '-g', '--depth=0', '--json'], capture_output=True, text=True)
        if result.returncode == 0:
            data = json.loads(result.stdout)
            return set(data.get('dependencies', {}).keys())
    except (subprocess.SubprocessError, FileNotFoundError, json.JSONDecodeError):
        pass
    return set()


def get_installed_pip_packages():
    """get list of installed pip packages."""
    try:
        result = subprocess.run(['pip3', 'list', '--format=json'], capture_output=True, text=True)
        if result.returncode == 0:
            packages = json.loads(result.stdout)
            return {pkg['name'].lower() for pkg in packages}
    except (subprocess.SubprocessError, FileNotFoundError, json.JSONDecodeError):
        pass
    return set()


def is_brew_tap_added(tap):
    """check if a brew tap is already added."""
    try:
        result = subprocess.run(['brew', 'tap'], capture_output=True, text=True)
        if result.returncode == 0:
            return tap in result.stdout
    except (subprocess.SubprocessError, FileNotFoundError):
        pass
    return False


def check_symlink(source, target):
    """check if a symlink exists and points to the correct location."""
    if os.path.islink(target):
        return os.readlink(target) == source
    return False


def load_config(config_path):
    """load configuration from YAML file."""
    with open(config_path, 'r') as f:
        return yaml.safe_load(f)


def install_brew_packages(packages, package_name="packages", check_only=False):
    """install missing brew packages."""
    if not packages:
        return
    
    # Separate formulae and casks
    installed_formulae = get_installed_brew_packages()
    installed_casks = get_installed_brew_casks()
    installed_all = installed_formulae | installed_casks
    
    missing = []
    for pkg in packages:
        # Remove any version suffix for checking (e.g., openjdk@21 -> openjdk)
        pkg_base = pkg.split('@')[0]
        if pkg not in installed_all and pkg_base not in installed_all:
            missing.append(pkg)
    
    if missing:
        print(f"\n{package_name}:")
        print(f"  missing ({len(missing)}): {', '.join(missing)}")
        if not check_only:
            print("  installing...")
            run_command(f"brew install {' '.join(missing)}")
    else:
        print(f"\n{package_name}: all {len(packages)} packages already installed")


def install_npm_packages(packages, check_only=False):
    """install missing global npm packages."""
    if not packages:
        return
        
    installed = get_installed_npm_packages()
    missing = []
    
    for pkg in packages:
        # Extract package name without scope for checking
        pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
        if pkg not in installed and pkg_name not in installed:
            missing.append(pkg)
    
    if missing:
        print("\nnpm packages:")
        print(f"  missing ({len(missing)}): {', '.join(missing)}")
        if not check_only:
            print("  installing...")
            for package in missing:
                run_command(f"npm install -g {package}")
    else:
        print(f"\nnpm packages: all {len(packages)} packages already installed")


def install_pip_packages(packages, check_only=False):
    """install missing pip packages."""
    if not packages:
        return
        
    installed = get_installed_pip_packages()
    missing = []
    
    for pkg in packages:
        if pkg.lower() not in installed:
            missing.append(pkg)
    
    if missing:
        print("\npython packages:")
        print(f"  missing ({len(missing)}): {', '.join(missing)}")
        if not check_only:
            print("  installing...")
            run_command(f"pip3 install {' '.join(missing)}")
    else:
        print(f"\npython packages: all {len(packages)} packages already installed")


def apply_symlinks(symlinks, script_dir, home, check_only=False):
    """apply symlinks from configuration."""
    print("\nchecking symlinks...")
    needs_update = []
    
    for symlink in symlinks:
        source = symlink['source'].format(script_dir=script_dir, home=home)
        target = symlink['target'].format(script_dir=script_dir, home=home)
        
        if os.path.exists(target):
            if check_symlink(source, target):
                print(f"  ✓ {symlink['name']} is correctly linked")
            else:
                print(f"  ✗ {symlink['name']} exists but points to wrong location")
                needs_update.append(symlink)
        else:
            print(f"  ✗ {symlink['name']} is missing")
            needs_update.append(symlink)
    
    if needs_update and not check_only:
        print("\nupdating symlinks...")
        for symlink in needs_update:
            source = symlink['source'].format(script_dir=script_dir, home=home)
            target = symlink['target'].format(script_dir=script_dir, home=home)
            
            if symlink.get('force', False):
                link_force(symlink['name'], source, target)
            else:
                link(symlink['name'], source, target)


def detect_emacs_setup(home):
    """detect existing emacs setup (doom or light)."""
    doom_path = os.path.join(home, ".config/doom")
    light_path = os.path.join(home, ".emacs")
    
    if os.path.exists(doom_path) or os.path.exists(os.path.join(home, ".config/emacs")):
        return "doom"
    elif os.path.exists(light_path):
        return "light"
    return None


def main():
    # Parse command line arguments
    parser = argparse.ArgumentParser(description='setup development environment')
    parser.add_argument('--check', action='store_true', help='only check what would be installed')
    parser.add_argument('--update', action='store_true', help='update existing packages')
    args = parser.parse_args()
    
    # Get script directory
    script_dir = get_script_dir()
    if not script_dir:
        script_dir = os.getcwd()
    
    # Load configuration
    config_path = os.path.join(script_dir, 'setup.yaml')
    if not os.path.exists(config_path):
        print(f"error: configuration file {config_path} not found")
        sys.exit(1)
    
    config = load_config(config_path)
    
    # Change to HOME directory
    home = os.path.expanduser("~")
    os.chdir(home)
    
    # Create ~/bin if it doesn't exist
    bin_dir = os.path.join(home, "bin")
    if not os.path.exists(bin_dir):
        os.makedirs(bin_dir)
    
    # Check oh-my-zsh installation
    print("\nchecking shell configuration...")
    oh_my_zsh_path = os.path.join(home, ".oh-my-zsh", "oh-my-zsh.sh")
    zsh_custom = os.environ.get("ZSH_CUSTOM", os.path.join(home, ".oh-my-zsh/custom"))
    p10k_path = os.path.join(zsh_custom, "themes/powerlevel10k")
    
    if os.path.exists(oh_my_zsh_path):
        print("  ✓ oh-my-zsh is installed")
        # Check powerlevel10k
        if os.path.exists(p10k_path):
            print("  ✓ powerlevel10k theme is installed")
        else:
            print("  ✗ powerlevel10k theme is missing")
            if not args.check:
                print("installing powerlevel10k...")
                run_command(f"git clone --depth=1 https://github.com/romkatv/powerlevel10k.git {p10k_path}")
    else:
        print("  ✗ oh-my-zsh is not installed")
        if not args.check:
            # Remove existing directories
            for path in [os.path.join(home, ".oh-my-zsh"), "/etc/oh-my-zsh"]:
                if os.path.exists(path):
                    shutil.rmtree(path)
            
            print("\ninstalling oh-my-zsh...")
            env = os.environ.copy()
            env["RUNZSH"] = "no"
            run_command('sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"', env=env)
            
            print("Installing powerlevel10k...")
            run_command(f"git clone --depth=1 https://github.com/romkatv/powerlevel10k.git {p10k_path}")
    
    # Create symlinks from configuration
    apply_symlinks(config['symlinks'], script_dir, home, args.check)
    
    # Detect existing Emacs setup or ask user
    existing_setup = detect_emacs_setup(home)
    if existing_setup:
        print(f"\ndetected existing {existing_setup} Emacs setup")
        use_existing = input(f"continue with {existing_setup} setup? [Y/n]: ").strip()
        if not use_existing or use_existing.lower() in ["y", "yes"]:
            doom_choice = "1" if existing_setup == "doom" else "2"
        else:
            print("\nchoose emacs setup:")
            print(" 1) doom")
            print(" 2) light") 
            print(" 3) skip")
            doom_choice = input().strip()
    else:
        print("\nchoose emacs setup:")
        print(" 1) doom")
        print(" 2) light")
        print(" 3) skip")
        doom_choice = input().strip()
    
    if doom_choice == "1":
        config_dir = os.path.join(home, ".config")
        if not os.path.exists(config_dir):
            os.makedirs(config_dir)
        emacs_link = config['emacs_symlinks']['doom']
        link(emacs_link['name'], 
             emacs_link['source'].format(script_dir=script_dir, home=home),
             emacs_link['target'].format(script_dir=script_dir, home=home))
    elif doom_choice == "2":
        emacs_link = config['emacs_symlinks']['light']
        link(emacs_link['name'], 
             emacs_link['source'].format(script_dir=script_dir, home=home),
             emacs_link['target'].format(script_dir=script_dir, home=home))
    else:
        print("skipping emacs")
    
    # macOS-specific installations
    if platform.system() == "Darwin":
        # Install Homebrew if not present
        if not command_exists("brew"):
            print("installing homebrew...")
            run_command('/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"')
        
        if doom_choice == "1":
            # Check and install nvm if needed
            if not command_exists("nvm"):
                install_brew_packages(["nvm"], "node version manager", args.check)
            
            # Setup nvm and install node if not in check mode
            if not args.check and command_exists("nvm"):
                run_command("nvm install 20")
                run_command("nvm use 20")
            
            # Install emacs and dependencies
            print("\nchecking emacs installation...")
            needs_emacs_install = not command_exists("emacs")
            
            if needs_emacs_install:
                print("  Emacs not found, will install")
            else:
                print("  Emacs is already installed")
            
            # Always check/install dependencies as they might be missing after OS upgrade
            install_brew_packages(config['brew']['emacs_dependencies'], "emacs dependencies", args.check)
            install_pip_packages(config['pip']['emacs_dependencies'], args.check)
            
            # language servers
            install_brew_packages(config['brew']['language_servers'], "language servers", args.check)
            npm_packages = [pkg for pkg in config['npm']['global'] if 'language-server' in pkg]
            install_npm_packages(npm_packages, args.check)
            
            # Install emacs-plus if needed and not in check mode
            if needs_emacs_install and not args.check:
                # Add taps
                for tap in config['brew']['taps']:
                    if not is_brew_tap_added(tap):
                        print(f"Adding tap: {tap}")
                        run_command(f"brew tap {tap}")
                
                # Install emacs formula
                emacs_formula = config['brew']['emacs_formula']
                install_cmd = f"brew install {' '.join(emacs_formula['options'])} {emacs_formula['name']}"
                print(f"installing {emacs_formula['name']}...")
                run_command(install_cmd)
                run_command("defaults write org.gnu.Emacs TransparentTitleBar DARK")
            
            # Check doom emacs installation
            emacs_config_path = os.path.join(home, ".config/emacs")
            doom_bin = os.path.join(emacs_config_path, "bin/doom")
            
            print("\nchecking doom emacs installation...")
            if os.path.exists(emacs_config_path):
                print("  Doom Emacs is installed")
                # Check if doom binary exists and offer repair
                if not os.path.exists(doom_bin):
                    print("  Warning: doom binary not found")
                    if not args.check:
                        repair = input("  Repair Doom Emacs installation? [Y/n]: ").strip()
                        if not repair or repair.lower() in ["y", "yes"]:
                            print("  running doom sync...")
                            env = os.environ.copy()
                            env["PATH"] = "/opt/homebrew/bin:" + env.get("PATH", "")
                            run_command(f"{doom_bin} sync", env=env)
            else:
                print("  Doom Emacs not found")
                
            # Always check/install doom dependencies
            install_brew_packages(config['brew']['doom_dependencies'], "doom dependencies", args.check)
            
            # Install doom emacs if needed and not in check mode
            if not os.path.exists(emacs_config_path) and not args.check:
                print("\ninstalling doom emacs...")
                run_command(f"git clone --depth 1 https://github.com/doomemacs/doomemacs {emacs_config_path}")
                
                # Add /opt/homebrew/bin to PATH for doom commands
                env = os.environ.copy()
                env["PATH"] = "/opt/homebrew/bin:" + env.get("PATH", "")
                
                run_command(f"{doom_bin} install --force --env --install --fonts --hooks", env=env)
                run_command(f"{doom_bin} sync", env=env)
                
            # Check and install dictionaries
            spelling_dir = os.path.join(home, "Library/Spelling")
            os.makedirs(spelling_dir, exist_ok=True)
            
            dict_files = [
                ("de_DE_frami.aff", "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/de/de_DE_frami.aff"),
                ("de_DE_frami.dic", "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/de/de_DE_frami.dic"),
                ("en_US.aff", "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff"),
                ("en_US.dic", "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic")
            ]
            
            missing_dicts = []
            for filename, url in dict_files:
                if not os.path.exists(os.path.join(spelling_dir, filename)):
                    missing_dicts.append((filename, url))
            
            if missing_dicts:
                print(f"\ndictionaries: {len(missing_dicts)} missing")
                if not args.check:
                    print("installing dictionaries...")
                    for filename, url in missing_dicts:
                        run_command(f"wget -nc {url} -O {spelling_dir}/{filename}")
            else:
                print("\ndictionaries: all installed")
        
        # Ask about dev environment installation
        install_dev = input("install dev environment and tools? [Y/n]: ").strip()
        
        if not install_dev or install_dev.lower() in ["y", "yes"]:
            print("installing environment...")
            
            # Install packages by category
            brew_config = config['brew']
            install_brew_packages(brew_config['cloud_storage'], "cloud storage", args.check)
            install_brew_packages(brew_config['development_tools'], "development tools", args.check)
            install_brew_packages(brew_config['java_tools'], "java tools", args.check)
            install_brew_packages(brew_config['productivity_tools'], "productivity tools", args.check)
            install_brew_packages(brew_config['communication'], "communication apps", args.check)
            install_brew_packages(brew_config['browsers'], "browsers", args.check)
            install_brew_packages(brew_config['other_apps'], "other applications", args.check)
            
            # npm and pip packages
            install_npm_packages(config['npm']['global'], args.check)
            install_pip_packages(config['pip']['other'], args.check)
            
            # Java setup - only if jenv is installed and not in check mode
            if command_exists("jenv") and not args.check:
                # Check if Java 21 is already configured
                result = subprocess.run(['jenv', 'versions'], capture_output=True, text=True)
                if '21' not in result.stdout:
                    print("\nconfiguring java 21...")
                    run_command("jenv add /opt/homebrew/opt/openjdk@21")
                    run_command("jenv global 21")
            
            # Link jdtls configs if not in check mode
            if not args.check:
                jdtls_patterns = glob.glob("/opt/homebrew/Cellar/jdtls/*/libexec/config_mac")
                if jdtls_patterns:
                    # Create jdtls directory if it doesn't exist
                    jdtls_dir = os.path.join(home, "tools/jdtls")
                    os.makedirs(jdtls_dir, exist_ok=True)
                    link("jdtls/config_mac", jdtls_patterns[0], os.path.join(jdtls_dir, "config_mac"))
                
                jdtls_plugin_patterns = glob.glob("/opt/homebrew/Cellar/jdtls/*/libexec/plugins")
                if jdtls_plugin_patterns:
                    link("jdtls/plugins", jdtls_plugin_patterns[0], os.path.join(home, "tools/jdtls/plugins"))
    
    # Summary if in check mode
    if args.check:
        print("\n" + "="*50)
        print("check complete. run without --check to install/update.")
        print("="*50)


if __name__ == "__main__":
    main()
