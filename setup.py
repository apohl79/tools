#!/usr/bin/env python3
import os
import sys
import subprocess
import platform
import shutil
import glob
import argparse
import json
import importlib

# ANSI color codes
GREEN = '\033[92m'
RED = '\033[91m'
BLUE = '\033[94m'
CYAN = '\033[96m'
RESET = '\033[0m'

# ANSI cursor control
SAVE_CURSOR = '\033[s'
RESTORE_CURSOR = '\033[u'
CLEAR_LINE = '\033[K'

# Terminal title control
def set_terminal_title(title):
    """Set terminal window/tab title."""
    print(f"\033]0;{title}\007", end='', flush=True)

# Global progress tracking
class ProgressTracker:
    def __init__(self):
        self.total_tasks = 0
        self.completed_tasks = 0
        self.enabled = False

    def set_total(self, total):
        self.total_tasks = total
        self.completed_tasks = 0
        self.enabled = total > 0

    def start_task(self, task_description):
        if not self.enabled:
            return
        percentage = int((self.completed_tasks / self.total_tasks) * 100)
        set_terminal_title(f"[{percentage}%] {task_description}")

    def complete_task(self):
        if not self.enabled:
            return
        self.completed_tasks += 1

    def finish(self):
        set_terminal_title("complete")

# Global instance
progress = ProgressTracker()

# Python 3.11+ has tomllib built-in, older versions need tomli
try:
    import tomllib
except ModuleNotFoundError:
    try:
        import tomli as tomllib
    except ModuleNotFoundError:
        print("tomli module not found (required for Python < 3.11)")
        print("Installing tomli automatically...")
        result = subprocess.run([sys.executable, '-m', 'pip', 'install', 'tomli'], capture_output=True, text=True)
        if result.returncode == 0:
            print("✓ tomli installed successfully")

            # Dynamically import the newly installed tomli module
            tomllib = importlib.import_module('tomli')
            ignored_path = os.path.expanduser("~/.config/setup-tools/ignored_packages.toml")
            os.makedirs(os.path.dirname(ignored_path), exist_ok=True)

            # Load existing ignored packages if file exists
            ignored = {'pip_packages': set()}
            if os.path.exists(ignored_path):
                try:
                    with open(ignored_path, 'rb') as f:
                        existing = tomllib.load(f)
                        ignored['pip_packages'] = set(existing.get('pip_packages', []))
                except:
                    pass

            # Add tomli to ignored pip packages
            if 'tomli' not in ignored['pip_packages']:
                ignored['pip_packages'].add('tomli')

                # Read the full file to preserve other sections
                file_content = ""
                if os.path.exists(ignored_path):
                    with open(ignored_path, 'r') as f:
                        file_content = f.read()

                # Find and update pip_packages section, or add it
                if 'pip_packages = [' in file_content:
                    # Update existing section
                    lines = file_content.split('\n')
                    new_lines = []
                    in_pip_section = False
                    for line in lines:
                        if 'pip_packages = [' in line:
                            in_pip_section = True
                            new_lines.append(line)
                            # Add tomli if not already there
                            if '    "tomli",' not in file_content:
                                new_lines.append('    "tomli",')
                        elif in_pip_section and line.strip() == ']':
                            in_pip_section = False
                            new_lines.append(line)
                        elif not (in_pip_section and '"tomli"' in line):
                            new_lines.append(line)
                        elif in_pip_section and '"tomli"' in line:
                            # Keep existing tomli line
                            new_lines.append(line)

                    with open(ignored_path, 'w') as f:
                        f.write('\n'.join(new_lines))
                else:
                    # Add new section
                    if file_content and not file_content.endswith('\n\n'):
                        file_content += '\n\n' if file_content.endswith('\n') else '\n\n'
                    file_content += 'pip_packages = [\n"tomli",\n]\n'
                    with open(ignored_path, 'w') as f:
                        f.write(file_content)

                print("Added tomli to ignored packages")
        else:
            print("✗ Failed to install tomli")
            print("Please install manually with: pip3 install tomli")
            sys.exit(1)


def get_script_dir():
    """get the directory of the script, resolving symlinks."""
    script_path = os.path.realpath(sys.argv[0])
    return os.path.dirname(script_path)


def link(name, source, target):
    """create a symlink if it doesn't exist."""
    # Use lexists to detect both valid symlinks and broken symlinks
    if not os.path.lexists(target):
        print(f"linking {name} ({source} -> {target})")
        os.symlink(source, target)
    elif os.path.islink(target):
        # Symlink exists - check if it points to the correct location
        if os.readlink(target) == source:
            print(f"skipping {name} (already linked correctly)")
        else:
            print(f"skipping {name} (symlink exists but points to {os.readlink(target)})")
    else:
        print(f"skipping {name} (exists as regular file/directory)")


def link_force(name, source, target):
    """create a symlink, removing existing target if needed."""
    # Use lexists to detect both valid symlinks and broken symlinks
    if os.path.lexists(target):
        if os.path.islink(target):
            os.unlink(target)
        elif os.path.isdir(target):
            shutil.rmtree(target)
        else:
            os.remove(target)
    link(name, source, target)


def run_command(cmd, shell=True, check=False, env=None, stream_output=True, prompt_on_error=True):
    """run a shell command and optionally prompt on failure.

    Args:
        check: If True, raise CalledProcessError on non-zero exit (default: False for prompting)
        stream_output: If True, stream output in real-time. If False, capture and print after completion.
        prompt_on_error: If True, prompt user to continue or abort on error (default: True)
    """
    if stream_output:
        # Stream output in real-time (no capture)
        result = subprocess.run(cmd, shell=shell, check=False, env=env)
        success = result.returncode == 0
    else:
        # Capture output and print after completion
        result = subprocess.run(cmd, shell=shell, check=False, capture_output=True, text=True, env=env)
        if result.stdout:
            print(result.stdout, end='')
        if result.stderr and result.returncode != 0:
            print(result.stderr, end='', file=sys.stderr)
        success = result.returncode == 0

    # Handle failure
    if not success:
        if prompt_on_error:
            print(f"\n{RED}✗ Command failed with exit code {result.returncode}{RESET}")
            response = input(f"Continue anyway? [y/N]: ").strip().lower()
            if response not in ['y', 'yes']:
                print(f"\n{RED}Aborting setup.{RESET}")
                sys.exit(result.returncode)
        elif check:
            # If check is True and prompting is disabled, raise exception
            raise subprocess.CalledProcessError(result.returncode, cmd)

    return success


def command_exists(cmd):
    """check if a command exists in PATH."""
    return shutil.which(cmd) is not None


def get_all_brew_packages():
    """get list of all installed brew packages (including dependencies)."""
    try:
        result = subprocess.run(['brew', 'list', '--formula'], capture_output=True, text=True)
        if result.returncode == 0:
            packages = [pkg.strip() for pkg in result.stdout.strip().split('\n') if pkg.strip()]
            # brew list --formula doesn't include tap prefix, so no normalization needed
            # but we'll normalize anyway for consistency
            normalized = set()
            for pkg in packages:
                pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
                normalized.add(pkg_name)
            return normalized
    except (subprocess.SubprocessError, FileNotFoundError):
        pass
    return set()


def get_installed_brew_packages():
    """get list of explicitly installed brew packages (not dependencies)."""
    try:
        result = subprocess.run(['brew', 'leaves'], capture_output=True, text=True)
        if result.returncode == 0:
            packages = [pkg.strip() for pkg in result.stdout.strip().split('\n') if pkg.strip()]
            # Normalize: strip tap prefix (e.g., "getsentry/tools/sentry-cli" -> "sentry-cli")
            normalized = set()
            for pkg in packages:
                pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
                normalized.add(pkg_name)
            return normalized
    except (subprocess.SubprocessError, FileNotFoundError):
        pass
    return set()


def get_installed_brew_casks():
    """get list of installed brew casks."""
    try:
        result = subprocess.run(['brew', 'list', '--cask'], capture_output=True, text=True)
        if result.returncode == 0:
            packages = [pkg.strip() for pkg in result.stdout.strip().split('\n') if pkg.strip()]
            # Normalize: strip tap prefix
            normalized = set()
            for pkg in packages:
                pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
                normalized.add(pkg_name)
            return normalized
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


def get_pip_command():
    """Get the correct pip command for the Homebrew Python installation."""
    # Look for versioned python3.X commands in /opt/homebrew/bin or /usr/local/bin
    import re
    import glob

    # Check common Homebrew locations
    homebrew_paths = ['/opt/homebrew/bin', '/usr/local/bin']

    for brew_path in homebrew_paths:
        if os.path.exists(brew_path):
            # Find all python3.X binaries
            python_bins = glob.glob(os.path.join(brew_path, 'python3.*'))
            if python_bins:
                # Sort to get the highest version
                python_bins.sort(reverse=True)
                for python_bin in python_bins:
                    # Extract version from filename (e.g., python3.14 -> 3.14)
                    version_match = re.search(r'python(3\.\d+)', os.path.basename(python_bin))
                    if version_match:
                        version = version_match.group(1)
                        pip_cmd = f"pip{version}"
                        # Check if this pip exists
                        if shutil.which(pip_cmd):
                            return pip_cmd

    # Fallback to pip3
    return "pip3"


def get_installed_pip_packages():
    """get list of installed pip packages."""
    try:
        pip_cmd = get_pip_command()
        result = subprocess.run([pip_cmd, 'list', '--format=json'], capture_output=True, text=True)
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
    """load configuration from TOML file."""
    with open(config_path, 'rb') as f:
        return tomllib.load(f)


def load_ignored_packages():
    """load ignored packages from ~/.config/setup-tools/ignored_packages.toml."""
    ignored_path = os.path.expanduser("~/.config/setup-tools/ignored_packages.toml")
    if os.path.exists(ignored_path):
        with open(ignored_path, 'rb') as f:
            return tomllib.load(f)
    return {}


def save_ignored_packages(ignored_packages, keep_dependencies=None):
    """save ignored packages to ~/.config/setup-tools/ignored_packages.toml."""
    ignored_path = os.path.expanduser("~/.config/setup-tools/ignored_packages.toml")
    os.makedirs(os.path.dirname(ignored_path), exist_ok=True)

    content = []
    content.append("# Packages to ignore during sync\n")
    content.append("# This file is read by setup.py when syncing packages\n\n")

    if ignored_packages.get('brew_formulae'):
        content.append("brew_formulae = [\n")
        for pkg in sorted(ignored_packages['brew_formulae']):
            content.append(f'    "{pkg}",\n')
        content.append("]\n\n")

    if ignored_packages.get('brew_casks'):
        content.append("brew_casks = [\n")
        for pkg in sorted(ignored_packages['brew_casks']):
            content.append(f'    "{pkg}",\n')
        content.append("]\n\n")

    if ignored_packages.get('npm_packages'):
        content.append("npm_packages = [\n")
        for pkg in sorted(ignored_packages['npm_packages']):
            content.append(f'    "{pkg}",\n')
        content.append("]\n\n")

    if ignored_packages.get('pip_packages'):
        content.append("pip_packages = [\n")
        for pkg in sorted(ignored_packages['pip_packages']):
            content.append(f'    "{pkg}",\n')
        content.append("]\n")

    # Add kept dependencies section
    if keep_dependencies:
        content.append("\n# Dependency packages to keep (not remove during sync)\n")
        content.append("[keep_dependencies]\n")

        if keep_dependencies.get('brew_formulae'):
            content.append("brew_formulae = [\n")
            for pkg in sorted(keep_dependencies['brew_formulae']):
                content.append(f'    "{pkg}",\n')
            content.append("]\n")

    with open(ignored_path, 'w') as f:
        f.writelines(content)


def install_brew_packages(packages, check_only=False):
    """install missing brew packages."""
    if not packages:
        return

    installed_formulae = get_all_brew_packages()
    installed_casks = get_installed_brew_casks()
    installed_all = installed_formulae | installed_casks

    missing = []
    for pkg in packages:
        # Strip tap prefix if present (e.g., "getsentry/tools/sentry-cli" -> "sentry-cli")
        pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
        # Strip version suffix (e.g., "python@3.13" -> "python")
        pkg_base = pkg_name.split('@')[0]

        # Check if package or its base name is installed
        if pkg_name not in installed_all and pkg_base not in installed_all:
            missing.append(pkg)

    if missing:
        print(f"missing ({len(missing)}): {', '.join(missing)}")
        if not check_only:
            print("installing...")
            for package in missing:
                progress.start_task(f"brew / {package}")
                run_command(f"brew install -f {package}")
                progress.complete_task()
    else:
        print(f"all {len(packages)} packages already installed")


def install_npm_packages(packages, check_only=False):
    """install missing global npm packages."""
    if not packages:
        return

    installed = get_installed_npm_packages()
    missing = []

    for pkg in packages:
        pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
        if pkg not in installed and pkg_name not in installed:
            missing.append(pkg)

    if missing:
        print(f"missing ({len(missing)}): {', '.join(missing)}")
        if not check_only:
            for package in missing:
                progress.start_task(f"npm / {package}")
                run_command(f"npm install -g {package} --force")
                progress.complete_task()
    else:
        print(f"all {len(packages)} packages already installed")


def install_pip_packages(packages, check_only=False):
    """install missing pip packages."""
    if not packages:
        return

    # Get the correct pip command
    pip_cmd = get_pip_command()

    installed = get_installed_pip_packages()
    missing = []

    for pkg in packages:
        if pkg.lower() not in installed:
            missing.append(pkg)

    if missing:
        print(f"missing ({len(missing)}): {', '.join(missing)}")
        if not check_only:
            print(f"using {pip_cmd} for installation...")
            for package in missing:
                progress.start_task(f"pip / {package}")
                run_command(f"{pip_cmd} install {package}")
                progress.complete_task()
    else:
        print(f"all {len(packages)} packages already installed")


def apply_symlinks(symlinks, script_dir, home, check_only=False):
    """apply symlinks from configuration."""
    needs_update = []

    for symlink in symlinks:
        source = symlink['source'].format(script_dir=script_dir, home=home)
        target = symlink['target'].format(script_dir=script_dir, home=home)

        if os.path.exists(target):
            if check_symlink(source, target):
                print(f"✓ {symlink['name']} is correctly linked")
            else:
                print(f"✗ {symlink['name']} exists but points to wrong location")
                needs_update.append(symlink)
        else:
            print(f"✗ {symlink['name']} is missing")
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


def layer0_shell(config, home, check_only=False):
    """Layer 0: Setup oh-my-zsh and powerlevel10k."""
    print(f"\n{GREEN}=== Layer 0: Shell Setup ==={RESET}\n")

    oh_my_zsh_path = os.path.join(home, ".oh-my-zsh", "oh-my-zsh.sh")
    zsh_custom = os.environ.get("ZSH_CUSTOM", os.path.join(home, ".oh-my-zsh/custom"))
    p10k_path = os.path.join(zsh_custom, "themes/powerlevel10k")

    if os.path.exists(oh_my_zsh_path):
        print("✓ oh-my-zsh is installed")
        if os.path.exists(p10k_path):
            print("✓ powerlevel10k theme is installed")
        else:
            print("✗ powerlevel10k theme is missing")
            if not check_only:
                print("installing powerlevel10k...")
                run_command(f"git clone --depth=1 {config['layer0']['powerlevel10k_repo']} {p10k_path}")
    else:
        print("✗ oh-my-zsh is not installed")
        if not check_only:
            for path in [os.path.join(home, ".oh-my-zsh"), "/etc/oh-my-zsh"]:
                if os.path.exists(path):
                    shutil.rmtree(path)

            print("installing oh-my-zsh...")
            env = os.environ.copy()
            env["RUNZSH"] = "no"
            run_command(f'sh -c "$(curl -fsSL {config["layer0"]["oh_my_zsh_url"]})"', env=env)

            print("installing powerlevel10k...")
            run_command(f"git clone --depth=1 {config['layer0']['powerlevel10k_repo']} {p10k_path}")


def layer1_sudo(config, script_dir, home, check_only=False):
    """Layer 1: Configure sudo (Touch ID or custom binary)."""
    print(f"\n{GREEN}=== Layer 1: Sudo Configuration ==={RESET}\n")

    # Check Touch ID status
    pam_file = config['layer1']['touchid']['pam_file']
    pam_line = config['layer1']['touchid']['pam_line']
    touchid_enabled = False

    if os.path.exists(pam_file):
        try:
            with open(pam_file, 'r') as f:
                content = f.read()
                touchid_enabled = pam_line in content
        except PermissionError:
            # Can't read the file, try with system sudo
            result = subprocess.run(['/usr/bin/sudo', 'cat', pam_file], capture_output=True, text=True)
            if result.returncode == 0:
                touchid_enabled = pam_line in result.stdout

    # Check custom sudo status
    custom_sudo_config = config['layer1']['custom_sudo']
    source_file = custom_sudo_config['source_file'].format(script_dir=script_dir, home=home)
    target_binary = custom_sudo_config['target_binary'].format(script_dir=script_dir, home=home)
    custom_sudo_installed = False

    if os.path.exists(target_binary):
        stat_info = os.stat(target_binary)
        is_suid = (stat_info.st_mode & 0o4000) != 0
        is_root = stat_info.st_uid == 0
        custom_sudo_installed = is_suid and is_root

    # Report status
    if touchid_enabled:
        print("✓ Touch ID for sudo is enabled")
    else:
        print("✗ Touch ID for sudo is not enabled")

    if custom_sudo_installed:
        print("✓ Custom sudo binary is installed")
    else:
        print("✗ Custom sudo binary is not installed")

    if check_only:
        return

    # Ask user what they want to do
    print("\nChoose sudo configuration:")
    print("1) Enable Touch ID for sudo (recommended)")
    print("2) Install custom sudo binary")
    print("3) None (remove both configurations)")
    print("4) Skip")

    choice = input("\nSelect option [1]: ").strip()
    if not choice:
        choice = "1"

    if choice == "4":
        print("Skipping sudo configuration")
        return

    # Option 1: Enable Touch ID (remove custom sudo if exists)
    if choice == "1":
        if custom_sudo_installed:
            print("\nRemoving custom sudo binary...")
            if not remove_custom_sudo(custom_sudo_config, script_dir, home):
                print("\n✗ Failed to remove custom sudo")
                sys.exit(1)
        if not touchid_enabled:
            print("\nEnabling Touch ID for sudo...")
            if not enable_touchid_sudo(config['layer1']['touchid']):
                print("\n✗ Failed to enable Touch ID for sudo")
                sys.exit(1)
        else:
            print("\nTouch ID is already enabled")

    # Option 2: Install custom sudo (remove Touch ID if exists)
    elif choice == "2":
        if touchid_enabled:
            print("\nDisabling Touch ID for sudo...")
            if not disable_touchid_sudo(config['layer1']['touchid']):
                print("\n✗ Failed to disable Touch ID for sudo")
                sys.exit(1)
        if not custom_sudo_installed:
            print("\nInstalling custom sudo binary...")
            if not install_custom_sudo(custom_sudo_config, script_dir, home):
                print("\n✗ Custom sudo installation failed")
                sys.exit(1)
        else:
            print("\nCustom sudo is already installed")

    # Option 3: None (remove both)
    elif choice == "3":
        if touchid_enabled:
            print("\nDisabling Touch ID for sudo...")
            if not disable_touchid_sudo(config['layer1']['touchid']):
                print("\n✗ Failed to disable Touch ID for sudo")
                sys.exit(1)
        if custom_sudo_installed:
            print("\nRemoving custom sudo binary...")
            if not remove_custom_sudo(custom_sudo_config, script_dir, home):
                print("\n✗ Failed to remove custom sudo")
                sys.exit(1)
        if not touchid_enabled and not custom_sudo_installed:
            print("\nNo sudo configurations to remove")


def layer2_base_packages(config, check_only=False):
    """Layer 2: Install all base system packages."""
    print(f"\n{GREEN}=== Layer 2: Base System Packages ==={RESET}")

    # Add homebrew taps
    if not check_only:
        for tap in config['layer2']['brew_taps']:
            if not is_brew_tap_added(tap):
                print(f"adding tap: {tap}")
                run_command(f"brew tap {tap}")

    # Install brew packages
    print("\nbrew packages:")
    install_brew_packages(config['layer2']['brew_packages'], check_only)

    # Install npm packages
    print("\nnpm packages:")
    install_npm_packages(config['layer2']['npm_packages'], check_only)

    # Install pip packages
    print("\npython packages:")
    install_pip_packages(config['layer2']['pip_packages'], check_only)

    # Run post-install commands
    post_install_commands = config['layer2'].get('post_install_commands', [])
    if post_install_commands and not check_only:
        print("\npost-install commands:")
        for cmd in post_install_commands:
            print(f"running: {cmd}")
            run_command(cmd)


def layer3_emacs(config, home, check_only=False):
    """Layer 3: Install Emacs."""
    print(f"\n{GREEN}=== Layer 3: Emacs Installation ==={RESET}")

    needs_emacs = not command_exists("emacs")

    if needs_emacs:
        print("✗ Emacs not found")
        if not check_only:
            emacs_formula = config['layer3']['formula']
            options = ' '.join(config['layer3']['options'])
            install_cmd = f"brew install {options} {emacs_formula}"
            print(f"installing {emacs_formula}...")
            run_command(install_cmd)
            run_command("defaults write org.gnu.Emacs TransparentTitleBar DARK")
    else:
        print("✓ Emacs is already installed")

    # Install dictionaries
    spelling_dir = os.path.join(home, "Library/Spelling")
    os.makedirs(spelling_dir, exist_ok=True)

    missing_dicts = []
    for dict_entry in config['layer3']['dictionaries']:
        dict_path = os.path.join(spelling_dir, dict_entry['filename'])
        if not os.path.exists(dict_path):
            missing_dicts.append(dict_entry)

    if missing_dicts:
        print(f"\ndictionaries: {len(missing_dicts)} missing")
        if not check_only:
            print("installing dictionaries...")
            for dict_entry in missing_dicts:
                run_command(f"wget -nc {dict_entry['url']} -O {spelling_dir}/{dict_entry['filename']}")
    else:
        print("✓ all dictionaries installed")


def layer4_doom(config, script_dir, home, check_only=False):
    """Layer 4: Install Doom Emacs and setup config."""
    print(f"\n{GREEN}=== Layer 4: Doom Emacs ==={RESET}")

    # First, determine and setup emacs config symlink
    existing_doom = os.path.exists(os.path.join(home, ".config/doom"))
    existing_light = os.path.exists(os.path.join(home, ".emacs"))

    emacs_choice = None
    if existing_doom or existing_light:
        setup_type = "doom" if existing_doom else "light"
        print(f"detected existing {setup_type} Emacs setup")
        if not check_only:
            use_existing = input(f"  continue with {setup_type} setup? [Y/n]: ").strip()
            if not use_existing or use_existing.lower() in ["y", "yes"]:
                emacs_choice = setup_type
            else:
                print("\nchoose emacs setup:")
                print("1) doom")
                print("2) light")
                print("3) skip")
                choice = input("  > ").strip()
                emacs_choice = "doom" if choice == "1" else "light" if choice == "2" else None
        else:
            emacs_choice = setup_type
    else:
        if not check_only:
            print("\nchoose emacs setup:")
            print("1) doom")
            print("2) light")
            print("3) skip")
            choice = input("  > ").strip()
            emacs_choice = "doom" if choice == "1" else "light" if choice == "2" else None
        else:
            emacs_choice = None

    # Apply emacs config symlink if chosen
    if emacs_choice:
        emacs_link = config['layer5']['emacs_symlinks'][emacs_choice]
        source = emacs_link['source'].format(script_dir=script_dir, home=home)
        target = emacs_link['target'].format(script_dir=script_dir, home=home)

        # Create .config directory if needed for doom
        if emacs_choice == "doom":
            config_dir = os.path.join(home, ".config")
            if not os.path.exists(config_dir):
                os.makedirs(config_dir)

        if not check_only:
            print(f"setting up {emacs_choice} config...")
            link(emacs_link['name'], source, target)

    # Skip Doom installation if user chose light or skip
    if emacs_choice != "doom":
        if emacs_choice == "light":
            print("skipping Doom Emacs (using light config)")
        else:
            print("skipping Doom Emacs installation")
        return

    # Now install Doom Emacs
    emacs_config_path = config['layer4']['install_path'].format(home=home)
    doom_bin = os.path.join(emacs_config_path, "bin/doom")

    if os.path.exists(emacs_config_path):
        print("✓ Doom Emacs is installed")
        if not os.path.exists(doom_bin):
            print("✗ doom binary not found")
            if not check_only:
                repair = input("  repair Doom Emacs installation? [Y/n]: ").strip()
                if not repair or repair.lower() in ["y", "yes"]:
                    print("running doom sync...")
                    env = os.environ.copy()
                    env["PATH"] = "/opt/homebrew/bin:" + env.get("PATH", "")
                    run_command(f"{doom_bin} sync", env=env)
    else:
        print("✗ Doom Emacs not found")
        if not check_only:
            print("installing Doom Emacs...")
            run_command(f"git clone --depth 1 {config['layer4']['repo']} {emacs_config_path}")

            env = os.environ.copy()
            env["PATH"] = "/opt/homebrew/bin:" + env.get("PATH", "")

            run_command(f"{doom_bin} install --force --env --install --fonts --hooks", env=env)
            run_command(f"{doom_bin} sync", env=env)


def setup_python_symlinks(check_only=False):
    """Create python3 and pip3 symlinks to versioned Homebrew Python."""
    import re

    # Determine Homebrew bin directory
    homebrew_paths = ['/opt/homebrew/bin', '/usr/local/bin']
    homebrew_bin = None

    for path in homebrew_paths:
        if os.path.exists(path):
            homebrew_bin = path
            break

    if not homebrew_bin:
        print("✗ Homebrew bin directory not found")
        return

    # Find all versioned Python installations
    python_bins = glob.glob(os.path.join(homebrew_bin, 'python3.*'))
    if not python_bins:
        print("✗ No Homebrew Python installation found")
        return

    # Sort to get the highest version
    python_bins.sort(reverse=True)
    latest_python = python_bins[0]
    version_match = re.search(r'python(3\.\d+)', os.path.basename(latest_python))

    if not version_match:
        print("✗ Could not determine Python version")
        return

    version = version_match.group(1)
    python_target = f"python{version}"
    pip_target = f"pip{version}"

    python3_link = os.path.join(homebrew_bin, "python3")
    pip3_link = os.path.join(homebrew_bin, "pip3")

    # Check if pip exists
    pip_target_path = os.path.join(homebrew_bin, pip_target)
    if not os.path.exists(pip_target_path):
        print(f"✗ {pip_target} not found")
        return

    # Report current status
    python3_needs_update = True
    pip3_needs_update = True

    if os.path.islink(python3_link):
        if os.readlink(python3_link) == python_target:
            print(f"✓ python3 -> {python_target}")
            python3_needs_update = False
        else:
            print(f"✗ python3 -> {os.readlink(python3_link)} (should be {python_target})")
    elif os.path.exists(python3_link):
        print(f"✗ python3 exists but is not a symlink")
    else:
        print(f"✗ python3 symlink missing")

    if os.path.islink(pip3_link):
        if os.readlink(pip3_link) == pip_target:
            print(f"✓ pip3 -> {pip_target}")
            pip3_needs_update = False
        else:
            print(f"✗ pip3 -> {os.readlink(pip3_link)} (should be {pip_target})")
    elif os.path.exists(pip3_link):
        print(f"✗ pip3 exists but is not a symlink")
    else:
        print(f"✗ pip3 symlink missing")

    # Create/update symlinks if needed
    if not check_only and (python3_needs_update or pip3_needs_update):
        if python3_needs_update:
            if os.path.lexists(python3_link):
                os.remove(python3_link)
            os.symlink(python_target, python3_link)
            print(f"created python3 -> {python_target}")

        if pip3_needs_update:
            if os.path.lexists(pip3_link):
                os.remove(pip3_link)
            os.symlink(pip_target, pip3_link)
            print(f"created pip3 -> {pip_target}")


def layer5_symlinks(config, script_dir, home, check_only=False):
    """Layer 5: Setup general symlinks."""
    print(f"\n{GREEN}=== Layer 5: Symlinks ==={RESET}")

    # Apply all general symlinks (zsh, p10k, editorconfig, etc.)
    apply_symlinks(config['layer5']['symlinks'], script_dir, home, check_only)

    # Setup Python symlinks (python3 -> python3.X, pip3 -> pip3.X)
    print("\npython symlinks:")
    setup_python_symlinks(check_only)

    # Setup jdtls links if present
    # Use link_force to update symlinks when jdtls version changes
    if not check_only and command_exists("jenv"):
        jdtls_patterns = glob.glob("/opt/homebrew/Cellar/jdtls/*/libexec/config_mac")
        if jdtls_patterns:
            jdtls_dir = os.path.join(home, "tools/jdtls")
            os.makedirs(jdtls_dir, exist_ok=True)
            link_force("jdtls/config_mac", jdtls_patterns[0], os.path.join(jdtls_dir, "config_mac"))

        jdtls_plugin_patterns = glob.glob("/opt/homebrew/Cellar/jdtls/*/libexec/plugins")
        if jdtls_plugin_patterns:
            link_force("jdtls/plugins", jdtls_plugin_patterns[0], os.path.join(home, "tools/jdtls/plugins"))

    # Configure Java if jenv is installed
    if command_exists("jenv") and not check_only:
        result = subprocess.run(['jenv', 'versions'], capture_output=True, text=True)
        if '21' not in result.stdout:
            print("\nconfiguring java 21...")
            run_command("jenv add /opt/homebrew/opt/openjdk@21")
            run_command("jenv global 21")


def enable_touchid_sudo(touchid_config):
    """Enable Touch ID authentication for sudo."""
    pam_file = touchid_config['pam_file']
    pam_line = touchid_config['pam_line']
    insert_after = touchid_config['insert_after']

    try:
        # Read current content (use system sudo)
        result = subprocess.run(['/usr/bin/sudo', 'cat', pam_file], capture_output=True, text=True, check=True)
        lines = result.stdout.split('\n')

        # Find insertion point
        insert_index = -1
        for i, line in enumerate(lines):
            if insert_after in line:
                insert_index = i + 1
                break

        if insert_index == -1:
            print(f"✗ Could not find insertion point: {insert_after}")
            return False

        # Insert the Touch ID line
        lines.insert(insert_index, pam_line)
        new_content = '\n'.join(lines)

        # Write back using system sudo
        process = subprocess.Popen(['/usr/bin/sudo', 'tee', pam_file], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        process.communicate(input=new_content.encode())

        if process.returncode == 0:
            print("✓ Touch ID for sudo enabled successfully")
            return True
        else:
            print("✗ Failed to write PAM configuration")
            return False

    except Exception as e:
        print(f"✗ Error enabling Touch ID: {e}")
        return False


def disable_touchid_sudo(touchid_config):
    """Disable Touch ID authentication for sudo."""
    pam_file = touchid_config['pam_file']
    pam_line = touchid_config['pam_line']

    try:
        # Read current content (use system sudo)
        result = subprocess.run(['/usr/bin/sudo', 'cat', pam_file], capture_output=True, text=True, check=True)
        lines = result.stdout.split('\n')

        # Remove the Touch ID line if present
        new_lines = [line for line in lines if pam_line not in line]

        # Check if anything was removed
        if len(new_lines) == len(lines):
            print("Touch ID was not enabled")
            return True

        new_content = '\n'.join(new_lines)

        # Write back using system sudo
        process = subprocess.Popen(['/usr/bin/sudo', 'tee', pam_file], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        process.communicate(input=new_content.encode())

        if process.returncode == 0:
            print("✓ Touch ID for sudo disabled successfully")
            return True
        else:
            print("✗ Failed to write PAM configuration")
            return False

    except Exception as e:
        print(f"✗ Error disabling Touch ID: {e}")
        return False


def install_custom_sudo(custom_sudo_config, script_dir, home):
    """Install custom sudo binary."""
    source_file = custom_sudo_config['source_file'].format(script_dir=script_dir, home=home)
    target_binary = custom_sudo_config['target_binary'].format(script_dir=script_dir, home=home)

    # Check if source file exists
    if not os.path.exists(source_file):
        print(f"✗ Source file not found: {source_file}")
        return False

    # Compile the binary
    print("Compiling custom sudo...")
    compile_cmd = custom_sudo_config['compile_command'].format(
        source=source_file,
        target=target_binary
    )

    if not run_command(compile_cmd, prompt_on_error=False):
        print("✗ Failed to compile custom sudo")
        return False

    # Install with proper permissions
    print("Setting SUID permissions (requires sudo)...")
    for install_cmd in custom_sudo_config['install_commands']:
        # Use absolute path to system sudo to avoid using the newly compiled one
        cmd = install_cmd.format(target=target_binary)
        cmd = cmd.replace('sudo ', '/usr/bin/sudo ', 1)  # Replace first occurrence only
        if not run_command(cmd, prompt_on_error=False):
            print(f"✗ Failed to execute: {cmd}")
            return False

    print("✓ Custom sudo installed successfully")
    return True


def remove_custom_sudo(custom_sudo_config, script_dir, home):
    """Remove custom sudo binary."""
    target_binary = custom_sudo_config['target_binary'].format(script_dir=script_dir, home=home)

    if not os.path.exists(target_binary):
        print("Custom sudo binary was not installed")
        return True

    try:
        # Remove the binary (may be owned by root, so use system sudo)
        result = subprocess.run(['/usr/bin/sudo', 'rm', target_binary], capture_output=True, text=True)
        if result.returncode == 0:
            print("✓ Custom sudo binary removed successfully")
            return True
        else:
            print(f"✗ Failed to remove custom sudo: {result.stderr}")
            return False
    except Exception as e:
        print(f"✗ Error removing custom sudo: {e}")
        return False


def get_all_installed_packages():
    """Get all installed packages categorized by type."""
    packages = {
        'brew_formulae': set(),
        'brew_casks': set(),
        'npm': set(),
        'pip': set()
    }

    # Get brew formulae
    packages['brew_formulae'] = get_installed_brew_packages()

    # Get brew casks
    packages['brew_casks'] = get_installed_brew_casks()

    # Get npm packages
    packages['npm'] = get_installed_npm_packages()

    # Get pip packages
    packages['pip'] = get_installed_pip_packages()

    return packages


def compare_packages(installed, config_packages, ignored_packages, keep_dependencies=None):
    """Compare installed packages with config and return differences."""
    if keep_dependencies is None:
        keep_dependencies = {}

    diffs = {
        'brew_formulae': {'to_add': set(), 'to_remove': set(), 'to_remove_deps': set()},
        'brew_casks': {'to_add': set(), 'to_remove': set()},
        'npm': {'to_add': set(), 'to_remove': set()},
        'pip': {'to_add': set(), 'to_remove': set()}
    }

    # Get all brew packages (including dependencies) to identify dependency-only packages
    all_brew_formulae = get_all_brew_packages()

    # Brew packages (combined formulae and casks)
    config_brew = set(config_packages.get('brew_packages', []))
    installed_brew = installed['brew_formulae'] | installed['brew_casks']
    ignored_brew_formulae = set(ignored_packages.get('brew_formulae', []))
    ignored_brew_casks = set(ignored_packages.get('brew_casks', []))
    keep_deps_brew_formulae = set(keep_dependencies.get('brew_formulae', []))

    # Create a normalized version of config_brew for comparison (strip tap prefixes)
    config_brew_normalized = set()
    for pkg in config_brew:
        pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
        config_brew_normalized.add(pkg_name)

    # Separate installed into formulae and casks for diff
    # Note: installed packages are already normalized (tap prefixes stripped)
    for pkg in installed['brew_formulae']:
        if pkg not in config_brew_normalized and pkg not in ignored_brew_formulae:
            diffs['brew_formulae']['to_add'].add(pkg)

    for pkg in installed['brew_casks']:
        if pkg not in config_brew_normalized and pkg not in ignored_brew_casks:
            diffs['brew_casks']['to_add'].add(pkg)

    # Check for packages in config but not explicitly installed
    # Note: installed_brew and all_brew_formulae are already normalized
    for pkg in config_brew:
        # Normalize package name (strip tap prefix)
        pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg

        if pkg_name not in installed_brew:
            # Check if it's installed as a dependency (in all packages but not in leaves)
            if pkg_name in all_brew_formulae:
                # It's installed but only as a dependency
                # Only mark for removal if not in keep_dependencies
                if pkg not in keep_deps_brew_formulae and pkg_name not in keep_deps_brew_formulae:
                    diffs['brew_formulae']['to_remove_deps'].add(pkg)
            else:
                # Not installed at all - genuinely removed
                diffs['brew_formulae']['to_remove'].add(pkg)

    # NPM packages
    config_npm = set(config_packages.get('npm_packages', []))
    ignored_npm = set(ignored_packages.get('npm_packages', []))

    for pkg in installed['npm']:
        if pkg not in config_npm and pkg not in ignored_npm:
            diffs['npm']['to_add'].add(pkg)

    for pkg in config_npm:
        if pkg not in installed['npm']:
            diffs['npm']['to_remove'].add(pkg)

    # PIP packages
    config_pip = set(config_packages.get('pip_packages', []))
    ignored_pip = set(ignored_packages.get('pip_packages', []))

    for pkg in installed['pip']:
        if pkg not in config_pip and pkg not in ignored_pip:
            diffs['pip']['to_add'].add(pkg)

    for pkg in config_pip:
        if pkg not in installed['pip']:
            diffs['pip']['to_remove'].add(pkg)

    return diffs


def write_toml_config(config_path, config, package_updates, ignored_updates, keep_dependencies=None):
    """Write updated config back to TOML file."""
    import shutil

    # Create backup
    backup_path = f"{config_path}.bak"
    shutil.copy2(config_path, backup_path)
    print(f"\nCreated backup: {backup_path}")

    # Save ignored packages to separate file
    save_ignored_packages(ignored_updates, keep_dependencies)

    # Read original file to preserve comments
    with open(config_path, 'r') as f:
        lines = f.readlines()

    # Update brew packages
    brew_packages = sorted(
        (set(config['layer2']['brew_packages']) |
         package_updates['brew_formulae']['add'] |
         package_updates['brew_casks']['add']) -
        package_updates['brew_formulae']['remove'] -
        package_updates['brew_casks']['remove']
    )

    # Update npm packages
    npm_packages = sorted(
        (set(config['layer2']['npm_packages']) |
         package_updates['npm']['add']) -
        package_updates['npm']['remove']
    )

    # Update pip packages
    pip_packages = sorted(
        (set(config['layer2']['pip_packages']) |
         package_updates['pip']['add']) -
        package_updates['pip']['remove']
    )

    # Build new TOML content
    content = []

    # Copy Layer 0 and Layer 1 from original file
    in_layer0_or_1 = False
    for line in lines:
        if line.startswith("# Layer 0:") or line.startswith("[layer0]"):
            in_layer0_or_1 = True
        elif line.startswith("# Layer 2:") or line.startswith("[layer2]"):
            in_layer0_or_1 = False
            break
        if in_layer0_or_1:
            content.append(line)

    # Layer 2 (Base system packages)
    content.append("# Layer 2: Base system packages\n")
    content.append("[layer2]\n")
    content.append('name = "Base System Packages"\n\n')

    content.append("# Homebrew taps to add before installing packages\n")
    content.append("brew_taps = [\n")
    for tap in config['layer2']['brew_taps']:
        content.append(f'    "{tap}",\n')
    content.append("]\n\n")

    content.append("# All brew packages (formulae and casks) to install\n")
    content.append("brew_packages = [\n")
    for pkg in brew_packages:
        content.append(f'    "{pkg}",\n')
    content.append("]\n\n")

    content.append("# NPM global packages\n")
    content.append("npm_packages = [\n")
    for pkg in npm_packages:
        content.append(f'    "{pkg}",\n')
    content.append("]\n\n")

    content.append("# Python packages\n")
    content.append("pip_packages = [\n")
    for pkg in pip_packages:
        content.append(f'    "{pkg}",\n')
    content.append("]\n\n")

    # Copy layer3, layer4, and layer5 sections from original
    in_layer3_or_later = False
    for line in lines:
        if line.startswith("# Layer 3:") or line.startswith("[layer3]"):
            in_layer3_or_later = True
        if in_layer3_or_later:
            content.append(line)

    # Write updated content
    with open(config_path, 'w') as f:
        f.writelines(content)

    print(f"Updated: {config_path}")
    print(f"Updated: ~/.config/setup-tools/ignored_packages.toml")


def interactive_package_menu(diffs, current_ignored):
    """Interactive curses menu for selecting packages to add/remove/ignore."""
    try:
        import curses
    except ImportError:
        print("Error: curses module not available")
        return None, None

    def menu_main(stdscr):
        # Initialize curses
        curses.curs_set(0)
        curses.init_pair(1, curses.COLOR_GREEN, curses.COLOR_BLACK)
        curses.init_pair(2, curses.COLOR_RED, curses.COLOR_BLACK)
        curses.init_pair(3, curses.COLOR_YELLOW, curses.COLOR_BLACK)
        curses.init_pair(4, curses.COLOR_CYAN, curses.COLOR_BLACK)
        curses.init_pair(5, curses.COLOR_WHITE, curses.COLOR_BLUE)

        # Build menu items
        categories = [
            {
                'name': 'Brew Formulae',
                'key': 'brew_formulae',
                'to_add': sorted(diffs['brew_formulae']['to_add']),
                'to_remove': sorted(diffs['brew_formulae']['to_remove']),
                'to_remove_deps': sorted(diffs['brew_formulae']['to_remove_deps']),
                'expanded': False
            },
            {
                'name': 'Brew Casks',
                'key': 'brew_casks',
                'to_add': sorted(diffs['brew_casks']['to_add']),
                'to_remove': sorted(diffs['brew_casks']['to_remove']),
                'to_remove_deps': [],
                'expanded': False
            },
            {
                'name': 'NPM Packages',
                'key': 'npm',
                'to_add': sorted(diffs['npm']['to_add']),
                'to_remove': sorted(diffs['npm']['to_remove']),
                'to_remove_deps': [],
                'expanded': False
            },
            {
                'name': 'Python Packages',
                'key': 'pip',
                'to_add': sorted(diffs['pip']['to_add']),
                'to_remove': sorted(diffs['pip']['to_remove']),
                'to_remove_deps': [],
                'expanded': False
            }
        ]

        # Track package states: 'add', 'remove', 'ignore', 'keep'
        package_states = {}
        for cat in categories:
            for pkg in cat['to_add']:
                package_states[f"{cat['key']}:{pkg}"] = 'add'
            for pkg in cat['to_remove']:
                package_states[f"{cat['key']}:{pkg}"] = 'remove'
            # Auto-mark dependency-only packages for removal
            for pkg in cat.get('to_remove_deps', []):
                package_states[f"{cat['key']}:{pkg}"] = 'remove'

        current_row = 0
        current_col = 0

        while True:
            stdscr.clear()
            h, w = stdscr.getmaxyx()

            # Draw header
            header = "Package Sync - Arrows: navigate | Enter/Tab: expand/collapse | Space: cycle state | s: save | q: quit"
            stdscr.attron(curses.color_pair(5))
            stdscr.addstr(0, 0, header[:w-1].ljust(w-1))
            stdscr.attroff(curses.color_pair(5))

            # Count changes
            to_add_count = sum(1 for v in package_states.values() if v == 'add')
            to_remove_count = sum(1 for v in package_states.values() if v == 'remove')
            to_ignore_count = sum(1 for v in package_states.values() if v == 'ignore')

            summary = f"Add to config: {to_add_count} | Remove from config: {to_remove_count} | Ignore: {to_ignore_count}"
            stdscr.addstr(1, 2, summary)

            # Draw categories and packages
            row = 3
            menu_items = []

            for cat_idx, cat in enumerate(categories):
                if row >= h - 1:
                    break

                total_to_add = len(cat['to_add'])
                total_to_remove = len(cat['to_remove'])
                total_to_remove_deps = len(cat.get('to_remove_deps', []))
                total_in_cat = total_to_add + total_to_remove + total_to_remove_deps

                if total_in_cat == 0:
                    continue

                # Category header
                menu_items.append(('category', cat_idx, None))
                prefix = "▼" if cat['expanded'] else "▶"
                deps_info = f" ({total_to_remove_deps} deps)" if total_to_remove_deps > 0 else ""
                cat_line = f"{prefix} {cat['name']} ({total_in_cat}{deps_info})"

                if current_row == len(menu_items) - 1:
                    stdscr.attron(curses.color_pair(4) | curses.A_BOLD)
                    stdscr.addstr(row, 2, cat_line[:w-3])
                    stdscr.attroff(curses.color_pair(4) | curses.A_BOLD)
                else:
                    stdscr.attron(curses.A_BOLD)
                    stdscr.addstr(row, 2, cat_line[:w-3])
                    stdscr.attroff(curses.A_BOLD)
                row += 1

                # Show packages if expanded
                if cat['expanded']:
                    for pkg in cat['to_add']:
                        if row >= h - 1:
                            break
                        key = f"{cat['key']}:{pkg}"
                        state = package_states.get(key, 'keep')
                        menu_items.append(('package', cat_idx, pkg))

                        state_str = f"[{state.upper()}]".ljust(10)
                        pkg_line = f"    {state_str} +{pkg}"

                        if current_row == len(menu_items) - 1:
                            color = 1 if state == 'add' else 3 if state == 'ignore' else 0
                            stdscr.attron(curses.color_pair(color) | curses.A_REVERSE)
                            stdscr.addstr(row, 2, pkg_line[:w-3])
                            stdscr.attroff(curses.color_pair(color) | curses.A_REVERSE)
                        else:
                            color = 1 if state == 'add' else 3 if state == 'ignore' else 0
                            if color:
                                stdscr.attron(curses.color_pair(color))
                            stdscr.addstr(row, 2, pkg_line[:w-3])
                            if color:
                                stdscr.attroff(curses.color_pair(color))
                        row += 1

                    for pkg in cat['to_remove']:
                        if row >= h - 1:
                            break
                        key = f"{cat['key']}:{pkg}"
                        state = package_states.get(key, 'keep')
                        menu_items.append(('package', cat_idx, pkg))

                        state_str = f"[{state.upper()}]".ljust(10)
                        pkg_line = f"    {state_str} -{pkg}"

                        if current_row == len(menu_items) - 1:
                            color = 2 if state == 'remove' else 3 if state == 'ignore' else 0
                            stdscr.attron(curses.color_pair(color) | curses.A_REVERSE)
                            stdscr.addstr(row, 2, pkg_line[:w-3])
                            stdscr.attroff(curses.color_pair(color) | curses.A_REVERSE)
                        else:
                            color = 2 if state == 'remove' else 3 if state == 'ignore' else 0
                            if color:
                                stdscr.attron(curses.color_pair(color))
                            stdscr.addstr(row, 2, pkg_line[:w-3])
                            if color:
                                stdscr.attroff(curses.color_pair(color))
                        row += 1

                    for pkg in cat.get('to_remove_deps', []):
                        if row >= h - 1:
                            break
                        key = f"{cat['key']}:{pkg}"
                        state = package_states.get(key, 'keep')
                        menu_items.append(('package', cat_idx, pkg))

                        state_str = f"[{state.upper()}]".ljust(10)
                        pkg_line = f"    {state_str} -{pkg} (dependency)"

                        if current_row == len(menu_items) - 1:
                            color = 2 if state == 'remove' else 3 if state == 'ignore' else 0
                            stdscr.attron(curses.color_pair(color) | curses.A_REVERSE)
                            stdscr.addstr(row, 2, pkg_line[:w-3])
                            stdscr.attroff(curses.color_pair(color) | curses.A_REVERSE)
                        else:
                            color = 2 if state == 'remove' else 3 if state == 'ignore' else 0
                            if color:
                                stdscr.attron(curses.color_pair(color))
                            stdscr.addstr(row, 2, pkg_line[:w-3])
                            if color:
                                stdscr.attroff(curses.color_pair(color))
                        row += 1

            stdscr.refresh()

            # Handle input
            key = stdscr.getch()

            if key == ord('q') or key == 27:  # q or ESC
                return None, None, None
            elif key == ord('s'):
                # Save and exit
                result_updates = {
                    'brew_formulae': {'add': set(), 'remove': set()},
                    'brew_casks': {'add': set(), 'remove': set()},
                    'npm': {'add': set(), 'remove': set()},
                    'pip': {'add': set(), 'remove': set()}
                }
                result_ignored = {
                    'brew_formulae': set(current_ignored.get('brew_formulae', [])),
                    'brew_casks': set(current_ignored.get('brew_casks', [])),
                    'npm_packages': set(current_ignored.get('npm_packages', [])),
                    'pip_packages': set(current_ignored.get('pip_packages', []))
                }
                result_kept_deps = {
                    'brew_formulae': set()
                }

                # Track which packages are dependency packages
                dep_packages = set()
                for cat in categories:
                    for pkg in cat.get('to_remove_deps', []):
                        dep_packages.add(f"{cat['key']}:{pkg}")

                for key, state in package_states.items():
                    cat_key, pkg = key.split(':', 1)
                    if state == 'add':
                        result_updates[cat_key]['add'].add(pkg)
                    elif state == 'remove':
                        result_updates[cat_key]['remove'].add(pkg)
                    elif state == 'ignore':
                        ignored_key = cat_key if cat_key.startswith('brew_') else f"{cat_key}_packages"
                        result_ignored[ignored_key].add(pkg)
                    elif state == 'keep' and key in dep_packages:
                        # This is a dependency package being kept
                        if cat_key == 'brew_formulae':
                            result_kept_deps['brew_formulae'].add(pkg)

                return result_updates, result_ignored, result_kept_deps
            elif key == curses.KEY_UP:
                current_row = max(0, current_row - 1)
            elif key == curses.KEY_DOWN:
                current_row = min(len(menu_items) - 1, current_row + 1)
            elif key == ord('\n') or key == curses.KEY_ENTER or key == ord('\t') or key == 9:
                # Toggle category expansion (Enter/Tab)
                if current_row < len(menu_items):
                    item_type, cat_idx, pkg = menu_items[current_row]
                    if item_type == 'category':
                        # On category: toggle expansion
                        categories[cat_idx]['expanded'] = not categories[cat_idx]['expanded']
                    elif item_type == 'package':
                        # On package: collapse the category
                        categories[cat_idx]['expanded'] = False
            elif key == ord(' '):
                # Cycle package state
                if current_row < len(menu_items):
                    item_type, cat_idx, pkg = menu_items[current_row]
                    if item_type == 'package':
                        cat = categories[cat_idx]
                        key = f"{cat['key']}:{pkg}"
                        current_state = package_states.get(key, 'keep')

                        # Cycle: add -> ignore -> keep -> add (for new packages)
                        # Cycle: remove -> keep -> remove (for removed packages)
                        # Cycle: remove -> keep (for dependency packages, auto-marked for removal)
                        if pkg in cat['to_add']:
                            if current_state == 'add':
                                package_states[key] = 'ignore'
                            elif current_state == 'ignore':
                                package_states[key] = 'keep'
                            else:
                                package_states[key] = 'add'
                        elif pkg in cat['to_remove']:
                            if current_state == 'remove':
                                package_states[key] = 'keep'
                            else:
                                package_states[key] = 'remove'
                        elif pkg in cat.get('to_remove_deps', []):
                            # Dependencies: just toggle remove/keep
                            if current_state == 'remove':
                                package_states[key] = 'keep'
                            else:
                                package_states[key] = 'remove'

    return curses.wrapper(menu_main)


def sync_packages(config_path, config):
    """Sync installed packages with setup.toml."""
    print(f"{GREEN}=== Package Sync ==={RESET}\n")
    print("Scanning installed packages...")

    # Get all installed packages
    installed = get_all_installed_packages()

    # Filter out packages that are installed in other layers (layer 3+)
    # These are handled specially and shouldn't be in layer 2
    layer_specific_packages = set()

    # Layer 3: Emacs formula
    if 'layer3' in config:
        emacs_formula = config['layer3'].get('formula', '')
        if emacs_formula:
            # Normalize emacs formula name (strip tap prefix and options)
            emacs_name = emacs_formula.split('/')[-1] if '/' in emacs_formula else emacs_formula
            layer_specific_packages.add(emacs_name)

    # Filter out layer-specific packages from installed
    installed['brew_formulae'] = installed['brew_formulae'] - layer_specific_packages

    # Debug: Show counts of installed packages
    print(f"Found {len(installed['brew_formulae'])} brew formulae")
    print(f"Found {len(installed['brew_casks'])} brew casks")
    print(f"Found {len(installed['npm'])} npm packages")
    print(f"Found {len(installed['pip'])} pip packages")

    # Load current config packages
    config_packages = {
        'brew_packages': config['layer1'].get('brew_packages', []),
        'npm_packages': config['layer1'].get('npm_packages', []),
        'pip_packages': config['layer1'].get('pip_packages', [])
    }

    print(f"Config has {len(config_packages['brew_packages'])} brew packages")
    print(f"Config has {len(config_packages['npm_packages'])} npm packages")
    print(f"Config has {len(config_packages['pip_packages'])} pip packages")

    # Load ignored packages from ~/.config/setup-tools/ignored_packages.toml
    ignored_config = load_ignored_packages()
    ignored_packages = {
        'brew_formulae': ignored_config.get('brew_formulae', []),
        'brew_casks': ignored_config.get('brew_casks', []),
        'npm_packages': ignored_config.get('npm_packages', []),
        'pip_packages': ignored_config.get('pip_packages', [])
    }
    keep_dependencies = ignored_config.get('keep_dependencies', {})

    # Compare
    diffs = compare_packages(installed, config_packages, ignored_packages, keep_dependencies)

    # Check if there are any differences
    has_diffs = any(
        diffs[cat]['to_add'] or diffs[cat]['to_remove'] or diffs[cat].get('to_remove_deps', set())
        for cat in ['brew_formulae', 'brew_casks', 'npm', 'pip']
    )

    if not has_diffs:
        print("\n✓ All packages are in sync!")
        return

    # Show summary
    print("\nPackage differences found:")
    print("(Installed but not in config / In config but not installed)")
    for cat_name, cat_key in [('Brew Formulae', 'brew_formulae'),
                               ('Brew Casks', 'brew_casks'),
                               ('NPM Packages', 'npm'),
                               ('Python Packages', 'pip')]:
        to_add = len(diffs[cat_key]['to_add'])
        to_remove = len(diffs[cat_key]['to_remove'])
        to_remove_deps = len(diffs[cat_key].get('to_remove_deps', set()))
        if to_add or to_remove or to_remove_deps:
            deps_str = f", {to_remove_deps} dependencies to clean" if to_remove_deps > 0 else ""
            print(f"{cat_name}: +{to_add} to add to config, -{to_remove} to remove from config{deps_str}")

    print("\nLaunching interactive menu...")
    print("(Arrow keys: navigate | Enter/Tab: expand/collapse | Space: cycle state | s: save | q: quit)\n")

    # Show interactive menu
    updates, ignored_updates, kept_deps = interactive_package_menu(diffs, ignored_packages)

    if updates is None:
        print("\nSync cancelled.")
        return

    # Merge kept dependencies with existing ones
    if kept_deps:
        if 'brew_formulae' in kept_deps:
            existing_kept = set(keep_dependencies.get('brew_formulae', []))
            keep_dependencies['brew_formulae'] = list(existing_kept | kept_deps['brew_formulae'])

    # Apply updates
    print("\nApplying changes...")
    write_toml_config(config_path, config, updates, ignored_updates, keep_dependencies)

    print("\n✓ Sync complete!")


def interactive_manage_ignored(ignored_packages, keep_dependencies):
    """Interactive curses menu for managing ignored and kept packages."""
    try:
        import curses
    except ImportError:
        print("Error: curses module not available")
        return None

    def menu_main(stdscr):
        # Initialize curses
        curses.curs_set(0)
        curses.init_pair(1, curses.COLOR_GREEN, curses.COLOR_BLACK)
        curses.init_pair(2, curses.COLOR_RED, curses.COLOR_BLACK)
        curses.init_pair(3, curses.COLOR_YELLOW, curses.COLOR_BLACK)
        curses.init_pair(4, curses.COLOR_CYAN, curses.COLOR_BLACK)
        curses.init_pair(5, curses.COLOR_WHITE, curses.COLOR_BLUE)

        # Build package lists
        categories = [
            {
                'name': 'Ignored Brew Formulae',
                'key': 'brew_formulae',
                'packages': sorted(ignored_packages.get('brew_formulae', [])),
                'expanded': False
            },
            {
                'name': 'Ignored Brew Casks',
                'key': 'brew_casks',
                'packages': sorted(ignored_packages.get('brew_casks', [])),
                'expanded': False
            },
            {
                'name': 'Ignored NPM Packages',
                'key': 'npm_packages',
                'packages': sorted(ignored_packages.get('npm_packages', [])),
                'expanded': False
            },
            {
                'name': 'Ignored Python Packages',
                'key': 'pip_packages',
                'packages': sorted(ignored_packages.get('pip_packages', [])),
                'expanded': False
            },
            {
                'name': 'Kept Dependencies (Brew Formulae)',
                'key': 'keep_deps_brew',
                'packages': sorted(keep_dependencies.get('brew_formulae', [])),
                'expanded': False
            }
        ]

        # Track package states: 'keep' or 'remove'
        package_states = {}
        for cat in categories:
            for pkg in cat['packages']:
                package_states[f"{cat['key']}:{pkg}"] = 'keep'

        current_row = 0

        while True:
            stdscr.clear()
            h, w = stdscr.getmaxyx()

            # Draw header
            header = "Manage Ignored/Kept Packages - Arrows: navigate | Enter/Tab: expand/collapse | Space: toggle | s: save | q: quit"
            stdscr.attron(curses.color_pair(5))
            stdscr.addstr(0, 0, header[:w-1].ljust(w-1))
            stdscr.attroff(curses.color_pair(5))

            # Count changes
            to_remove_count = sum(1 for v in package_states.values() if v == 'remove')

            summary = f"Packages to remove from lists: {to_remove_count}"
            stdscr.addstr(1, 2, summary)

            # Draw categories and packages
            row = 3
            menu_items = []

            for cat_idx, cat in enumerate(categories):
                if row >= h - 1:
                    break

                total_in_cat = len(cat['packages'])
                if total_in_cat == 0:
                    continue

                # Category header
                menu_items.append(('category', cat_idx, None))
                prefix = "▼" if cat['expanded'] else "▶"
                cat_line = f"{prefix} {cat['name']} ({total_in_cat})"

                if current_row == len(menu_items) - 1:
                    stdscr.attron(curses.color_pair(4) | curses.A_BOLD)
                    stdscr.addstr(row, 2, cat_line[:w-3])
                    stdscr.attroff(curses.color_pair(4) | curses.A_BOLD)
                else:
                    stdscr.attron(curses.A_BOLD)
                    stdscr.addstr(row, 2, cat_line[:w-3])
                    stdscr.attroff(curses.A_BOLD)
                row += 1

                # Show packages if expanded
                if cat['expanded']:
                    for pkg in cat['packages']:
                        if row >= h - 1:
                            break
                        key = f"{cat['key']}:{pkg}"
                        state = package_states.get(key, 'keep')
                        menu_items.append(('package', cat_idx, pkg))

                        if state == 'remove':
                            state_str = "[REMOVE]  "
                            color = 2  # red
                        else:
                            state_str = "[KEEP]    "
                            color = 1  # green

                        pkg_line = f"    {state_str} {pkg}"

                        if current_row == len(menu_items) - 1:
                            stdscr.attron(curses.color_pair(color) | curses.A_REVERSE)
                            stdscr.addstr(row, 2, pkg_line[:w-3])
                            stdscr.attroff(curses.color_pair(color) | curses.A_REVERSE)
                        else:
                            stdscr.attron(curses.color_pair(color))
                            stdscr.addstr(row, 2, pkg_line[:w-3])
                            stdscr.attroff(curses.color_pair(color))
                        row += 1

            stdscr.refresh()

            # Handle input
            key = stdscr.getch()

            if key == ord('q') or key == 27:  # q or ESC
                return None
            elif key == ord('s'):
                # Save and exit
                result_ignored = {
                    'brew_formulae': set(),
                    'brew_casks': set(),
                    'npm_packages': set(),
                    'pip_packages': set()
                }
                result_kept_deps = {
                    'brew_formulae': set()
                }

                for key, state in package_states.items():
                    if state == 'keep':
                        cat_key, pkg = key.split(':', 1)
                        if cat_key == 'keep_deps_brew':
                            result_kept_deps['brew_formulae'].add(pkg)
                        else:
                            result_ignored[cat_key].add(pkg)

                return result_ignored, result_kept_deps
            elif key == curses.KEY_UP:
                current_row = max(0, current_row - 1)
            elif key == curses.KEY_DOWN:
                current_row = min(len(menu_items) - 1, current_row + 1)
            elif key == ord('\n') or key == curses.KEY_ENTER or key == ord('\t') or key == 9:
                # Toggle category expansion
                if current_row < len(menu_items):
                    item_type, cat_idx, pkg = menu_items[current_row]
                    if item_type == 'category':
                        categories[cat_idx]['expanded'] = not categories[cat_idx]['expanded']
                    elif item_type == 'package':
                        categories[cat_idx]['expanded'] = False
            elif key == ord(' '):
                # Toggle package state
                if current_row < len(menu_items):
                    item_type, cat_idx, pkg = menu_items[current_row]
                    if item_type == 'package':
                        cat = categories[cat_idx]
                        key = f"{cat['key']}:{pkg}"
                        current_state = package_states.get(key, 'keep')
                        # Toggle between keep and remove
                        package_states[key] = 'remove' if current_state == 'keep' else 'keep'

    return curses.wrapper(menu_main)


def manage_ignored_packages():
    """Manage ignored and kept packages interactively."""
    print(f"{GREEN}=== Manage Ignored/Kept Packages ==={RESET}\n")

    # Load current ignored packages
    ignored_config = load_ignored_packages()
    ignored_packages = {
        'brew_formulae': ignored_config.get('brew_formulae', []),
        'brew_casks': ignored_config.get('brew_casks', []),
        'npm_packages': ignored_config.get('npm_packages', []),
        'pip_packages': ignored_config.get('pip_packages', [])
    }
    keep_dependencies = ignored_config.get('keep_dependencies', {})

    # Count packages
    total_ignored = (len(ignored_packages['brew_formulae']) +
                     len(ignored_packages['brew_casks']) +
                     len(ignored_packages['npm_packages']) +
                     len(ignored_packages['pip_packages']))
    total_kept = len(keep_dependencies.get('brew_formulae', []))

    print(f"Currently managing:")
    print(f"{total_ignored} ignored packages")
    print(f"{total_kept} kept dependency packages")

    if total_ignored == 0 and total_kept == 0:
        print("\nNo ignored or kept packages to manage.")
        return

    print("\nLaunching interactive menu...")
    print("(Arrow keys: navigate | Enter/Tab: expand/collapse | Space: toggle | s: save | q: quit)\n")

    # Show interactive menu
    result = interactive_manage_ignored(ignored_packages, keep_dependencies)

    if result is None:
        print("\nCancelled.")
        return

    result_ignored, result_kept_deps = result

    # Save changes
    print("\nSaving changes...")
    save_ignored_packages(result_ignored, result_kept_deps)
    print(f"Updated: ~/.config/setup-tools/ignored_packages.toml")

    print("\n✓ Management complete!")


def install_command_line_tools(check_only=False):
    """Install Xcode Command Line Tools if not present."""
    # Check if Command Line Tools are installed
    result = subprocess.run(['xcode-select', '-p'], capture_output=True, text=True)

    if result.returncode == 0:
        print("✓ Command Line Tools are already installed")
        return True

    print("✗ Command Line Tools not found")

    if check_only:
        print("Command Line Tools would be installed")
        return False

    print("Installing Command Line Tools...")
    print("A dialog will appear - please follow the prompts to install")

    # Trigger the installation dialog
    subprocess.run(['xcode-select', '--install'])

    print("\nWaiting for Command Line Tools installation to complete...")
    print("(This script will continue once installation is done)")

    # Wait for installation to complete
    import time
    max_wait = 600  # 10 minutes max
    waited = 0
    while waited < max_wait:
        result = subprocess.run(['xcode-select', '-p'], capture_output=True, text=True)
        if result.returncode == 0:
            print("✓ Command Line Tools installed successfully")
            return True
        time.sleep(5)
        waited += 5

    print("✗ Command Line Tools installation timed out")
    print("Please install manually with: xcode-select --install")
    return False


def main():
    parser = argparse.ArgumentParser(description='setup development environment in layers')
    parser.add_argument('-c', '--check', action='store_true', help='only check what would be installed')
    parser.add_argument('-l', '--layer', type=int, choices=[0, 1, 2, 3, 4, 5], help='run specific layer only')
    parser.add_argument('-s', '--sync', action='store_true', help='sync installed packages with setup.toml')
    parser.add_argument('-m', '--manage-ignored', action='store_true', help='manage ignored and kept packages')
    args = parser.parse_args()

    # macOS only for now
    if platform.system() != "Darwin":
        print("error: this script currently only supports macOS")
        sys.exit(1)

    # Get paths
    script_dir = get_script_dir()
    if not script_dir:
        script_dir = os.getcwd()

    home = os.path.expanduser("~")
    os.chdir(home)

    # Create ~/bin if needed
    bin_dir = os.path.join(home, "bin")
    if not os.path.exists(bin_dir):
        os.makedirs(bin_dir)

    # Load configuration
    config_path = os.path.join(script_dir, 'setup.toml')
    if not os.path.exists(config_path):
        print(f"error: configuration file {config_path} not found")
        sys.exit(1)

    config = load_config(config_path)

    # Handle manage-ignored mode
    if args.manage_ignored:
        manage_ignored_packages()
        return

    # Handle sync mode
    if args.sync:
        sync_packages(config_path, config)
        return

    # Install Command Line Tools if not present (required for Homebrew)
    print(f"{GREEN}=== Command Line Tools ==={RESET}\n")
    install_command_line_tools(args.check)

    # Install Homebrew if not present
    if not command_exists("brew"):
        print(f"\n{GREEN}=== Installing Homebrew ==={RESET}\n")
        if not args.check:
            run_command('/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"')
        else:
            print("homebrew would be installed")

    # Count total packages to install (for progress bar)
    if not args.check and not args.sync and not args.manage_ignored:
        total_packages = 0
        # Count brew packages
        if 'layer2' in config:
            installed_formulae = get_all_brew_packages()
            installed_casks = get_installed_brew_casks()
            installed_all = installed_formulae | installed_casks
            for pkg in config['layer2'].get('brew_packages', []):
                pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
                pkg_base = pkg_name.split('@')[0]
                if pkg_name not in installed_all and pkg_base not in installed_all:
                    total_packages += 1

            # Count npm packages
            installed_npm = get_installed_npm_packages()
            for pkg in config['layer2'].get('npm_packages', []):
                pkg_name = pkg.split('/')[-1] if '/' in pkg else pkg
                if pkg not in installed_npm and pkg_name not in installed_npm:
                    total_packages += 1

            # Count pip packages
            installed_pip = get_installed_pip_packages()
            for pkg in config['layer2'].get('pip_packages', []):
                if pkg.lower() not in installed_pip:
                    total_packages += 1

        progress.set_total(total_packages)

    # Run layers
    if args.layer is None or args.layer == 0:
        layer0_shell(config, home, args.check)

    if args.layer is None or args.layer == 1:
        layer1_sudo(config, script_dir, home, args.check)

    if args.layer is None or args.layer == 2:
        layer2_base_packages(config, args.check)

    if args.layer is None or args.layer == 3:
        layer3_emacs(config, home, args.check)

    if args.layer is None or args.layer == 4:
        layer4_doom(config, script_dir, home, args.check)

    if args.layer is None or args.layer == 5:
        layer5_symlinks(config, script_dir, home, args.check)

    # Finish progress bar
    progress.finish()

    # Summary
    if args.check:
        print("\n" + "="*50)
        print("check complete. run without --check to install.")
        print("="*50)
    else:
        print("\n" + "="*50)
        print("setup complete!")
        print("="*50)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        progress.finish()
        print(f"\n\n{RED}Setup interrupted by user{RESET}")
        sys.exit(130)
    except subprocess.CalledProcessError as e:
        progress.finish()
        print(f"\n{RED}✗ Command failed with exit code {e.returncode}: {e.cmd}{RESET}", file=sys.stderr)
        sys.exit(e.returncode)
