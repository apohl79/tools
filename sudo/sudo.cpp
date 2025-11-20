/*
 * sudo clang++ --std=c++14 -O3 sudo.cpp -o ~/bin/sudo && sudo chown root: ~/bin/sudo && sudo chmod +s ~/bin/sudo
 */
#include <iostream>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "usage: " << argv[0] << " [command]" << std::endl;
        return 1;
    }
    if (0 == setuid(0) && 0 == setgid(0)) {
        auto* file = argv[1];
        char* args[argc];
        for (int i = 1; i < argc; i++) {
            args[i - 1] = argv[i];
        }
        args[argc - 1] = nullptr;
        execvp(file, args);
    } else {
        std::cerr << "failed to become root" << std::endl;
        return 1;
    }
}
