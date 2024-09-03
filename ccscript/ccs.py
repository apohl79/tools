#!/usr/bin/env python3

import time, os, sys, argparse, glob

CXX = 'g++'
CXXFLAGS = '-O0 -std=c++14 -DNDEBUG=1 -Wall '
LDFLAGS = '-std=c++14 -stdlib=libc++ '
OBJECTS = '-lpthread '
INCLUDES = []
JUCE_ROOT = '{}/audio/JUCE_obj'.format(os.getenv('HOME'))
JUCE_CXXFLAGS = "-I" + JUCE_ROOT + "/include -DJUCE_GLOBAL_MODULE_SETTINGS_INCLUDED=1 -DJUCE_MODULE_AVAILABLE_juce_analytics=1 -DJUCE_MODULE_AVAILABLE_juce_audio_basics=1 -DJUCE_MODULE_AVAILABLE_juce_audio_devices=1 -DJUCE_MODULE_AVAILABLE_juce_audio_formats=1 -DJUCE_MODULE_AVAILABLE_juce_audio_processors=1 -DJUCE_MODULE_AVAILABLE_juce_audio_utils=1 -DJUCE_MODULE_AVAILABLE_juce_blocks_basics=1 -DJUCE_MODULE_AVAILABLE_juce_core=1 -DJUCE_MODULE_AVAILABLE_juce_cryptography=1 -DJUCE_MODULE_AVAILABLE_juce_data_structures=1 -DJUCE_MODULE_AVAILABLE_juce_dsp=1 -DJUCE_MODULE_AVAILABLE_juce_events=1 -DJUCE_MODULE_AVAILABLE_juce_graphics=1 -DJUCE_MODULE_AVAILABLE_juce_gui_basics=1 -DJUCE_MODULE_AVAILABLE_juce_gui_extra=1 -DJUCE_MODULE_AVAILABLE_juce_opengl=1 -DJUCE_MODULE_AVAILABLE_juce_osc=1 -DJUCE_MODULE_AVAILABLE_juce_product_unlocking=1 -DJUCE_MODULE_AVAILABLE_juce_video=1 -DJUCE_STANDALONE_APPLICATION=1 -DJUCE_USE_CURL=0 -DJUCE_WEB_BROWSER=0 -DJUCE_STANDALONE_APPLICATION=1"
JUCE_LDFLAGS = "-isysroot /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk -framework CoreAudioKit -framework DiscRecording -framework CoreAudio -framework CoreMIDI -framework AudioToolbox -framework Accelerate -framework OpenGL -framework Carbon -framework QuartzCore -framework Cocoa -framework Foundation -framework IOKit -framework WebKit -framework AVKit -framework AVFoundation -framework CoreMedia"

ts = int(time.time())
fin = '/tmp/script.{}.cpp'.format(int(ts))
obj = '/tmp/script.{}.o'.format(int(ts))
exe = '/tmp/script.{}'.format(int(ts))

template = '''
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <iostream>
#include <iomanip>
#include <math.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <thread>
#include <atomic>
#include <bitset>
##incl##

using namespace std;
//using namespace __gnu_cxx;
//using namespace pt;

##func##

int main(int argc, char **argv) {
##code##
}
'''

def loadFile(sfile):
    funcs = ''
    code = ''
    with open(sfile) as f:
        isFunc = False
        for line in f.readlines():
            if "#func_start" in line:
                isFunc = True
            elif "#func_end" in line:
                isFunc = False
            else:
                if isFunc:
                    funcs += line + '\n'
                else:
                    code += line + '\n'
    return (code, funcs)

def cpp(code, funcs):
    incl = ''
    for i in INCLUDES:
        if len(i) > 0:
            incl += '#include <{}>\n'.format(i)

    code = template.replace('##incl##', incl).replace('##func##', funcs).replace('##code##', code)

    with open(fin, 'w') as f:
        f.write(code)


def build():
    global CXXFLAGS, LDFLAGS

    envCXXFLAGS = os.getenv('CXXFLAGS')
    if envCXXFLAGS is not None:
        CXXFLAGS += envCXXFLAGS

    envLDFLAGS = os.getenv('LDFLAGS')
    if envLDFLAGS is not None:
        LDFLAGS += envLDFLAGS

    # compile
    cmd = '{} {} -c -o {} {}'.format(CXX, CXXFLAGS, obj, fin)
    #print(cmd)
    os.system(cmd)

    # link
    cmd = '{} {} {} {} -o {}'.format(CXX, LDFLAGS, OBJECTS, obj, exe)
    #print(cmd)
    os.system(cmd)

def run():
    os.system(exe)

def cleanup():
    os.remove(fin)
    os.remove(obj)
    os.remove(exe)

def main():
    parser = argparse.ArgumentParser(description='C++ Script')
    parser.add_argument('-i,--target', dest='includes', type=str,
                        help='Add one or multiple headers to include, separated by comma')
    parser.add_argument('-e,--exec', dest='code', type=str, metavar='CODE',
                        help='Execute the given code')
    parser.add_argument('-f,--file', dest='sfile', type=str, metavar='SCRIPT',
                        help='Execute the given script file')
    parser.add_argument('--clang', dest='clang', action='store_true', default=False,
                        help='Run the executable in clang')
    parser.add_argument('--gdb', dest='gdb', action='store_true', default=False,
                        help='Run the executable in gdb')
    parser.add_argument('--valgrind', dest='valgrind', action='store_true', default=False,
                        help='Run the executable in valgrind')
    parser.add_argument('--juce', dest='juce', action='store_true', default=False,
                        help='Add JUCE support')

    args = parser.parse_args()

    code = args.code
    funcs = ''

    global CXXFLAGS, LDFLAGS, INCLUDES, OBJECTS

    if code is None and args.sfile is not None:
        (code, funcs) = loadFile(args.sfile)
    if args.includes is not None:
        for i in args.includes.split(','):
            if len(i) > 0:
                INCLUDES.append(i)
    if args.juce:
        INCLUDES.append('JuceHeader.h')
        for o in glob.glob(JUCE_ROOT + '/*.o'):
            OBJECTS += o + ' '
        CXXFLAGS += JUCE_CXXFLAGS + ' '
        LDFLAGS += JUCE_LDFLAGS + ' '

    if code is not None:
        cpp(code, funcs)
        build()
        run()
        #cleanup()
    else:
        parser.print_usage()

if __name__ == "__main__":
    main()
