;;; Compiled snippets and support files for `c++-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c++-mode
                     '(("withAsyncScope" "withAsyncScope" "withAsyncScope" nil nil nil
                        "/Users/andreas/.config/doom/snippets/c++-mode/withAsyncScope" nil nil)
                       ("ulk" "std::unique_lock<std::mutex> lock($0);" "unique_lock" nil nil nil
                        "/Users/andreas/.config/doom/snippets/c++-mode/unique_lock" nil nil)
                       ("sdna_hpp"
                        "/*\n * Copyright (c) 2025 Andreas Pohl\n *\n * Author: Andreas Pohl <andreas.pohl@syncdna.com>\n */\n\n#ifndef ${1:_`(upcase (file-name-base buffer-file-name))`_HPP_$(upcase yas-text)}\n#define $1\n\nnamespace syncdna {\n\n$0\n\n} // namespace syncdna\n\n#endif // $1\n"
                        "sdna_hpp" nil nil nil "/Users/andreas/.config/doom/snippets/c++-mode/sdna_hpp" nil nil)
                       ("sdna_cpp"
                        "/*\n * Copyright (c) 2025 Andreas Pohl\n *\n * Author: Andreas Pohl <andreas.pohl@syncdna.com>\n */\n\n#include \"`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`.hpp\"\n\nnamespace syncdna {\n\n$0\n\n} // namespace syncdna\n"
                        "sdna_cpp" nil nil nil "/Users/andreas/.config/doom/snippets/c++-mode/sdna_cpp" nil nil)
                       ("method" "${1:ReturnType} ${2:Class}::${3:MethodName}(${4:params}) {\n    $0\n}\n" "method" nil
                        nil nil "/Users/andreas/.config/doom/snippets/c++-mode/method" nil nil)
                       ("mlg" "std::lock_guard<std::mutex> lock($0);" "lockguard" nil nil nil
                        "/Users/andreas/.config/doom/snippets/c++-mode/lockguard" nil nil)
                       ("ag_hpp"
                        "/*\n * Copyright (c) 2022 Andreas Pohl\n * Licensed under MIT (https://github.com/apohl79/audiogridder/blob/master/COPYING)\n *\n * Author: Andreas Pohl\n */\n\n#ifndef ${1:_`(upcase (file-name-base buffer-file-name))`_HPP_$(upcase yas-text)}\n#define $1\n\nnamespace e47 {\n\n$0\n\n} // namespace e47\n\n#endif // $1\n"
                        "ag_hpp" nil nil nil "/Users/andreas/.config/doom/snippets/c++-mode/ag_hpp" nil nil)
                       ("ag_cpp"
                        "/*\n * Copyright (c) 2022 Andreas Pohl\n * Licensed under MIT (https://github.com/apohl79/audiogridder/blob/master/COPYING)\n *\n * Author: Andreas Pohl\n */\n\n#include \"`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`.hpp\"\n\nnamespace e47 {\n\n$0\n\n} // namespace e47\n"
                        "ag_cpp" nil nil nil "/Users/andreas/.config/doom/snippets/c++-mode/ag_cpp" nil nil)
                       ("__main.cpp"
                        "/*\n * Copyright (c) 2025 Andreas Pohl\n *\n * Author: Andreas Pohl\n */\n\n#include <iostream>\n\nint main(int argc, char *argv[]) {\n    $0\n\n    return 0;\n}\n"
                        "__main.cpp" nil nil nil "/Users/andreas/.config/doom/snippets/c++-mode/__main.cpp" nil nil)
                       ("__hpp"
                        "/*\n * Copyright (c) 2025 Andreas Pohl\n *\n * Author: Andreas Pohl\n */\n\n#ifndef ${1:_`(upcase (file-name-base buffer-file-name))`_HPP_$(upcase yas-text)}\n#define $1\n\n$0\n\n#endif // $1\n"
                        "__hpp" nil nil nil "/Users/andreas/.config/doom/snippets/c++-mode/__hpp" nil nil)
                       ("__cpp"
                        "/*\n * Copyright (c) 2025 Andreas Pohl\n *\n * Author: Andreas Pohl\n */\n\n#include \"`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`.hpp\"\n\n$0\n"
                        "__cpp" nil nil nil "/Users/andreas/.config/doom/snippets/c++-mode/__cpp" nil nil)))


;;; Do not edit! File generated at Sun Jul 13 23:13:06 2025
