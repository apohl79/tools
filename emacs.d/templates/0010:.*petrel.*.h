/*
 * Copyright (c) `(format-time-string "%Y")` Andreas Pohl
 * Licensed under MIT (see COPYING)
 *
 * Author: Andreas Pohl
 */

#ifndef ${1:`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H}
#define $1

namespace petrel {

/*
 * ${2:`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`} class
 *
 * \brief Description for $2
 */
class $2 {
  public:
    $2();
    virtual ~$2();
$0
};

}  // petrel

#endif  // $1
