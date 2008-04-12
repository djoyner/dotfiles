;;; ~/emacs/lisp/skeleton-config.el

(require 'skeleton)

(define-skeleton dot-cpp-file-header-skeleton
  "Insert a .cpp file header"
  nil
  "/// @file" \n
  "/// @brief " _ \n
  "///" \n
  "///  Copyright (c) " (format-time-string "%Y" (current-time)) " by Spirent Communications Inc." \n
  "///  All Rights Reserved." \n
  "///" \n
  "///  This software is confidential and proprietary to Spirent Communications Inc." \n
  "///  No part of this software may be reproduced, transmitted, disclosed or used" \n
  "///  in violation of the Software License Agreement without the expressed" \n
  "///  written consent of Spirent Communications Inc." \n
  "///" \n
  \n
)

(define-skeleton dot-h-file-header-skeleton
  "Insert a .h file header"
  nil
  "/// @file" \n
  "/// @brief " _ \n
  "///" \n
  "///  Copyright (c) " (format-time-string "%Y" (current-time)) " by Spirent Communications Inc." \n
  "///  All Rights Reserved." \n
  "///" \n
  "///  This software is confidential and proprietary to Spirent Communications Inc." \n
  "///  No part of this software may be reproduced, transmitted, disclosed or used" \n
  "///  in violation of the Software License Agreement without the expressed" \n
  "///  written consent of Spirent Communications Inc." \n
  "///" \n
  \n
  "#ifndef _FOO_H_" \n
  "#define _FOO_H_" \n
  \n
  "#endif" \n
)

;; Setup skeleton autoinsert for new files
(require 'autoinsert)

(auto-insert-mode)
(setq auto-insert-query nil)

(setq auto-insert-alist
      '((("\\.\\(cc\\|cpp\\)\\'" . "C++ Source File") nil (dot-cpp-file-header-skeleton))
        (("\\.\\([h]\\|hh\\|hpp\\|tcc\\)\\'" . "C++ Header File") nil (dot-h-file-header-skeleton))
	))

;;; end ~/emacs/lisp/skeleton-config.el
