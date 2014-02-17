;;;;; Config to make emacs auto complete my C++ code. 
;;;;; I will show them you can be super efficient C++ coder too (not just Javascript!)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(global-ede-mode t)



;; ccache g++ -c -pipe -U__STRICT_ANSI__ -isystem /home/andrew/Nutonian/source-code/third-party/install/include/ -isystem /home/andrew/Nutonian/source-code/tparty -std=c++0x -msse2 -fno-omit-frame-pointer -W -Wall -Wextra -pedantic -Wcast-qual -Wnon-virtual-dtor -Woverloaded-virtual -Wno-unused-parameter -Wno-unused-private-field -isystem /home/andrew/Nutonian/source-code/tparty/ -isystem "/home/andrew/Nutonian/source-code/tparty/cpp-netlib" -g -O0 -g 
;;-D_REENTRANT -fPIC -Wall -W -DBOOST_THREAD_USE_LIB -DBOOST_NETWORK_ENABLE_HTTPS -DBOOST_IOSTREAMS_NO_LIB -D_DEBUG
;; -I../../third-party/install/mkspecs/linux-g++
;; -I../../ncloud -I../../nu
;; -I../../third-party/install/include
;; -I../../../source-code
;;  -I../../tparty/cpp-netlib
;;  -I../../tparty/zlib
;;  -I../../tparty/eureqa-api
;;  -I../../tparty
;;  -I../../../../Nutonian
;;  -I../../ncloud


(ede-cpp-root-project "Nutonian"
                :name "Nutonian Source Code"
                :file "~/Nutonian/source-code/all.pro"
                :include-path '("/"
                                "/third-party/install/mkspecs/linux-g++"
                                "/ncloud"
                                "/nu"
                                "/third-party/install/include"
                                "/tparty/cpp-netlib"
                                "/tparty/zlib"
                                "/tparty/eureqa-api"
                                "/tparty"
                                "/Libs"
                                )
                :system-include-path '("~/exp/include")
                :spp-table '(("isUnix" . "")
                             ("BOOST_TEST_DYN_LINK" . "")
                             ("BOOST_THREAD_USE_LIB " . "")
                             ("BOOST_NETWORK_ENABLE_HTTPS" . "")
                             ("BOOST_IOSTREAMS_NO_LIB" . "")
                             ("_DEBUG" . "")
                             )
)



(add-to-list 'load-path "/home/andrew/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/andrew/.emacs.d/ac-dict")
(ac-config-default)
