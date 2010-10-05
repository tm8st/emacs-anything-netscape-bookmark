;;; anything-netscape-bookmark.el --- anything interface for netscape format bookmark file.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <http://twitter.com/tm8st>
;; Version: 0.1
;; Keywords: url, bookmark, browse, netscape

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received ba  copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Netscapeのブックマークマネージャーからエクスポートしたブックマークファイルの内容を、
;; anythingするためのanything-sourceです。
;; エクスポートされたファイル形式が [NETSCAPE-Bookmark-file-1] だったためこの名前になっています。

;; 作成にあたって、
;; anything-hatena-bookmark(http://github.com/k1LoW/anything-hatena-bookmark)を参考にさせていただきました。

;; 設定例

;; (require 'anything-netscape-bookmark)
;; (global-set-key (kbd "C-q C-a C-b") 'anything-netscape-bookmark)
;; (global-set-key (kbd "C-q C-a C-v") 'anything-netscape-bookmark-get-dump)

;;; Code:

(eval-when-compile (require 'cl))
(require 'anything)
(require 'url)
(require 'xml)
(require 'sha1)

(defcustom anything-netscape-bookmark-file
  "~/Documents/Bookmarks.html"
  "Bookmarkの元ファイル。ChromeのExport等で作成されたものを使用。"
  :type 'string
  :group 'anything
  )
(defcustom anything-netscape-bookmark-dump-file
  "~/.emacs.d/emacs-netscape-bookmarks"
  "anything用に加工済のBookmarkファイル"
  :type 'string
  :group 'anything
  )
(defvar anything-netscape-bookmark-candidate-number-limit 9999)
(defvar anything-netscape-bookmark-requires-pattern 3)
(defvar anything-netscape-bookmark-samewindow anything-samewindow)

(defun anything-netscape-bookmark-get-dump ()
  "Get Netscape Bookmark dump file."
  (interactive)
  (let(
       (bokkmark-buffer (get-buffer-create "*anything netscape bookmark dump*"))
       (list '())
       )
    (switch-to-buffer bokkmark-buffer)
    (insert-file-contents anything-netscape-bookmark-file)
    (goto-char (point-min))
    (while (re-search-forward "HREF=\"http\\([s]\\)?:\\([a-zA-Z.-_?#0-9]+\\)" nil t)
      (add-to-list 'list (match-string 0)))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (dolist (i list)
	    (insert (concat (substring i 6 (length i)) "\n")))
    (write-file anything-netscape-bookmark-dump-file)
    (kill-buffer (current-buffer))))

(defvar anything-c-source-netscape-bookmark
  `((name . "Netscape Bookmark")
    (init
     . (lambda ()
           (with-current-buffer (anything-candidate-buffer 'global)
             (insert-file-contents ,anything-netscape-bookmark-dump-file))))
    (candidates-in-buffer)
    (candidate-number-limit . ,anything-netscape-bookmark-candidate-number-limit)
    (requires-pattern . ,anything-netscape-bookmark-requires-pattern)    
    (migemo)
    ;; (multiline)
    (action
     ("Browse URL" . (lambda (candidate)
                       (browse-url candidate)))
     ("Show URL" . (lambda (candidate)
                     (message candidate)))
     ("Insert URL" . (lambda (candidate)
                     (insert candidate)))))
  "")

(defun anything-netscape-bookmark ()
  "Search Netscape Bookmark using `anything'."
  (interactive)
  (let ((anything-samewindow anything-netscape-bookmark-samewindow))
    (unless (file-exists-p anything-netscape-bookmark-dump-file)
      (anything-netscape-bookmark-get-dump))
    (anything
     '(anything-c-source-netscape-bookmark) nil "Find Bookmark: " nil nil)))

(provide 'anything-netscape-bookmark)

