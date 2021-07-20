;;; org-mind-map-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-mind-map" "org-mind-map.el" (0 0 0 0))
;;; Generated autoloads from org-mind-map.el

(autoload 'org-mind-map-write-with-prompt "org-mind-map" "\
Prompt for an output FILENAME (without extension) to write your output and .dot files." nil nil)

(autoload 'org-mind-map-write "org-mind-map" "\
Create a digraph based on all org trees in the current buffer.
The digraph will be named the same name as the current buffer.
To customize, see the org-mind-map group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'.

\(fn &optional PROMPTP)" t nil)

(autoload 'org-mind-map-write-current-branch "org-mind-map" "\
Create a directed graph output based on just the current org tree branch.
To customize, see the org-mind-map group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'.

\(fn &optional PROMPTP)" t nil)

(autoload 'org-mind-map-write-current-tree "org-mind-map" "\
Create a directed graph output based on the whole current org tree.
If called with prefix arg (or PROMPTP is non-nil), then call `org-mind-map-write-with-prompt'.

\(fn &optional PROMPTP)" t nil)

(autoload 'org-mind-map-make-node-fn "org-mind-map" "\
Create a function org-mind-map-NAME-node for use with :OMM-NODE-FMT writing node properties.
The created function should be added to `org-mind-map-node-formats' and the associated string
can be used as the :OMM-NODE-FMT for a tree. 
Document the function with the DOC arg.
PROPS is a list of either property & format string pairs, or individual property names,
which will be placed in each node, e.g: ((\"PROB\" \"probability=%s\") \"COST\"). 
For property names with no format string, \"%s=%s\" will be used with the property name and value.

The node shape and background color can be specified with the optional SHAPE and COLOR arguments, 
and any other attributes (e.g. \"fontsize=30\") can be specified with the OTHER argument.
Each of these arguments can be either a string or a form which is evaluated for each node, 
and returns a string.

Example: (org-mind-map-make-node-fn decisiontree \"Draw decision tree\" (\"COST\" (\"NOTES\" \"Notes: %s\")) nil
			   (cond ((equal (org-mind-map-get-property :todo-keyword el) \"ACTION\") \"red\")
				 ((equal (org-mind-map-get-property :todo-keyword el) \"STATE\") \"yellow\")
				 ((equal (org-mind-map-get-property :todo-keyword el) \"DECISION\") \"green\")))

You could put this code in your emacs startup file (e.g. ~/.emacs) and then add to `org-mind-map-node-formats' 
the pair '(\"decisiontree\" . org-mind-map-decisiontree-node), and use \":OMM-NODE-FMT: decisiontree\" as a
tree property.

\(fn NAME DOC PROPS &optional SHAPE COLOR OTHER)" nil t)

(autoload 'org-mind-map-make-edge-fn "org-mind-map" "\
Create a function org-mind-map-write-NAME for writing edge properties which can be used for :OMM-EDGE-FMT.
Document the function with the DOC arg.
PROPS is a list of either property & format string pairs, or individual property names,
which will concatenated and used to label the edges, e.g: ((\"PROB\" \"probability=%s\") \"COST\"). 
For property names with no format string \"%s=%s\" will be used with the property name and value.

The edge style and color can be specified with the optional STYLE and COLOR arguments,
and any other attributes (e.g. \"fontsize=30\") can be specified with the OTHER argument.
Each of these arguments can be either a string or a form which is evaluated for each node, 
and returns a string.

Example: (org-mind-map-make-edge-fn decisiontree \"Draw decision tree\" (\"PROB\"))

You could put this code in your emacs startup file (e.g. ~/.emacs) and then add to `org-mind-map-edge-formats' 
the pair '(\"decisiontree\" . org-mind-map-decisiontree-edge), and use \":OMM-EDGE-FMT: decisiontree\" as a
tree property.

\(fn NAME DOC PROPS &optional STYLE COLOR OTHER)" nil t)

(register-definition-prefixes "org-mind-map" '("org-mind-map-" "ox-graphviz-export"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-mind-map-autoloads.el ends here
