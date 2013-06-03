(ns todo-mvc.html-templates
  (:use [io.pedestal.app.templates :only [tfn dtfn tnodes]]))

(defmacro todo-mvc-templates
  []
  {:todo-mvc (dtfn (tnodes "todo-templates.html" "todo-mvc" [[:.main] [:.footer]]) #{})
   :todos-section (dtfn (tnodes "todo-templates.html" "todos-section" [[:.todo-list]]) #{:id})
   :todos (dtfn (tnodes "todo-templates.html" "todos" [[:.todo]]) #{:id})
   :todo (dtfn (tnodes "todo-templates.html" "todo") #{:id})
   :footer (dtfn (tnodes "todo-templates.html" "footer" [[:.todo-count] [:.filters] [:.clear-completed]]) #{:id})
   :filter (dtfn (tnodes "todo-templates.html" "filter") #{:id})
   :remaining (dtfn (tnodes "todo-templates.html" "remaining") #{:id})
   :completed (dtfn (tnodes "todo-templates.html" "completed") #{:id})})

;; Note: this file will not be reloaded automatically when it is changed.
