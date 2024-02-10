(declare-project
  :name "snake"
  :dependencies [{:url "https://github.com/janet-lang/jaylib.git"
                  :tag "f62fb0d9bc780c1095a9d5ac7e4c380eebacc0a1"}
                 {:url "https://github.com/janet-lang/spork.git"
                  :tag "641b27238e073c5f5f963ec16c79b51643d3e66f"}])
                  
(declare-executable
 :name "snakegame"
 :entry "main.janet"
 :install true)

(task "dev" []
  (shell "jpm -l janet -d main.janet"))
