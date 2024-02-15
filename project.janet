(declare-project
  :name "snake"
  :dependencies [{:url "https://github.com/janet-lang/jaylib.git"
                  :tag "f62fb0d9bc780c1095a9d5ac7e4c380eebacc0a1"}
                 {:url "https://github.com/janet-lang/spork.git"
                  :tag "7a4eff4bfb9486a6c6079ee8bb12e6789cce4564"}])
                  
(declare-executable
 :name "snakegame"
 :entry "main.janet"
 :install true)

(task "dev" []
  (shell "jpm -l janet -d main.janet"))
