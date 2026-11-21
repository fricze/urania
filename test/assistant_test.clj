(ns assistant-test
  "Checks the assistant's JSON and reply splitting. No agent or display needed:
  jolt -A:test -m assistant-test"
  (:require [acp]
            [assistant :as a]
            [cui.core :as ui]
            [clojure.string :as str]))

(defn -main [& _]
  (let [src "{\"a\":[1,-2.5e1,\"x\\u00e9\\n\\\"\",null,true,false],\"b\":{},\"c\":[],\"e\":\"\\ud83d\\ude00\"}"
        v (acp/read-json src)]
    (assert (= v {:a [1 -25.0 "xé\n\"" nil true false] :b {} :c [] :e "😀"}) (pr-str v))
    (assert (= v (acp/read-json (acp/write-json v)))))
  (let [reply "Three options:\n\n1. **Cache** it\n   with a TTL\n2. Batch the calls\n- Drop it\n\n```clj\n(a)\n\n(b)\n```\nAfter."]
    (assert (= (a/blocks reply)
               ["Three options:" "1. **Cache** it\n   with a TTL" "2. Batch the calls" "- Drop it"
                "```clj\n(a)\n\n(b)\n```\nAfter."])
            (pr-str (a/blocks reply))))
  (assert (= (a/quoted "line one\nline two" "Why?") "> line one\n> line two\n\nWhy?"))
  (assert (= (a/quoted "  " "Just ask") "Just ask"))
  (let [cs [{:title "Cat names!" :sid "abcdef123456"
             :turns [{:role :user :text "3 names"}
                     {:role :agent :thought "hmm" :done true
                      :items [{:type :tool :title "ls" :kind "execute" :status "completed"}
                              {:type :text :text "1. Pixel\n2. Mochi"}]}]}
            {:title "Żółw ideas" :sid "99887766aa" :parent 0 :from 1
             :turns [{:role :user :text "> 2. Mochi\n\nMore?" :images ["/tmp/cat 1.png"]}]}]
        names (mapv a/file-name cs)]
    (assert (= names ["cat-names-abcdef12.md" "żółw-ideas-99887766.md"]) (pr-str names))
    (assert (str/starts-with? (a/chat->md cs names 0)
               (str "# Cat names!\n\n- Folder: `" @a/cwd "`\n- Session: `abcdef123456`\n\n"
                    "## You\n\n3 names\n\n"
                    "## Claude\n\n<details><summary>Thinking</summary>\n\nhmm\n\n</details>\n\n"
                    "- Tool: **ls** (execute, completed)\n\n1. Pixel\n2. Mochi\n\n"
                    "→ Subchat: [Żółw ideas](żółw-ideas-99887766.md)\n\n<!-- muse-chat "))
            (a/chat->md cs names 0))
    (assert (str/includes? (a/chat->md cs names 1) "## You\n\n![](</tmp/cat 1.png>)\n\n> 2. Mochi"))
    (assert (str/includes? (a/chat->md cs names 1) "- Subchat of: [Cat names!](cat-names-abcdef12.md)"))
    ;; saved files load back: turns, parent by session id, a "-->" in text kept
    (let [dir (str (System/getProperty "java.io.tmpdir") "/muse-test-" (System/currentTimeMillis))
          cs (-> cs (assoc-in [0 :created] 1) (assoc-in [1 :created] 2)
                 (assoc-in [1 :turns 0 :text] "a --> b"))]
      (with-redefs [a/chats-dir dir a/chats (atom cs)]
        (a/save-all!)
        (assert (nil? @a/save-error) @a/save-error)
        (let [back (a/load-chats)]
          (assert (= (mapv :title back) ["Cat names!" "Żółw ideas"]) (pr-str (mapv :title back)))
          (assert (= (:turns (back 0)) (:turns (cs 0))) (pr-str (:turns (back 0))))
          (assert (= ["/tmp/cat 1.png"] (get-in back [1 :turns 0 :images])))
          (assert (= "a --> b" (get-in back [1 :turns 0 :text])))
          (assert (= [nil 0] (mapv :parent back)))
          (assert (every? :restored back))))))
  (assert (= ["image/png" "image/jpeg" nil nil] (mapv a/image-type ["a.PNG" "/x/b.jpeg" "notes.txt" "png"])))
  (with-redefs [a/attached (atom ["a.png"])]
    (a/attach! ["a.png" "b.txt" "c.webp"])
    (assert (= ["a.png" "c.webp"] @a/attached) (pr-str @a/attached)))
  (with-redefs [a/draft (atom "") a/chats-dir (str (System/getProperty "java.io.tmpdir") "/muse-p-" (System/currentTimeMillis))
                a/prompts (atom [{:title "Fix" :text "Fix it."} {:title "Tests" :text "Add tests."}])]
    (a/use-prompt! (@a/prompts 0))
    (a/use-prompt! (@a/prompts 1))
    (assert (= "Fix it.\n\nAdd tests." @a/draft) (pr-str @a/draft))
    (assert (= a/default-prompts (a/load-prompts)))
    (a/save-prompts!)
    (assert (= @a/prompts (a/load-prompts))))
  ;; picking another prompt in the editor must not copy it over the previous one
  (let [real-button ui/button
        click (atom nil)
        u (ui/create {:theme :dark})]
    (ui/set-viewport u 1000 700 1)
    (with-redefs [a/prompts (atom a/default-prompts)
                  a/prompts-open (atom true)
                  a/save-prompts! (fn [])
                  ui/button (fn [u label & [opts]] (or (= label @click) (real-button u label opts)))]
      (a/edit-prompt! 1)
      (doseq [pick [nil "Tests" nil "Explain" nil]]
        (reset! click pick)
        (ui/with-frame (ui/begin-frame u (/ 1.0 60)) (a/prompts-dialog u) (ui/end-frame u)))
      (assert (= a/default-prompts @a/prompts) (pr-str @a/prompts))
      (assert (= 0 @a/prompt-sel)))
    (ui/destroy u))
  (println "assistant test passed"))
