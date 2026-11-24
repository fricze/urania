(ns assistant-test
  "Checks the assistant: JSON, reply splitting, saving, prompts, and the UI
  rendered headlessly. No agent or display needed:
  jolt -A:test -m assistant-test"
  (:require [acp]
            [assistant :as a]
            [glimmer.ratom :as r]
            [glimmer-cui.headless :as h]
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
  ;; switching prompts in the editor must not copy one over another
  (let [s (h/open {:width 1000 :height 700 :theme :dark})]
    (with-redefs [a/prompts (r/atom a/default-prompts)
                  a/prompts-open (r/atom true)
                  a/save-prompts! (fn [])]
      (h/mount! s [a/prompts-dialog])
      (doseq [i [1 3 0 2 0]]
        (a/edit-prompt! i)
        (h/step! s 3))
      (assert (= a/default-prompts @a/prompts) (pr-str @a/prompts))
      ;; an edit lands on the selected prompt only
      (a/update-prompt! 0 :title "Explain it")
      (h/step! s 2)
      (assert (= (assoc-in a/default-prompts [0 :title] "Explain it") @a/prompts) (pr-str @a/prompts)))
    (h/close! s))
  ;; the whole app renders: a finished turn with thinking and a tool call,
  ;; an image, a subchat link. SHOT=path also writes a screenshot.
  (let [s (h/open {:width 1440 :height 900 :theme :dark :accent :neutral})]
    (with-redefs [a/status (r/atom nil)
                  a/prompts (r/atom a/default-prompts)
                  a/chats (r/atom [{:sid "s1" :title "Cache ideas"
                                    :usage {:used 42000 :size 200000 :cost {:amount 0.12 :currency "USD"}}
                                    :plan [{:content "Read the code" :status "completed"}
                                           {:content "Write the answer" :status "in_progress"}]
                                    :turns [{:role :user :text "How should I cache this?" :images ["/nonexistent/cat.png"]}
                                            {:role :agent :thought "Look at the callers first." :done true
                                             :t0 0 :thought-end 2000 :t1 5000
                                             :items [{:type :tool :title "Read src/api.clj" :kind "read" :status "completed"
                                                      :t0 100 :t1 900}
                                                     {:type :text :text "Three options:\n\n1. **Memoize** it\n2. Batch the calls"}]}]}
                                   {:sid "s2" :title "Batching" :parent 0 :from 1
                                    :turns [{:role :user :text "> 2. Batch the calls\n\nHow?"}]}])]
      (h/mount! s [a/app])
      (h/step! s 30)
      (when-let [out (System/getenv "SHOT")] (h/screenshot! s out))
      (reset! a/current 1)
      (h/step! s 10))
    (h/close! s))
  (println "assistant test passed"))
