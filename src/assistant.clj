(ns assistant
  "A chat app that talks to Claude through the Agent Client Protocol: it starts
  `claude-agent-acp` (via npx) and streams reasoning, tool calls, plans and
  answers into CUI's chat components, written as glimmer components. Each chat
  is an ACP session working in the given directory. The workspace panel closes
  below 1180 points and the chat list becomes a drawer below 760.
  Run: jolt run -m assistant [working-dir]"
  (:require [acp]
            [glimmer.ratom :as r]
            [glimmer.core :as g]
            [glimmer-cui.core :as gc]
            [cui.core :as ui]
            [cui.raw :as raw]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def agent-cmd ["npx" "-y" "@agentclientprotocol/claude-agent-acp"])

;; Ratoms are what the components read; a change re-renders only the
;; components that read it. The ACP reader thread writes them too: glimmer
;; queues those re-renders onto the window loop and wakes it.
(def conn (atom nil))
(def status (r/atom "Starting Claude…"))  ; nil once the agent is ready
(def cwd (r/atom (System/getProperty "user.dir")))
(def chats (r/atom []))    ; [{:sid :title :turns [..] :busy :plan [..] :usage {..}}]
(def current (r/atom 0))
(def draft (atom ""))      ; the message box's text; it is an immediate-mode island
(def attached (r/atom []))  ; image paths waiting to go with the next message
(def textures (atom {}))   ; path -> {:tex :w :h}, or :failed
(def models (r/atom []))   ; the agent's "model" config options
(def model (r/atom 0))
(def perms (r/atom []))    ; queued session/request_permission requests
(def perm-open (r/atom false))
(def narrow (r/atom false))
(def wide (r/atom true))
(def drawer (r/atom false))
(def clock (r/atom 0))     ; ticks while a chat is busy, so timers move
(def branch (r/atom nil))  ; the reply being split: {:chat :turn :blocks [..] :picked #{..}}
(def branch-open (r/atom false))
(def branch-quote (r/atom ""))
(def branch-ask (r/atom ""))
(def chats-dir (str (System/getProperty "user.home") "/Muse"))
(def save-error (r/atom nil))
(def prompts (r/atom []))     ; saved prompts [{:title :text}], kept in chats-dir/prompts.json
(def prompts-open (r/atom false))
(def prompt-sel (r/atom 0))   ; the prompt being edited

(defn now [] (System/currentTimeMillis))

(defn follow!
  "Scrolls the conversation to its end on the next frame.
  ponytail: follows every update, even if you scrolled up mid-answer."
  []
  (gc/with-ui #(ui/scroll-to % "chat" 1e9)))

;; ---- chat state, updated from the ACP reader thread

(defn chat-idx [sid] (first (keep-indexed #(when (= sid (:sid %2)) %1) @chats)))

(defn update-chat! [sid f & args]
  ;; ponytail: chats are only ever appended, so an index stays valid
  (when-let [i (chat-idx sid)]
    (swap! chats #(apply update % i f args))
    (follow!)))

(defn update-turn!
  "Applies f to the chat's last turn when it is the agent's."
  [sid f]
  (update-chat! sid update :turns
                (fn [ts] (if (= :agent (:role (peek ts))) (conj (pop ts) (f (peek ts))) ts))))

(defn add-item [turn item]
  (-> turn (update :items conj item) (update :thought-end #(or % (now)))))

(defn add-text [turn text]
  (let [last* (peek (:items turn))]
    (if (= :text (:type last*))
      (assoc-in turn [:items (dec (count (:items turn))) :text] (str (:text last*) text))
      (add-item turn {:type :text :text text}))))

(defn tool-output [{:keys [content rawOutput]}]
  (let [parts (keep (fn [{:keys [type content path]}]
                      (case type
                        "content" (:text content)
                        "diff" (str "Edited " path)
                        nil))
                    content)]
    (cond (seq parts) (str/join "\n" parts)
          (string? rawOutput) rawOutput
          rawOutput (acp/write-json rawOutput))))

(defn update-tool [turn {:keys [toolCallId title kind status rawInput] :as u}]
  (update turn :items
          (fn [items]
            (mapv (fn [it]
                    (if (= toolCallId (:id it))
                      (cond-> it
                        title (assoc :title title)
                        kind (assoc :kind kind)
                        status (assoc :status status)
                        (seq rawInput) (assoc :input (acp/write-json rawInput))
                        (tool-output u) (assoc :output (tool-output u))
                        (#{"completed" "failed"} status) (assoc :t1 (now)))
                      it))
                  items))))

(defn on-update [{:keys [sessionId update]}]
  (let [u update]
    (case (when-not (:restoring (some->> (chat-idx sessionId) (get @chats))) (:sessionUpdate u))
      "agent_message_chunk" (when-let [t (-> u :content :text)] (update-turn! sessionId #(add-text % t)))
      "agent_thought_chunk" (when-let [t (-> u :content :text)] (update-turn! sessionId #(update % :thought str t)))
      "tool_call" (update-turn! sessionId
                                #(update-tool (add-item % {:type :tool :id (:toolCallId u) :title (:title u)
                                                           :status "pending" :t0 (now)})
                                              u))
      "tool_call_update" (update-turn! sessionId #(update-tool % u))
      "plan" (update-chat! sessionId assoc :plan (:entries u))
      "usage_update" (update-chat! sessionId assoc :usage (select-keys u [:used :size :cost]))
      ;; a fork's agent title is the parent's plus "(fork)"; a subchat keeps its own
      "session_info_update" (when (:title u)
                              (update-chat! sessionId #(cond-> % (not (:parent %)) (assoc :title (:title u)))))
      nil)))

(defn on-request [c id method params]
  (if (= method "session/request_permission")
    (do (swap! perms conj {:id id :sid (:sessionId params)
                           :title (or (-> params :toolCall :title) "Allow this action?")
                           :options (:options params)})
        (reset! perm-open true))
    (acp/respond-error! c id -32601 (str "Method not supported: " method))))

(defn answer-permission!
  "Answers queued request p (default: the one on screen) and drops it."
  ([outcome] (answer-permission! (first @perms) outcome))
  ([p outcome]
   (swap! perms (fn [ps] (filterv #(not= p %) ps)))
   (reset! perm-open (boolean (seq @perms)))
   (acp/respond! @conn (:id p) {:outcome outcome})))

;; ---- talking to the agent

(defn set-model! [sid]
  (when-let [m (get @models @model)]
    (acp/request! @conn "session/set_config_option" {:sessionId sid :configId "model" :value (:value m)}
                  (fn [_ e] (when e (update-chat! sid update :turns conj
                                                  {:role :agent :done true
                                                   :items [{:type :error :text (str "Model: " (:message e))}]}))))))

(defn open-session!
  "Adds a chat, shows it and starts its ACP session with method (session/new
  or session/fork). (on-ready i) runs once the session exists."
  [chat method params on-ready]
  (let [i (count @chats)]
    (swap! chats conj (merge {:title "New chat" :turns [] :plan [] :cwd @cwd :created (now)} chat))
    (reset! current i)
    (acp/request! @conn method (merge {:cwd @cwd :mcpServers []} params)
                  (fn [r e]
                    (if e
                      (swap! chats assoc-in [i :turns]
                             [{:role :agent :done true
                               :items [{:type :error :text (str "Could not start a session: " (:message e))}]}])
                      (let [sid (:sessionId r)
                            opt (first (filter #(= "model" (:id %)) (:configOptions r)))]
                        (swap! chats assoc-in [i :sid] sid)
                        (when (and opt (empty? @models))
                          (reset! models (:options opt))
                          (reset! model (or (first (keep-indexed #(when (= (:value %2) (:currentValue opt)) %1)
                                                                 @models))
                                            0)))
                        (when (and opt (not= (:value (get @models @model)) (:currentValue opt)))
                          (set-model! sid))
                        (when on-ready (on-ready i))))
                    (follow!)))))

(defn new-chat! [] (open-session! {} "session/new" {} nil))

(declare send-prompt! send-prompt*)

(defn ensure-live!
  "Runs (k) once chat i has a live session. A chat restored from disk is
  reopened with session/load first; the history the agent replays is
  ignored, since the chat already shows it."
  [i k]
  (let [{:keys [sid restored busy] c-cwd :cwd} (get @chats i)]
    (cond
      (not restored) (k)
      busy nil
      :else (do (update-chat! sid assoc :busy true :restoring true)
                (acp/request! @conn "session/load" {:sessionId sid :cwd (or c-cwd @cwd) :mcpServers []}
                              (fn [_ e]
                                (update-chat! sid assoc :busy false :restoring false :restored (boolean e))
                                (if e
                                  (update-chat! sid update :turns conj
                                                {:role :agent :done true
                                                 :items [{:type :error :text (str "Could not reopen this chat: " (:message e))}]})
                                  (k))))))))

(defn blocks
  "Splits a markdown reply into the parts you can quote: paragraphs and list
  items. A fenced code block stays in one part."
  [text]
  (loop [[l & ls :as all] (str/split-lines text) cur [] out [] fence false]
    (if (empty? all)
      (vec (remove str/blank? (map #(str/join "\n" %) (conj out cur))))
      (let [f? (str/starts-with? (str/triml l) "```")]
        (cond
          fence (recur ls (conj cur l) out (not f?))
          f? (recur ls (conj cur l) out true)
          (str/blank? l) (recur ls [] (conj out cur) false)
          (re-find #"^\s{0,3}([-*+]|\d+[.)])\s" l) (recur ls [l] (conj out cur) false)
          :else (recur ls (conj cur l) out false))))))

(defn quoted [quote question]
  (str/trim (str (when-not (str/blank? quote)
                   (str (str/join "\n" (map #(str "> " %) (str/split-lines (str/trim quote)))) "\n\n"))
                 question)))

(defn branch!
  "Forks the parent chat's session into a subchat and asks about the quote
  there. The parent chat is left as it is."
  [parent turn quote question]
  (let [psid (:sid (get @chats parent))
        text (quoted quote question)
        title (str/trim (if (str/blank? question) quote question))]
    (when (and psid (not (str/blank? text)))
      ;; ponytail: the fork carries the whole parent conversation, also turns after
      ;; the quoted one; the quote is what points Claude at the right part
      (ensure-live! parent
                    #(open-session! {:title (if (> (count title) 40) (str (subs title 0 40) "…") title)
                                     :parent parent :from turn}
                                    "session/fork" {:sessionId psid}
                                    (fn [i] (send-prompt! i text)))))))

;; ---- saving chats as markdown, one file per chat in chats-dir

(def image-types {"png" "image/png" "jpg" "image/jpeg" "jpeg" "image/jpeg" "gif" "image/gif" "webp" "image/webp"})

(defn image-type [path] (image-types (str/lower-case (or (second (re-find #"\.([^./]+)$" path)) ""))))

(defn image-block [path]
  {:type "image" :mimeType (image-type path)
   :data (.encodeToString (java.util.Base64/getEncoder)
                          (java.nio.file.Files/readAllBytes (.toPath (io/file path))))})

(defn attach! [paths]
  (swap! attached #(into % (comp (filter image-type) (remove (set %))) paths)))

(defn file-name [{:keys [title sid]}]
  (let [s (str/replace (str/lower-case (or title "chat")) #"[^\p{L}\p{N}]+" "-")
        s (str/replace (subs s 0 (min 60 (count s))) #"^-+|-+$" "")]
    (str (if (str/blank? s) "chat" s) "-" (subs sid 0 (min 8 (count sid))) ".md")))

(defn turn->md [t]
  (if (= :user (:role t))
    (str "## You\n\n" (str/join "\n\n" (concat (for [p (:images t)] (str "![](<" p ">)")) [(:text t)])))
    (str/join "\n\n"
              (concat ["## Claude"]
                      (when (seq (:thought t))
                        [(str "<details><summary>Thinking</summary>\n\n" (:thought t) "\n\n</details>")])
                      (for [it (:items t)]
                        (case (:type it)
                          :text (:text it)
                          :tool (str "- Tool: **" (or (:title it) "Tool") "** (" (or (:kind it) "tool") ", " (:status it) ")")
                          :error (str "> **Error:** " (:text it))
                          :note (str "_" (:text it) "_")))))))

(defn chat->data
  "What load-chats needs to rebuild chat i; the parent by session id, since
  indices change between runs."
  [cs i]
  (let [{:keys [parent] :as c} (cs i)]
    (cond-> (select-keys c [:title :sid :cwd :created :from :turns])
      parent (assoc :parentSid (:sid (cs parent))))))

(defn chat->md
  "Chat i as markdown. names holds every chat's file name, for the links
  between a chat and its subchats."
  [cs names i]
  (let [{:keys [title sid parent turns] c-cwd :cwd} (cs i)
        subs-of (fn [k] (for [[ci c] (map-indexed vector cs)
                              :when (and (= i (:parent c)) (= k (:from c)) (names ci))]
                          (str "→ Subchat: [" (:title c) "](" (names ci) ")")))]
    (str (str/join "\n\n"
                   (concat [(str "# " title)
                            (str "- Folder: `" (or c-cwd @cwd) "`\n- Session: `" sid "`"
                                 (when (and parent (names parent))
                                   (str "\n- Subchat of: [" (:title (cs parent)) "](" (names parent) ")")))]
                           (mapcat (fn [k t] (cons (turn->md t) (subs-of k))) (range) turns)))
         ;; JSON escapes no "-", so spell "--" inside strings as escapes to keep the comment closed
         "\n\n<!-- muse-chat " (str/replace (acp/write-json (chat->data cs i)) "--" "\\u002d\\u002d") " -->\n")))

(defn md->chat
  "The chat saved in a file written by chat->md, or nil."
  [text]
  (when-let [[_ json] (re-find #"<!-- muse-chat (.*) -->" text)]
    (-> (acp/read-json json)
        (update :turns (fn [ts] (mapv (fn [t] (cond-> (update t :role keyword)
                                                (:items t) (update :items (fn [its] (mapv #(update % :type keyword) its)))))
                                      ts))))))

(defn load-chats
  "Every chat saved in chats-dir, oldest first, marked :restored so its
  session is reopened when it is next used. Unreadable files are skipped."
  []
  (let [loaded (->> (.listFiles (io/file chats-dir))
                    (filter #(str/ends-with? (.getName %) ".md"))
                    (keep (fn [f] (try (some-> (md->chat (slurp f)) (assoc :file (.getName f)))
                                       (catch Exception _ nil))))
                    (filter :sid)
                    (sort-by #(or (:created %) 0))
                    vec)
        idx (into {} (map-indexed (fn [i c] [(:sid c) i]) loaded))]
    (mapv (fn [c] (-> c
                      (assoc :restored true :plan [])
                      (assoc :parent (idx (:parentSid c)))
                      (dissoc :parentSid)))
          loaded)))

(defn save-all!
  "Writes every chat that has turns to chats-dir; a renamed chat's old file
  goes. Runs on the ACP reader thread after each answer."
  []
  (try
    (let [cs @chats
          names (mapv #(when (and (:sid %) (seq (:turns %))) (file-name %)) cs)]
      (io/make-parents (io/file chats-dir "x"))
      (doseq [[i c] (map-indexed vector cs) :when (names i)]
        (spit (io/file chats-dir (names i)) (chat->md cs names i))
        (when (and (:file c) (not= (:file c) (names i)))
          (io/delete-file (io/file chats-dir (:file c)) true))
        (swap! chats assoc-in [i :file] (names i))))
    (reset! save-error nil)
    (catch Exception e (reset! save-error (ex-message e)))))

(def default-prompts
  [{:title "Explain" :text "Explain how this works, step by step:"}
   {:title "Review" :text "Review the current changes for bugs and risky edge cases."}
   {:title "Summarize" :text "Summarize this in five bullet points."}
   {:title "Tests" :text "Write tests for this and run them."}])

(defn prompts-file [] (io/file chats-dir "prompts.json"))

(defn load-prompts
  "The saved prompts, or the defaults when there is no file yet."
  []
  (let [f (prompts-file)]
    (if (.exists f)
      (try (vec (acp/read-json (slurp f))) (catch Exception _ default-prompts))
      default-prompts)))

(defn save-prompts! []
  (try (io/make-parents (prompts-file))
       (spit (prompts-file) (acp/write-json @prompts))
       (catch Exception e (reset! save-error (ex-message e)))))

(defn use-prompt!
  "Puts a saved prompt into the message box, after anything typed so far."
  [{:keys [text]}]
  (swap! draft #(if (str/blank? %) text (str (str/trimr %) "\n\n" text))))

(defn edit-prompt! [i] (reset! prompt-sel i))

(defn update-prompt! [i k v]
  (when (and (< i (count @prompts)) (not= v (get-in @prompts [i k])))
    (swap! prompts assoc-in [i k] v)
    (save-prompts!)))

(defn send-prompt!
  ([text] (send-prompt! @current text))
  ([i text] (send-prompt! i text nil))
  ([i text images]
   (when-not (and (str/blank? text) (empty? images))
     (ensure-live! i #(send-prompt* i text images)))))

(defn- send-prompt* [i text images]
  (let [{:keys [sid busy]} (get @chats i)
        blocks (try (cond-> (mapv image-block images)
                      (not (str/blank? text)) (conj {:type "text" :text text}))
                    (catch Exception e (reset! save-error (str "Could not read image: " (ex-message e))) nil))]
    (when (and sid (not busy) (seq blocks))
      (update-chat! sid #(-> %
                             (assoc :busy true)
                             (update :turns conj (cond-> {:role :user :text text} (seq images) (assoc :images (vec images)))
                                     {:role :agent :thought "" :items [] :t0 (now)})))
      (acp/request! @conn "session/prompt" {:sessionId sid :prompt blocks}
                    (fn [r e]
                      (update-turn! sid #(cond-> (assoc % :done true :t1 (now))
                                           e (add-item {:type :error :text (:message e)})
                                           (= "cancelled" (:stopReason r)) (add-item {:type :note :text "Stopped"})))
                      (update-chat! sid assoc :busy false)
                      (save-all!))))))

(defn cancel! []
  (when-let [sid (:sid (get @chats @current))]
    (acp/notify! @conn "session/cancel" {:sessionId sid})
    (doseq [p @perms :when (= sid (:sid p))] (answer-permission! p {:outcome "cancelled"}))))

(defn start! []
  (let [c (acp/connect agent-cmd {:notify (fn [m p] (when (= m "session/update") (on-update p)))
                                  :request on-request})]
    (reset! conn c)
    (acp/request! c "initialize" {:protocolVersion 1
                                  :clientCapabilities {:fs {:readTextFile false :writeTextFile false}
                                                       :terminal false}
                                  :clientInfo {:name "cui-jolt-assistant" :version "0.1"}}
                  (fn [_ e]
                    (if e
                      (reset! status (str "Claude did not start: " (:message e)))
                      (do (reset! chats (load-chats))
                          (reset! status nil)
                          (new-chat!)))))))

;; ---- UI

;; ---- UI: glimmer components. A component re-renders when a ratom it read
;; changes; CUI-only bits (image textures, drops, the message box) are
;; immediate-mode islands that run every frame.

(defn sidebar [drawer?]
  (let [st @status
        cur @current
        kids (group-by :parent (map-indexed #(assoc %2 :i %1) @chats))
        ;; newest first, each subchat indented under the chat it came from
        rows ((fn walk [parent depth]
                (mapcat (fn [{:keys [i]}] (cons [i depth] (walk i (inc depth))))
                        (reverse (kids parent))))
              nil 0)]
    [:column {:width (if drawer? :fill 264) :height :fill :bg (when-not drawer? :surface-muted)
              :pad (if drawer? 0 12) :gap 2}
     [:row {:gap 10 :pad-x 6 :pad-top 4 :pad-bottom 10 :align :center}
      [:box {:width 28 :height 28 :radius 9 :bg :accent :dir :stack :align :center :justify :center}
       [:text {:text "M" :font :display :size 16 :color :accent-foreground :line-height 1.0}]]
      [:text {:text "Muse" :font :medium :size 15 :width :fill :nowrap true}]
      [:icon-button {:icon :square-pen :size :sm :tooltip "New chat"
                     :on-click #(when (nil? @status) (new-chat!) (reset! drawer false))}]]
     [:nav-section {:title "Chats"}]
     (for [[i depth] rows
           :let [{:keys [title busy parent]} (get @chats i)]]
       [:box {:key i :width :fill :pad-left (* 16 depth)}
        [:nav-item {:label title :icon (if parent :git-branch :message-square) :selected (= cur i)
                    :group "chats" :badge (when busy "…")
                    :on-click #(do (reset! current i) (reset! drawer false))}]])
     [:spacer]
     [:row {:gap 8 :align :center :pad-x 6}
      [:icon {:icon (if st :circle-alert :sparkles) :size 14 :color (if st :text-muted :accent)}]
      [:text {:text (or st "Claude via ACP") :size 12 :color :text-secondary :width :fill}]]]))

(defn secs [t0 t1] (/ (- (or t1 (now)) t0) 1000.0))

(def tool-icons {"read" :file-text "edit" :pencil "delete" :trash-2 "move" :arrow-right "search" :search
                 "execute" :terminal "fetch" :globe "think" :sparkles})

(def tool-status {"pending" :pending "in_progress" :running "completed" :done "failed" :failed})

(defn open-branch! [i k text]
  (reset! branch {:chat i :turn k :blocks (blocks text) :picked #{}})
  (reset! branch-quote "")
  (reset! branch-ask "")
  (reset! branch-open true))

(defn agent-turn [i k {:keys [thought items done t0 thought-end]} prompt chat-busy]
  (when-not done @clock)                ; running timers tick with the clock
  (let [text (str/join "\n\n" (keep :text (filter #(= :text (:type %)) items)))]
    [:column {:width :fill :gap 14}
     (when (seq thought)
       [:reasoning {:id (str "think-" i "-" k) :thinking (and (not done) (nil? thought-end)) :seconds (secs t0 thought-end)}
        [:text {:text thought :size 13.5 :color :text-secondary}]])
     (for [[j it] (map-indexed vector items)]
       (case (:type it)
         :text [:markdown {:key j :id (str "md-" i "-" k "-" j) :text (:text it) :width :fill :size 15 :streaming (not done)}]
         :tool [:tool-call {:key j :id (str "tool-" i "-" k "-" j) :name (or (:title it) "Tool")
                            :icon (tool-icons (:kind it) :code) :detail (:kind it)
                            :status (tool-status (:status it) :pending)
                            :seconds (secs (:t0 it) (or (:t1 it) (when done (now))))
                            :input (:input it) :output (:output it)}]
         :error [:alert {:key j :title (:text it) :tone :danger}]
         :note [:text {:key j :text (:text it) :size 12 :color :text-muted}]))
     (when (and (not done) (empty? items) (empty? thought))
       [:agent-status {:text "Thinking" :status :running :seconds (secs t0 nil)}])
     (when done
       [:row {:gap 4 :align :center :wrap true}
        [:response-actions {:id (str "actions-" i "-" k) :copy-text text :no-share true
                            :on-result #(when (pos? (bit-and % raw/CUI_RESPONSE_REGENERATE)) (send-prompt! prompt))}]
        (when (seq text)
          [:button {:label "Subchat" :icon :git-branch :variant :ghost :size :sm :disabled (boolean chat-busy)
                    :tooltip "Ask about part of this reply in a new chat"
                    :on-click #(open-branch! i k text)}])])
     (when done
       (for [[ci c] (keep-indexed (fn [ci c] (when (and (= i (:parent c)) (= k (:from c))) [ci c])) @chats)]
         [:row {:key ci :gap 6 :align :center}
          [:icon {:icon :git-branch :size 14 :color :text-muted}]
          [:link {:text (:title c) :on-click #(reset! current ci)}]]))]))

(def suggestions [{:icon :search :title "Explain this project" :detail "Read the README and the code layout"}
                  {:icon :code :title "Find a bug" :detail "Look for something that could break"}
                  {:icon :file-text :title "Summarize the recent commits"}
                  {:icon :sparkles :title "Suggest an improvement"}])

(defn empty-state []
  [:column {:width :fill :gap 16 :pad-top 80 :align :center}
   [:text {:text "What can I help with?" :font :display :size 26}]
   [:text {:text @cwd :size 13 :color :text-muted}]
   [:prompt-suggestions {:id "sugg" :items suggestions :columns (if @narrow 1 2)
                         :on-result #(when (>= % 0) (send-prompt! (:title (suggestions %))))}]])

(defn texture [u path]
  (or (@textures path)
      (let [w (atom 0) h (atom 0)
            tex (try (ui/image-load u path w h) (catch Exception _ 0))
            t (if (and tex (pos? tex)) {:tex tex :w @w :h @h} :failed)]
        (swap! textures assoc path t)
        t)))

(defn user-images
  "Thumbnails of a message's images, right-aligned like the message. Textures
  need the ui handle, so this is an immediate-mode island."
  [paths]
  [:cui/immediate
   {:render (fn [u]
              (ui/row u {:width :fill :justify :end :gap 8 :wrap true}
                (doseq [p paths]
                  (let [t (texture u p)]
                    (if (= :failed t)
                      (ui/text u (str "Missing image: " (.getName (io/file p))) {:size 12 :color :text-muted})
                      (let [s (min 1.0 (/ 240.0 (max 1 (:w t))) (/ 240.0 (max 1 (:h t))))]
                        (ui/image u (:tex t) {:width (* s (:w t)) :height (* s (:h t)) :radius 10})))))))}])

(defn prompt-bar
  "One-click saved prompts above the message box."
  []
  (let [ps @prompts]
    [:row {:width :fill :gap 6 :wrap true :align :center}
     (for [[i pr] (map-indexed vector ps)]
       [:button {:key i :label (:title pr) :variant :outline :size :sm :tooltip (:text pr)
                 :on-click #(use-prompt! pr)}])
     [:button {:label (if (seq ps) "" "Saved prompts") :variant :ghost :size :sm :icon :square-pen
               :tooltip "Edit saved prompts"
               :on-click #(do (edit-prompt! 0) (reset! prompts-open true))}]]))

(defn composer-frame
  "The message box: CUI's chat composer, drawn immediate-mode every frame,
  since its text, attachment removal and send flags are one-frame results."
  [u]
  (let [{:keys [sid busy]} (get @chats @current)
        removed (atom -1)
        ev (ui/chat-composer u "composer" draft {:placeholder "Ask Claude" :busy (boolean busy)
                                                 :attachments (mapv #(.getName (io/file %)) @attached)
                                                 :removed removed})]
    (when (and (pos? (bit-and ev raw/CUI_COMPOSER_SEND)) sid (not busy))
      (send-prompt! @current @draft @attached) (reset! draft "") (reset! attached []))
    (when (pos? (bit-and ev raw/CUI_COMPOSER_ATTACH))
      (let [out (atom "")]
        (when (ui/app-file-dialog (ui/current-app) {:kind :open :title "Attach image" :filter-name "Images"
                                                    :filter (str/join ";" (keys image-types))} out)
          (attach! [@out]))))
    (when (and (pos? (bit-and ev raw/CUI_COMPOSER_REMOVE)) (>= @removed 0))
      (swap! attached #(into (subvec % 0 @removed) (subvec % (inc @removed)))))
    (when (pos? (bit-and ev raw/CUI_COMPOSER_STOP)) (cancel!))))

(defn header []
  (let [n? @narrow
        {:keys [sid usage]} (get @chats @current)]
    [:row {:width :fill :pad-x (if n? 12 20) :pad-y 12 :gap (if n? 6 10) :align :center}
     (when n? [:icon-button {:icon :menu :tooltip "Chats" :on-click #(reset! drawer true)}])
     (when (seq @models)
       ;; a searchable select, since CUI's model-picker menu does not scroll
       [:select {:id "model" :items (mapv :name @models) :index @model :searchable true :width 220
                 :on-change #(do (reset! model %) (when sid (set-model! sid)))}])
     [:spacer]
     (when (:size usage) [:context-meter {:id "ctx" :used (:used usage) :limit (:size usage)}])]))

(defn conversation []
  (let [n? @narrow
        st @status
        cur @current
        {:keys [sid turns busy parent]} (get @chats cur)]
    [:column {:width :fill :height :fill}
     [header]
     [:separator]
     [:scroll {:id "chat" :width :fill :height :fill :fade-edges true}
      [:row {:width :fill :justify :center :pad-x (if n? 12 24) :pad-y (if n? 16 24)}
       [:column {:width :fill :max-width 760 :gap 14}
        (cond
          st [:agent-status {:text st :status (if (str/starts-with? st "Starting") :running :failed)}]
          (and (nil? sid) (empty? turns)) [:agent-status {:text "Starting a session" :status :running}]
          (empty? turns) [empty-state])
        (when parent
          [:row {:gap 6 :align :center}
           [:icon {:icon :git-branch :size 14 :color :text-muted}]
           [:text {:text "Subchat of" :size 13 :color :text-muted}]
           [:link {:text (:title (get @chats parent)) :on-click #(reset! current parent)}]])
        (for [[k t] (map-indexed vector turns)]
          (if (= :user (:role t))
            [:column {:key [cur k] :width :fill :gap 8}
             (when (seq (:images t)) [user-images (:images t)])
             (when-not (str/blank? (:text t)) [:chat-message {:text (:text t) :mine true :markdown true}])]
            [:column {:key [cur k] :width :fill}
             [agent-turn cur k t (:text (get turns (dec k))) busy]]))]]]
     [:row {:width :fill :justify :center :pad-x (if n? 12 24) :pad-bottom (if n? 12 20)}
      [:column {:width :fill :max-width 760 :gap 8}
       [prompt-bar]
       [:cui/immediate {:render composer-frame}]
       [:text {:text "Claude can read and change files in the working directory."
               :size 12 :color :text-muted :align :center :width :fill}]]]]))

(def plan-status {"pending" :pending "in_progress" :running "completed" :done})

(defn side-panel []
  (let [{:keys [plan usage turns]} (get @chats @current)
        tools (for [t turns, it (:items t) :when (= :tool (:type it))] it)
        err @save-error]
    [:column {:width 330 :height :fill :bg :surface :pad 20 :gap 16}
     [:text {:text "Workspace" :font :medium :size 15}]
     [:label {:label "Folder"}]
     [:row {:gap 10 :align :center :width :fill :pad 8 :radius 12 :bg :surface-muted}
      [:icon {:icon :folder :size 16 :color :text-secondary}]
      [:text {:text @cwd :size 13 :width :fill :nowrap true}]]
     (if (seq plan)
       [:agent-plan {:id "plan" :title "Plan"
                     :steps (mapv (fn [e] {:title (:content e) :status (plan-status (:status e) :pending)}) plan)}]
       [:text {:text "Claude shows its plan here when a task has several steps." :size 13 :color :text-muted}])
     [:label {:label "Saved chats"}]
     [:row {:gap 10 :align :center :width :fill :pad 8 :radius 12 :bg :surface-muted}
      [:icon {:icon (if err :circle-alert :file-text) :size 16 :color :text-secondary}]
      [:text {:text (or err chats-dir) :size 13 :width :fill :nowrap true}]]
     [:label {:label "Session"}]
     [:text {:text (str (count tools) (if (= 1 (count tools)) " tool call" " tool calls"))
             :size 13 :color :text-secondary}]
     (when-let [c (:cost usage)]
       [:text {:text (format "Cost %.2f %s" (double (:amount c)) (:currency c)) :size 13 :color :text-secondary}])]))

(defn permission-dialog []
  (when-let [{:keys [title options]} (first @perms)]
    [:dialog {:title "Claude wants to run a tool" :open @perm-open :description title
              :persistent true :no-close-button true}
     [:dialog-footer
      (for [{:keys [optionId name kind]} options]
        [:button {:key optionId :label name :variant (if (str/starts-with? kind "allow") :primary :outline)
                  :on-click #(answer-permission! {:outcome "selected" :optionId optionId})}])]]))

(defn pick-block! [j]
  (let [{:keys [blocks picked]} @branch
        p (if (picked j) (disj picked j) (conj picked j))]
    (swap! branch assoc :picked p)
    (reset! branch-quote (str/join "\n\n" (map blocks (sort p))))))

(defn branch-blocks
  "The reply's parts as cards to pick. A card's click is the return of
  cui_card_begin, which glimmer-cui's :card drops, so this is an island."
  [blocks picked]
  [:cui/immediate
   {:render (fn [u]
              (ui/scroll u {:id "blocks" :width :fill :height 260 :gap 8}
                (doseq [[j b] (map-indexed vector blocks)]
                  (ui/push-id-int u j)
                  (let [clicked (ui/card-begin u {:hoverable true :selected (contains? picked j) :pad 10 :width :fill})]
                    (ui/markdown u "b" b {:width :fill :size 13.5 :compact true})
                    (ui/card-end u)
                    (when clicked (pick-block! j)))
                  (ui/pop-id u))))}])

(defn branch-dialog []
  ;; CUI text has no mouse selection, so you pick whole parts and trim the quote
  (when-let [{:keys [chat turn blocks picked]} @branch]
    (let [q @branch-quote
          ask @branch-ask
          ready (not (str/blank? (quoted q ask)))
          go #(do (reset! branch-open false) (branch! chat turn @branch-quote @branch-ask))]
      [:dialog {:title "Start a subchat" :open @branch-open :on-close #(reset! branch-open false)
                :description "Pick the parts of the reply to ask about. This chat stays as it is." :width 640}
       [branch-blocks blocks picked]
       [:textarea {:id "quote" :value q :label "Quote" :height 90 :width :fill
                   :placeholder "Pick parts above, then cut this down to what matters"
                   :on-change #(reset! branch-quote %)}]
       [:input {:value ask :label "Your message" :placeholder "What do you want to know about it?"
                :autofocus true :width :fill
                :on-change #(reset! branch-ask %)
                :on-submit #(when ready (go))}]
       [:dialog-footer
        [:button {:label "Cancel" :variant :ghost :on-click #(reset! branch-open false)}]
        [:button {:label "Start subchat" :variant :primary :icon :git-branch :disabled (not ready) :on-click go}]]])))

(defn prompt-editor [i]
  (let [p (get @prompts i)]
    [:column {:width :fill :gap 10}
     [:input {:value (:title p) :label "Name" :width :fill :on-change #(update-prompt! i :title %)}]
     [:textarea {:id "ptext" :value (:text p) :label "Prompt" :height 200 :width :fill
                 :on-change #(update-prompt! i :text %)}]
     [:button {:label "Delete" :variant :danger :size :sm :icon :trash-2
               :on-click (fn []
                           (swap! prompts #(into (subvec % 0 i) (subvec % (inc i))))
                           (save-prompts!)
                           (edit-prompt! (max 0 (dec i))))}]]))

(defn prompts-dialog []
  (let [ps @prompts
        i @prompt-sel]
    [:dialog {:title "Saved prompts" :open @prompts-open :on-close #(reset! prompts-open false)
              :description "Click a prompt above the message box to put it in your message." :width 720}
     [:row {:width :fill :gap 16 :align :stretch}
      [:scroll {:id "prompt-list" :width 200 :height 300 :gap 4}
       (for [[j pr] (map-indexed vector ps)]
         [:nav-item {:key j :label (if (str/blank? (:title pr)) "Untitled" (:title pr))
                     :selected (= i j) :group "prompts" :on-click #(edit-prompt! j)}])]
      (if (< i (count ps))
        ;; keyed by the prompt, so switching prompts gives fresh fields
        ^{:key i} [prompt-editor i]
        [:text {:text "No saved prompts yet." :color :text-muted}])]
     [:dialog-footer
      [:button {:label "New prompt" :variant :outline :icon :plus
                :on-click #(do (swap! prompts conj {:title "New prompt" :text ""})
                               (save-prompts!)
                               (edit-prompt! (dec (count @prompts))))}]
      [:button {:label "Done" :variant :primary :on-click #(reset! prompts-open false)}]]]))

(defn window-frame
  "Runs every frame: tracks the window width and takes dropped files."
  [u]
  (let [vw (:x (ui/viewport-size u))
        n? (< vw 760)
        w? (>= vw 1180)]
    (when (not= n? @narrow) (reset! narrow n?) (when-not n? (reset! drawer false)))
    (when (not= w? @wide) (reset! wide w?))
    (attach! (map #(ui/dropped-file u %) (range (ui/dropped-file-count u))))))

(defn app []
  [:column {:width :fill :height :fill}
   [:cui/immediate {:render window-frame}]
   [:row {:width :fill :height :fill :align :stretch :bg :background}
    (when-not @narrow (list [sidebar false] [:separator]))
    [conversation]
    (when @wide (list [:separator] [side-panel]))]
   [:sheet {:title "Muse" :open @drawer :side :left :size 300 :on-change #(reset! drawer %)}
    [sidebar true]]
   [branch-dialog]
   [prompts-dialog]
   [permission-dialog]])

(defn tick!
  "Moves the clock once a second while a chat is busy, for the timers, until
  running is reset to false. Returns the thread's future."
  [running]
  (future (while @running
            (Thread/sleep 1000)
            (when (some :busy @chats) (reset! clock (now))))))

(defn -main [& [dir]]
  (when dir (reset! cwd dir))
  (reset! prompts (load-prompts))
  (start!)
  ;; every thread must end once the window closes, or the process (and the
  ;; closed window with it) stays alive after Cmd-Q
  (let [running (atom true)
        ticker (tick! running)]
    (try
      (g/run app :title "Muse" :width 1440 :height 900 :theme :dark :accent :neutral)
      (finally
        (reset! running false)
        ;; ends the agent; its reader threads finish when its pipes close,
        ;; after any chat save they are in the middle of
        (acp/close! @conn)
        @ticker
        ;; futures run on a non-daemon pool that keeps the process alive even
        ;; when idle; this lets running ones finish, then releases it
        (shutdown-agents)))))
