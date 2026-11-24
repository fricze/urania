# Urania

Urania (the app calls itself Muse) is a desktop chat app for Claude, written in [Jolt](https://jolt-lang.net) (Clojure on Chez
Scheme) with [glimmer-cui](https://github.com/jolt-lang/glimmer-cui): reagent-style components
and ratoms over [CUI](https://github.com/yappologistic/CUI)'s native widgets. Its layout follows CUI's
[`assistant.c`](https://yappologistic.github.io/CUI/#/app/assistant).

![Muse](docs/images/assistant.png)

It starts [`claude-agent-acp`](https://www.npmjs.com/package/@agentclientprotocol/claude-agent-acp)
with `npx` and talks to it over the [Agent Client Protocol](https://agentclientprotocol.com)
(`src/acp.clj`). It has:

- several chats, with streamed markdown answers, reasoning, tool calls, the agent's plan,
  permission prompts, model choice, context use and cost
- subchats: pick parts of a reply and branch a new chat from them, leaving the original as it is
- image attachments, from the attach button or by dropping files on the window
- saved prompts above the message box, inserted with one click and edited in a dialog
- every chat saved as markdown in `~/Muse` and reopened on the next launch

## Running

You need Jolt 0.8.25 or newer, CMake, a C compiler, Node and a logged-in Claude Code. The
first run fetches glimmer-cui and builds its libcui.

```sh
git clone https://github.com/fricze/urania && cd urania
jolt start        # start Muse in the current directory
jolt start ~/code # or in another working directory
jolt test         # tests, no agent or display needed
```

## How it is built

The UI is glimmer components (`src/assistant.clj`): each re-renders only when a ratom it
reads changes, and the ACP reader thread writes those ratoms directly, since glimmer queues
the re-render onto the window loop and wakes it. A few parts stay immediate-mode islands
(`:cui/immediate`), drawn every frame: the message box, image thumbnails, the reply parts
in the subchat dialog, and the window-width and file-drop checks.
