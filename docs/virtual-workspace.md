# Host-Supplied Virtual Workspaces

`Cabal.Virtual_workspace` lets a host application provide a bounded, cited
workspace to an agent backend without teaching Cabal about the host domain.

The host owns authorization, resource lookup, policy decisions, and durability.
Cabal validates generic resource shapes, asks the host reader for bounded
windows, renders a manifest plus cited context, and prepares completion prompts
that tell the backend how to use those citations.

## Contract

- The host supplies only resources the current requester is allowed to read.
- Cabal does not fetch resources, evaluate access rules, persist workspace
  content, or log workspace content.
- Reader errors are converted to bounded error codes.
- The system prompt receives generic workspace-use instructions only.
  Workspace window content remains in the user prompt context.
- Citation ids are host-provided and should be stable enough for the host to
  map answers back to source material.

## Minimal Flow

1. Build `resource_descriptor` values for host-authorized resources.
2. Implement `read_window : read_window_request -> (read_window_result, string) result`.
3. Call `collect_workspace` with bounded `read_limits`.
4. Call `prepare_completion` or `Backend_completer.complete_with_workspace`.
5. Apply any host policy gate before displaying or storing backend output.

## Migration Notes For Host Applications

Existing prompt-only integrations can migrate incrementally:

1. Keep current host authorization and resource selection unchanged.
2. Replace ad hoc concatenation with descriptors plus a host reader.
3. Use `collect_workspace` to read all bounded windows needed for the task.
4. Use `prepare_completion` to combine the rendered workspace with the user
   task while preserving the caller's system prompt.
5. Keep long-running orchestration, retries, result storage, and policy gates
   in the host application.

Do not move host-specific access control, storage handles, or policy decisions
into Cabal when migrating.
