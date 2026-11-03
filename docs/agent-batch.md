# Asynchronous agent batches

`Agent_batch` adds a separate, opt-in path for independent Claude Messages
requests and a later grouped reduce. It does not change Cabal's synchronous
agent backends.

## Lifecycle

1. The host assigns stable opaque `custom_id` and `group` values to map
   requests. A group can be any caller-defined reduce unit, such as a repository
   or document. Keep source text, titles and secrets out of these identifiers.
2. The host supplies an authenticated HTTPS `transport` whose relative paths
   go only to `api.anthropic.com`. `Agent_batch.submit` validates identifiers,
   limits and the structural Messages envelope before sending one provider
   batch. Anthropic validates the full Messages schema. Persist
   `ticket_to_json` durably on return.
3. A host timer calls `collect` until it returns `Complete`. A host event or
   webhook may wake the same code early. `collect` always verifies the provider
   state first, then fetches and correlates the JSONL results. The host should
   persist the complete result before acknowledging its own event.
4. `plan_reducers` takes only a complete result. Under `Require_all`, a group
   with any failed, incomplete, canceled or expired map is listed in `blocked`; other groups
   may proceed. Under `Accept_partial`, the reducer builder receives every
   outcome, including failures, and decides how to represent them. The returned
   `ready` requests can be submitted as a second batch. Reducer IDs must be
   stable so a host can deduplicate submissions and downstream effects.
5. The host persists reduce state and applies external effects only once. It
   must recheck the target state before applying those effects; Cabal does not
   own that state or the scheduler.

There is no `callback_url` field in the documented Claude Message Batches
creation API. The documented completion mechanism is polling `GET
/v1/messages/batches/{id}` and then retrieving JSONL results. Cabal does not
launch a public server. A host that receives provider or internal webhooks owns
authentication, replay protection and durable event deduplication; a polling
timer remains necessary for missed notifications. A notification is only a
wake-up signal, never evidence that the batch completed.

## Limits and cost

The provider currently limits one batch to 100,000 requests or 256 MB, and
expires incomplete work at 24 hours. Results remain available for up to 29
days. Cabal checks the submission limits and rejects missing/duplicate result
IDs. It returns measured input, output and cache token counts for successful
and incomplete provider responses. It does not invent USD cost from mutable model pricing; the host may
price the measured usage or use the provider's cost report. Failed requests
remain explicit outcomes. The transport streams JSONL result lines to Cabal;
the host should cap each line and total bytes so a provider response cannot
exhaust memory.

Because batch creation has no documented idempotency key, a crash after the
provider accepts a submission but before the host persists its ticket can
leave an orphaned paid batch. A retry can create another batch. Hosts should
persist their request manifest before submission, reconcile provider batches
after an uncertain response, and never apply results twice. Cabal tickets
contain only batch ID and ID/group mapping; prompts, credentials, provider
responses and result bodies are not serialized into tickets.

Batch request/response data is held by Anthropic for up to 29 days, and this
feature is [not eligible for zero data retention](https://platform.claude.com/docs/en/build-with-claude/batch-processing).

## References

- [Claude batch guide](https://platform.claude.com/docs/en/build-with-claude/batch-processing)
- [Claude batch API reference](https://platform.claude.com/docs/en/api/messages/batches)
