#!/usr/bin/env node
/**
 * Minimal loopback-only compatibility proxy for Pi + Ollama tool calling.
 *
 * Qwen3-Coder's Ollama streaming tool parser can omit the terminal
 * `finish_reason`, which makes strict OpenAI-stream consumers (including Pi)
 * abandon an otherwise valid agent turn.  This proxy asks Ollama for the
 * authoritative non-streaming OpenAI response and emits the equivalent small
 * SSE transcript.  It is deliberately not an internet-facing proxy.
 */
import http from "node:http";

const host = process.env.HOST ?? "127.0.0.1";
const port = Number(process.env.PORT ?? "11435");
const upstream = process.env.OLLAMA_OPENAI_URL ?? "http://127.0.0.1:11434/v1/chat/completions";

const sendJson = (res, status, value) => {
  res.writeHead(status, { "content-type": "application/json" });
  res.end(JSON.stringify(value));
};

const sse = (res, value) => res.write(`data: ${JSON.stringify(value)}\n\n`);

const chunk = (completion, delta, finishReason = null) => ({
  id: completion.id,
  object: "chat.completion.chunk",
  created: completion.created,
  model: completion.model,
  system_fingerprint: completion.system_fingerprint,
  choices: [{ index: 0, delta, finish_reason: finishReason }],
});

const server = http.createServer(async (req, res) => {
  if (req.method !== "POST" || req.url !== "/v1/chat/completions") {
    sendJson(res, 404, { error: { message: "only POST /v1/chat/completions is supported" } });
    return;
  }
  let raw = "";
  for await (const part of req) raw += part;
  let request;
  try { request = JSON.parse(raw); } catch {
    sendJson(res, 400, { error: { message: "invalid JSON" } });
    return;
  }
  const wantsStream = request.stream === true;
  try {
    const upstreamResponse = await fetch(upstream, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ ...request, stream: false }),
    });
    const payload = await upstreamResponse.json();
    if (!upstreamResponse.ok) {
      sendJson(res, upstreamResponse.status, payload);
      return;
    }
    if (!wantsStream) {
      sendJson(res, 200, payload);
      return;
    }
    const choice = payload.choices?.[0];
    const message = choice?.message ?? { role: "assistant", content: "" };
    res.writeHead(200, {
      "content-type": "text/event-stream",
      "cache-control": "no-cache",
      connection: "keep-alive",
    });
    sse(res, chunk(payload, { role: message.role ?? "assistant" }));
    if (message.content) sse(res, chunk(payload, { content: message.content }));
    for (const [index, call] of (message.tool_calls ?? []).entries()) {
      sse(res, chunk(payload, {
        tool_calls: [{
          index: call.index ?? index,
          id: call.id,
          type: call.type ?? "function",
          function: call.function,
        }],
      }));
    }
    sse(res, chunk(payload, {}, choice?.finish_reason ?? "stop"));
    res.end("data: [DONE]\n\n");
  } catch (error) {
    sendJson(res, 502, { error: { message: String(error) } });
  }
});

server.listen(port, host, () => console.log(`ollama non-stream proxy listening on http://${host}:${port}`));
