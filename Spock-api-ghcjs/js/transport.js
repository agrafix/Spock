// GHC's interruptible FFI supplies the continuation as the final argument.
function h$spock_fetch(configuration, continuation) {
  var request = JSON.parse(configuration);
  var controller = new AbortController();
  var finished = false;
  function finish(result) {
    if (finished) return;
    finished = true;
    clearTimeout(timer);
    continuation(result);
  }
  var timer = setTimeout(function () {
    controller.abort();
    finish({error: 'timeout'});
  }, request.timeout);
  (async function () {
    try {
      var response = await fetch(request.url, {
        method: request.method, headers: request.headers,
        body: request.body === null ? undefined : request.body,
        credentials: request.credentials, redirect: 'error',
        cache: 'no-store', signal: controller.signal
      });
      // HTTP errors do not require reading a potentially large error body.
      if (!response.ok) {
        controller.abort();
        finish({error: '', status: response.status, body: ''});
        return;
      }
      var reader = response.body ? response.body.getReader() : null;
      var chunks = [], length = 0;
      if (reader) {
        while (true) {
          var chunk = await reader.read();
          if (chunk.done) break;
          length += chunk.value.byteLength;
          if (length > request.maxBytes) {
            controller.abort();
            finish({error: 'large'});
            return;
          }
          chunks.push(chunk.value);
        }
      }
      var bytes = new Uint8Array(length), offset = 0;
      for (var part of chunks) { bytes.set(part, offset); offset += part.byteLength; }
      var text;
      try { text = new TextDecoder('utf-8', {fatal: true}).decode(bytes); }
      catch (_) { finish({error: 'decode'}); return; }
      finish({error: '', status: response.status, body: text});
    } catch (_) {
      finish({error: 'network'});
    }
  })();
}
