import http from 'node:http';
import { spawn } from 'node:child_process';
import { once } from 'node:events';
import { performance } from 'node:perf_hooks';
import { setTimeout as delay } from 'node:timers/promises';

const [binary, countArg = '10000', concurrencyArg = '64', repeatsArg = '3', ...selected] = process.argv.slice(2);
const count = Number(countArg), concurrency = Number(concurrencyArg), repeats = Number(repeatsArg);
if (!binary || ![count, concurrency, repeats].every(n => Number.isSafeInteger(n) && n > 0)) {
  throw new Error('Usage: node run.mjs BINARY [REQUESTS CONCURRENCY REPEATS [MODE...]]');
}
const modes = selected.length ? selected : ['warp', 'core', 'default', 'always', 'on-demand', 'disabled', 'scotty'];
const port = Number(process.env.SPOCK_BENCH_PORT || 18083);
const cookiePolicy = process.env.SPOCK_BENCH_COOKIES || 'discard';
if (!['discard', 'reuse'].includes(cookiePolicy)) throw new Error('SPOCK_BENCH_COOKIES must be discard or reuse');
const paths = [['/echo/hello-world', 'Hello World'], ['/echo/plain/hello', 'hello'], ['/echo/regex/42', '42']];

function request(agent, path, cookie) {
  return new Promise((resolve, reject) => {
    const start = performance.now();
    const req = http.get({ host: '127.0.0.1', port, path, agent,
      headers: cookie ? { Cookie: cookie } : {} }, res => {
      const parts = [];
      res.on('data', part => parts.push(part));
      res.on('error', reject);
      res.on('end', () => resolve({ status: res.statusCode, body: Buffer.concat(parts).toString(),
        headers: res.headers, latency: performance.now() - start }));
    });
    req.setTimeout(5000, () => req.destroy(Object.assign(new Error('Request timed out'), { code: 'TIMEOUT' })));
    req.on('error', reject);
  });
}

async function ready(child, agent) {
  for (let attempt = 0; attempt < 100; attempt++) {
    if (child.exitCode !== null) throw new Error(`Server exited: ${child.exitCode}`);
    try { await request(agent, paths[0][0]); return; } catch { await delay(50); }
  }
  throw new Error('Server did not start');
}

async function verify(agent) {
  for (const [path, expected] of [...paths, ['/echo/plain/caf%C3%A9', 'café'], ['/echo/regex/0042', '0042']]) {
    const result = await request(agent, path);
    if (result.status !== 200 || result.body !== expected || !result.headers['content-type']?.startsWith('text/plain')) {
      throw new Error(`Unexpected response for ${path}: ${JSON.stringify(result)}`);
    }
  }
  for (const path of ['/echo/regex/not-a-number', '/echo/regex/-1', '/missing']) {
    if ((await request(agent, path)).status !== 404) throw new Error(`Expected 404 for ${path}`);
  }
}

console.log('mode,path,repeat,requests,concurrency,rps,p50_ms,p95_ms,p99_ms,errors,timeouts,cookie_policy,cookies_issued');
for (const mode of modes) {
  const child = spawn(binary, [mode, String(port), '+RTS', '-N2', '-RTS'], { stdio: ['ignore', 'ignore', 'inherit'] });
  let spawnError;
  child.on('error', error => { spawnError = error; });
  const agent = new http.Agent({ keepAlive: true, maxSockets: concurrency });
  try {
    await once(child, 'spawn');
    await ready(child, agent);
    if (spawnError) throw spawnError;
    await verify(agent);
    for (const [path, expected] of paths) {
      for (let repeat = 1; repeat <= repeats; repeat++) {
        let next = 0, errors = 0, timeouts = 0, cookiesIssued = 0;
        const latencies = [];
        const start = performance.now();
        await Promise.all(Array.from({ length: concurrency }, async () => {
          // Each worker is an independent client, with its own cookie jar.
          let cookie;
          while (next++ < count) {
            try {
              const result = await request(agent, path, cookie);
              if (result.status !== 200 || result.body !== expected) throw new Error('Response validation failed');
              const issued = result.headers['set-cookie']?.find(value => value.startsWith('spockcookie='));
              if (issued) {
                cookiesIssued++;
                if (cookiePolicy === 'reuse') cookie = issued.split(';', 1)[0];
              }
              latencies.push(result.latency);
            } catch (error) { errors++; if (error.code === 'TIMEOUT') timeouts++; }
          }
        }));
        const elapsed = (performance.now() - start) / 1000;
        latencies.sort((a, b) => a - b);
        const percentile = p => latencies[Math.min(latencies.length - 1, Math.floor(latencies.length * p))] ?? NaN;
        console.log([mode, path, repeat, count, concurrency, ((count - errors) / elapsed).toFixed(2),
          ...[.5, .95, .99].map(p => percentile(p).toFixed(3)), errors, timeouts, cookiePolicy, cookiesIssued].join(','));
        if (errors) throw new Error(`${errors} failed requests in ${mode}`);
        const expectedCookies = mode === 'always' ? (cookiePolicy === 'reuse' ? Math.min(count, concurrency) : count) : 0;
        if (cookiesIssued !== expectedCookies) throw new Error(`Expected ${expectedCookies} cookies, got ${cookiesIssued}`);
      }
    }
  } finally {
    agent.destroy();
    if (child.pid && child.exitCode === null && child.signalCode === null) {
      const exited = once(child, 'exit');
      child.kill('SIGTERM');
      await exited;
    }
  }
}
