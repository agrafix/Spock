// Run with Node 22, Playwright 1.61.1, and the built example executable.
const assert = require('node:assert/strict');
const { spawn } = require('node:child_process');
const { randomBytes } = require('node:crypto');
const net = require('node:net');
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');

async function freePort() {
  const server = net.createServer();
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const port = server.address().port;
  await new Promise(resolve => server.close(resolve));
  return port;
}

async function main() {
  const binary = process.argv[2];
  assert(binary, 'Pass the path from cabal list-bin spock-cookie-example');
  const port = await freePort();
  const origin = `http://127.0.0.1:${port}`;
  // Ephemeral test key stays in memory and the child environment, never a file.
  const key = randomBytes(32).toString('base64');
  let child;
  let browser;
  async function stop() {
    if (child && child.exitCode === null && child.signalCode === null) {
      const exited = new Promise(resolve => child.once('exit', resolve));
      child.kill('SIGTERM');
      await exited;
    }
    child = undefined;
  }
  async function start(secret) {
    child = spawn(binary, ['--local-http', String(port)], {
      env: { ...process.env, SPOCK_COOKIE_KEY: secret }, stdio: ['ignore', 'ignore', 'pipe']
    });
    await new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error('Example did not start')), 15000);
      let output = '';
      child.stderr.on('data', chunk => {
        output += chunk;
        if (output.includes('Listening on')) { clearTimeout(timer); resolve(); }
      });
      child.once('error', error => { clearTimeout(timer); reject(error); });
      child.once('exit', code => { clearTimeout(timer); reject(new Error(`Example exited: ${code}`)); });
    });
    // The readiness message precedes bind; poll the harmless root route.
    for (let attempt = 0; attempt < 100; attempt++) {
      try { if ((await fetch(origin)).ok) return; } catch { /* binding */ }
      await new Promise(resolve => setTimeout(resolve, 20));
    }
    throw new Error('Example did not bind');
  }
  try {
    await start(key);
    browser = await chromium.launch({ executablePath: process.env.CHROMIUM_EXECUTABLE || undefined });
    const context = await browser.newContext();
    const page = await context.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    const valueIs = value => page.waitForFunction(expected =>
      document.querySelector('#value').textContent === expected && !document.querySelector('#increment').disabled, value);
    await page.goto(origin);
    await valueIs('0');
    let cookie = (await context.cookies()).find(c => c.name === 'spockcookie');
    assert(cookie && cookie.httpOnly && cookie.sameSite === 'Lax' && cookie.path === '/');
    assert(cookie.value.startsWith('v1.current.'));
    const forbidden = await page.evaluate(async () => (await fetch('/increment', { method: 'POST' })).status);
    assert.equal(forbidden, 403);
    await page.click('#increment');
    await valueIs('1');
    await page.reload();
    await valueIs('1');
    await stop();
    await start(key);
    await page.reload();
    await valueIs('1');
    await page.click('#increment');
    await valueIs('2');
    await page.click('#reset');
    await valueIs('0');
    await page.click('#increment');
    await valueIs('1');
    // A new deployment key rejects the previous cookie instead of accepting it.
    await stop();
    await start(randomBytes(32).toString('base64'));
    await page.reload();
    await valueIs('0');
    assert.deepEqual(errors, []);
    console.log('Browser cookie flags, CSRF, updates, refresh, restart, reset and key removal passed');
  } finally {
    if (browser) await browser.close();
    await stop();
  }
}
main().catch(error => { console.error(error); process.exitCode = 1; });
