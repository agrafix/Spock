// Node 22 + Playwright 1.61.1. Arguments: built server executable, public assets.
const assert = require('node:assert/strict');
const { spawn } = require('node:child_process');
const net = require('node:net');
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');

async function main() {
  const [binary, assets] = process.argv.slice(2);
  assert(binary && assets, 'Pass the built server executable and public asset directory');
  const listener = net.createServer();
  await new Promise(resolve => listener.listen(0, '127.0.0.1', resolve));
  const port = listener.address().port;
  await new Promise(resolve => listener.close(resolve));
  const origin = `http://127.0.0.1:${port}`;
  const server = spawn(binary, ['--local-http', String(port), assets], {stdio: ['ignore', 'ignore', 'pipe']});
  let browser;
  try {
    await new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error('Server did not start')), 15000);
      let output = '';
      server.stderr.on('data', chunk => {
        output += chunk;
        if (output.includes('Listening on')) { clearTimeout(timer); resolve(); }
      });
      server.once('error', error => { clearTimeout(timer); reject(error); });
      server.once('exit', code => { clearTimeout(timer); reject(new Error(`Server exited: ${code}`)); });
    });
    for (let i = 0; i < 100; i++) {
      try { if ((await fetch(origin)).ok) break; } catch { /* binding */ }
      await new Promise(resolve => setTimeout(resolve, 20));
    }
    browser = await chromium.launch({executablePath: process.env.CHROMIUM_EXECUTABLE || undefined});
    const context = await browser.newContext();
    const page = await context.newPage();
    const errors = [], requests = [];
    page.on('pageerror', err => errors.push(err.message));
    page.on('request', req => { if (req.url().includes('/api/')) requests.push({method: req.method(), url: req.url(), headers: req.headers()}); });
    await page.goto(origin);
    await page.waitForFunction(() => document.documentElement.dataset.ready === 'true');
    const noteIs = text => page.waitForFunction(expected => document.querySelector('#note').textContent === expected && !document.querySelector('#create').disabled, text);
    const errorIs = code => page.waitForFunction(expected => document.querySelector('#status').dataset.error === expected, code);
    await noteIs('No note yet');
    const cookie = (await context.cookies()).find(c => c.name === 'spockcookie');
    assert(cookie && cookie.httpOnly && cookie.sameSite === 'Lax');
    assert.equal(await page.evaluate(async () => (await fetch('/api/note', {method: 'POST', body: '"forged"', headers: {'Content-Type': 'application/json'}})).status), 403);
    await page.fill('#input', 'λ😀 <strong>literal text</strong>');
    await page.click('#create');
    await noteIs('λ😀 <strong>literal text</strong>');
    assert.equal(await page.locator('#note strong').count(), 0);
    await page.fill('#input', 'Hello');
    await page.click('#replace');
    await noteIs('Hello');
    await page.fill('#input', ' world');
    await page.click('#append');
    await noteIs('Hello world');
    await page.reload();
    await page.waitForFunction(() => document.documentElement.dataset.ready === 'true');
    await noteIs('Hello world');
    await page.click('details:has(#echo) summary');
    await page.click('#echo');
    await page.waitForFunction(() => document.querySelector('#echo-output').textContent.length > 0);
    assert.deepEqual(JSON.parse(await page.locator('#echo-output').textContent()), {
      echoName: 'report/a.b λ😀', echoSearch: 'a+b&λ', echoOffset: 2,
      echoTags: ['first', 'two words'], echoCaller: 'browser', echoOptional: 'optional'
    });
    await page.click('#delete');
    await noteIs('No note yet');
    for (const method of ['GET', 'POST', 'PUT', 'PATCH', 'DELETE']) assert(requests.some(r => r.method === method));
    assert(requests.some(r => r.method === 'PATCH' && r.headers['x-csrf-token']));

    async function mock(handler, expected) {
      await page.route('**/api/note', handler);
      await page.click('#load');
      await errorIs(expected);
      await page.unroute('**/api/note', handler);
    }
    await mock(route => route.fulfill({status: 200, contentType: 'application/json', body: 'not-json'}), 'DecodeFailure');
    await mock(route => route.fulfill({status: 503, body: 'private error body must stay hidden'}), 'HttpError 503');
    assert(!(await page.locator('body').textContent()).includes('private error body must stay hidden'));
    await mock(route => route.fulfill({status: 200, contentType: 'application/json', body: JSON.stringify('a'.repeat(5000))}), 'ResponseTooLarge');
    await mock(route => route.abort('failed'), 'NetworkFailure');
    await mock(async route => {
      await new Promise(resolve => setTimeout(resolve, 2500));
      // The timeout deliberately aborts this request before its response.
      await route.abort('failed').catch(() => {});
    }, 'RequestTimedOut');
    await page.click('#load');
    await errorIs('');
    await noteIs('No note yet');
    await require('./routing.cjs')(page, origin);
    assert.deepEqual(errors, []);
    console.log('Compiled Haskell browser: CRUD, Unicode, typed parameters, CSRF, sessions, error decoding, size limit, network failure and timeout passed');
  } finally {
    if (browser) await browser.close();
    if (server.exitCode === null && server.signalCode === null) {
      const exited = new Promise(resolve => server.once('exit', resolve));
      server.kill('SIGTERM');
      await exited;
    }
  }
}
main().catch(error => { console.error(error); process.exitCode = 1; });
