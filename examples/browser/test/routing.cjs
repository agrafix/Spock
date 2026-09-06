const assert = require('node:assert/strict');

module.exports = async function routing(page, origin) {
  const viewIs = text => page.waitForFunction(expected => document.querySelector('#route-view').textContent === expected, text);
  const statusIs = text => page.waitForFunction(expected => document.querySelector('#router-status').textContent === expected, text);
  const renders = () => page.locator('#route-view').getAttribute('data-renders').then(Number);
  const length = () => page.evaluate(() => history.length);
  await viewIs('Home');
  await page.evaluate(() => { window.navigationMarker = 79; });
  const originalLength = await length();
  await page.click('#about-link span');
  await viewIs('About');
  assert.equal(await length(), originalLength + 1);
  const encoded = '/app/note/a%2Fb%20%CE%BB%F0%9F%98%80';
  assert.equal(await page.locator('#note-link').getAttribute('href'), encoded);
  await page.click('#note-link');
  await viewIs('Note: a/b λ😀');
  assert.equal(new URL(page.url()).pathname, encoded);
  assert.equal(await page.evaluate(() => window.navigationMarker), 79, 'navigation must preserve the document');
  const sameLength = await length(), sameRenders = await renders();
  await page.click('#note-link');
  await page.waitForFunction(expected => Number(document.querySelector('#route-view').dataset.renders) === expected, sameRenders + 1);
  assert.equal(await length(), sameLength, 'the current URL must not create a duplicate history entry');
  await page.goBack();
  await viewIs('About');
  await page.goForward();
  await viewIs('Note: a/b λ😀');

  async function link(href) {
    await page.evaluate(value => {
      const a = document.createElement('a'); a.href = value; document.body.append(a); a.click(); a.remove();
    }, href);
  }
  await link('/app/note/item?x=a%2Bb&tag=1&tag=2#part');
  await viewIs('Note: item');
  assert.equal(new URL(page.url()).search, '?x=a%2Bb&tag=1&tag=2');
  assert.equal(new URL(page.url()).hash, '#part');
  await page.locator('#routing-controls').evaluate(element => { element.open = true; });
  await page.evaluate(() => history.replaceState({owner: 'caller'}, '', location.href));
  const beforeReplace = await length();
  await page.click('#replace-route');
  await viewIs('About');
  assert.equal(await length(), beforeReplace);
  assert.equal(new URL(page.url()).search, '?from=replace');
  assert.equal(new URL(page.url()).hash, '#details');
  assert.deepEqual(await page.evaluate(() => history.state), {owner: 'caller'});

  const beforeHash = await renders();
  await link('#native-anchor');
  await page.waitForFunction(expected => Number(document.querySelector('#route-view').dataset.renders) === expected, beforeHash + 1);
  await page.waitForTimeout(80);
  assert.equal(await renders(), beforeHash + 1, 'popstate/hashchange must not render twice');

  // Observe the router before cancelling the browser's default action. No
  // external page is opened; the test checks which clicks the adapter owns.
  async function intercepted(options) {
    return page.evaluate(options => {
      const anchor = document.createElement('a'); anchor.href = options.href;
      for (const [key, value] of Object.entries(options.attrs || {})) anchor.setAttribute(key, value);
      let base;
      if (options.baseTarget) { base = document.createElement('base'); base.target = options.baseTarget; document.head.append(base); }
      if (options.alreadyPrevented) anchor.addEventListener('click', event => event.preventDefault());
      document.body.append(anchor);
      let result;
      document.addEventListener('click', event => { result = event.defaultPrevented; event.preventDefault(); }, {once: true});
      anchor.dispatchEvent(new MouseEvent('click', {bubbles: true, cancelable: true, button: 0, ...options.event}));
      anchor.remove(); if (base) base.remove();
      return result;
    }, options);
  }
  const untouched = [
    {href: 'https://example.invalid/app/about'}, {href: '//example.invalid/app/about'},
    {href: 'mailto:hello@example.invalid'}, {href: 'javascript:void(0)'},
    {href: '/api/note'}, {href: '/application/about'}, {href: '/app/%GG'},
    {href: origin.replace('://', '://user:password@') + '/app/about'},
    {href: '/app/about', attrs: {download: ''}}, {href: '/app/about', attrs: {target: '_blank'}},
    {href: '/app/about', attrs: {rel: 'external'}}, {href: '/app/about', attrs: {'data-no-router': ''}},
    {href: '/app/about', baseTarget: '_blank'}, {href: '#another-anchor'},
    ...['ctrlKey', 'metaKey', 'shiftKey', 'altKey'].map(key => ({href: '/app/about', event: {[key]: true}})),
    {href: '/app/about', event: {button: 1}}
  ];
  const beforeIgnored = await renders(), ignoredUrl = page.url();
  for (const options of untouched) assert.equal(await intercepted(options), false, JSON.stringify(options));
  assert.equal(await intercepted({href: '/app/about', alreadyPrevented: true}), true);
  await page.waitForTimeout(80);
  assert.equal(page.url(), ignoredUrl);
  assert.equal(await renders(), beforeIgnored);

  // A browser History exception reports an error without changing URL/view.
  await page.evaluate(() => { window.originalPush = history.pushState; history.pushState = () => { throw new DOMException('unavailable', 'SecurityError'); }; });
  await page.click('#home-link');
  await statusIs('HistoryUnavailable');
  assert.equal(page.url(), ignoredUrl);
  assert.equal(await renders(), beforeIgnored);
  await page.evaluate(() => { history.pushState = window.originalPush; delete window.originalPush; });
  await page.click('#home-link');
  await viewIs('Home');

  // The Haskell demo calls unmount twice. Events after cleanup do no work, and
  // repeated remounting produces one dispatch per click with no released callback errors.
  for (let i = 0; i < 3; i++) {
    await page.click('#stop-router');
    await statusIs('Routing stopped');
    const stopped = await renders();
    assert.equal(await intercepted({href: '/app/note/stopped'}), false);
    await page.evaluate(() => { history.pushState(history.state, '', '/app/about'); window.dispatchEvent(new PopStateEvent('popstate')); });
    await page.waitForTimeout(80);
    assert.equal(await renders(), stopped);
    await page.click('#start-router');
    await statusIs('Routing active');
    await viewIs('About');
    assert.equal(await renders(), stopped + 1);
    await page.click('#start-router');
    await statusIs('AlreadyMounted');
    assert.equal(await renders(), stopped + 1);
    await page.click('#note-link');
    await viewIs('Note: a/b λ😀');
    assert.equal(await renders(), stopped + 2);
  }
  await link('/app/does-not-exist');
  await viewIs('Page not found');
  assert.equal(await page.evaluate(() => window.navigationMarker), 79);
  assert.equal((await page.reload()).status(), 200);
  await page.waitForFunction(() => document.documentElement.dataset.ready === 'true');
  await viewIs('Page not found');
  assert.equal((await page.goto(origin + encoded + '?source=deep#section')).status(), 200);
  await page.waitForFunction(() => document.documentElement.dataset.ready === 'true');
  await viewIs('Note: a/b λ😀');
  assert.equal(new URL(page.url()).search, '?source=deep');
  assert.equal(new URL(page.url()).hash, '#section');
  assert.equal((await page.reload()).status(), 200);
  await page.waitForFunction(() => document.documentElement.dataset.ready === 'true');
  await viewIs('Note: a/b λ😀');
  assert.equal(await page.evaluate(async () => (await fetch('/api/unknown')).status), 404);
  console.log('Browser routing: typed links, history, scope, native click behavior, cleanup, remounting and deep-link reloads passed');
};
