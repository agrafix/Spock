var h$spock_history_active = null;

function h$spock_history_contains(state, url) {
  return url.origin === location.origin && !url.username && !url.password &&
    (state.scope === '/' || url.pathname === state.scope || url.pathname.startsWith(state.scope + '/'));
}

function h$spock_history_mount(scope, callback) {
  if (h$spock_history_active) return {error: 4};
  var state = {error: 0, scope: scope.replace(/\/+$/, '') || '/', callback: callback,
    active: true, last: location.href};
  state.emit = function (force) {
    if (!state.active || (!force && state.last === location.href)) return;
    state.last = location.href;
    if (h$spock_history_contains(state, new URL(location.href))) {
      state.callback({error: 0, url: location.pathname + location.search + location.hash});
    }
  };
  state.pop = function () { state.emit(false); };
  state.click = function (event) {
    if (event.defaultPrevented || !event.cancelable || event.button !== 0 ||
        event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
    var element = event.target instanceof Element ? event.target : event.target.parentElement;
    var anchor = element && element.closest('a[href]');
    if (!anchor || anchor.hasAttribute('download') || anchor.hasAttribute('data-no-router') ||
        anchor.relList.contains('external')) return;
    var target = anchor.getAttribute('target');
    if (target === null) {
      var base = document.querySelector('base[target]');
      target = base ? base.getAttribute('target') : '';
    }
    if (target && target.toLowerCase() !== '_self') return;
    var url;
    try { url = new URL(anchor.href, location.href); decodeURIComponent(url.pathname); }
    catch (_) { return; }
    if (!h$spock_history_contains(state, url)) return;
    if (url.pathname === location.pathname && url.search === location.search && url.hash) return;
    event.preventDefault();
    var result = h$spock_history_navigate(state, false, url.pathname + url.search + url.hash);
    if (result) state.callback({error: result, url: ''});
  };
  window.addEventListener('popstate', state.pop);
  window.addEventListener('hashchange', state.pop);
  document.addEventListener('click', state.click);
  h$spock_history_active = state;
  return state;
}

function h$spock_history_initial(state) {
  return {present: h$spock_history_contains(state, new URL(location.href)) ? 1 : 0,
    error: 0, url: location.pathname + location.search + location.hash};
}

function h$spock_history_navigate(state, replace, value) {
  if (!state.active) return 5;
  var url;
  try { url = new URL(value, location.href); } catch (_) { return 1; }
  if (!h$spock_history_contains(state, url)) return 2;
  try {
    if (replace) history.replaceState(history.state, '', url.href);
    else if (url.href !== location.href) history.pushState(history.state, '', url.href);
  } catch (_) { return 3; }
  state.emit(true);
  return 0;
}

function h$spock_history_unmount(state) {
  if (!state.active) return;
  state.active = false;
  window.removeEventListener('popstate', state.pop);
  window.removeEventListener('hashchange', state.pop);
  document.removeEventListener('click', state.click);
  state.callback = null;
  if (h$spock_history_active === state) h$spock_history_active = null;
}
