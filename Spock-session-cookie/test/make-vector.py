#!/usr/bin/env python3
"""Regenerate the public interoperability fixture with PyNaCl/libsodium.

This fixed key and nonce are TEST DATA, never production credentials.
Run with PyNaCl 1.6.2; the Haskell tests need only the checked-in JSON fixture.
"""
import base64
import json
from pathlib import Path
from nacl.bindings import crypto_aead_xchacha20poly1305_ietf_encrypt

payload = {"id": "independent-id", "csrf": "independent-csrf",
           "expires": "2030-01-01T00:01:00Z", "data": "private-user"}
aad = ["Spock.session.cookie", "v1", "test-app", "spockcookie", "old"]
compact = lambda value: json.dumps(value, separators=(",", ":")).encode()
nonce = bytes(range(24))
cipher = crypto_aead_xchacha20poly1305_ietf_encrypt(compact(payload), compact(aad), nonce, bytes(range(32)))
token = "v1.old." + base64.urlsafe_b64encode(nonce + cipher).decode().rstrip("=")
Path(__file__).with_name("libsodium-vector.json").write_text(json.dumps({"cookie": token, "session": payload}, indent=2) + "\n")
