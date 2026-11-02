#!/usr/bin/env python3
"""asc.py -- the once-only App Store Connect setup, through its API.

    tools/asc.py setup    register the App ID, make the App Store profile and
                          download it, then say whether the app record exists
    tools/asc.py check    only say what exists

What Xcode would do behind the scenes, for an app with no Xcode project.  The
one thing the API cannot do is create the app record in App Store Connect;
that is a form in a browser, and `check` says when it is still missing.

Needs only the standard library and openssl: the API's token is an ES256 JWT,
signed here with the .p8 key in ~/.appstoreconnect/private_keys.  Reads
ASC_KEY_ID, ASC_ISSUER_ID and IOS_DISTRIBUTION_PROFILE from the environment
(source local.env first: tools/testflight.sh does).
"""

import base64, json, os, subprocess, sys, time, urllib.error, urllib.request

BUNDLE_ID = "com.burnsidemk.behemoth"
APP_NAME = "Behemoth"
PROFILE_NAME = "Behemoth App Store"
CERTIFICATE = "Apple Distribution: Matthew Kennedy"
API = "https://api.appstoreconnect.apple.com/v1"


def b64url(data):
    return base64.urlsafe_b64encode(data).rstrip(b"=")


def der_to_raw(der):
    """An ECDSA signature as openssl writes it (DER) to the r||s JWT wants."""
    assert der[0] == 0x30
    i = 2 if der[1] < 0x80 else 2 + (der[1] & 0x7F)
    parts = []
    for _ in range(2):
        assert der[i] == 0x02
        n = der[i + 1]
        parts.append(der[i + 2:i + 2 + n].lstrip(b"\0").rjust(32, b"\0"))
        i += 2 + n
    return b"".join(parts)


def token():
    key_id, issuer = os.environ["ASC_KEY_ID"], os.environ["ASC_ISSUER_ID"]
    key = os.path.expanduser(f"~/.appstoreconnect/private_keys/AuthKey_{key_id}.p8")
    now = int(time.time())
    signing_input = b".".join([
        b64url(json.dumps({"alg": "ES256", "kid": key_id, "typ": "JWT"}).encode()),
        b64url(json.dumps({"iss": issuer, "iat": now, "exp": now + 1200,
                           "aud": "appstoreconnect-v1"}).encode())])
    der = subprocess.run(["openssl", "dgst", "-sha256", "-sign", key],
                         input=signing_input, capture_output=True, check=True).stdout
    return (signing_input + b"." + b64url(der_to_raw(der))).decode()


TOKEN = None


def call(method, path, body=None):
    global TOKEN
    TOKEN = TOKEN or token()
    request = urllib.request.Request(
        API + path, method=method,
        data=json.dumps(body).encode() if body else None,
        headers={"Authorization": f"Bearer {TOKEN}", "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(request) as response:
            return json.load(response)
    except urllib.error.HTTPError as error:
        sys.exit(f"{method} {path}: {error.code}\n{error.read().decode()}")


def find_bundle_id():
    found = call("GET", f"/bundleIds?filter[identifier]={BUNDLE_ID}")["data"]
    # The filter matches prefixes too.
    return next((b for b in found if b["attributes"]["identifier"] == BUNDLE_ID), None)


def find_profile():
    found = call("GET", f"/profiles?filter[name]={urllib.request.quote(PROFILE_NAME)}")["data"]
    return next((p for p in found if p["attributes"]["profileState"] == "ACTIVE"), None)


def find_app():
    found = call("GET", f"/apps?filter[bundleId]={BUNDLE_ID}")["data"]
    return next((a for a in found if a["attributes"]["bundleId"] == BUNDLE_ID), None)


def keychain_serial():
    pem = subprocess.run(["security", "find-certificate", "-c", CERTIFICATE, "-p"],
                         capture_output=True, check=True).stdout
    out = subprocess.run(["openssl", "x509", "-noout", "-serial"], input=pem,
                         capture_output=True, check=True).stdout.decode()
    return out.strip().split("=")[1].upper().lstrip("0")


def distribution_certificate():
    serial = keychain_serial()
    for kind in ("DISTRIBUTION", "IOS_DISTRIBUTION"):
        for cert in call("GET", f"/certificates?filter[certificateType]={kind}")["data"]:
            if cert["attributes"]["serialNumber"].upper().lstrip("0") == serial:
                return cert
    sys.exit(f"no certificate in the account matches {CERTIFICATE} in the keychain")


def setup():
    bundle = find_bundle_id()
    if bundle:
        print(f"App ID {BUNDLE_ID}: already registered")
    else:
        bundle = call("POST", "/bundleIds", {"data": {"type": "bundleIds", "attributes": {
            "identifier": BUNDLE_ID, "name": APP_NAME, "platform": "IOS"}}})["data"]
        print(f"App ID {BUNDLE_ID}: registered")

    profile = find_profile()
    if profile:
        print(f"profile {PROFILE_NAME!r}: already exists")
    else:
        cert = distribution_certificate()
        profile = call("POST", "/profiles", {"data": {
            "type": "profiles",
            "attributes": {"name": PROFILE_NAME, "profileType": "IOS_APP_STORE"},
            "relationships": {
                "bundleId": {"data": {"type": "bundleIds", "id": bundle["id"]}},
                "certificates": {"data": [{"type": "certificates", "id": cert["id"]}]}}}})["data"]
        print(f"profile {PROFILE_NAME!r}: made")

    path = os.path.expanduser(os.environ["IOS_DISTRIBUTION_PROFILE"])
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as out:
        out.write(base64.b64decode(profile["attributes"]["profileContent"]))
    print(f"profile written to {path}")
    check_app()


def check_app():
    app = find_app()
    if app:
        print(f"app record: {app['attributes']['name']!r}, Apple ID {app['id']}")
    else:
        print("app record: MISSING -- create it at https://appstoreconnect.apple.com/apps"
              f" (New App, iOS, bundle ID {BUNDLE_ID})")
        sys.exit(2)


if __name__ == "__main__":
    command = sys.argv[1] if len(sys.argv) > 1 else "check"
    if command == "setup":
        setup()
    elif command == "check":
        print("App ID:", "registered" if find_bundle_id() else "missing")
        print("profile:", "exists" if find_profile() else "missing")
        check_app()
    else:
        sys.exit(__doc__)
