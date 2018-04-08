#!/bin/bash
set -o nounset -o pipefail -o errexit

if [[ "${TRAVIS_PULL_REQUEST-false}" != "false" || \
        "${TRAVIS_BRANCH-}" != "master" ]]; then
    echo "Not on master so not deploying..." >&2
    exit 0
fi

DEPLOY_REPO="$(git config remote.origin.url)"
DEPLOY_SSH_REPO=${DEPLOY_REPO/https:\/\/github.com\//git@github.com:}

DEPLOY_BRANCH="gh-pages"

DEPLOY_AUTHOR_EMAIL="travis.zfoh.ch@jaspervdj.be"
DEPLOY_AUTHOR_NAME="Travis CI"

git config user.name "$DEPLOY_AUTHOR_NAME"
git config user.email "$DEPLOY_AUTHOR_EMAIL"

DEPLOY_KEY="deploy/travis.zfoh.ch.key"
DEPLOY_KEY_ENC="$DEPLOY_KEY.enc"

openssl aes-256-cbc \
    -K "${encrypted_ccab536b289e_key-}" -iv "${encrypted_ccab536b289e_iv-}" \
    -in "$DEPLOY_KEY_ENC" -out "$DEPLOY_KEY" -d

chmod 600 "$DEPLOY_KEY"
eval "$(ssh-agent -s)"
ssh-add "$DEPLOY_KEY"

DEPLOY_DIR="$(mktemp -d)"
git clone --single-branch --branch "$DEPLOY_BRANCH" \
    "$DEPLOY_SSH_REPO" "$DEPLOY_DIR"

rsync -v -r --delete \
    --exclude '.git/' --exclude 'CNAME' \
    "_site/" "$DEPLOY_DIR/"

cd "$DEPLOY_DIR"

git add -A .

if git commit -m 'CI commit'; then
    git push origin "$DEPLOY_BRANCH"
else
    echo "No commit was made, skipping deploy..." >&2
fi
