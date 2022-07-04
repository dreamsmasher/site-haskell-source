#!/bin/sh
#
# Deploy the website to a Github pages repository using CircleCI
#

# Fail on errors and unset variables:
set -o errexit -o nounset

# Configuration:
readonly DEPLOY_DIR="../deploy"
readonly SOURCE_DIR="$PWD"
git config --global user.name "CIBot"
git config --global user.email "$GIT_EMAIL"

# Clone the deployment repository:
git clone git@github.com:dreamsmasher/dreamsmasher.github.io.git "$DEPLOY_DIR"

# Enter the deployment directory and copy the built files over:
cd "$DEPLOY_DIR"
rm -rf *
cp -rv $SOURCE_DIR/_site/* .
echo "nliu.net" > CNAME # VERY IMPORTANT LOL keep the site live

# Commit the changes and push to the deployment repository:
git add .
cat > commit.msg <<EOF
CIBot: update site $(date '+%F %T %Z')
Source: $CIRCLE_REPOSITORY_URL
Commit: $CIRCLE_SHA1
EOF

if git commit -F commit.msg; then
  git --no-pager log -1
  git push origin master
else
  echo "nothing to deploy..."
fi

exit 0
