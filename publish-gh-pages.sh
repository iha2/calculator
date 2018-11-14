
BRANCH="$(git branch | grep \* | cut -d ' ' -f2)"

shadow-cljs release app
cp -r public/img public/css public/index.html docs/
git reset --hard HEAD
mkdir temp
cp -r docs temp
git checkout gh-pages
cp temp/ .
git add docs
git commit -m "$1"
git push -f origin gh-pages
rm -r temp
git checkout ${BRANCH}
