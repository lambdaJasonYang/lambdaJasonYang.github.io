---
title: Autobuild Nextjs github pages
tags: tech, prog, CICD
---

add to package.json
```json

  "scripts": {
    "dev": "next dev",
    "build": "next build",
    "start": "next start",
    "lint": "next lint",
    "export" : "next export"
  }
```

npm export builds static pages into the out folder


```yml
  steps:
  ....
  ....
    - run: npm run export
    - run: touch ./out/.nojekyll
    
    
    - name: Deploy 🚀
      uses: JamesIves/github-pages-deploy-action@4.1.5
      with:
        branch: gh-pages # The branch the action should deploy to.
        folder: out # The folder the action should deploy.
```

on your github.com/<username>/<repo>/settings/pages

Select Source
Choose "Branch: gh-pages"  "/ (root)"
Save


### Follow through


in your next.config.js 

```js
module.exports = {
  basePath: '/reponame',
  assetPrefix: '/reponame'
}
```


You may notice GET ERR_ABORTED 404 on your www.github.io/repoName/_next/...




Github runs jekyll on every underscore folder including "_next" folders which we don't want
create a ".nojekyll" file, we already did that for you in the github actions command