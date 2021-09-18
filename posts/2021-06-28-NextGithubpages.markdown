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
