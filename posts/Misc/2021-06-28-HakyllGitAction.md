---
title: Autobuild Hakyll
tags: tech, prog, HakyllSetupSeries, DevOps
---

**Hakyll Setup Series**  

1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-HakyllGitAction.html)
4. [Very Simple Hakyll Pandoc Filtering Example](2021-08-23-PandocFiltering.html)
5. [Add Railroad Syntax to Hakyll](2021-10-01-RailroadSyntax.html)
6. [Table Of Content in Hakyll](2021-10-01-TableOfContent.html)
7. [Hakyll Access on LAN server](2021-11-07-HakyllAccessOnLAN.html)

# python scripting

`python -c “print ('\xef' * 20)”` to run python in bash without needing to save a file  

`echo 6 | python -c "a=input(); print(a)"` cool way to pipe output to python

# Autogenerate static site on git push to github

Building a Hakyll site from source takes an hour the first time you do it.  
Once it is cached, it only takes a few seconds.  

But what if you wanted to update your blog post while away from your personal computer.  
If only we could just push the markdown file to github and have the project build itself.

And we can with github action!  
Simply make a ".github\\workflows" folder in your root hakyll directory and make a new *.yml file and copy the code below then place it in the workflows folder.

This github action will automatically find the name of your Hakyll project to build.   
It will also cache the workspace so the next build will a fraction of the time on github servers.  


``` {.yml .numberLines}
# UserJY - Github action to autobuild Hakyll static pages on push

name: Autobuild Hakyll Site

# Controls when the workflow will run
on:
  push:
    branches: [main]
  # pull_request:
  #   branches: [ main ]

  # Allows you to run this workflow manually from the Actions tab
  workflow_dispatch:

# A workflow run is made up of one or more jobs that can run sequentially or in parallel
jobs:
  # This workflow contains a single job called "build"
  build:
    # The type of runner that the job will run on
    runs-on: ubuntu-latest

    # Steps represent a sequence of tasks that will be executed as part of the job
    steps:
      # Checks-out your repository under $GITHUB_WORKSPACE, so your job can access it
      - uses: actions/checkout@v2

      - name: step 1
        run: |
             pwd
             ls

      - uses: actions/cache@v2
        with:
          key: ${{ runner.os }}-stack-${{ hashFiles('**/stack.yaml.lock') }}
          path: |
            ~/.stack
            ~/.stack-work
          restore-keys: |
            ${{ runner.os }}-stack-

      - name: step 2
        run: |
             pwd
             ls

      - uses: haskell/actions/setup@v1
        with:
          ghc-version: '8.8.3' # Exact version of ghc to use
          # cabal-version: 'latest'. Omitted, but defaults to 'latest'
          enable-stack: true
          stack-version: 'latest'

      - name: step 3
        run: |
             pwd
             ls

      - name: Autofinding hakyll blog full path
        run: |
            echo "tpath=$( find ${{ github.workspace }} -name '*.cabal' | head )" >> $GITHUB_ENV
           
      - name: Detect executable name through cabal file
        run: |
            echo "autotarget=$(grep "executable" $tpath | cut -d' ' -f 2)" >> $GITHUB_ENV
            

      - name: Checking
        run: echo $autotarget
      #- run: stack --system-ghc build --only-dependencies
      
      - name: Stack Installing hakyll environment
        run: stack install --fast --flag hakyll:-previewserver --flag hakyll:-watchserver

      - name: step 4
        run: |
             pwd
             ls
             
            
      - name: Cleaning hakyll
        run: stack exec -- $autotarget clean
            
      - name: Building hakyll static pages
        run: stack exec -- $autotarget build

      - name: step 5
        run: |
             pwd
             ls

      # Runs a set of commands using the runners shell
      - name: commit built site
        run: |
           git config user.name github-actions
           git config user.email github-actions@github.com
           git add .
           if ! git diff-index --quiet HEAD --; then
              git commit -m "Autobuild Hakyll site github action commit"
              git push
              echo "pushing changes"
           fi
```

Now you can update your blog anywhere, just  
1. pull your hakyll repo from github  
2. add your new markdown file to your posts folder  
3. push it to your hakyll repo **WITHOUT NEEDING TO SPEND 1 HOUR BUILDING IT BEFOREHAND**  

All this will take 2 minutes at most.   