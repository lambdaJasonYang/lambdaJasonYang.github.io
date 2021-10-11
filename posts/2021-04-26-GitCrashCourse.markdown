---
title: Git Crashcourse
tags: prog
---

WARNING: The terms I use are nonstandard from most other tutorials.

### Setup Credentials

Setting email and username for local repo or global env
```bash
git config user.email "example@example.com"
git config user.name "example"

git config --global user.email "example@example.com"
git config --global user.name "example"
```

### Removing Credentials

``` bash
git config --global  credential.usehttppath true
```

### Saving remote github Credential on local repo
```bash
git config credential.helper store
git push -u origin main:main
```
By pushing to remote repo you will be asked to enter username and password( password is really the auth token you have to generate in github settings).   
The above command makes it so the setting is saved.  


---

### Preconfig Remote Github repo variable
configure remote variable we name "origin"  
or remove to reuse variable
``` bash
git remote remove origin
git remote add origin https://github.com/UserJY/bleh.git
```
the -u flag adds the remote commit to your local git tree.

### Create a new repo locally and push create github repo

```bash
git init
git remote add origin https://github.com/userJY/testrepo.git
git add .
git commit -m 'update'
git push -u origin main:main

---

### On git push and pull

`git push -u origin <SrcLocalBranch>:<TargetRemoteBranch>`{.bash}  
`git pull origin <SrcRemoteBranch>:<TargetLocalBranch>`{.bash}

### The process

``` bash
git add .
git commit -m "some message"
```
updating to remote repo like github
``` bash
git push origin main
```

### Pushing updates to Team repo
``` bash
git checkout -b Fixedbugbranch
git add .
git commit -m "fixed bugs"
git push origin Fixedbugbranch
```
You see a push but on the team's side, they see a pull request(PR).  
PR is ran through github actions(CI system) that runs tests.  
If accepted, they merge your PR branch with the main branch.  

### More depth, Concept
Three layers to know: commit-references(ref), branch-alias(alias), HEAD   

* layer 1 - ref : static nodes of the commit tree.
* layer 2 - alias : static names that point to the refs
    * alias = name of branch, default branch is "main"
* layer 3 - HEAD : dynamic pointer that can point to alias or refs

Example git tree:

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ alias("somebranch")  
\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 🠫  
\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ref(branch_new)  
\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \   🡑  
ref(commit_new) ← ref(commit_old) ← ref(commit_oldest)  
🠩  
alias("main")  
⭫  
HEAD  

Description:  
HEAD(layer 1) points to branch alias(layer 2) which points to   
commit ref(layer 3).  
Aliases(layer 2) always points to the local head(layer 1).  

#### Creating new Aliases = Creating new branches

Old commits(layer 1) aka non-local-head commits do not have aliases(layer 2) pointing to them but if you do make an alias, they will become a local-head(layer 1) by branching.


Do not confuse the "HEAD" pointer with the "local head of a branch".  
The "local head of a branch" refers to the local head of commit-refs at layer 1.    
The "HEAD" pointer refers to the layer 3 pointer.   

| name    | command    |
| --------- | --------- |
|ref (layer 1) | git commit - adds a new ref node.  Moves HEAD to alias of new node|
| alias (layer 2)  | git branch - assign an alias to ref node on new branch. Moves HEAD to new alias|
| HEAD (layer 3) | git checkout - Moves head pointer to either alias or ref|



Every git node has a ref that looks like 3bed42g14584b3c0c04fdcc4f503346f92be1003  

We can point HEAD(layer 3) to aliases(layer 2) like "main" with
git checkout main

Switching branch = move HEAD(layer 3) to alias(layer 2) of new branch
git checkout someotherbranch  

git checkout can   
1) switch branch   
2) go to older version  

**Pointing HEAD(layer 3) directly to ref(layer 1), Detached HEAD mode**
We can point HEAD(layer 3) to ref(layer 1) like "3bed42g1..." with
git checkout "3bed42g1..."

If you modify files, then commit.
Any change will be discarded if you switch back to main (git checkout main)

To make changes permanent, you have to build a branch by making an alias(layer 2) with your commit(layer 1).
git branch SomeName

**Why and When do we point HEAD to naked refs**
The newest version has compatibility issues and you want to build off an old version.

transverse git commit tree using the HEAD pointer.
HEAD pointer can point to alias like "main" or refs like "3bed42g..."

"git branch" vs "git checkout -b"  
git branch makes a new alias and branch, BUT HEAD stays on current branch  
git checkout -b behaves the same as git branch BUT moves HEAD to new branch  

### Extras

tags are just renaming refs, they are NOT at the same level as aliases.
Meaning if you point HEAD to a tag, you are in a detached head state.


In gitk
yellow node HEAD pointer points to
blue untagged node(meaning it is not the local head of a branch)git


--- 

### Merging

##### Git Pull

git pull is how we merge

default setting for gitpull is rebase=false

```bash
git pull --rebase #default merge
git pull --no-rebase
git pull --ff-only #fast-forward-only
```



##### Example

Example:
clone git project first commit 

```bash
commit 0382315a65a3fa24595718b3bf141f09a49b6469 (HEAD -> main, origin/main, origin/HEAD)
Author: JY <example@example.com>
Date:   Mon Sep 27 16:04:23 2021 -0700

    Initial commit
```

0382315a65a3fa24595718b3bf141f09a49b6469 is the commit on both server and local repo.

* Modify README.md locally (write "hello" to README.md)

```bash
$ git log
commit 070f18087be629ba7387a9242af03f85ac656910 (HEAD -> main)
Author: jy <example@example.com>
Date:   Mon Sep 27 16:15:11 2021 -0700

    update

commit 0382315a65a3fa24595718b3bf141f09a49b6469 (origin/main, origin/HEAD)
Author: jy <example@example.com>
Date:   Mon Sep 27 16:04:23 2021 -0700

    Initial commit
```

* Modify README.md on github website. (write "goodbye" to README.md)

remote new head: 3295ca935359bc5fc4e8592c9057d3c3c5cd7a6d


* Notice Remote and Local repo are out of sync now
  * Local repo head: 070f18087be629ba7387a9242af03f85ac656910
  * Remote repo head: 3295ca935359bc5fc4e8592c9057d3c3c5cd7a6d
  * Point of divergence: 0382315a65a3fa24595718b3bf141f09a49b6469

Let's trying pushing local 070f1 changes to remote anyway

```bash
$ git push -u origin main:main
To https://github.com/userJY/gittmp.git
 ! [rejected]        main -> main (fetch first)
error: failed to push some refs to 'https://github.com/userJY/gittmp.git'
hint: Updates were rejected because the remote contains work that you do
hint: not have locally. This is usually caused by another repository pushing
hint: to the same ref. You may want to first integrate the remote changes
hint: (e.g., 'git pull ...') before pushing again.
hint: See the 'Note about fast-forwards' in 'git push --help' for details.
```
It failed as expected  
Let's follow instructions and use git pull

```bash
$ git pull
remote: Enumerating objects: 5, done.
remote: Counting objects: 100% (5/5), done.
remote: Total 3 (delta 0), reused 0 (delta 0), pack-reused 0
Unpacking objects: 100% (3/3), 641 bytes | 91.00 KiB/s, done.
From https://github.com/userJY/gittmp
   0382315..3295ca9  main       -> origin/main
Auto-merging README.md
CONFLICT (content): Merge conflict in README.md
Automatic merge failed; fix conflicts and then commit the result.
```

The above line shows local..remote conflict.  
`   0382315..3295ca9  main       -> origin/main`{.bash}

on our console we see we have been automatically put into MERGE mode.
```bash
User@WindowsPC MINGW64 ~/Desktop/gittmp (main|MERGING)
```
If you enter your README.md you will find the below content while in MERGE mode. 

Pre-Merge state:

```bash
<<<<<< HEAD
hello


Hello 
=======
goodbye

Goodbye 
>>>>>>> 3295ca935359bc5fc4e8592c9057d3c3c5cd7a6d
```

Aside: You can even commit this pre-merge state.  
This will update local README.md to the above content.  
You can even push this new pre-merge state commit to remote github.   But clearly you shouldn't do this as it defeats the entire point of merging!!  


Use abort to leave MERGE mode to return to local repo
```bash
git merge --abort
```


##### To fix up the MERGE conflicts

Go back to MERGE mode
```bash
git pull #back to merge mode
git mergetool #will ask to start default vimdiff
```

##### Non-conflict automerge
image if local was
```bash
hello
```
and remote was
```bash

yello
```
Git pull, would merge by 

```bash
$ git pull
remote: Enumerating objects: 5, done.
remote: Counting objects: 100% (5/5), done.
remote: Compressing objects: 100% (2/2), done.
remote: Total 3 (delta 1), reused 0 (delta 0), pack-reused 0
Unpacking objects: 100% (3/3), 639 bytes | 91.00 KiB/s, done.
From https://github.com/userJY/gittmp
   8401a16..d93f27c  main       -> origin/main
Auto-merging README.md
Merge made by the 'recursive' strategy.
 README.md | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)

```

#### Vim micro tut
ctrl-w-w to switch screen on 3 way commit

esc-u to undo
esc-:diffupdate   after undo to rescan difference

esc-:diffget LO to apply Local, left most window
esc-:diffget remote to apply Remote, right most window
esc-:diffget BA to apply base, top center window

esc-i to insert mode
esc-dd to delete line

esc-]-c to move to next conflict
esc-[-c to move to prev conflict

### Undo last commit

Undo last commit but keep content changes
```bash
git reset --soft HEAD~1
```

Undo last commit, do not keep content change
```bash
git reset --hard HEAD~1
```

If you have unstaged changes you want to remove
```bash
git reset --hard
```

---

### Merging two unrelated histories

Example.  
You made a github repo with a license.  
Then you made a local repo with some web server and used git init.   
Now you want to push your local repo on that "empty" remote repo.  

Not so simple, we first have to merge it but it will give us an error on a naive merge since your local and remote repo have no common ancestor


```bash
git pull origin main --allow-unrelated-histories 
```

