---
title: Git Crashcourse
tags: musings
---
### Preconfig
configure remote variable we name "origin"  
or remove to reuse variable
``` bash
git remote add origin https://github.com/UserJY/bleh.git
git remote remove origin
```
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
Three Levels to know: commit-references(ref), alias, HEAD   

* ref : static nodes of the commit tree.
* alias : static names that point to the refs
    * alias = name of branch, default branch is "main"
* HEAD : dynamic pointer that can point to alias or refs

Example git tree:

ref(commit_new) ← ref(commit_old) ← ref(commit_oldest)  
↑  
alias("SomeBranchName")  
↑  
HEAD  



| name    | command    |
| --------- | --------- |
|ref | git commit - adds a new ref node.  Moves HEAD to alias of new node|
| alias         | git branch - assign an alias to ref node on new branch. Moves HEAD to new alias|
| HEAD | git checkout - Moves head pointer to either alias or ref|

Structure of git tree  
 
* There is always a main branch
* Creating a new alias = Creating a new branch
* #leafs of the tree = # of current versions


Every git node has a ref that looks like 3bed42g14584b3c0c04fdcc4f503346f92be1003  



We can point HEAD to aliases like "main" with
git checkout main

Switching branch = move HEAD pointer to alias of new branch
git checkout someotherbranch  

git checkout can   
1) switch branch   
2) go to older version  

We can point HEAD to ref like "3bed42g1..." with
git checkout "3bed42g1..."

naked refs = refs without aliases = old commits

**Pointing HEAD to a commit ref directly results in detached HEAD mode.**  
Think of it like sandbox mode.   
Switch back to main, to discard changes.  
git checkout main

alias
To make a new  alias, we use 
git branch bleh

**Why and When do we point HEAD to naked refs**
The newest version has compatibility issues and you want to build off an old version.

transverse git commit tree using the HEAD pointer.
HEAD pointer can point to alias like "main" or refs like "3bed42g..."

"git branch" vs "git checkout -b"  
git branch makes a new alias and branch, BUT HEAD stays on current branch  
git checkout -b behaves the same as git branch BUT moves HEAD to new branch  