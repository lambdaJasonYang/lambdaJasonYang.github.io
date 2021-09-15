---
title: Git Crashcourse
tags: prog
---

WARNING: The terms I use are nonstandard from most other tutorials.

### Preconfig
configure remote variable we name "origin"  
or remove to reuse variable
``` bash
git remote add -u origin https://github.com/UserJY/bleh.git
git remote remove origin
```
the -u flag adds the remote commit to your local git tree.

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

#Merging


git config merge.tool vimdiff


```
error: Merging is not possible because you have unmerged files.
hint: Fix them up in the work tree, and then use 'git add/rm <file>'
hint: as appropriate to mark resolution and make a commit.
fatal: Exiting because of an unresolved conflict.
```
use git merge --abort


To merge a remote repo with local, first checkout remote repo onto local branch
main is the name of the remote branch to fetch from
remoteb is the name of the new branch to create locally

git fetch origin main:remoteb

git mergetool
