---
title: CI/CD in GithubAction
tags: prog, DevOps
toc: y
---


# CI :exec after push local repo to github
CI is used to integrate tests, typically your github action will run tests on the code.

1) Give name
``` yaml
name: SomeRandomName
```
2) Run on push to main branch
``` yaml
on:
	push: 
		branches: [main]

``` 
3) Define the job to start
	Steps) are the instructions to test and run code
			actions/checkout@v2 is what clones the repo to your VM


WARNING 
jobs will run in parallel, steps do not
if you need a job to wait, add a "needs:..." parameter under
``` yaml
jobs:
	build-job:
		...
	publish-job:
		needs:build-job
```
``` yaml
jobs:
	yourJobName:
		runs-on: ubuntu-latest
		steps: 
			- uses: actions/checkout@v2
			- uses: actions/setup-node@v1
			  with:
				node-version: 12
				
			- run: npm ci  <--- npm install
			- run: npm test
			- run: npm run build
```


# CD :exec on merge PullRequest
CD is used to push your updates to some cloud server so the internet can see your new updated website.

You have to connect to servers like AWS to deploy your website
First you need a secret token from AWS
on Github: Settings > Secrets > Add a new secret
Now you can securely access with github action securely
``` yaml
on:
	pull_request:
		branches: [main]
```

``` yaml
jobs:
	yourJobName:
		runs-on: ubuntu-latest
		steps: 
			- uses: actions/checkout@v2
			- uses: actions/setup-node@v1
			  with:
				node-version: 12
				
			- run: npm ci  <--- npm install
			- run: npm test
			- run: npm run build
			- uses: bleh/someAWSaction@main
			  with: 
				args: deploy --only hosting
			  env:
				AWS_TOKEN: ${{ secrets.myAWS_TOKENNAME }}
```

# Conclusion
1. push local dev branch to remote github
2. CI Starts test on new code
3. * if tests fail -> fix bugs
   * if tests succeed -> Ask PR to merge dev with main branch
4. Maintainer accepts PR
5. main branch merges with dev 
6. CD Starts uploading new main branch to servers
7. new update live on internet or server

# Quirks
## Concurrent behavior in run
``` yml
run: |
	 bleh=4
	 echo $bleh
```
The echo will show no output because run will call the two commands concurrently.  
To call sequentially, the commands need to be in two separate run.

## Github environment variable
``` {.yml .numberLines}
echo "somevar=3" >> $GITHUB_ENV
${{ env.somevar }}
$somevar
```
line 2 and 3 are the same   
use $somevar over ${{ env.somevar }}

The local environment is decarded on separate runs, so its best to store variables in $GITHUB_ENV.