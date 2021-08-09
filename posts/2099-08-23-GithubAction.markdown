---
title: CI/CD in GithubAction
tags: musings
---


CI is used to check when the developer is sending a pull-request. 
CI is used to integrate tests, typically your github action will run tests on the code.

1) Give name
``` yaml
name: SomeRandomName
```
2) Run on pull-requests to the master branch
``` yaml
on:
	pull_request:
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


CD - Continuous deployment - runs after merging a pull-request
CD is used to push your updates to some cloud server so the internet can see your new updated website.

You have to connect to servers like AWS to deploy your website
First you need a secret token from AWS
on Github: Settings > Secrets > Add a new secret
Now you can securely access with github action securely
``` yaml
on:
	push:
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

### Conclusion

if tests fail -> fix bugs
if tests succeed -> continue to CD
CD: runs deploy actions -> new update live on internet