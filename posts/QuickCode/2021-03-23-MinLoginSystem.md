---
title: Min - Login System Express
tags: prog, QuickCode, cloud, frontend
---

```{.js filename=index.js}
const express = require('express')

const jwt = require("jsonwebtoken") //for giving state of logged in
const bcrypt = require("bcrypt") //for hashing, hash-comparison of passwords
const fs = require('fs'); //for read/write files
var bodyParser = require('body-parser') //for extracting form data from POST

const app = express()
const port = 3300


const myHTMLsite = `<!DOCTYPE html>
<h1>Register</h1>
<form action ="/register" method="post">
  <input type="text" name="username" >
  <input type="text" name="password" >
  <input type='submit'>
</form>
<h1>Login</h1>
<form action ="/login" method="post">
  <input type="text" name="username" >
  <input type="text" name="password" >
  <input type='submit'>
</form>

<div>
<h2>Only press when logged IN</h2>
<form action ="/dostuff" method="get">
  <input type='submit' value="do stuff as a logged in member">
</form></div>
JWT is blank if not logged-in
`
let myJWT = ""

//helper functions for reading/writing file
//we wrap them in new promises so we can use `await` on the results

const readFilePromisfied = async (filepath) => {
  return new Promise((resolve, reject) => {
    fs.readFile(filepath, 'utf-8', (err, data) => {
      if (err) { reject(err) };
      resolve(data);
    })
  })
}

const writeFilePromisfied = async (filepath, data) => {
  return new Promise((resolve, reject) => {
    fs.writeFile(filepath, data, (err) => {
      if (err) { reject(err) };
      resolve(1);
    })
  })
}

//Home page that shows Account database and the Current JWT
//Current JWT represents the account we are logged into
app.get('/', async (req, res) => {
  console.log(myJWT)
  //Read DB JSON accounts to ememory
  const rawDB = await readFilePromisfied("accounts.json")
  const dictDB = JSON.parse(rawDB)

  //paint HTML page
  let htmlusers = ""
  dictDB["accounts"].forEach((account) => {
    for (var accprop in account) {
      htmlusers = htmlusers + `<li> ${accprop} : ${account[accprop]} </li>`
    }
  })
  res.send(myHTMLsite +
    `<p>JWT: ${myJWT}</p> ` +
    `<h3>DB JSON accounts </h3><ul>${htmlusers}</ul>`)

})


//Registration ONLY uses bcrypt, DOES NOT use JWT
let registerBodyParser = bodyParser.urlencoded({ extended: false })
app.post('/register', registerBodyParser, async (req, res) => {
  const saltrounds = 1
  //bcrypt hashes the plaintext password request data into passwordHash
  const passwordHash = await bcrypt.hash(req.body.password, saltrounds)

  //DO NOT store the plaintext password
  //ONLY store the passwordhash with the username 
  const newUser = {
    "username": req.body.username,
    "passHash": passwordHash,
  }
  console.log(newUser)

  //Read DB JSON account to memory
  const rawDB = await readFilePromisfied("accounts.json")
  const dictDB = JSON.parse(rawDB)

  //Append the new {username,passHash} object to the JSON account in memory
  const newDB = { "accounts": [...dictDB["accounts"], newUser] }

  //Write from memory to DB JSON
  const status = await writeFilePromisfied("accounts.json", JSON.stringify(newDB))

  //paint HTML page
  res.send(`<a href='/'>home</a> <div>writeFileResult: ${status}</div> <div>${JSON.stringify(newDB)}</div>`);

})


//Login requires Bcrypt to check password, JWT to represent logged in
let loginBodyParser = bodyParser.urlencoded({ extended: false })
app.post('/login', loginBodyParser, async (req, res) => {
  //read DB JSON accounts to memory
  const rawDB = await readFilePromisfied("accounts.json")
  const dictDB = JSON.parse(rawDB)

  //find account DB JSON object by comparing request username
  const targetAccount = dictDB["accounts"].find(account => account.username === req.body.username)

  //check if DB JSON object passhash can be formed out of our request plaintxt password
  const loginResponse = await bcrypt.compare(req.body.password, targetAccount.passHash)
  if (loginResponse === true) {console.log("correct pass")}else {res.send("incorrect pass");return } 

  //Only use username for creating JWT
  const userForToken = {
    "username": targetAccount.username
  }
  const theJWT = jwt.sign(userForToken, "somePrivateKey")

  //paint HTML page
  myJWT = theJWT;
  res.send(`<a href='/'>home</a> 
  <p>bcrypt comparison of the 2 items below passed: <li>password: ${req.body.password}</li> <li>passhash: ${targetAccount.passHash}</li></p>
  <p>${JSON.stringify(userForToken)} was used to build JWT: ${myJWT} </p>`)

})

//Member-Only area when logged in
//Here we only need the JWT to get back the username
app.get("/dostuff", async (req,res) => {
  //verify is how we extract the username from the JWT
  decodedJWT = jwt.verify(myJWT,"somePrivateKey")

  console.log(decodedJWT);
  res.send(`<a href='/'>home</a> 
            ${JSON.stringify(decodedJWT)} you did stuff with this account`)

})


app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
```

```{.json filename="accounts.json"} 
{"accounts":[]}
```


# Concepts

| Code | Concept |
|---| --- |
|`bcrypt.hash(req.body.password, saltrounds)` | **User Registration** new passHash into DB |
|`bcrypt.compare(req.body.password, targetAccount.passHash)` | **User Login Check** check if plaintext password given is valid when processed then compared to passhash|
|`let myJWT = jwt.sign(username, "somePrivateKey")` | **Login Session** given to browser|
| `jwt.verify(myJWT,"somePrivateKey")` | **Member activity** |



* `jwt.verify` is bascially the inverse of `jwt.sign`
  * Example if member needs to update blog, the server will use `jwt.verify` to get username to update blog
  * `jwt.verify` returns username and iat which is just time which JWT was used.