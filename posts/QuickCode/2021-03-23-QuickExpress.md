---
title: Quick Express
tags: prog, QuickCode, cloud, frontend
---

# POST request

* update means you cant natively grab data from POST request
* you require a `bodyParser`
```js
let urlencodedParser = bodyParser.urlencoded({ extended: false })
app.post('/register', urlencodedParser, async (req,res) => {..}
```

# async await

* await just wraps the rest of the codeblock inside a `.then(()=>...)`
* the below 2 code are equivalent

```js
await f()
console.log('hi')
```

```js
f.then(()=> console.log('hi'))
```

* async/await REQUIRE functions that return promises

```js
//BAD - DOESNT WORK 
  const rawDB = await fs.readFile("accounts.json",(err,data) => { return data})
  console.log(rawDB);
```

```js
//GOOD - we wrap the readFile into a promise
const readFilePromisfied = async (filepath) => {
  return new Promise((resolve,reject) => {
    fs.readFile(filepath,'utf-8', (err,data)=>{
      if (err) {reject(err)};
      resolve(data);
    })
  })
}

const rawDB = awaitreadFilePromisfied("accounts.json");
console.log(rawDB);
```


# Login Registration system

* Password Hashing is only done on server-side
  * This way even if a hacker stole the hashed password: 
    * Attempt 1: Enter hashed-pass into client login, -> double hashed password which fails login
    * Attempt 2: Tries to get plaintext pass but can't due to hash being one-way function
<!--  -->
* Server Registration system 
  * `bcrypt.hash(plaintextpass,saltrounds)` is stored with the newly registered username
* Server login system 
  * search record for submitted login username then compare pass 
  * `bcrypt.compare(plaintextpass,hashedpass)` for comparing plaintext pass submission and DB hashed pass
  * `jwt.sign(username)` for giving access to member-services

Notice **BOTH** Registration and Login require bcrypt for hashing and hashcomparison.   
**ONLY** Login requires JWT for stateful access to member-services.  


# Dynamic routes

* data is stored in `req.params.____`
* all routes need `res.send()` or else any visitor will get a (pending)  

```js
app.get('/:bleh', (req, res) => {
  console.log(req.params.bleh)
  res.send(req.params.bleh)
})
```

