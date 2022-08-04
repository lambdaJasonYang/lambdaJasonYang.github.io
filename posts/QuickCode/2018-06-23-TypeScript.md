---
title: Typescript
tags: prog, cloud, frontend
toc: y
---

* Any project using node_modules requires webpack !

# Example

* installing node_modules in a normal file directory and running a live-server will not work.
  * You need webpack to build and link the node_modules.
  * you will notice something like `import Graph from 'graphology'` and IDE will say it cant find graphology.  
<!--  -->

Webpack will build the `/myproject/src` folder into static files in the `/myproject/out` folder. BUT we must first make the `index.html` manually first.


* myproject/
  * package.json
  * node_modules/
  * dist/
    * index.html
  * src/ 
    * index.js


```bash
mkdir myproject
mkdir src
mkdir dist
touch /myproject/dist/index.html

cd myproject
npm init

npm install --save-dev typescript
npm install --save-dev webpack webpack-cli
npm install sigma graphology
```

```html
<!DOCTYPE html>
<html>
<head>
    <title>hi</title>

</head>
<body>
    <script type="module" defer src="main.js"></script>
    <div id="sigma-container" style="height: 50vh;width: 70vw;"></div>
<h1>hi</h1>
</body>
</html>
```

```{.json filename = tsconfig.json}
{
    "compilerOptions": {
      "target": "ES2015",
      "module": "es2022",
      "strict": true,
      "esModuleInterop": true,
      "skipLibCheck": true,
      "moduleResolution": "node",
      "forceConsistentCasingInFileNames": true,
      "strictPropertyInitialization": false
    },
    "$schema": "https://json.schemastore.org/tsconfig",
    "display": "Recommended"
  }
```


```{.json filename=package.json}
...
    "tsc": "tsc",
    "build": "webpack --mode production",
```

```bash
npm run tsc
npm run build
```

# Node modules



```bash
mkdir someproj
#all npm init is create a single package.json file
npm init

#install node modules 
npm install typescript --save-dev
```
modify package.json "scripts" to include   
`"tsc" : "tsc"` 

create `tsconfig.json`

```{.json filename="tsconfig.json"}
{
  "compilerOptions": {
    "target": "ES2015",
    "module": "es2022",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "moduleResolution": "node",
    "forceConsistentCasingInFileNames": true
  },
  "$schema": "https://json.schemastore.org/tsconfig",
  "display": "Recommended"
}
```

To build index.js from index.tsx, use `npx tsc` or `npm run tsc`

```bash
npm install http-server
```
add to package.json "scripts" `"dev": "http-server ."`


* module
  * es2022 - `import { something } as "somepath"
  * commonjs - `require('something')`
* target - affects output of js code
  * es2022 - newest features but less compatible
* moduleResolution - how your project searches for module files in folder
  * node - use this
  * classic
* lib - 
  * "es2022","dom"
    * "dom" gives us access to browser-based globals like `window` or `document`

# Koans

## Define Interfaces

```typescript

export type User = {
    name: string;
    age: number;
    occupation: string;
};

export const users: User[] = [
    {
        name: 'Max Mustermann',
        age: 25,
        occupation: 'Chimney sweep'
    },
    {
        name: 'Kate Müller',
        age: 23,
        occupation: 'Astronaut'
    }
];

export function logPerson(user: User) {
    console.log(` - ${user.name}, ${user.age}`);
}
```

## Type narrowing

```typescript

interface User {
    name: string;
    age: number;
    occupation: string;
}

interface Admin {
    name: string;
    age: number;
    role: string;
}

export type Person = User | Admin;

export const persons: Person[] = [
    {
        name: 'Max Mustermann',
        age: 25,
        occupation: 'Chimney sweep'
    },
    {
        name: 'Jane Doe',
        age: 32,
        role: 'Administrator'
    },
    {
        name: 'Kate Müller',
        age: 23,
        occupation: 'Astronaut'
    },
    {
        name: 'Bruce Willis',
        age: 64,
        role: 'World saver'
    }
];

export function logPerson(person: Person) {
    let additionalInformation: string;
    if ("role" in person) { //THIS IS OUR EDIT
        additionalInformation = person.role;
    } else {
        additionalInformation = person.occupation;
    }
    console.log(` - ${person.name}, ${person.age}, ${additionalInformation}`);
}
```

```ts
interface User {
    type: 'user';
    name: string;
    age: number;
    occupation: string;
}

interface Admin {
    type: 'admin';
    name: string;
    age: number;
    role: string;
}

export type Person = User | Admin;

export const persons: Person[] = [
    { type: 'user', name: 'Max Mustermann', age: 25, occupation: 'Chimney sweep' },
    { type: 'admin', name: 'Jane Doe', age: 32, role: 'Administrator' },
    { type: 'user', name: 'Kate Müller', age: 23, occupation: 'Astronaut' },
    { type: 'admin', name: 'Bruce Willis', age: 64, role: 'World saver' }
];

export function isAdmin(person: Person) {
    return person.type === 'admin';
}

export function isUser(person: Person) {
    return person.type === 'user';
}

export function logPerson(person: Person) {
    let additionalInformation: string = '';
    if (isAdmin(person)) {
        additionalInformation = (person as Admin).role;
    }
    if (isUser(person)) {
        additionalInformation = (person as User).occupation;
    }
    console.log(` - ${person.name}, ${person.age}, ${additionalInformation}`);
```

```typescript
/*

Intro:

    Time to filter the data! In order to be flexible
    we filter users using a number of criteria and
    return only those matching all of the criteria.
    We don't need Admins yet, we only filter Users.

Exercise:

    Without duplicating type structures, modify
    filterUsers function definition so that we can
    pass only those criteria which are needed,
    and not the whole User information as it is
    required now according to typing.

Higher difficulty bonus exercise:

    Exclude "type" from filter criterias.

*/

interface User {
    type: 'user';
    name: string;
    age: number;
    occupation: string;
}

interface Admin {
    type: 'admin';
    name: string;
    age: number;
    role: string;
}

export type Person = User | Admin;

export const persons: Person[] = [
    { type: 'user', name: 'Max Mustermann', age: 25, occupation: 'Chimney sweep' },
    {
        type: 'admin',
        name: 'Jane Doe',
        age: 32,
        role: 'Administrator'
    },
    {
        type: 'user',
        name: 'Kate Müller',
        age: 23,
        occupation: 'Astronaut'
    },
    {
        type: 'admin',
        name: 'Bruce Willis',
        age: 64,
        role: 'World saver'
    },
    {
        type: 'user',
        name: 'Wilson',
        age: 23,
        occupation: 'Ball'
    },
    {
        type: 'admin',
        name: 'Agent Smith',
        age: 23,
        role: 'Administrator'
    }
];

export const isAdmin = (person: Person): person is Admin => person.type === 'admin';
export const isUser = (person: Person): person is User => person.type === 'user';

export function logPerson(person: Person) {
    let additionalInformation = '';
    if (isAdmin(person)) {
        additionalInformation = person.role;
    }
    if (isUser(person)) {
        additionalInformation = person.occupation;
    }
    console.log(` - ${person.name}, ${person.age}, ${additionalInformation}`);
}

export function filterUsers(persons: Person[], criteria: Partial<User>): User[] {
    return persons.filter(isUser).filter((user) => {
        const criteriaKeys = Object.keys(criteria) as (keyof User)[];
        return criteriaKeys.every((fieldName) => {
            return user[fieldName] === criteria[fieldName];
        });
    });
}

console.log('Users of age 23:');

filterUsers(
    persons,
    {
        age: 23
    }
).forEach(logPerson);

// In case if you are stuck:
// https://www.typescriptlang.org/docs/handbook/utility-types.html
// https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-8.html#predefined-conditional-types

```

# refactor type-casting to generic

```ts
let criteriaKeys = Object.keys(criteria) as (keyof Person)[];


let getObjectKeys = <T>(obj: T) => Object.keys(obj) as (keyof T)[]
let criteriaKeys = getObjectKeys(criteria)
```

