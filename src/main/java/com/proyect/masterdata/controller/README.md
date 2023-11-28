# Controllers documentation

## Overview
API for powip backend

## Endpoints

### POST /category
- Description : add one category to the database
- Request: none
- Parameters : 

1. name : name of category
2. description : description of the category
3. user : username of the user who creates category

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/category?name=tennis&description=zapatos%20para%20correr&user=admin1

### POST /category/categories
- Description : add multiple categories to the database
- Request :

[

    {
        "name":"name of the category",
        "description:"description of the category"
    },
    ...
]

- Parameters : 

1. user : username of the user who creates categories

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

Url

http://localhost:8080/masterdata/category/categories?user=admin1

Body

[
    {

        "name":"camisetas",
        "description":"manga corta sin cuello"
    },
    {

        "name":"blusas",
        "description":"de lana sin mangas"
    }
]

### POST /closing-channel

- Description : add one closing channel to the database

- Request : none

- Parameters : 

1. name : name of the closing channel
2. tokenUser : username of the user who creates closing channel

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/closing-channel?name=instagram&tokenUser=admin1

### POST /entry-channel

- Description : add one entry channel to the database

- Request : none

- Parameters : 

1. name : name of the entry channel
2. tokenUser : username of the user who creates entry channel

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/entry-channel?name=facebook&tokenUser=admin1

### POST /auth/login

- Description : log the user in the backend and retrieves a token

- Request : 

{

    "username": username,
    "password": password
}

- Parameters : none

- Response : 

{

    "user": ... user data,
    "jwt": token that authenticates the user
}

- Example :

Url

http://localhost:8080/masterdata/auth/login

Body

{

    "username":"admin1",
    "password":"123abc+"
}

### POST /store-type

- Description : add one store type to the database

- Request : none

- Parameters : 

1. name : name of the store type
2. tokenUser : username of the user who creates store type 

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/store-type?name=shopify&tokenUser=admin1